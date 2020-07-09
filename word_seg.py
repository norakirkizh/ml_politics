import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import wordsegment
import re
from wordsegment import load, segment
import nltk
from nltk.corpus import stopwords
import ssl
from scipy.sparse import csr_matrix

import datetime
import statsmodels.api as sm
from scipy import sparse
from scipy.stats.stats import pearsonr
from sklearn.decomposition import LatentDirichletAllocation
from sklearn.model_selection import train_test_split
from sklearn.model_selection import KFold
from sklearn.metrics import roc_auc_score

load()

df = pd.read_csv('germany_url100.csv', sep=';')
prt = pd.read_csv('party_ids.csv')
dta = pd.merge(df, prt, on='panelist_id')
dta = dta.loc[dta['panelist_id'] != '83b2cafc8731c8fb']  # remove an outlier
data = dta[['group', 'panelist_id', 'url', 'domain', 'duration', 'used_at']]  # df with variables of interest
data = data[data['duration'] > 5]  # remove urls with duration <= 5 sec. (sufficient?)
data['domain'] = data['domain'].str.split(".").str[0]  # remove domains, after "."

#  LDA for Domains

# test = data.iloc[0:10000, :].copy()  # subset 10,000 urls
test = data.sample(frac=0.02, replace=True, random_state=1)  # 2% of 'data' df

# test['split_url'] = test['url'].apply(lambda x: x.split("/", 1)[-1]) # remove domains from urls
# don't remove websites' domains names because they can contain important keywords while urls don't
# tell much like netflix

test['tokens'] = test['url'].apply(lambda x: segment(x))  # word segmentation
# remove strings that contain digits:
test['tokens'] = test['tokens'].apply(lambda x: [y for y in x if not any(c.isdigit() for c in y)])
# remove single or two letter strings:
test['tokens'] = test['tokens'].apply(lambda x: [[i for i in x if len(i) > 2]])

# remove stop words:

# to validate a certificate
try:
    _create_unverified_https_context = ssl._create_unverified_context
except AttributeError:
    pass
else:
    ssl._create_default_https_context = _create_unverified_https_context

nltk.download('stopwords')
stop_g = stopwords.words('german')
stop_e = stopwords.words('english')
test['tokens'] = test['tokens'].apply(lambda x: [item for item in x if item not in stop_g])
test['tokens'] = test['tokens'].apply(lambda x: [item for item in x if item not in stop_e])
test['tokens'] = test['tokens'].str[0]  # remove one level of list

# stop words for texts does not make much sense for urls because urls are full of technical acronyms.
# we should create stop words for url specifics.
# dictionary for stop words from the web technical details:
# homepage, id, http, www, com, web, app, login, sign, page,
# aspx, ref, redirect, registry, php, index, html, redirect, encoding, anmelden, app


# constructing matrix:

df_m = test.groupby('panelist_id')['tokens'].agg(sum).to_frame()

m_dta = df_m['tokens'].values
indptr = [0]
indices = []
data = []
vocabulary = {}

for d in m_dta:
    for term in d:
        index = vocabulary.setdefault(term, len(vocabulary))
        indices.append(index)
        data.append(1)
    indptr.append(len(indices))

matrix = csr_matrix((data, indices, indptr), dtype=int)  # the main sparse matrix
m_array = matrix.toarray()
words = list(vocabulary.items())  # words with indices
words = pd.DataFrame(words)
panelist_id = df_m.index.tolist()  # panelists_id with indices
panelist_id = pd.DataFrame(panelist_id)

#  reducing the data - removing rare url words from the matrix:
word_names = words[0]
user_id = panelist_id[0]

rows = 50  # 50
cols = 10  # 150

shape = (-1, -1)
while matrix.shape != shape:
    shape = matrix.shape
    # create masks that fulfill the criteria
    user_mask = matrix.sum(1).A1 >= rows
    word_mask = matrix.sum(0).A1 >= cols
    # reduce the matrix
    matrix = matrix[user_mask][:, word_mask]
    # remove user_row and likes to keep the correspondence to the matrix
    user_id = user_id[user_mask]
    word_names = word_names[word_mask]

word_names = word_names.reset_index(drop=True)
user_id = user_id.reset_index(drop=True)

# delta according to Kosinski should be 200 / m.shape[1],
# but they use 0.1
# not sure about the gibbs method in Python but there is this parameter
k = 5
lda = LatentDirichletAllocation(n_components=k,
                                # alpha
                                doc_topic_prior=50 / k,
                                # delta
                                topic_word_prior=200 / matrix.shape[1],
                                # learning method
                                learning_method='online',
                                # randomization seed
                                random_state=0)

lda.fit(matrix)
# get topics for some given samples:
# lda.transform(m[-2:])
m_t = lda.transform(matrix)


# Display topics:
def display_topics(model, feature_names, no_top_words):
    for topic_idx, topic in enumerate(model.components_):
        print("Topic:" + str(topic_idx))
        print("\t" + "\n\t".join([feature_names[i]
                                  for i in topic.argsort()[:-no_top_words - 1:-1]]))


display_topics(lda, word_names, 5)


#  LDA for Domains
print(test['domain'].split())


# tokenizer function:
def convert(string):
    li = list(string.split(""))
    return li


test['domain'] = test['domain'].apply(lambda x: convert(x))  # word tokenizer
df_domain = test.groupby('panelist_id')['domain'].agg(sum).to_frame()

domains = df_domain['domain'].values
d_indptr = [0]
d_indices = []
d_data = []
d_vocabulary = {}

for d in domains:
    for term in d:
        index = d_vocabulary.setdefault(term, len(d_vocabulary))
        d_indices.append(index)
        d_data.append(1)
    d_indptr.append(len(d_indices))

domain_matrix = csr_matrix((d_data, d_indices, d_indptr), dtype=int)  # the main sparse matrix
domain_array = domain_matrix.toarray()
domains_indices = list(d_vocabulary.items())  # domains with indices
domains_indices = pd.DataFrame(domains_indices)
domain_panelist_id = df_domain.index.tolist()  # panelists_id with indices
domain_panelist_id = pd.DataFrame(domain_panelist_id)

#  reducing the data - removing rare url words from the matrix:
dom_pan_id = domain_panelist_id[0]
domain_names = domains_indices[0]

rows = 10  # 50
cols = 10  # 150


shape = (-1, -1)
while domain_matrix.shape != shape:
    shape = domain_matrix.shape
    # create masks that fulfill the criteria
    user_mask = domain_matrix.sum(1).A1 >= rows
    domain_mask = domain_matrix.sum(0).A1 >= cols
    # reduce the matrix
    domain_matrix = domain_matrix[user_mask][:, domain_mask]
    # remove user_row and likes to keep the correspondence to the matrix
    dom_pan_id = dom_pan_id[user_mask]
    domain_names = domain_names[domain_mask]


domain_names = domain_names.reset_index(drop=True)
dom_pan_id = dom_pan_id.reset_index(drop=True)

k = 3
lda = LatentDirichletAllocation(n_components=k,
                                # alpha
                                doc_topic_prior=50 / k,
                                # delta
                                topic_word_prior=200 / domain_matrix.shape[1],
                                # learning method
                                learning_method='online',
                                # randomization seed
                                random_state=0)

lda.fit(domain_matrix)
m_t = lda.transform(domain_matrix)
display_topics(lda, domain_names, 10)  # defined earlier above

# to build a network with domains: PRRV vs nonPRRV
