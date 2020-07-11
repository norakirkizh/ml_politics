import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import wordsegment
import re
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

test = pd.read_csv('test_subset.csv', sep=';')
# I removed urls with duration <= 5 sec. and domains after "." in the subsetting code


def convert(string):  # first, tokenizer function:
    li = list(string.split(" "))
    return li


test['domain'] = test['domain'].apply(lambda x: convert(x))  # word tokenizer

#  Matrix for Domains
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

# domain_array = domain_matrix.toarray()

domains_indices = list(d_vocabulary.items())  # domains with indices
domains_indices = pd.DataFrame(domains_indices)
domains_indices.to_csv('domain_names.csv', index=False, header=True, sep=';', encoding='utf-8')

domain_panelist_id = df_domain.index.tolist()  # panelists_id with indices
domain_panelist_id = pd.DataFrame(domain_panelist_id)
domain_panelist_id = domain_panelist_id.to_csv('domain_panelist_id.csv',
                                               index=False, header=True, sep=';', encoding='utf-8')

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


# preprocessing with segmented urls

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
