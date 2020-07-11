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

test = pd.read_csv('segmented_urls.csv', sep=';')

# Later: test['split_url'] = test['url'].apply(lambda x: x.split("/", 1)[-1]) # remove domains from urls
# We don't remove websites' domains names because they can contain important keywords while urls don't
# tell much like netflix

# remove digits from strings:
test['tokens'] = test['tokens'].str.replace('\d+', '')
# test['tokens'] = test['tokens'].apply(lambda x: [y for y in x if not any(c.isdigit() for c in y)])
# remove single or two letter strings:
# test['tokens'] = test['tokens'].apply(lambda x: [[i for i in x if len(i) > 2]])


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

# stop words for texts does not make much sense for urls because urls are full of technical acronyms.
# we should create stop words for url specifics.
# dictionary for stop words from the web technical details:
# homepage, id, http, www, com, web, app, login, sign, page,
# aspx, ref, redirect, registry, php, index, html, redirect, encoding, anmelden, app

test['tokens'] = test['tokens'].str[0]  # remove one level of list

