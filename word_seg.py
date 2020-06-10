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

load()

df = pd.read_csv('germany_url100.csv', sep=';')
prt = pd.read_csv('party_ids.csv')
dta = pd.merge(df, prt, on='panelist_id')
dta = dta.loc[dta['panelist_id'] != '83b2cafc8731c8fb']  # remove an outlier
data = dta[['group', 'panelist_id', 'url', 'domain', 'duration', 'used_at']]  # df with variables of interest

# test df for word segmentation
test = data.iloc[0:200, :].copy()  # subset 200 urls
test = test[test['duration'] > 5]  # remove urls with duration <= 5

# test['split_url'] = test['url'].apply(lambda x: x.split("/", 1)[-1])  # remove domains from urls
# don't remove websites' domains because they can contain important keywords while urls don't tell much like netflix

test['tokens'] = test['url'].apply(lambda x: segment(x))  # word segmentation
# remove strings that contain digits
test['tokens'] = test['tokens'].apply(lambda x: [y for y in x if not any(c.isdigit() for c in y)])
# remove single letter strings
test['tokens'] = test['tokens'].apply(lambda x: [[i for i in x if len(i) > 1]])

# remove stop words
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
# we should create stop words for url specifics:
# id, http, www, de, com, pc, web, app, login, aspx, ref, redirect, registry
