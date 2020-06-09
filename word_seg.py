import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import wordsegment
import re
from wordsegment import load, segment
load()

df = pd.read_csv('germany_url100.csv', sep=';')
prt = pd.read_csv('party_ids.csv')
dta = pd.merge(df, prt, on='panelist_id')
dta = dta.loc[dta['panelist_id'] != '83b2cafc8731c8fb']  # remove an outlier
data = dta[['group', 'panelist_id', 'url', 'domain', 'duration', 'used_at']]  # df with variables of interest

# test df for word segmentation
test = data.iloc[0:200, :].copy()  # subset 200 urls
test = test[test['duration'] > 5]  # remove urls with duration <= 5
test['split_url'] = test['url'].apply(lambda x: x.split("/", 1)[-1])  # # remove domains from urls
test['tokens'] = test['split_url'].apply(lambda x: segment(x))  # word segmentation


test['tokens'] = [x for x in test['tokens'] if not any(c.isdigit() for c in x)]
