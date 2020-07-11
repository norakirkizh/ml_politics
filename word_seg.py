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

test = pd.read_csv('test_subset.csv', sep=';')
# I removed urls with duration <= 5 sec. and domains after "." in the subsetting code

test['tokens'] = test['url'].apply(lambda x: segment(x))  # word segmentation

test.to_csv('segmented_urls.csv', index=False, header=True, sep=';', encoding='utf-8')
