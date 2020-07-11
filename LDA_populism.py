import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import wordsegment
import re
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

#  LDA for Domains
# delta according to Kosinski should be 200 / m.shape[1],
# but they use 0.1
# not sure about the gibbs method in Python but there is this parameter
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


# Display topics:
def display_topics(model, feature_names, no_top_words):
    for topic_idx, topic in enumerate(model.components_):
        print("Topic:" + str(topic_idx))
        print("\t" + "\n\t".join([feature_names[i]
                                  for i in topic.argsort()[:-no_top_words - 1:-1]]))


display_topics(lda, domain_names, 10)
