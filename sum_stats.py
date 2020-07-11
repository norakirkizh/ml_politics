import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.sparse import csr_matrix
import math

data = pd.read_csv('germany_url100.csv', sep=';')
list(data.columns)
data = data[['panelist_id', 'url', 'domain', 'used_at', 'duration']]
data['domain'] = data['domain'].str.split(".").str[0]
len(data)  # 1187552
data.drop(data[data.duration < 5].index, inplace=True)
len(data)  # 697130
# data.shape

domains = data[['panelist_id', 'domain']]
domains['domain'].nunique()

dom = domains.drop_duplicates(subset=['panelist_id', 'domain'], keep='first')
dom.groupby('panelist_id').domain.size()

domains.drop_duplicates(subset='panelist_id')
users = pd.DataFrame(domains.drop_duplicates(subset='panelist_id'))
users = users[['panelist_id']]
domains_index = pd.DataFrame(domains.drop_duplicates(subset='domain'))
domains_index = domains_index[['domain']]

dom['user_row'] = users.reset_index().set_index('panelist_id').loc[dom.panelist_id, 'index'].values
dom['domain_row'] = domains_index.reset_index().set_index('domain').loc[dom.domain, 'index'].values

m = csr_matrix(([1 for x in range(len(dom))], (dom['user_row'], dom['domain_row'])))
m.shape

np.asarray(m)
(m > 0).sum()

# trim the data

user_ids = users.panelist_id
domain_names = domains_index.domain

# Remove users/domains occurring less than 50/150 times
rows = 50  # min 50 domains per user
cols = 5  # min 5 users per domain

shape = (-1, -1)
while m.shape != shape:
    shape = m.shape

    # create masks that fulfill the criteria
    user_mask = m.sum(1).A1 >= rows
    domain_mask = m.sum(0).A1 >= cols

    # trim the matrix
    m = m[user_mask][:, domain_mask]

    # remove user_row and domains to keep the correspondence to the matrix
    user_ids = user_ids[user_mask]
    domain_names = domain_names[domain_mask]

domain_names = domain_names.reset_index(drop=True)
user_ids = user_ids.reset_index(drop=True)
reduced_users = users[users.userid.isin(user_ids)].reset_index(drop=True)
