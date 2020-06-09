import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import wordsegment
from wordsegment import load, segment

sns.set(style="darkgrid")

df = pd.read_csv('germany_url100.csv', sep=';')
prt = pd.read_csv('party_ids.csv')
dta = pd.merge(df, prt, on='panelist_id')
dta = dta.loc[dta['panelist_id'] != '83b2cafc8731c8fb']  # remove an outlier
data = dta[['group', 'panelist_id', 'url', 'domain', 'duration', 'used_at']]  # df with variables of interest

# summary stat by respondents id
data['n_visits'] = data.groupby('panelist_id')["url"].transform('count')
data['mean_duration'] = data.groupby('panelist_id')["duration"].transform('mean')
data['std_duration'] = data.groupby('panelist_id')["duration"].transform('std')
data['median_duration'] = data.groupby('panelist_id')["duration"].transform('median')
data['unique_domains'] = data.groupby('panelist_id')['domain'].transform('nunique')
data['unique_urls'] = data.groupby('panelist_id')['url'].transform('nunique')
data['mean_visits_per_uniquedomain'] = data['n_visits'] / data['unique_domains']
data['mean_visits_per_uniqueurl'] = data['n_visits'] / data['unique_urls']

data['domain'] = data['domain'].str.split(".").str[0]  # remove domains, after "."

# df with unique domain visits by respondents id
dm = data.groupby(['panelist_id', 'domain']).size()  # count visits per unique domain
dm = dm.reset_index()  # from series to df
dm = dm.rename(columns={0: 'domain_visits'})  # rename a column

# mapping party id to panelist is
d = prt.set_index('panelist_id')['group'].to_dict()
dm['group'] = dm['panelist_id'].map(d)

# mapping n-visits to panelist_id
v = data.set_index('panelist_id')['n_visits'].to_dict()
dm['n_visits'] = dm['panelist_id'].map(v)

# visits by domain by prt id
dm['sum_domain_visits'] = dm.groupby(['domain', 'group'])['domain_visits'].transform('sum')

# share of visits to domains of total n_visits per panelist id
dm['domain_share'] = dm['domain_visits']/dm['n_visits']
# count domains with largest shares per panelist id
dm['shares_recode'] = dm['domain_share']
dm['shares_recode'] = np.where(dm['shares_recode'].between(0, 0.05), 0, dm['shares_recode'])
(dm['shares_recode'] != 0).sum().sum()
# 137 web sites with share > 0.05 out of 8531 unique domains and 32 panelists
# so on average four web sites with share > 0.05 per respondent

# subset by prt id
prrv = dm[dm['group'] == 'PRRV']
nonprrv = dm[dm['group'] == 'nonPRVV']
nonprrv.group.value_counts()

# domain with largest share by party id
(prrv['shares_recode'] != 0).sum().sum()  # 41 for 11 respondents: 3.7
(nonprrv['shares_recode'] != 0).sum().sum()  # 96 for 21 respondents: 4.6 per respondents

# sort sum visits per domain
prrv_sorted = prrv.sort_values(['sum_domain_visits'])
nonprrv_sorted = nonprrv.sort_values(['sum_domain_visits'])

# unique users per domain
prrv_sorted['unique_users'] = prrv_sorted.groupby('domain')['panelist_id'].transform('count')
nonprrv_sorted['unique_users'] = nonprrv_sorted.groupby('domain')['panelist_id'].transform('count')

# rank score per domain
prrv_sorted['rank_score'] = (prrv_sorted['sum_domain_visits'] * prrv_sorted['unique_users']) / 10000
nonprrv_sorted['rank_score'] = (nonprrv_sorted['sum_domain_visits'] * nonprrv_sorted['unique_users']) / 10000
# sort rank scores
domain_prrv = prrv_sorted.sort_values(['rank_score'])
domain_nonprrv = nonprrv_sorted.sort_values(['rank_score'])

# select vars of interest
rank_prrv = domain_prrv[['group', 'domain', 'rank_score']]
rank_nonprrv = domain_nonprrv[['group', 'domain', 'rank_score']]

# top 50 domains per prt id
rank_prrv = rank_prrv.drop_duplicates(['domain'])
rank_nonprrv = rank_nonprrv.drop_duplicates(['domain'])
rank_prrv.nlargest(50, 'rank_score')
rank_nonprrv.nlargest(50, 'rank_score')

# density plot for mean and median duration
data.groupby(pd.Grouper('group'))['mean_duration'].plot.kde(legend=True, bw_method=0.5)
data.groupby(pd.Grouper('group'))['median_duration'].plot.kde(legend=True, bw_method=0.5)
plt.xlabel('Duration (sec.)')
plt.ylabel('Density')
plt.show()

# density plot for n visits
data.groupby(pd.Grouper('group'))['n_visits'].plot.kde(legend=True, bw_method=0.5)
plt.xlabel("N visits")
plt.ylabel('Density')
plt.show()

# density plots of n visits to unique domains per panelist id
dm.groupby(pd.Grouper('panelist_id'))['domain_visits'].plot.kde(bw_method=0.03, xlim=(-20, 50), color='gray',
                                                                linewidth=2, alpha=0.2)
plt.xlabel('Visits per unique domain')
plt.ylabel('Density')
plt.show()

# density plot for prrv
prrv.groupby(pd.Grouper('panelist_id'))['domain_visits'].plot.kde(bw_method=0.05, xlim=(-50, 100), color='blue',
                                                                  linewidth=2, alpha=0.3)
nonprrv.groupby(pd.Grouper('panelist_id'))['domain_visits'].plot.kde(bw_method=0.05, xlim=(-50, 100), color='red',
                                                                     linewidth=2, alpha=0.1)
prrv['domain_visits'].plot.kde(bw_method=0.001, xlim=(-50, 100), linewidth=3, color='blue')
nonprrv['domain_visits'].plot.kde(bw_method=0.001, xlim=(-50, 100), linewidth=3, color='red')
plt.xlabel('Visits per unique domain')
plt.title("PRR vs. nonPRR voters")
plt.ylabel('Density')
plt.show()

# duration by domain per panelist id
data['duration_sum_domain'] = data.groupby(['domain', 'group'])['duration'].transform('sum')
dd = data[['group', 'domain', 'duration_sum_domain']]
dd = dd.drop_duplicates(subset=['domain', 'group'])
dd.groupby(['group'])['duration_sum_domain'].nlargest(10)  # domains with largest duration by prt id

# timing
time = data[['group', 'panelist_id', 'used_at', ]]
time['day'], time['time'] = data['used_at'].str.split(' ', 1).str

day = time.groupby(['panelist_id', 'day']).size()  # number of visits per day by panelist
day = day.reset_index()
day = day.rename(columns={0: 'visits_perday'})
day['group'] = day['panelist_id'].map(d)

timing = sns.relplot(x='day', y='visits_perday', kind='line', data=day)
timing.fig.autofmt_xdate()
plt.show()

day['visits_perday'].plot(linewidth=0.5);
plt.show()

# time series plot for visits per day by panelist id
test_id = day.pivot(index='day', columns='panelist_id', values='visits_perday')
test_id.plot(legend=False, color='gray', alpha=0.3)
plt.show()

# time series plot for visits per day by prt id
test_prt = day.pivot(index='day', columns='group', values='visits_perday')
test_prt.plot(legend=False, color='gray', alpha=0.3)
plt.show()

