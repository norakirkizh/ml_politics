## Machine Learning Application to Study Political Attitudes

In this repository I share replication materials for the article "Identifying Political Attitudes from Web Browsing Histories:
Machine Learning Approach".

The paper aims to introduce machine learning approach to identify political attitudes based on peoples' website choices. Specifically, I use web tracking data of 1,000 German voters generated by them after three months of tracking. I propose two methods: SVD and categorization of websites based on existing domains. When matching website domains with existing categories from [Webshrinker](https://webshrinker.com/), each category represents a predicting variable in a regression. I use the following regressions: ElasticNet and Random Forest.

The list of available replication materials:

#### Method I: Categorized website domains and ML models

- [Table](https://github.com/norakirkizh/ml_politics/blob/master/domain_categories-v2.csv) with domain categories from Webshrinker that we managed to match with domains from our initial web tracking data.
- [R code](https://github.com/norakirkizh/ml_politics/blob/master/category_stat.R) for exploring domain categories from Webshrinker: A [table](https://github.com/norakirkizh/ml_politics/blob/master/Sum_of_visits.csv) with descriptive statistics like sum of visits by group of domain categories;
- Distribution plots: [code](https://github.com/norakirkizh/ml_politics/blob/master/distribution_plot.r).
- Plots with descriptive OLS estimates, with controlls: [Selected](https://github.com/norakirkizh/ml_politics/blob/master/combined.pdf) political attitudes and domain categories, [the rest](https://github.com/norakirkizh/ml_politics/blob/master/combined_appendix.pdf) of the political attitudes;
- [Plots](https://github.com/norakirkizh/ml_politics/blob/master/combined_appendix.pdf) with OLS estimates with controlls for the rest of the political attitudes;
- OLS, Random Forest and ElasticNet summary [plot](https://github.com/norakirkizh/ml_politics/blob/master/R2_corr.pdf): Pearson correlations and R2 for all political attitudes.

Additionally, plots for validation of web tracking data: [browsing behavior](https://github.com/norakirkizh/ml_politics/blob/master/ivw_germany.pdf) and [privacy policy](https://github.com/norakirkizh/ml_politics/blob/master/plot_privacy_noad.pdf) of web tracking vs national German panel.
