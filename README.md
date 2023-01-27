# ml_politics
Machine Learning Application to Study Political Attitudes

In this repository I share replication materials for the article "Identifying Political Attitudes from Web Browsing Behavior:
Machine Learning Approach" which is available in a working paper form on [Overleaf](https://www.overleaf.com/read/vfpdgvfbmzkc).

The paper aims to introduce machine learning approach to identify political attitudes based on peoples' website choices. Specifically, I use web tracking data of 1,000 German voters generated by them after three months of tracking. I propose two methods: SVD and categorization of websites based on existing domains. When matching website domains with existing categories from Webshrinker, each category represents a predicting variable in a regression. I use the following regressions: ElasticNet and Random Forest.

The list of available replication materials:

- [Table](https://github.com/norakirkizh/ml_politics/blob/master/domain_categories-v2.csv) with domain categories from Webshrinker that we managed to match with domains from our initial web tracking data.
- [R code](https://github.com/norakirkizh/ml_politics/blob/master/category_stat.R) for exploring domain categories from Webshrinker: a [table](https://github.com/norakirkizh/ml_politics/blob/master/Sum_of_visits.csv) with descriptive statistics like sum of visits by group of domain categories;
- R code with regressions based on domain categories;
- R code with visualizations of regression outputs.

Additionally, plots for validation of web tracking data: [browsing behavior](https://github.com/norakirkizh/ml_politics/blob/master/ivw_germany.pdf) and [privacy policy] of web tracking vs national German panel. 
