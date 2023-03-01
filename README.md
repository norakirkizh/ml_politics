## Machine Learning Application to Predict Political Attitudes from Web Browsing Histories

In this repository I share replication materials for the article "Predicting Political Attitudes from Web Browsing Histories:
Machine Learning Approach".

The paper aims to introduce machine learning approach to identify political attitudes based on peoples' website choices. Specifically, I use web tracking data of 1,000 German voters generated by them after three months of tracking. I propose to use categorization of websites based on existing domains. When matching website domains with existing categories from [Webshrinker](https://webshrinker.com/), each category represents a predicting variable in a regression. I use the following regressions: Linear Regression, ElasticNet, and Random Forest.

The list of available replication materials:

#### Machine Learning Method: Dimentionality reduction, Linear regression, Random Forest and Elastic Net

- [Table](https://github.com/norakirkizh/ml_politics/blob/master/domain_categories-v2.csv) with domain categories from Webshrinker that we managed to match with domains from our initial web tracking data.
- [R code](https://github.com/norakirkizh/ml_politics/blob/master/category_stat.R) for exploring domain categories from Webshrinker: A [table](https://github.com/norakirkizh/ml_politics/blob/master/Sum_of_visits.csv) with descriptive statistics like sum of visits by group of domain categories;
- Distribution plots: [code](https://github.com/norakirkizh/ml_politics/blob/master/distribution_plot.r).
- Plots with descriptive OLS estimates, with controlls: [Selected](https://github.com/norakirkizh/ml_politics/blob/master/combined.pdf) political attitudes and domain categories, [the rest](https://github.com/norakirkizh/ml_politics/blob/master/combined_appendix.pdf) of the political attitudes;
- [Plots](https://github.com/norakirkizh/ml_politics/blob/master/combined_appendix.pdf) with OLS estimates with controlls for the rest of the political attitudes;
- OLS, Random Forest and ElasticNet summary [plot](https://github.com/norakirkizh/ml_politics/blob/master/R2_corr.pdf): Pearson correlations and R2 for all political attitudes ([R code](https://github.com/norakirkizh/ml_politics/blob/master/R2_plot.r));
- [Plot](https://github.com/norakirkizh/ml_politics/blob/master/varImp_alpha.pdf) with Variable Importance Rank of domain categories for each political attitude ([R code]() that can also produce an interactive plot with plotly): [Variable importance from Random Forest](https://github.com/norakirkizh/ml_politics/blob/master/rf_varImp.pdf), and [Linear regression](https://github.com/norakirkizh/ml_politics/blob/master/varImp_alpha.pdf). 

Additionally, plots for validation of web tracking data: [browsing behavior](https://github.com/norakirkizh/ml_politics/blob/master/ivw_germany.pdf) and [privacy policy](https://github.com/norakirkizh/ml_politics/blob/master/plot_privacy_noad.pdf) of web tracking vs national German panel.

This project has also an anonymous [repository](https://osf.io/us4dz/?view_only=65edf1069f7341a88380f40b1ec2c43d) on OSF.
