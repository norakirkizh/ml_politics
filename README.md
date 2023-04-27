## Machine Learning Application to Predict Political Attitudes from Web Browsing Histories

In this repository I share replication materials for the article "Predicting Political Attitudes from Web Browsing Histories: Machine Learning Approach".

The paper aims to introduce machine learning approach to identify political attitudes based on peoples' website choices. Specifically, I use web tracking data of 1,000 German voters generated by them after three months of tracking. I propose to use categorization of websites based on existing domains. When matching website domains with existing categories from [Webshrinker](https://webshrinker.com/), each category represents a predicting variable in a regression. I use the following regressions: Linear Regression, Elastic Net, and Random Forest.

Below is the of available replication materials and supplementary files for replication and further research.

### Machine Learning Methods: Dimentionality reduction, Linear regression, Random Forest and Elastic Net

- [Table](https://github.com/norakirkizh/ml_politics/blob/master/domain_categories-v2.csv) with domain categories from Webshrinker that we managed to match with domains from our initial web tracking data.
- [R code](https://github.com/norakirkizh/ml_politics/blob/master/category_stat.R) for exploring domain categories from Webshrinker: A [table](https://github.com/norakirkizh/ml_politics/blob/master/Sum_of_visits.csv) with descriptive statistics like sum of visits by group of domain categories;
- Distribution plots: [code](https://github.com/norakirkizh/ml_politics/blob/master/distribution_plot.r).
- [Table](https://github.com/norakirkizh/ml_politics/blob/master/top5_domains_per_category.csv) with top 5 domains per category. Note that Weshrinker offered subcategories withing main categories like Business. The table shows top domains for each subcategory.
- Plots with descriptive OLS estimates, with controlls: [Selected](https://github.com/norakirkizh/ml_politics/blob/master/combined.pdf) political attitudes and domain categories, [the rest](https://github.com/norakirkizh/ml_politics/blob/master/combined_appendix.pdf) of the political attitudes;
- [Plots](https://github.com/norakirkizh/ml_politics/blob/master/combined_appendix.pdf) with OLS estimates with controlls for the rest of the political attitudes;
- OLS, Random Forest and ElasticNet summary [plot](https://github.com/norakirkizh/ml_politics/blob/master/R2_corr.pdf): Pearson correlations and R2 for all political attitudes ([R code](https://github.com/norakirkizh/ml_politics/blob/master/R2_plot.r) to make this plot);
- Plots with Variable Importance Rank of domain categories for each political attitude ([R code](https://github.com/norakirkizh/ml_politics/blob/master/rf_varImp.r) that can also produce an interactive plot with plotly): [Variable importance from Random Forest](https://github.com/norakirkizh/ml_politics/blob/master/rf_varImp.pdf), and [Linear regression](https://github.com/norakirkizh/ml_politics/blob/master/varImp_alpha.pdf).
- Two models showed significant predictions: support for democratic political system and interest in politics. Ploted variable importance rank for both models: [Plot 1](https://github.com/norakirkizh/ml_politics/blob/master/plot_varImp_dem.pdf) and [Plot 2](https://github.com/norakirkizh/ml_politics/blob/master/plot_varImp_polint.pdf) respectively.

#### Summary

We combined survey and web tracking data to build machine learning models where web site visits predict self-reported political attitues. There are several findings about predicting models and their applications in social science. The evidence is mixed and requires further research. We built machine learning model for each political attitude of interest, 15 in total. Two models showed significant prediction: interest in politics and support for democratic system.

Additionally, plots for validation of web tracking data: [browsing behavior](https://github.com/norakirkizh/ml_politics/blob/master/ivw_germany.pdf) and [privacy policy](https://github.com/norakirkizh/ml_politics/blob/master/plot_privacy_noad.pdf) of web tracking vs national German panel.
