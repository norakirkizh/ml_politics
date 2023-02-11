library(tidyverse)
library(ggplot2)

# ggplots with R2 from RF, EN, LM

categories <- read.csv2(file = "categories.csv", sep = ",", header = TRUE)

categories$R2 <- as.numeric(categories$R2)

categories$model <- recode(categories$model,
                           ElasticNet = "Elastic Net",
                           RandomForestRegressor = "Random Forest",
                           linear_model = "Baseline (linear model)")

categories$response <- factor(categories$response, levels = c("climaterealW2", "euintegrationW2",
                        "trustinst_nationparlW0", "trustinst_policeW0", "satisfactdemocW0",
                        "democracy_freelectW2", "order_democrW2", "order_dictW2", 
                        "democracy_obedienceW2", "immprobs_crimeW0", "immprobs_jobsW0",
                        "issueopinions_islam1W1", "leftpopattid_bigbusW2", "leftpopattid_govredW2",
                        "leftpopattid_socbenlazyW2", "polinterestW0", "ageW0", "eduW1",
                        "inctotalW0", "genderW0"))

categories$response <- recode(categories$response,
                      satisfactdemocW0 = "Democracy: Satisfaction",
                      euintegrationW2 = "Support EU integration",
                      ageW0 = "Age",
                      climaterealW2 = "Climate change",
                      democracy_freelectW2 = "Democracy: Support free elections",
                      democracy_obedienceW2 = "Democracy: Obeying rulers",
                      eduW1 = "Education",
                      genderW0 = "Gender",
                      immprobs_crimeW0 = "Immigration: Crime",
                      immprobs_jobsW0 = "Immigration: Jobs",
                      inctotalW0 = "Income",
                      issueopinions_islam1W1 = "Immigration: Islam",
                      leftpopattid_bigbusW2 = "Populism: Exploitation by big business",
                      leftpopattid_govredW2 = "Populism: Income redistribution",
                      leftpopattid_socbenlazyW2 = "Populism: Socbenefits",
                      order_democrW2 = "Democracy: Support democratic system",
                      order_dictW2 = "Democracy: Strong leader",
                      polinterestW0 = "Interest in politics",
                      trustinst_nationparlW0 = "Trust: national parliament",
                      trustinst_policeW0 = "Trust: police"
                      )

categories <- categories %>% rename(Algorithm = model)

# R2
categories %>% select(Algorithm, R2, response) %>% filter(Algorithm == "Random Forest"| 
  Algorithm == "Elastic Net" | Algorithm == "Baseline (linear model)") %>% 
  filter(response != "Democracy: Strong leader") %>% 
  ggplot(aes(x=R2, y=response, fill = Algorithm)) +
  geom_boxplot(aes(color = Algorithm), alpha = 0.4, outlier.colour = NA) +
  xlim(-1.5, 1) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  labs(y = "") +
  geom_vline(xintercept = 0, color = "black", size=0.5) +
  geom_hline(yintercept = 4.5, alpha = 0.5) + 
  geom_hline(yintercept = 8.5, alpha = 0.5) + 
  geom_hline(yintercept = 11.5, alpha = 0.5) + 
  geom_hline(yintercept = 15.5, alpha = 0.5) + 
  theme_bw()


