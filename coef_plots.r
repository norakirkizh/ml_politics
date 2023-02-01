library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)
install.packages("wesanderson")
library(wesanderson)
library(nnet)
library(broom)
library(leaps)
library(mlbench)
library(caret)
library(randomForest)
library(data.table)
library(plotly)
library(caret)
library(glmnet)
library(MASS)
library(patchwork)
library(ggpubr)



df <- read.csv2(file = "category_visits_per_participant.csv", sep = ",", header = TRUE)
# df_1min <- read.csv2(file = "category_visits_per_participant_1min.csv", sep = ",", header = TRUE) # for robustness checks

# trustinst_policeW0, trustinst_nationparlW0: 1 - yes, 5 - not at all 
# leftrightW0, polinterestW0
# euintegrationW2: 1 - too far, 11 - push further
# leftpopattid_govredW2, leftpopattid_bigbusW2, leftpopattid_socbenlazyW2: 1 - disagree, 5 - agree
# issueopinions_islam1W1, immprobs_jobsW0, immprobs_crimeW0: 1 - disagree, 5 - agree 
# climaterealW2: 1 - it's natural, 5 - it's human's fault
# democracy_freelectW2, democracy_obedienceW2": 1 - not necessary, 11 - necessary
# (not used) order_dictW2 (dictatorship): 1 - bad, 4 - good
# order_democrW2 (democracy): 1 - bad, 4 - good
# satisfactdemocW0: 1 - yes, 4 - no

# variables of interest: 
# euintegrationW2, democracy_freelectW2, immprobs_crimeW0, leftpopattid_govredW2, climaterealW2

############### Interest in politics ############### 

model_pol <- lm(as.numeric(polinterestW0) ~ 
                  search.engines.and.portals + shopping + business + vehicles + education +
                  social.networking + gambling + news.and.media + travel + games + 
                  economy.and.finance + streaming.media + food.and.recipes + entertainment +
                  health + information.tech + job.related + sports + # content.server +
                  message.boards.and.forums + illegal.content + # uncategorized +
                  black.list + media.sharing + chat.and.messaging + # advertising +
                  blogs.and.personal + proxy.and.filter.avoidance + adult + alcohol.and.tobacco +
                  real.estate + translators + religion + humor + # personals + # parked
                  dating.and.personals + drugs + weapons, data = df)
summary(model_pol)

tt <- broom::tidy(model_pol, conf.int=TRUE)
tt <- dplyr::filter(tt, term!="(Intercept)")

# rename categories with dots
tt$term <- recode(tt$term, 
                  search.engines.and.portals = "search engines",
                  streaming.media = "streaming media",
                  information.tech = "information tech",
                  job.related = "job related",
                  illegal.content = "illegal content",
                  media.sharing = "media sharing",
                  chat.and.messaging = "messaging",
                  social.networking = "social media",
                  content.server = "content server",
                  economy.and.finance = "finance",
                  food.and.recipes = "food/recipes",
                  blogs.and.personal = "blogs",
                  real.estate = "real estate",
                  black.list = "black list",
                  alcohol.and.tobacco = "alcohol/tobacco",
                  news.and.media = "news and media",
                  message.boards.and.forums = "forums",
                  proxy.and.filter.avoidance = "filter avoidance",
                  dating.and.personals = "dating"
)


tt$group <- fct_collapse(tt$term,
                         General = c("search engines", "information tech", "black list", "filter avoidance"),
                         Communication = c("social media", "forums", "messaging"), 
                         Media = c("news and media", "streaming media", "blogs", "illegal content", "media sharing"),
                         Consumption = c("shopping", "business", "vehicles", "finance", "alcohol/tobacco", "real estate", "weapons"),
                         Life = c("travel", "food/recipes", "health", "translators", "drugs", "education"),
                         Status = c("job related", "religion", "dating"),
                         Entertainment = c("entertainment", "games", "gambling", "adult", "humor", "sports"),
                         NULL = "H")

tt <- tt %>% mutate(Significance = case_when(p.value >= 0 & p.value <= 0.055 ~ 'p-value < 0.05',
                                             p.value > 0.05 ~ 'p-value > 0.05'))

tt$Significance <- factor(tt$Significance)

signifColors <- c("p-value < 0.05"="red", "p-value > 0.05"="black")
colorScale <- scale_colour_manual(name="Significance", values=signifColors)

pol_interest <- ggplot(tt, aes(x=estimate, y=term)) +
  geom_pointrange(aes(xmin=conf.low, xmax=conf.high, color = Significance), size = 0.1) +
  theme_bw() +
  geom_vline(xintercept = 0, color = "black", size=0.5) +
  facet_wrap(~group, nrow = 1, scales = "free") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(x="OLS estimate", y="", title = "Interest in politics: 1 - not at all, 4 - very interested") +
  colorScale + theme(legend.position = "none")

############### Immigration ############### 

model_imm <- lm(as.numeric(immprobs_crimeW0) ~ 
                  search.engines.and.portals + shopping + business + vehicles + education +
                  social.networking + gambling + news.and.media + travel + games + 
                  economy.and.finance + streaming.media + food.and.recipes + entertainment +
                  health + information.tech + job.related + sports + # content.server +
                  message.boards.and.forums + illegal.content + # uncategorized +
                  black.list + media.sharing + chat.and.messaging + # advertising +
                  blogs.and.personal + proxy.and.filter.avoidance + adult + alcohol.and.tobacco +
                  real.estate + translators + religion + humor + # personals + # parked
                  dating.and.personals + drugs + weapons, data = df)
summary(model_imm)

tt <- broom::tidy(model_imm,conf.int=TRUE)
tt <- dplyr::filter(tt, term!="(Intercept)")

tt$term <- recode(tt$term, 
                  search.engines.and.portals = "search engines",
                  streaming.media = "streaming media",
                  information.tech = "information tech",
                  job.related = "job related",
                  illegal.content = "illegal content",
                  media.sharing = "media sharing",
                  chat.and.messaging = "messaging",
                  social.networking = "social media",
                  # content.server = "content server",
                  economy.and.finance = "finance",
                  food.and.recipes = "food/recipes",
                  blogs.and.personal = "blogs",
                  real.estate = "real estate",
                  black.list = "black list",
                  alcohol.and.tobacco = "alcohol/tobacco",
                  news.and.media = "news and media",
                  message.boards.and.forums = "forums",
                  proxy.and.filter.avoidance = "filter avoidance",
                  dating.and.personals = "dating"
)

tt$group <- fct_collapse(tt$term,
                         General = c("search engines", "information tech", "black list", "filter avoidance"),
                         Communication = c("social media", "forums", "messaging"), 
                         Media = c("news and media", "streaming media", "blogs", "illegal content", "media sharing"),
                         Consumption = c("shopping", "business", "vehicles", "finance", "alcohol/tobacco", "real estate", "weapons"),
                         Life = c("travel", "food/recipes", "health", "translators", "drugs", "education"),
                         Status = c("job related", "religion", "dating"),
                         Entertainment = c("entertainment", "games", "gambling", "adult", "humor", "sports"),
                         NULL = "H")

tt <- tt %>% mutate(Significance = case_when(p.value >= 0 & p.value <= 0.055 ~ 'p-value < 0.05',
                                             p.value > 0.05 ~ 'p-value > 0.05'))

tt$Significance <- factor(tt$Significance)

signifColors <- c("p-value < 0.05"="red", "p-value > 0.05"="black")
colorScale <- scale_colour_manual(name="Significance", values=signifColors)

imm_crime <- ggplot(tt, aes(x=estimate, y=term)) +
  geom_pointrange(aes(xmin=conf.low, xmax=conf.high, color = Significance), size = 0.1) +
  theme_bw() +
  geom_vline(xintercept = 0, color = "black", size=0.5) +
  facet_wrap(~group, nrow = 1, scales = "free") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(x="OLS estimate", y="", title = "Immigrants make crime problems worse: 1 - strongly disagree, 5 - strongly agree") +
  colorScale +
  scale_x_continuous(limits = c(-0.001, 0.001)) +
  theme(legend.position = "none")


#################### Climate change #################### 

model_dem <- lm(as.numeric(climaterealW2) ~ 
                  search.engines.and.portals + shopping + business + vehicles + education +
                  social.networking + gambling + news.and.media + travel + games + 
                  economy.and.finance + streaming.media + food.and.recipes + entertainment +
                  health + information.tech + job.related + sports + # content.server +
                  message.boards.and.forums + illegal.content + # uncategorized +
                  black.list + media.sharing + chat.and.messaging + # advertising +
                  blogs.and.personal + proxy.and.filter.avoidance + adult + alcohol.and.tobacco +
                  real.estate + translators + religion + humor + # personals + # parked
                  dating.and.personals + drugs + weapons, data = df)

tt <- broom::tidy(model_dem,conf.int=TRUE)
tt <- dplyr::filter(tt, term!="(Intercept)")

tt$term <- recode(tt$term, 
                  search.engines.and.portals = "search engines",
                  streaming.media = "streaming media",
                  information.tech = "information tech",
                  job.related = "job related",
                  illegal.content = "illegal content",
                  media.sharing = "media sharing",
                  chat.and.messaging = "messaging",
                  social.networking = "social media",
                  economy.and.finance = "finance",
                  food.and.recipes = "food/recipes",
                  blogs.and.personal = "blogs",
                  real.estate = "real estate",
                  black.list = "black list",
                  alcohol.and.tobacco = "alcohol/tobacco",
                  news.and.media = "news and media",
                  message.boards.and.forums = "forums",
                  proxy.and.filter.avoidance = "filter avoidance",
                  dating.and.personals = "dating"
)

tt$group <- fct_collapse(tt$term,
                         General = c("search engines", "information tech", "black list", "filter avoidance"),
                         Communication = c("social media", "forums", "messaging"), 
                         Media = c("news and media", "streaming media", "blogs", "illegal content", "media sharing"),
                         Consumption = c("shopping", "business", "vehicles", "finance", "alcohol/tobacco", "real estate", "weapons"),
                         Life = c("travel", "food/recipes", "health", "translators", "drugs", "education"),
                         Status = c("job related", "religion", "dating"),
                         Entertainment = c("entertainment", "games", "gambling", "adult", "humor", "sports"),
                         NULL = "H")

tt <- tt %>% mutate(Significance = case_when(p.value >= 0 & p.value <= 0.055 ~ 'p-value < 0.05',
                                             p.value > 0.05 ~ 'p-value > 0.05'))

tt$Significance <- factor(tt$Significance)

signifColors <- c("p-value < 0.05"="red", "p-value > 0.05"="black")
colorScale <- scale_colour_manual(name="Significance", values=signifColors)

climate <- ggplot(tt, aes(x=estimate, y=term)) +
  geom_pointrange(aes(xmin=conf.low, xmax=conf.high, color = Significance), size = 0.1) +
  theme_bw() +
  geom_vline(xintercept = 0, color = "black", size=0.5) +
  facet_wrap(~group, nrow = 1, scales = "free") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(x="OLS estimate", y="", title = "Climate change is caused by... : 1 - natural processes, 5 - human activity") +
  colorScale + theme(legend.position = "none")


#################### Populism ####################
# leftpopattid_govredW2, leftpopattid_bigbusW2, leftpopattid_socbenlazyW2: 1 - disagree, 5 - agree

model_pop <- lm(as.numeric(leftpopattid_govredW2) ~ 
                  search.engines.and.portals + shopping + business + vehicles + education +
                  social.networking + gambling + news.and.media + travel + games + 
                  economy.and.finance + streaming.media + food.and.recipes + entertainment +
                  health + information.tech + job.related + sports + # content.server +
                  message.boards.and.forums + illegal.content + # uncategorized +
                  black.list + media.sharing + chat.and.messaging + # advertising +
                  blogs.and.personal + proxy.and.filter.avoidance + adult + alcohol.and.tobacco +
                  real.estate + translators + religion + humor + # personals + # parked
                  dating.and.personals + drugs + weapons, data = df)

tt <- broom::tidy(model_pop,conf.int=TRUE)
tt <- dplyr::filter(tt, term!="(Intercept)")

tt$term <- recode(tt$term, 
                  search.engines.and.portals = "search engines",
                  streaming.media = "streaming media",
                  information.tech = "information tech",
                  job.related = "job related",
                  illegal.content = "illegal content",
                  media.sharing = "media sharing",
                  chat.and.messaging = "messaging",
                  social.networking = "social media",
                  economy.and.finance = "finance",
                  food.and.recipes = "food/recipes",
                  blogs.and.personal = "blogs",
                  real.estate = "real estate",
                  black.list = "black list",
                  alcohol.and.tobacco = "alcohol/tobacco",
                  news.and.media = "news and media",
                  message.boards.and.forums = "forums",
                  proxy.and.filter.avoidance = "filter avoidance",
                  dating.and.personals = "dating"
)

tt$group <- fct_collapse(tt$term,
                         General = c("search engines", "information tech", "black list", "filter avoidance"),
                         Communication = c("social media", "forums", "messaging"), 
                         Media = c("news and media", "streaming media", "blogs", "illegal content", "media sharing"),
                         Consumption = c("shopping", "business", "vehicles", "finance", "alcohol/tobacco", "real estate", "weapons"),
                         Life = c("travel", "food/recipes", "health", "translators", "drugs", "education"),
                         Status = c("job related", "religion", "dating"),
                         Entertainment = c("entertainment", "games", "gambling", "adult", "humor", "sports"),
                         NULL = "H")

tt <- tt %>% mutate(Significance = case_when(p.value >= 0 & p.value <= 0.055 ~ 'p-value < 0.05',
                                             p.value > 0.05 ~ 'p-value > 0.05'))

tt$Significance <- factor(tt$Significance)

signifColors <- c("p-value < 0.05"="red", "p-value > 0.05"="black")
colorScale <- scale_colour_manual(name="Significance", values=signifColors)

populism <- ggplot(tt, aes(x=estimate, y=term)) +
  geom_pointrange(aes(xmin=conf.low, xmax=conf.high, color = Significance), size = 0.1) +
  theme_bw() +
  geom_vline(xintercept = 0, color = "black", size=0.5) +
  facet_wrap(~group, nrow = 1, scales = "free") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(x="OLS estimate", y="", title = "Government should redistribute income: 1 - strongly disagree, 5 - strongly agree") +
  colorScale + theme(legend.position = "none")


#################### Democracy #################### 
# democracy_freelectW2, democracy_obedienceW2": 1 - not necessary, 11 - necessary
# (not used) order_dictW2 (dictatorship): 1 - bad, 4 - good
# order_democrW2 (democracy): 1 - bad, 4 - good
# satisfactdemocW0: 1 - yes, 4 - no

model_dem <- lm(as.numeric(satisfactdemocW0) ~ 
                  search.engines.and.portals + shopping + business + vehicles + education +
                  social.networking + gambling + news.and.media + travel + games + 
                  economy.and.finance + streaming.media + food.and.recipes + entertainment +
                  health + information.tech + job.related + sports + # content.server +
                  message.boards.and.forums + illegal.content + # uncategorized +
                  black.list + media.sharing + chat.and.messaging + # advertising +
                  blogs.and.personal + proxy.and.filter.avoidance + adult + alcohol.and.tobacco +
                  real.estate + translators + religion + humor + # personals + # parked
                  dating.and.personals + drugs + weapons, data = df)
summary(model_dem)

tt <- broom::tidy(model_dem,conf.int=TRUE)
tt <- dplyr::filter(tt, term!="(Intercept)")

tt$term <- recode(tt$term, 
                  search.engines.and.portals = "search engines",
                  streaming.media = "streaming media",
                  information.tech = "information tech",
                  job.related = "job related",
                  illegal.content = "illegal content",
                  media.sharing = "media sharing",
                  chat.and.messaging = "messaging",
                  social.networking = "social media",
                  economy.and.finance = "finance",
                  food.and.recipes = "food/recipes",
                  blogs.and.personal = "blogs",
                  real.estate = "real estate",
                  black.list = "black list",
                  alcohol.and.tobacco = "alcohol/tobacco",
                  news.and.media = "news and media",
                  message.boards.and.forums = "forums",
                  proxy.and.filter.avoidance = "filter avoidance",
                  dating.and.personals = "dating"
)

tt$group <- fct_collapse(tt$term,
                         General = c("search engines", "information tech", "black list", "filter avoidance"),
                         Communication = c("social media", "forums", "messaging"), 
                         Media = c("news and media", "streaming media", "blogs", "illegal content", "media sharing"),
                         Consumption = c("shopping", "business", "vehicles", "finance", "alcohol/tobacco", "real estate", "weapons"),
                         Life = c("travel", "food/recipes", "health", "translators", "drugs", "education"),
                         Status = c("job related", "religion", "dating"),
                         Entertainment = c("entertainment", "games", "gambling", "adult", "humor", "sports"),
                         NULL = "H")

tt <- tt %>% mutate(Significance = case_when(p.value >= 0 & p.value <= 0.055 ~ 'p-value < 0.05',
                                             p.value > 0.05 ~ 'p-value > 0.05'))

tt$Significance <- factor(tt$Significance)

signifColors <- c("p-value < 0.05"="red", "p-value > 0.05"="black")
colorScale <- scale_colour_manual(name="Significance", values=signifColors)

democracy <- ggplot(tt, aes(x=estimate, y=term)) +
  geom_pointrange(aes(xmin=conf.low, xmax=conf.high, color = Significance), size = 0.1) +
  theme_bw() +
  geom_vline(xintercept = 0, color = "black", size=0.5) +
  facet_wrap(~group, nrow = 1, scales = "free") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(x="OLS estimate", y="", title = "Satisfaction with democracy: 1 - not at all, 4 - very satisfied") +
  colorScale + theme(legend.position = "none")


#################### EU #################### 
# euintegrationW2 

model_eu <- lm(as.numeric(euintegrationW2) ~ 
                    search.engines.and.portals + shopping + business + vehicles + education +
                    social.networking + gambling + news.and.media + travel + games + 
                    economy.and.finance + streaming.media + food.and.recipes + entertainment +
                    health + information.tech + job.related + sports + # content.server +
                    message.boards.and.forums + illegal.content + # uncategorized +
                    black.list + media.sharing + chat.and.messaging + # advertising +
                    blogs.and.personal + proxy.and.filter.avoidance + adult + alcohol.and.tobacco +
                    real.estate + translators + religion + humor + # personals + # parked
                    dating.and.personals + drugs + weapons, data = df)


tt <- broom::tidy(model_eu,conf.int=TRUE)
tt <- dplyr::filter(tt, term!="(Intercept)")

tt$term <- recode(tt$term, 
                  search.engines.and.portals = "search engines",
                  streaming.media = "streaming media",
                  information.tech = "information tech",
                  job.related = "job related",
                  illegal.content = "illegal content",
                  media.sharing = "media sharing",
                  chat.and.messaging = "messaging",
                  social.networking = "social media",
                  economy.and.finance = "finance",
                  food.and.recipes = "food/recipes",
                  blogs.and.personal = "blogs",
                  real.estate = "real estate",
                  black.list = "black list",
                  alcohol.and.tobacco = "alcohol/tobacco",
                  news.and.media = "news and media",
                  message.boards.and.forums = "forums",
                  proxy.and.filter.avoidance = "filter avoidance",
                  dating.and.personals = "dating"
)

tt$group <- fct_collapse(tt$term,
                         General = c("search engines", "information tech", "black list", "filter avoidance"),
                         Communication = c("social media", "forums", "messaging"), 
                         Media = c("news and media", "streaming media", "blogs", "illegal content", "media sharing"),
                         Consumption = c("shopping", "business", "vehicles", "finance", "alcohol/tobacco", "real estate", "weapons"),
                         Life = c("travel", "food/recipes", "health", "translators", "drugs", "education"),
                         Status = c("job related", "religion", "dating"),
                         Entertainment = c("entertainment", "games", "gambling", "adult", "humor", "sports"),
                         NULL = "H")

tt <- tt %>% mutate(Significance = case_when(p.value >= 0 & p.value <= 0.055 ~ 'p-value < 0.05',
                                             p.value > 0.05 ~ 'p-value > 0.05'))

tt$Significance <- factor(tt$Significance)

signifColors <- c("p-value < 0.05"="red", "p-value > 0.05"="black")
colorScale <- scale_colour_manual(name="Significance", values=signifColors)

eu <- ggplot(tt, aes(x=estimate, y=term)) +
  geom_pointrange(aes(xmin=conf.low, xmax=conf.high, color = Significance), size = 0.1) +
  theme_bw() +
  geom_vline(xintercept = 0, color = "black", size=0.5) +
  facet_wrap(~group, nrow = 1, scales = "free") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(x="OLS estimate", y="", title = "Satisfaction with democracy: 1 - not at all, 4 - very satisfied") +
  colorScale + theme(legend.position = "none")


combined_plot <- ggarrange(pol_interest, imm_crime, climate, populism, democracy, eu,
                           nrow = 6,
                           ncol = 1)
ggsave("combined.pdf", combined_plot, width=6, height=7, units="in", scale=3)






