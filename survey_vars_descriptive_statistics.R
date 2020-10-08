
library(tidyverse)
library(plyr)
library(xtable)
library(descr)
library(stargazer)
library(ggcorrplot)
library(ggplot2)
library(ggridges)
library(viridis)
library(grid)
library(gridExtra)

df <- read.csv(file = "attitudes_with_web_ids.csv", sep=';')
# write.csv2(df, "att.csv")

descr <- readRDS("web_descriptives.rds")
web_survey <- left_join(descr, df, by="panelist_id")

corr <- cor(cormatrix, method = c("spearman"), use = "complete.obs")
corr <- round(corr, 2)
p.mat <- cor_pmat(cormatrix)

p.mat <- data.frame(p.mat)
p.mat <- p.mat %>% select(mean, count, n_uniqdomains, mean_visdomain)
p.mat <- p.mat[-c(1:4),]
p.mat <- as.matrix(p.mat)

corrdf <- data.frame(corr)
corrdf <- corrdf %>% select(mean, count, n_uniqdomains, mean_visdomain)
corrdf <- corrdf[-c(1:4),]

corrm <- as.matrix(corrdf)
ggcorrplot(corrm, p.mat = p.mat)


######################## ridges plots

web_survey$leftright <- recode(web_survey$leftrightW0, 
                               `11` = "Right", `10` = "10", `9` = "9", `8` = "8", 
                               `7` = "7", `6` = "6", `5` = "5", 
                               `4` = "4", `3` = "3", `2` = "2", `1` = "Left")

web_survey$leftright <- as.factor(web_survey$leftright)
web_survey$leftright <- ordered(web_survey$leftright, 
                                levels = c("Right", "10", "9", "8", "7", "6", "5", "4",
                                           "3", "2", "Left"))

v <- web_survey %>% filter(!is.na(leftright)) %>% filter(n_visits <= 100000) %>% 
  ggplot(aes(x = n_visits, y = as.factor(leftright), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_viridis_c(name = "n visits", option = "C") +
  coord_cartesian(clip = "off") +
  # labs(subtitle = 
# 'In politics people sometimes talk of "left" and "right". Where would 
# you place yourself on this scale, where 1 means the left and 11 
# means the right? (N = 1,022)') + 
  xlab("Number of visits") +
  theme_ridges(font_size = 10, grid = TRUE) +
  theme(axis.title.y = element_blank())


d <- web_survey %>% filter(!is.na(leftright)) %>% filter(mean_duration_per_visit <= 250) %>%
  ggplot(aes(x = mean_duration_per_visit, y = as.factor(leftright), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_viridis_c(name = "Duration (sec.)", option = "C") +
  coord_cartesian(clip = "off") +
  # labs(subtitle = 
# 'In politics people sometimes talk of "left" and "right". Where would 
# you place yourself on this scale, where 1 means the left and 11 
# means the right?') +
  xlab("Mean duration per visit") +
  theme_ridges(font_size = 10, grid = TRUE) +
  theme(axis.title.y = element_blank())

grid.arrange(v, d, nrow=2)


web_survey %>% filter(!is.na(leftright)) %>% filter(mean_visits_per_uniq_domain <= 200) %>%
  ggplot(aes(x = mean_visits_per_uniq_domain, y = as.factor(leftright), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_viridis_c(name = "Duration (sec.)", option = "C") +
  coord_cartesian(clip = "off") +
  # labs(subtitle = 
         # 'In politics people sometimes talk of "left" and "right". Where would 
       # you place yourself on this scale, where 1 means the left and 11 
      #  means the right?') +
  theme_ridges(font_size = 10, grid = TRUE) +
  theme(axis.title.y = element_blank())

web_survey %>% filter(!is.na(leftrightW0)) %>% filter(mean_duration_per_uniq_domain <= 10000) %>%
  ggplot(aes(x = mean_duration_per_uniq_domain, y = as.factor(leftrightW0), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_viridis_c(name = "Duration (sec.)", option = "C") +
  coord_cartesian(clip = "off") +
  labs(subtitle = 
         'In politics people sometimes talk of "left" and "right". Where would 
       you place yourself on this scale, where 1 means the left and 11 
       means the right?') +
  theme_ridges(font_size = 10, grid = TRUE) +
  theme(axis.title.y = element_blank())

######################## 

# distribution

# trust
web_survey$trustinst_nationparl <- recode(web_survey$trustinst_nationparlW0, 
                               `1` = "not at all", `2` = "very little", `3` = "somewhat", 
                               `4` = "a lot", `5` = "a great deal")

web_survey$trustinst_nationparl <- ordered(web_survey$trustinst_nationparl, 
                                levels = c("not at all", "very little", "somewhat",
                                           "a lot", "a great deal"))

web_survey %>% filter(!is.na(trustinst_nationparl)) %>% 
  ggplot(aes(x=trustinst_nationparl)) +
  geom_bar(fill="steelblue", width = 0.8) +
  theme_minimal() +
  ylab("Number of panelists") + xlab("") +
  theme(axis.text.x=element_text(angle=30,hjust=1)) +
  labs(subtitle = "Trust in parliament")


web_survey$trustinst_police <- recode(web_survey$trustinst_policeW0, 
                                          `1` = "not at all", `2` = "very little", `3` = "somewhat", 
                                          `4` = "a lot", `5` = "a great deal")

web_survey$trustinst_police <- ordered(web_survey$trustinst_police, 
                                           levels = c("not at all", "very little", "somewhat",
                                                      "a lot", "a great deal"))

t <- web_survey %>% filter(!is.na(trustinst_police)) %>% 
  ggplot(aes(x=trustinst_police)) +
  geom_bar(fill="steelblue", width = 0.8) +
  theme_minimal() +
  ylab("Number of panelists") + xlab("") +
  theme(axis.text.x=element_text(angle=30,hjust=1)) +
  labs(subtitle = "Trust in the police") +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6), 
        plot.subtitle=element_text(size=8))

# climate

web_survey$climate <- recode(web_survey$climaterealW2, 
                                      `1` = "entirely natural", 
                             `2` = "mainly natural", `3` = "both", 
                                      `4` = "mainly human", 
                             `5` = "entirely human")

web_survey$climate <- ordered(web_survey$climate, 
                                       levels = c("entirely natural", 
                                                  "mainly natural", 
                                                  "both", 
                                                  "mainly human", 
                                                  "entirely human"))

c <- web_survey %>% filter(!is.na(climate)) %>% 
  ggplot(aes(x=climate)) +
  geom_bar(fill="steelblue", width = 0.8) +
  theme_minimal() +
  ylab("") + xlab("") +
  theme(axis.text.x=element_text(angle=30,hjust=1)) +
  labs(subtitle = "Causes of climate change") +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6), 
        plot.subtitle=element_text(size=8))


# populism
# Government should redistribute income from the better off to those who are less well off.

web_survey$populism <- recode(web_survey$leftpopattid_govredW2,
                              `1` = "strongly disagree", 
                              `2` = "somewhat disagree", 
                              `3` = "neither", 
                              `4` = "somewhat agree", 
                              `5` = "strongly agree")

web_survey$populism <- ordered(web_survey$populism,
                               levels=c("strongly disagree", 
                                        "somewhat disagree", 
                                        "neither", 
                                        "somewhat agree", 
                                        "strongly agree"))


p <- web_survey %>% filter(!is.na(populism)) %>% 
  ggplot(aes(x=populism)) +
  geom_bar(fill="steelblue", width = 0.8) +
  theme_minimal() +
  ylab("") + xlab("") +
  theme(axis.text.x=element_text(angle=30,hjust=1)) +
  labs(subtitle = "Redistribute income from rich to poor") +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6), 
        plot.subtitle=element_text(size=8))

# immigration

web_survey$immprobs_crime <- recode(web_survey$immprobs_crimeW0,
                                    `1` = "strongly disagree", 
                                    `2` = "somewhat disagree", 
                                    `3` = "somewhat agree", 
                                    `4` = "strongly agree")


web_survey$immprobs_crime <- ordered(web_survey$immprobs_crime,
                               levels=c("strongly disagree", 
                                        "somewhat disagree", 
                                        "somewhat agree", 
                                        "strongly agree"))


i <- web_survey %>% filter(!is.na(immprobs_crime)) %>% 
  ggplot(aes(x=immprobs_crime)) +
  geom_bar(fill="steelblue", width = 0.8) +
  theme_minimal() +
  ylab("Number of panelists") + xlab("") +
  theme(axis.text.x=element_text(angle=30,hjust=1)) +
  labs(subtitle = "Immigrants make crime problems worse") +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6), 
        plot.subtitle=element_text(size=8))


# democracy

web_survey$democracy_freelect <- recode(web_survey$democracy_freelectW2,
                                        `11` = "an essential", 
                                        `10` = "10", `9` = "9", `8` = "8", 
                                        `7` = "7", `6` = "6", `5` = "5", 
                                        `4` = "4", `3` = "3", `2` = "2", 
                                        `1` = "not at all essential")

web_survey$democracy_freelect <- ordered(web_survey$democracy_freelect,
                                         levels=c("an essential", "10", "9", "8", "7", "6", "5", "4",
                                                  "3", "2", "not at all essential"))


d <- web_survey %>% filter(!is.na(democracy_freelect)) %>% 
  ggplot(aes(x=democracy_freelect)) +
  geom_bar(fill="steelblue", width = 0.8) +
  theme_minimal() +
  ylab("") + xlab("") +
  theme(axis.text.x=element_text(angle=30,hjust=1)) +
  labs(subtitle = 
"People choose their leaders 
in free elections") +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6), 
        plot.subtitle=element_text(size=8))



web_survey$order_dict <- recode(web_survey$order_dictW2,
                                    `1` = "very bad", 
                                    `2` = "fairly bad", 
                                    `3` = "fairly good", 
                                    `4` = "very good")


web_survey$order_dict <- ordered(web_survey$order_dict,
                                     levels=c("very bad", 
                                              "fairly bad", 
                                              "fairly good", 
                                              "very good"))


e <- web_survey %>% filter(!is.na(order_dict)) %>% 
  ggplot(aes(x=order_dict)) +
  geom_bar(fill="steelblue", width = 0.8) +
  theme_minimal() +
  ylab("") + xlab("") +
  theme(axis.text.x=element_text(angle=30,hjust=1)) +
  labs(subtitle = 
"Having a strong leader who 
does not have to bother 
with Parliament and elections") +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6), 
        plot.subtitle=element_text(size=8))


grid.arrange(t, p, e, i, d, c, nrow=2)


