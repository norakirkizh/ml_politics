# Descriptive statitics for domain categories

library(tidyverse)
library(readxl)
library(haven)
library(RColorBrewer)
library(gridExtra)
library(purrr)
library(broom)


categories <- read.csv2(file = "category_visits_per_participant_no_political_attributes.csv", sep = ",", header = TRUE)

categories <- categories %>% rename("search engines" = search.engines.and.portals,
                      "streaming media" = streaming.media,
                      "information tech" = information.tech,
                      "job related" = job.related,
                      "illegal content" = illegal.content,
                      "media sharing" = media.sharing,
                      "messaging" = chat.and.messaging,
                      "social media" = social.networking,
                      "content server" = content.server,
                      "finance" = economy.and.finance,
                      "food/recipes" = food.and.recipes,
                      "blogs" = blogs.and.personal,
                      "real estate" = real.estate,
                      "black list" = black.list,
                      "alcohol/tobacco" = alcohol.and.tobacco,
                      "news and media" = news.and.media,
                      "forums" = message.boards.and.forums,
                      "filter avoidance" = proxy.and.filter.avoidance,
                      "dating" = dating.and.personals,
                      "general" = entertainment,
                      "virtual reality" = virtual.reality)


cat <- categories %>% pivot_longer(!X, names_to = "categories", values_to = "count")


cat$group <- fct_collapse(cat$categories,
                         General = c("search engines"),
                         `Tech service` = c("information tech", "black list", "filter avoidance", "content server", "parked"),
                         Communication = c("social media", "forums", "messaging"), 
                         Media = c("news and media", "streaming media", "blogs", "illegal content", "media sharing"),
                         Consumption = c("shopping", "business", "vehicles", "finance", "alcohol/tobacco", "real estate", "weapons"),
                         Life = c("travel", "food/recipes", "health", "drugs"),
                         Status = c("job related", "religion", "dating", "personals"),
                         Entertainment = c("games", "virtual reality", "humor"),
                         Gambling = c("gambling"),
                         Advertising = c("advertising"),
                         Education = c("education", "translators"),
                         General = c("general"),
                         Sports = c("sports"),
                         Adult = c("adult"),
                         Uncategorized = c("uncategorized"),
                         NULL = "H")


cat <- cat %>% select(-X)

sum <- cat %>%
  group_by(group, categories) %>% 
  summarise(sum = sum(count))

group <- sum %>% summarise(sum = sum(sum))
sum(group$sum)

# Categories with examples

examples <- read.csv2(file = "domain_categories-v2.csv", sep = ",", header = TRUE)


write.csv(sum, "Sum_of_visits.csv", row.names=FALSE)


