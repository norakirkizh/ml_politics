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

write.csv(sum, "Sum_of_visits.csv", row.names=FALSE)

# Examples of actual websites with categories from Webshrinker

load(file="./cs-transfer/2019-06-16_all.rda")
examples <- read.csv2(file = "domain_categories-v2.csv", sep = ",", header = TRUE)

domain_1min <- urls_final %>% filter(duration >= 60) %>% select(domain)

domain_1min <- domain_1min %>% count(domain, sort = TRUE)

domain_1min <- left_join(domain_1min, examples, by = "domain")

domain_1min %>% select(category_names) %>% summarise_all(funs(sum(is.na(.)))) # n of NAs

domain_1min <- domain_1min %>% filter(n >= 10)

top5 <- domain_1min %>% group_by(category_names) %>% top_n(5, n)

top5 <- top5 %>% rename(Category = category_names, Website = domain, Visits = n) %>% 
  arrange(Category)

write.csv(top5, "./top5_domains_per_category.csv", row.names=FALSE)







