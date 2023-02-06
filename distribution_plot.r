library(tidyverse)
library(ggplot2)
library(ggExtra)
library(ggpubr)

df <- read.csv2(file = "category_visits_per_participant.csv", sep = ",", header = TRUE)

# polinterestW0
# euintegrationW2: 1 - too far, 11 - push further
# leftpopattid_govredW2: 1 - disagree, 5 - agree
# immprobs_crimeW0: 1 - strongly agree, 4 - strongly disagree
# climaterealW2: 1 - it's natural, 5 - it's humans' fault
# satisfactdemocW0: 1 - yes, 4 - no

# the rest of the attitudes:
# trustinst_policeW0, trustinst_nationparlW0: 1 - yes, 5 - not at all 
# leftpopattid_bigbusW2, leftpopattid_socbenlazyW2: 1 - disagree, 5 - agree
# issueopinions_islam1W1: 1 - strongly disagree, 5 - strongly agree
# immprobs_jobsW0: 1 - strongly agree, 4 - strongly disagree
# democracy_freelectW2, democracy_obedienceW2": 1 - not necessary, 11 - necessary
# order_democrW2: 1 - very good way of governing this country, 4 - very bad way of governing this country

###############################################################
###################### Descriptive Plot #######################
###############################################################

df_cut <- df %>% select(satisfactdemocW0, issueopinions_islam1W1, climaterealW2,
                        travel, social.networking, education, 
                        search.engines.and.portals, shopping, business, vehicles, education, social.networking, 
                        gambling, news.and.media, travel, games, economy.and.finance, entertainment, 
                        streaming.media, food.and.recipes, health, job.related, sports, message.boards.and.forums, 
                        chat.and.messaging, blogs.and.personal, proxy.and.filter.avoidance, adult, 
                        alcohol.and.tobacco, real.estate, religion, dating.and.personals, weapons)

df_cut$satisfactdemocW0 <- as.numeric(df_cut$satisfactdemocW0)
df_cut$issueopinions_islam1W1 <- as.numeric(df_cut$issueopinions_islam1W1)
df_cut$climaterealW2 <- as.numeric(df_cut$climaterealW2)

df_cut <- na.omit(df_cut)

long_DF <- df_cut %>% gather(Category, Visits, travel:weapons)
long_DF$Visits <- as.numeric(long_DF$Visits)

long_DF$Category <- recode(long_DF$Category, 
                            search.engines.and.portals = "search engines",
                            streaming.media = "streaming media",
                            information.tech = "information tech",
                            job.related = "job related",
                            illegal.content = "illegal content",
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
                            dating.and.personals = "dating")

long_DF$Category <- factor(long_DF$Category)


long_DF %>% 
  ggplot(aes(x = Visits, y = satisfactdemocW0)) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ Category, nrow = 4, scales = "free") +
  labs(y="Satisfaction with democracy", x="Number of visits") +
  theme(axis.title=element_text(size=15)) +
  theme(axis.text.x = element_text(angle = 35, hjust=1))


p_sm <- long_DF %>% filter(Category == "social media") %>% 
  ggplot(aes(x = Visits, y = satisfactdemocW0)) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm") +
  # facet_wrap( ~ Category, nrow = 1, scales = "free") +
  labs(y="Satisfaction with democracy", x="Number of visits (social media)") +
  theme(axis.title=element_text(size=15)) +
  theme(axis.text.x = element_text(angle = 35, hjust=1))

p_gambling <- long_DF %>% filter(Category == "gambling") %>% 
  ggplot(aes(x = Visits, y = issueopinions_islam1W1)) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm") +
  # facet_wrap( ~ Category, nrow = 1, scales = "free") +
  labs(y="Immigration: Islam", x="Number of visits (gambling)") +
  theme(axis.title=element_text(size=15)) +
  theme(axis.text.x = element_text(angle = 35, hjust=1))

p_climate <- long_DF %>% filter(Category == "travel") %>% 
  ggplot(aes(x = Visits, y = climaterealW2)) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm") +
  # facet_wrap( ~ Category, nrow = 1, scales = "free") +
  labs(y="Climate change", x="Number of visits (travel)") +
  theme(axis.title=element_text(size=15)) +
  theme(axis.text.x = element_text(angle = 35, hjust=1))



sm <- ggMarginal(p_sm, type="histogram")
gambling <- ggMarginal(p_gambling, type = "histogram")
travel <- ggMarginal(p_climate, type = "histogram")


combined_plot <- ggarrange(sm, gambling, travel, 
                           nrow = 1, ncol = 3)
combined_plot

ggsave("distribution_comb.pdf", combined_plot, width=6, height=3, units="in", scale=3)

dev.off()




