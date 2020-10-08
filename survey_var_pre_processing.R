# summary statistics for WWW'21 paper: survey data

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(plyr)
library(xtable)
library(descr)
library(stargazer)


df <- readRDS("../analysis/paneldata.Rds") # survey data

dt <- df %>% filter(country == "Germany") %>% select(
                    # demographics
                    panelist_id, inctotalW0, genderW0, ageW0, eduW1,
                    # trust
                    polinterestW0, trustinst_nationparlW0, trustinst_policeW0, 
                    leftrightW0, euintegrationW2, # newsorgtrustW2
                    # populist att
                    leftpopattid_govredW2, leftpopattid_bigbusW2, leftpopattid_socbenlazyW2, 
                    # immigration
                    issueopinions_islam1W1, immprobs_jobsW0, immprobs_crimeW0, 
                    # immprobs_socialsystemW0, typeimm_1W0, typeimm_2W0, typeimm_3W0,
                    # climate
                    climaterealW2, # climateexpertsW2, climatepolicies_taxesW2, 
                    # democracy
                    democracy_freelectW2, # democracy_armyW2, democracy_civilrightW2, 
                    democracy_obedienceW2, # democracy_samerightW2, order_armyW2 
                    order_dictW2, # order_expertW2
                    order_democrW2, satisfactdemocW0
                    )

# demographics
dt$genderW0 <- revalue(dt$genderW0, c("female"="1", "male"="2", "other"=NA))
dt$genderW0 <- as.numeric(dt$genderW0)
# dt$eduW1 <- recode_factor(dt$eduW1, `1`="Lower", `2`="Middle", `3`="Higher")
dt$inctotalW0 <- as.factor(dt$inctotalW0)
dt$inctotalW0 <- recode_factor(dt$inctotalW0, "0" = NA_character_)
dt$inctotalW0 <- as.numeric(dt$inctotalW0)

# eu integration
dt$euintegrationW2 <- recode(dt$euintegrationW2, 
                             `0`="1", `1`="2", `2`="3", `3`="4",
                             `4`="5", `5`="6", `6`="7", `7`="8", `8`="9", `9`="10", 
                             `10`="11")
dt$euintegrationW2 <- as.numeric(dt$euintegrationW2)

# populist att
dt$leftpopattid_govredW2 <- as.factor(dt$leftpopattid_govredW2)
dt$leftpopattid_govredW2 <- recode_factor(dt$leftpopattid_govredW2, "0" = NA_character_)
dt$leftpopattid_govredW2 <- as.numeric(dt$leftpopattid_govredW2)

dt$leftpopattid_bigbusW2 <- as.factor(dt$leftpopattid_bigbusW2)
dt$leftpopattid_bigbusW2 <- recode_factor(dt$leftpopattid_bigbusW2, "0" = NA_character_)
dt$leftpopattid_bigbusW2 <- as.numeric(dt$leftpopattid_bigbusW2)

dt$leftpopattid_socbenlazyW2 <- as.factor(dt$leftpopattid_socbenlazyW2)
dt$leftpopattid_socbenlazyW2 <- recode_factor(dt$leftpopattid_socbenlazyW2, "0" = NA_character_)
dt$leftpopattid_socbenlazyW2 <- as.numeric(dt$leftpopattid_socbenlazyW2)

# imm
dt$immprobs_jobsW0 <- as.factor(dt$immprobs_jobsW0)
dt$immprobs_jobsW0 <- recode_factor(dt$immprobs_jobsW0, "0" = NA_character_)
dt$immprobs_jobsW0 <- as.numeric(dt$immprobs_jobsW0)

dt$immprobs_crimeW0 <- as.factor(dt$immprobs_crimeW0)
dt$immprobs_crimeW0 <- recode_factor(dt$immprobs_crimeW0, "0" = NA_character_)
dt$immprobs_crimeW0 <- as.numeric(dt$immprobs_crimeW0)

# climate
dt$climaterealW2 <- as.factor(dt$climaterealW2)
dt$climaterealW2 <- recode_factor(dt$climaterealW2, "0" = NA_character_)
dt$climaterealW2 <- as.numeric(dt$climaterealW2)

# democracy
dt$democracy_freelectW2 <- as.factor(dt$democracy_freelectW2)
dt$democracy_freelectW2 <- recode_factor(dt$democracy_freelectW2, "0" = NA_character_)
dt$democracy_freelectW2 <- as.numeric(dt$democracy_freelectW2)

dt$democracy_obedienceW2 <- as.factor(dt$democracy_obedienceW2)
dt$democracy_obedienceW2 <- recode_factor(dt$democracy_obedienceW2, "0" = NA_character_)
dt$democracy_obedienceW2 <- as.numeric(dt$democracy_obedienceW2)

dt$order_dictW2 <- as.factor(dt$order_dictW2)
dt$order_dictW2 <- recode_factor(dt$order_dictW2, "0" = NA_character_)
dt$order_dictW2 <- as.numeric(dt$order_dictW2)

dt$order_democrW2 <- as.factor(dt$order_democrW2)
dt$order_democrW2 <- recode_factor(dt$order_democrW2, "0" = NA_character_)
dt$order_democrW2 <- as.numeric(dt$order_democrW2)

saveRDS(dt, 'attitudes_with_survey_ids_only.rds')

#######################

# web tracking data
# web <- read.csv(file = "../germany_url.csv",  header = TRUE, sep=";")
# write_csv2(dt, "attitudesUPD.csv")
ids <- readRDS('web_ids_filtered.rds')
ids_attweb <- left_join(dt, ids, by='panelist_id')

mutual_ids <- ids_attweb %>% filter(!is.na(n_visits))
attitudes_with_web_ids <- mutual_ids %>% select(-n_visits)
write_csv2(attitudes_with_web_ids, "attitudes_with_web_ids.csv")

# descriptive stats table
scores <- attitudes_with_web_ids %>% dplyr::select(polinterestW0, trustinst_nationparlW0, trustinst_policeW0, 
                               leftrightW0, euintegrationW2, # newsorgtrustW2
                               # populist att
                               leftpopattid_govredW2, leftpopattid_bigbusW2, leftpopattid_socbenlazyW2, 
                               # immigration
                               issueopinions_islam1W1, immprobs_jobsW0, immprobs_crimeW0, 
                               # immprobs_socialsystemW0, typeimm_1W0, typeimm_2W0, typeimm_3W0
                               # climate
                               climaterealW2, # climateexpertsW2, climatepolicies_taxesW2, 
                               # democracy
                               democracy_freelectW2, # democracy_armyW2, democracy_civilrightW2, 
                               democracy_obedienceW2, # democracy_samerightW2, order_expertW2
                               order_dictW2, # order_armyW2, 
                               order_democrW2, satisfactdemocW0,
                               # demographics
                               genderW0, ageW0, eduW1, inctotalW0)

scores <- data.frame(scores)
stargazer(scores, summary.stat = c("n", "mean", "sd", "min", "max"), 
          covariate.labels = c( "Interest in politics", "Trust to parliament", 
                                "Trust to police", "Left-light ideology", "EU integration",
                                "Income redistribution", "Big business and the people", 
                                "Social benefits and laziness",
                                "Islam", "Immigrants and jobs", "Immigrants and crime",
                                "Climate change and humans",
                                "Free elections", "People obey their rulers",
                                "Strong leader", "Democratic political system", 
                                "Satisfaction with democracy",
                                "Gender", "Age", "Education", "Income"
          ), 
          title = "Descriptive statistics of predicted survey-based variables 
          that are measuring users' political attitudes.", 
          notes="\\parbox[t]{8cm}{Political attitudes scales: interest in politics (1 - not at all, 4 - quite interest);
          trust in parliament and police (1 - not at all, 5 - a great deal); 
          political left-right ideology (1 - left, 11 - right);
          EU integration (1 - gone too far, 11 - should be pushed further); 
          big business takes advantage of ordinary people and social benefits make people lazy, 
          and Islam promotes violence more than other religions, immigrants take jobs away from German people,
          immigrants make crime problems worse (1 - strongly disagree, 5 - strongly agree);
          climate change is caused by natural processes, human activity, or both (1- natural processes, 5 - human activity);
          the following things are essential characteristic of democracy (1 - not essential for democracy, 11 - essential for democracy);
          having a strong leader who does not have to bother with Parliament and elections (1 - very good, 5 - very bad); 
          satisfaction with democracy (1 - not at all, 4 - very satisfied).
          Demographics: gender (1 - female, 2 - male), education (1 - low, 2 - middle, 3 - higher);
          income in EUR (1 - less than 100, 5 - between 2,200 and 2,600, 10 - more than 5,390).}", 
          notes.append = TRUE, digits = 2, font.size = "footnotesize", # column.sep.width = "1.5pt", 
          table.placement = "H", header = FALSE, label = "tab:descriptives")





