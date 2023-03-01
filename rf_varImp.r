library(tidyverse)
library(ggplot2)
library(caret)
library(glmnet)
library(rlist)
library(plotly)


library(grid)
library(gridExtra)
library(RColorBrewer)
library(wesanderson)
library(nnet)
library(broom)
library(leaps)
library(mlbench)

library(randomForest)
library(data.table)
# library(MASS)

#################################################
# Variable importance for Random Forest Models ##
#################################################

df <- read.csv2(file = "category_visits_per_participant.csv", sep = ",", header = TRUE)

###################### satisfactdemocW0: 1 - not at all, 4 - very satisfied ###################### 
df %>% select(satisfactdemocW0)

attitude_demsat <- df %>% select(satisfactdemocW0,
                         search.engines.and.portals, shopping, business, vehicles, education,
                         social.networking, gambling, news.and.media, travel, games, 
                         economy.and.finance, streaming.media, food.and.recipes, entertainment,
                         health, information.tech, job.related, sports, content.server,
                         message.boards.and.forums, illegal.content, uncategorized,
                         black.list, media.sharing, chat.and.messaging, advertising,
                         blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                         real.estate, translators, religion, parked, humor, personals,
                         dating.and.personals, drugs, weapons)

attitude_demsat$satisfactdemocW0 <- as.numeric(attitude_demsat$satisfactdemocW0)
attitude_demsat <- na.omit(attitude_demsat)

# drop redundant features
# correlationMatrix <- cor(attitude[,2:37])
# print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
# highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print(highlyCorrelated)

# redundant features
attitude_demsat <- attitude_demsat %>% select(-personals, -education, -sports, -media.sharing,
                                -entertainment, -advertising, -uncategorized, -parked,
                                -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_demsat <- train(satisfactdemocW0~., data=attitude_demsat, method="rf", preProcess="scale", trControl=control)

# names(getModelInfo())
importance_demsat <- varImp(model_demsat, scale=FALSE)
# print(importance)
# plot(importance)

demsat_imp <- varImp(model_demsat)$importance
demsat_imp <- demsat_imp %>% rownames_to_column(var = "Feature")
colnames(demsat_imp)[1] <- "Democracy: satisfaction"
demsat_imp <- demsat_imp %>% arrange(desc(Overall))

######################### euintegrationW2: 1 - too far, 11 - push further ######################### 

attitude_eu <- df %>% select(euintegrationW2,
                                 search.engines.and.portals, shopping, business, vehicles, education,
                                 social.networking, gambling, news.and.media, travel, games, 
                                 economy.and.finance, streaming.media, food.and.recipes, entertainment,
                                 health, information.tech, job.related, sports, content.server,
                                 message.boards.and.forums, illegal.content, uncategorized,
                                 black.list, media.sharing, chat.and.messaging, advertising,
                                 blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                                 real.estate, translators, religion, parked, humor, personals,
                                 dating.and.personals, drugs, weapons)

attitude_eu$euintegrationW2 <- as.numeric(attitude_eu$euintegrationW2)
attitude_eu <- na.omit(attitude_eu)

# redundant features
attitude_eu <- attitude_eu %>% select(-personals, -education, -sports, -media.sharing,
                                              -entertainment, -advertising, -uncategorized, 
                                              -parked, -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_eu <- train(euintegrationW2~., data=attitude_eu, method="rf", preProcess="scale", trControl=control)
importance_eu <- varImp(model_eu, scale=FALSE)

eu_imp <- varImp(model_eu)$importance
eu_imp <- eu_imp %>% rownames_to_column(var = "Feature")
colnames(eu_imp)[1] <- "Support EU integration"
eu_imp <- eu_imp %>% arrange(desc(Overall))

######################### order_democrW2 (democracy): 1 - bad, 4 - good ######################### 

attitude_orderdem <- df %>% select(order_democrW2,
                             search.engines.and.portals, shopping, business, vehicles, education,
                             social.networking, gambling, news.and.media, travel, games, 
                             economy.and.finance, streaming.media, food.and.recipes, entertainment,
                             health, information.tech, job.related, sports, content.server,
                             message.boards.and.forums, illegal.content, uncategorized,
                             black.list, media.sharing, chat.and.messaging, advertising,
                             blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                             real.estate, translators, religion, parked, humor, personals,
                             dating.and.personals, drugs, weapons)

attitude_orderdem$order_democrW2 <- as.numeric(attitude_orderdem$order_democrW2)
attitude_orderdem <- na.omit(attitude_orderdem)

# redundant features
attitude_orderdem <- attitude_orderdem %>% select(-personals, -education, -sports, -media.sharing,
                                      -entertainment, -advertising, -uncategorized, 
                                      -parked, -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_orderdem <- train(order_democrW2~., data=attitude_orderdem, method="rf", preProcess="scale", trControl=control)
importance_orderdem <- varImp(model_orderdem, scale=FALSE)

# plot
pdf(file = "/Users/eleonorakirkiza/Documents/JITP/Analysis/plot_varImp_dem.pdf", plot_dem, width=5, height=7)
plot(importance_orderdem)
dev.off()

orderdem_imp <- varImp(model_orderdem)$importance
orderdem_imp <- orderdem_imp %>% rownames_to_column(var = "Feature")
colnames(orderdem_imp)[1] <- "Democracy: support democratic system"
orderdem_imp <- orderdem_imp %>% arrange(desc(Overall))

######################### democracy_freelectW2: 1 - not necessary, 11 - necessary #################

attitude_elect <- df %>% select(democracy_freelectW2,
                                   search.engines.and.portals, shopping, business, vehicles, education,
                                   social.networking, gambling, news.and.media, travel, games, 
                                   economy.and.finance, streaming.media, food.and.recipes, entertainment,
                                   health, information.tech, job.related, sports, content.server,
                                   message.boards.and.forums, illegal.content, uncategorized,
                                   black.list, media.sharing, chat.and.messaging, advertising,
                                   blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                                   real.estate, translators, religion, parked, humor, personals,
                                   dating.and.personals, drugs, weapons)

attitude_elect$democracy_freelectW2 <- as.numeric(attitude_elect$democracy_freelectW2)
attitude_elect <- na.omit(attitude_elect)
# redundant features
attitude_elect <- attitude_elect %>% select(-personals, -education, -sports, -media.sharing,
                                                  -entertainment, -advertising, -uncategorized, 
                                                  -parked, -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_elect <- train(democracy_freelectW2~., data=attitude_elect, method="rf", preProcess="scale", trControl=control)
importance_elect <- varImp(model_elect, scale=FALSE)

freelect_imp <- varImp(model_elect)$importance
freelect_imp <- freelect_imp %>% rownames_to_column(var = "Feature")
colnames(freelect_imp)[1] <- "Democracy: support free elections"
freelect_imp <- freelect_imp %>% arrange(desc(Overall))

###################### democracy_obedienceW2: 1 - not necessary, 11 - necessary #########

attitude_obed <- df %>% select(democracy_obedienceW2,
                                search.engines.and.portals, shopping, business, vehicles, education,
                                social.networking, gambling, news.and.media, travel, games, 
                                economy.and.finance, streaming.media, food.and.recipes, entertainment,
                                health, information.tech, job.related, sports, content.server,
                                message.boards.and.forums, illegal.content, uncategorized,
                                black.list, media.sharing, chat.and.messaging, advertising,
                                blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                                real.estate, translators, religion, parked, humor, personals,
                                dating.and.personals, drugs, weapons)

attitude_obed$democracy_obedienceW2 <- as.numeric(attitude_obed$democracy_obedienceW2)
attitude_obed <- na.omit(attitude_obed)
attitude_obed <- attitude_obed %>% select(-personals, -education, -sports, -media.sharing,
                                            -entertainment, -advertising, -uncategorized, 
                                            -parked, -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_obed <- train(democracy_obedienceW2~., data=attitude_obed, method="rf", preProcess="scale", trControl=control)
importance_obed <- varImp(model_obed, scale=FALSE)

obed_imp <- varImp(model_obed)$importance
obed_imp <- obed_imp %>% rownames_to_column(var = "Feature")
colnames(obed_imp)[1] <- "Democracy: obeying rulers"
obed_imp <- obed_imp %>% arrange(desc(Overall))

###################### climaterealW2: 1 - it's natural, 5 - it's human's fault #####

attitude_clima <- df %>% select(climaterealW2,
                               search.engines.and.portals, shopping, business, vehicles, education,
                               social.networking, gambling, news.and.media, travel, games, 
                               economy.and.finance, streaming.media, food.and.recipes, entertainment,
                               health, information.tech, job.related, sports, content.server,
                               message.boards.and.forums, illegal.content, uncategorized,
                               black.list, media.sharing, chat.and.messaging, advertising,
                               blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                               real.estate, translators, religion, parked, humor, personals,
                               dating.and.personals, drugs, weapons)

attitude_clima$climaterealW2 <- as.numeric(attitude_clima$climaterealW2)
attitude_clima <- na.omit(attitude_clima)
attitude_clima <- attitude_clima %>% select(-personals, -education, -sports, -media.sharing,
                                           -entertainment, -advertising, -uncategorized, 
                                           -parked, -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_clima <- train(climaterealW2~., data=attitude_clima, method="rf", preProcess="scale", trControl=control)
importance_clima <- varImp(model_clima, scale=FALSE)

clima_imp <- varImp(model_clima)$importance
clima_imp <- clima_imp %>% rownames_to_column(var = "Feature")
colnames(clima_imp)[1] <- "Climate change"
clima_imp <- clima_imp %>% arrange(desc(Overall))

###################### issueopinions_islam1W1: 1 - disagree, 5 - agree #############

attitude_islam <- df %>% select(issueopinions_islam1W1,
                                search.engines.and.portals, shopping, business, vehicles, education,
                                social.networking, gambling, news.and.media, travel, games, 
                                economy.and.finance, streaming.media, food.and.recipes, entertainment,
                                health, information.tech, job.related, sports, content.server,
                                message.boards.and.forums, illegal.content, uncategorized,
                                black.list, media.sharing, chat.and.messaging, advertising,
                                blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                                real.estate, translators, religion, parked, humor, personals,
                                dating.and.personals, drugs, weapons)

attitude_islam$issueopinions_islam1W1 <- as.numeric(attitude_islam$issueopinions_islam1W1)
attitude_islam <- na.omit(attitude_islam)
attitude_islam <- attitude_islam %>% select(-personals, -education, -sports, -media.sharing,
                                            -entertainment, -advertising, -uncategorized, 
                                            -parked, -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_islam <- train(issueopinions_islam1W1~., data=attitude_islam, method="rf", preProcess="scale", trControl=control)
importance_islam <- varImp(model_islam, scale=FALSE)

islam_imp <- varImp(model_islam)$importance
islam_imp <- islam_imp %>% rownames_to_column(var = "Feature")
colnames(islam_imp)[1] <- "Immigration: Islam"
islam_imp <- islam_imp %>% arrange(desc(Overall))

############### immprobs_jobsW0: 1 - disagree, 5 - agree ###############

attitude_job <- df %>% select(immprobs_jobsW0,
                                search.engines.and.portals, shopping, business, vehicles, education,
                                social.networking, gambling, news.and.media, travel, games, 
                                economy.and.finance, streaming.media, food.and.recipes, entertainment,
                                health, information.tech, job.related, sports, content.server,
                                message.boards.and.forums, illegal.content, uncategorized,
                                black.list, media.sharing, chat.and.messaging, advertising,
                                blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                                real.estate, translators, religion, parked, humor, personals,
                                dating.and.personals, drugs, weapons)

attitude_job$immprobs_jobsW0 <- as.numeric(attitude_job$immprobs_jobsW0)
attitude_job <- na.omit(attitude_job)
attitude_job <- attitude_job %>% select(-personals, -education, -sports, -media.sharing,
                                            -entertainment, -advertising, -uncategorized, 
                                            -parked, -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_job <- train(immprobs_jobsW0~., data=attitude_job, method="rf", preProcess="scale", trControl=control)
importance_job <- varImp(model_job, scale=FALSE)

job_imp <- varImp(model_job)$importance
job_imp <- job_imp %>% rownames_to_column(var = "Feature")
colnames(job_imp)[1] <- "Immigration: Jobs"
job_imp <- job_imp %>% arrange(desc(Overall))

############### immprobs_crimeW0: 1 - disagree, 5 - agree ###############

attitude_crime <- df %>% select(immprobs_crimeW0,
                              search.engines.and.portals, shopping, business, vehicles, education,
                              social.networking, gambling, news.and.media, travel, games, 
                              economy.and.finance, streaming.media, food.and.recipes, entertainment,
                              health, information.tech, job.related, sports, content.server,
                              message.boards.and.forums, illegal.content, uncategorized,
                              black.list, media.sharing, chat.and.messaging, advertising,
                              blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                              real.estate, translators, religion, parked, humor, personals,
                              dating.and.personals, drugs, weapons)

attitude_crime$immprobs_crimeW0 <- as.numeric(attitude_crime$immprobs_crimeW0)
attitude_crime <- na.omit(attitude_crime)
attitude_crime <- attitude_crime %>% select(-personals, -education, -sports, -media.sharing,
                                        -entertainment, -advertising, -uncategorized, 
                                        -parked, -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_crime <- train(immprobs_crimeW0~., data=attitude_crime, method="rf", preProcess="scale", trControl=control)
importance_crime <- varImp(model_crime, scale=FALSE)

crime_imp <- varImp(model_crime)$importance
crime_imp <- crime_imp %>% rownames_to_column(var = "Feature")
colnames(crime_imp)[1] <- "Immigration: crime"
crime_imp <- crime_imp %>% arrange(desc(Overall))

############## # leftpopattid_govredW2: 1 - disagree, 5 - agree #############

attitude_gov <- df %>% select(leftpopattid_govredW2,
                                search.engines.and.portals, shopping, business, vehicles, education,
                                social.networking, gambling, news.and.media, travel, games, 
                                economy.and.finance, streaming.media, food.and.recipes, entertainment,
                                health, information.tech, job.related, sports, content.server,
                                message.boards.and.forums, illegal.content, uncategorized,
                                black.list, media.sharing, chat.and.messaging, advertising,
                                blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                                real.estate, translators, religion, parked, humor, personals,
                                dating.and.personals, drugs, weapons)

attitude_gov$leftpopattid_govredW2 <- as.numeric(attitude_gov$leftpopattid_govredW2)
attitude_gov <- na.omit(attitude_gov)
attitude_gov <- attitude_gov %>% select(-personals, -education, -sports, -media.sharing,
                                            -entertainment, -advertising, -uncategorized, 
                                            -parked, -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_gov <- train(leftpopattid_govredW2~., data=attitude_gov, method="rf", preProcess="scale", trControl=control)
importance_gov <- varImp(model_gov, scale=FALSE)

popgov_imp <- varImp(model_gov)$importance
popgov_imp <- popgov_imp %>% rownames_to_column(var = "Feature")
colnames(popgov_imp)[1] <- "Populism: income redistribution"
popgov_imp <- popgov_imp %>% arrange(desc(Overall))

############ leftpopattid_bigbusW2: 1 - disagree, 5 - agree ############

attitude_bus <- df %>% select(leftpopattid_bigbusW2,
                              search.engines.and.portals, shopping, business, vehicles, education,
                              social.networking, gambling, news.and.media, travel, games, 
                              economy.and.finance, streaming.media, food.and.recipes, entertainment,
                              health, information.tech, job.related, sports, content.server,
                              message.boards.and.forums, illegal.content, uncategorized,
                              black.list, media.sharing, chat.and.messaging, advertising,
                              blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                              real.estate, translators, religion, parked, humor, personals,
                              dating.and.personals, drugs, weapons)

attitude_bus$leftpopattid_bigbusW2 <- as.numeric(attitude_bus$leftpopattid_bigbusW2)
attitude_bus <- na.omit(attitude_bus)
attitude_bus <- attitude_bus %>% select(-personals, -education, -sports, -media.sharing,
                                        -entertainment, -advertising, -uncategorized, 
                                        -parked, -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_bus <- train(leftpopattid_bigbusW2~., data=attitude_bus, method="rf", preProcess="scale", trControl=control)
importance_bus <- varImp(model_bus, scale=FALSE)

bus_imp <- varImp(model_bus)$importance
bus_imp <- bus_imp %>% rownames_to_column(var = "Feature")
colnames(bus_imp)[1] <- "Populism: exploitation by big business"
bus_imp <- bus_imp %>% arrange(desc(Overall))

############# leftpopattid_socbenlazyW2: 1 - disagree, 5 - agree #########

attitude_ben <- df %>% select(leftpopattid_socbenlazyW2,
                              search.engines.and.portals, shopping, business, vehicles, education,
                              social.networking, gambling, news.and.media, travel, games, 
                              economy.and.finance, streaming.media, food.and.recipes, entertainment,
                              health, information.tech, job.related, sports, content.server,
                              message.boards.and.forums, illegal.content, uncategorized,
                              black.list, media.sharing, chat.and.messaging, advertising,
                              blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                              real.estate, translators, religion, parked, humor, personals,
                              dating.and.personals, drugs, weapons)

attitude_ben$leftpopattid_socbenlazyW2 <- as.numeric(attitude_ben$leftpopattid_socbenlazyW2)
attitude_ben <- na.omit(attitude_ben)
attitude_ben <- attitude_ben %>% select(-personals, -education, -sports, -media.sharing,
                                        -entertainment, -advertising, -uncategorized, 
                                        -parked, -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_ben <- train(leftpopattid_socbenlazyW2~., data=attitude_ben, method="rf", preProcess="scale", trControl=control)
importance_ben <- varImp(model_ben, scale=FALSE)

ben_imp <- varImp(model_ben)$importance
ben_imp <- ben_imp %>% rownames_to_column(var = "Feature")
colnames(ben_imp)[1] <- "Populism: socbenefits make paople lazy"
ben_imp <- ben_imp %>% arrange(desc(Overall))

############## leftrightW0 #############

attitude_lr <- df %>% select(leftrightW0,
                              search.engines.and.portals, shopping, business, vehicles, education,
                              social.networking, gambling, news.and.media, travel, games, 
                              economy.and.finance, streaming.media, food.and.recipes, entertainment,
                              health, information.tech, job.related, sports, content.server,
                              message.boards.and.forums, illegal.content, uncategorized,
                              black.list, media.sharing, chat.and.messaging, advertising,
                              blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                              real.estate, translators, religion, parked, humor, personals,
                              dating.and.personals, drugs, weapons)

attitude_lr$leftrightW0 <- as.numeric(attitude_lr$leftrightW0)
attitude_lr <- na.omit(attitude_lr)
attitude_lr <- attitude_lr %>% select(-personals, -education, -sports, -media.sharing,
                                        -entertainment, -advertising, -uncategorized, 
                                        -parked, -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_lr <- train(leftrightW0~., data=attitude_lr, method="rf", preProcess="scale", trControl=control)
importance_lr <- varImp(model_lr, scale=FALSE)

lr_imp <- varImp(model_lr)$importance
lr_imp <- lr_imp %>% rownames_to_column(var = "Feature")
colnames(lr_imp)[1] <- "Left-right ideology"
lr_imp <- lr_imp %>% arrange(desc(Overall))

############ polinterestW0 ##############

attitude_pol <- df %>% select(polinterestW0,
                             search.engines.and.portals, shopping, business, vehicles, education,
                             social.networking, gambling, news.and.media, travel, games, 
                             economy.and.finance, streaming.media, food.and.recipes, entertainment,
                             health, information.tech, job.related, sports, content.server,
                             message.boards.and.forums, illegal.content, uncategorized,
                             black.list, media.sharing, chat.and.messaging, advertising,
                             blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                             real.estate, translators, religion, parked, humor, personals,
                             dating.and.personals, drugs, weapons)

attitude_pol$polinterestW0 <- as.numeric(attitude_pol$polinterestW0)
attitude_pol <- na.omit(attitude_pol)
attitude_pol <- attitude_pol %>% select(-personals, -education, -sports, -media.sharing,
                                      -entertainment, -advertising, -uncategorized, 
                                      -parked, -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_pol <- train(polinterestW0~., data=attitude_pol, method="rf", preProcess="scale", trControl=control)
importance_pol <- varImp(model_pol, scale=FALSE)

# plot
pdf(file = "/Users/eleonorakirkiza/Documents/JITP/Analysis/plot_varImp_pol.pdf", plot_dem, width=5, height=7)
plot(importance_pol)
dev.off()

pol_imp <- varImp(model_pol)$importance
pol_imp <- pol_imp %>% rownames_to_column(var = "Feature")
colnames(pol_imp)[1] <- "Interest in politics"
pol_imp <- pol_imp %>% arrange(desc(Overall))

################ trustinst_policeW0: 1 - yes, 5 - not at all #######

attitude_police <- df %>% select(trustinst_policeW0,
                              search.engines.and.portals, shopping, business, vehicles, education,
                              social.networking, gambling, news.and.media, travel, games, 
                              economy.and.finance, streaming.media, food.and.recipes, entertainment,
                              health, information.tech, job.related, sports, content.server,
                              message.boards.and.forums, illegal.content, uncategorized,
                              black.list, media.sharing, chat.and.messaging, advertising,
                              blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                              real.estate, translators, religion, parked, humor, personals,
                              dating.and.personals, drugs, weapons)

attitude_police$trustinst_policeW0 <- as.numeric(attitude_police$trustinst_policeW0)
attitude_police <- na.omit(attitude_police)
attitude_police <- attitude_police %>% select(-personals, -education, -sports, -media.sharing,
                                        -entertainment, -advertising, -uncategorized, 
                                        -parked, -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_police <- train(trustinst_policeW0~., data=attitude_police, method="rf", preProcess="scale", trControl=control)
importance_police <- varImp(model_police, scale=FALSE)

police_imp <- varImp(model_police)$importance
police_imp <- police_imp %>% rownames_to_column(var = "Feature")
colnames(police_imp)[1] <- "Trust: police"
police_imp <- police_imp %>% arrange(desc(Overall))

############# trustinst_nationparlW0: 1 - yes, 5 - not at all ###############

attitude_parl <- df %>% select(trustinst_nationparlW0,
                                 search.engines.and.portals, shopping, business, vehicles, education,
                                 social.networking, gambling, news.and.media, travel, games, 
                                 economy.and.finance, streaming.media, food.and.recipes, entertainment,
                                 health, information.tech, job.related, sports, content.server,
                                 message.boards.and.forums, illegal.content, uncategorized,
                                 black.list, media.sharing, chat.and.messaging, advertising,
                                 blogs.and.personal, proxy.and.filter.avoidance, adult, alcohol.and.tobacco,
                                 real.estate, translators, religion, parked, humor, personals,
                                 dating.and.personals, drugs, weapons)

attitude_parl$trustinst_nationparlW0 <- as.numeric(attitude_parl$trustinst_nationparlW0)
attitude_parl <- na.omit(attitude_parl)
attitude_parl <- attitude_parl %>% select(-personals, -education, -sports, -media.sharing,
                                              -entertainment, -advertising, -uncategorized, 
                                              -parked, -content.server)

# the model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_parl <- train(trustinst_nationparlW0~., data=attitude_parl, method="rf", preProcess="scale", trControl=control)
importance_parl <- varImp(model_parl, scale=FALSE)

parl_imp <- varImp(model_parl)$importance
parl_imp <- parl_imp %>% rownames_to_column(var = "Feature")
colnames(parl_imp)[1] <- "Trust: national parliament"
parl_imp <- parl_imp %>% arrange(desc(Overall))

#############

df_imp <- list.cbind(list(eu_imp, demsat_imp, orderdem_imp, freelect_imp, obed_imp, clima_imp, 
               islam_imp, job_imp, crime_imp, popgov_imp, bus_imp, ben_imp, pol_imp, 
               police_imp, parl_imp))

df_imp <- df_imp[c(T,F)]

data_long <- gather(df_imp, Attitude, Feature, 
                    `Support EU integration`:`Trust: national parliament`, factor_key=TRUE)

data_long$Feature <- recode(data_long$Feature, 
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

data_long$Category <- fct_collapse(data_long$Feature,
                   General = c("search engines", "information tech", "black list", "filter avoidance"),
                   `Media` = c("news and media", "streaming media", "blogs", "illegal content"),
                   Communication = c("social media", "forums", "messaging"),
                   Consumption = c("shopping", "business", "vehicles", "finance", "alcohol/tobacco", "real estate", "weapons"),
                   Life = c("travel", "food/recipes", "health", "translators", "drugs", "job related", "religion", "dating"),
                   Entertainment = c("games", "gambling", "adult", "humor"),
                   NULL = "H")

data_long$Dimention <- fct_collapse(data_long$Attitude,
                       Issues = c("Interest in politics", "Support EU integration", "Climate change", "Trust: national parliament", "Trust: police"),
                       Democracy = c("Democracy: satisfaction", "Democracy: support free elections", "Democracy: obeying rulers", "Democracy: support democratic system"),
                       Immigration = c("Immigration: Islam", "Immigration: crime", "Immigration: Jobs"),
                       Populism = c("Populism: exploitation by big business", "Populism: income redistribution", "Populism: socbenefits make paople lazy"))

data_long$Attitude <- factor(data_long$Attitude)
data_long$Feature <- factor(data_long$Feature)
data_long$Group <- factor(data_long$Category)

data_long$Rank <- rep(seq(1,30,by=1), each=1, times=nrow(data_long)/30)

######################

rf_varImp <- ggplot(data_long, aes(x = Rank, y = Attitude, fill = Category, alpha = -Rank)) +
  geom_tile() + theme_minimal() + coord_cartesian(xlim=c(1, 30)) +
  scale_x_continuous(breaks = c(1:30), 
                     labels = c("1", "", "", "", "5", "", "", "", "", "10", "",
                                "", "", "", "15", "", "", "", "", "20",
                                "", "", "", "", "25", "", "", "", "", "30")) +
  facet_wrap(~Dimention, ncol = 1, scales = "free") +
  labs(x="Variable importance rank", y="") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold"),
        legend.text=element_text(size=8), 
        legend.title=element_text(size=9),
        legend.key.size = unit(0.5, 'cm')) +
  scale_fill_manual(breaks = levels(data_long$Group), 
                    values = c("#99CCFF", "#0000CC", "#000099", "#FF9900", 
                               "#0000FF", "#FF6633")) + guides(alpha=FALSE)

ggsave("rf_varImp.pdf", rf_varImp, width=3, height=2, units="in", scale=3)

######################

p <- ggplot(data_long, aes(x = Rank, y = Attitude, fill = Category, 
                           text = paste("Websites:", Feature))) +
  geom_tile() + theme_minimal() + coord_cartesian(xlim=c(1, 30)) +
  scale_x_continuous(breaks = c(1:30), 
                     labels = c("1", "", "", "", "5", "", "", "", "", "10", "",
                                "", "", "", "15", "", "", "", "", "20",
                                "", "", "", "", "25", "", "", "", "", "30")) +
  facet_wrap(~Dimention, ncol = 1, scales = "free") +
  labs(x="Variable importance rank", y="") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold"),
        legend.text=element_text(size=8), 
        legend.title=element_text(size=9),
        legend.key.size = unit(0.5, 'cm')) +
  scale_fill_manual(breaks = levels(data_long$Group), 
                    values = c("#99CCFF", "#0000CC", "#000099", "#FF9900", 
                               "#0000FF", "#FF6633")) + guides(alpha=FALSE)

m <- list(
  l = 100,
  r = 100,
  b = 100,
  t = 50
  )

p <- ggplotly(p, tooltip="text", height = 700, width=1000) %>% 
  layout(autosize = F, margin = m) %>% 
           config(displayModeBar = F)

htmlwidgets::saveWidget(as_widget(p), "rf_varImp.html")

# plotly_build(p)

# https://compstat-lmu.shinyapps.io/Personality_Prediction/#section-rfimp
