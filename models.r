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

###################################################################
# Penalized Regression Essentials: LM, Elastic Net, Random forest #
###################################################################

df_cut$satisfactdemocW0 <- as.numeric(df_cut$satisfactdemocW0)
df_cut <- na.omit(df_cut)

set.seed(123)
training.samples <- df_cut$satisfactdemocW0 %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- df_cut[training.samples, ]
test.data <- df_cut[-training.samples, ]

########### ########### ########### ########### 
########### Computing regression3 ############# 
########### ########### ########### ###########

# Elastic net regression:
set.seed(123)
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

lm <- train(
  satisfactdemocW0 ~., data = df_cut, method = "lm",
  trControl = train.control, tuneLength = 10)

lm$results$Rsquared
cv.lm(lm)

elastic <- train(
  satisfactdemocW0 ~., data = df_cut, method = "glmnet",
  trControl = train.control, tuneLength = 10)

elastic$finalModel$
  
rf <- train(
    satisfactdemocW0 ~., data = dff_cut, method = "rf",
    trControl = train.control, tuneLength = 10)

rf_results <- rf$results

# names(getModelInfo())

# Model coefficients
coef(elastic$finalModel, elastic$bestTune$lambda)
# Make predictions
predictions <- elastic %>% predict(test.data)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test.data$satisfactdemocW0),
  Rsquare = R2(predictions, test.data$satisfactdemocW0))

rsquared <- elastic$results
rsquared$outcomevar <- rep("Democracy: Satisfaction", n = nrow(rsquared))
rsquared$Algorithm <- rep("Elastic Net", n = nrow(rsquared))

rf_results$outcomevar <- rep("Democracy: Satisfaction", n = nrow(rf_results))
rf_results$Algorithm <- rep("Random Forest", n = nrow(rf_results))


ggplot(rf_results, aes(x=Rsquared, y=outcomevar)) +
  geom_boxplot() +
  # geom_vline(xintercept = 0, color = "black", size=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# the code from http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/153-penalized-regression-essentials-ridge-lasso-elastic-net/

#####################
# Feature Selection #
##################### 

# define the control using a random forest selection function
control_rf <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(euinter[,2:31], euinter[,1], sizes=c(2:31), rfeControl=control_rf)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

table(df$euintegrationW2)
results$results
