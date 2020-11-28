
# ---------
# Libraries
# ---------

library(effects)
library(dplyr)
library(ggplot2)
library(car)
library(caret)
library(rpart)
library(gbm)
library(C50)
library(e1071)
library(rpart.plot)
library(pROC)
library(plyr)

install.packages(c("effects", "dplyr", "ggplot2", "car", "caret", "rpart", "gbm", "C50", "e1071", "rpart.plot", "pROC"))

#Load the data 

setwd("C:/Users/zapcu/Desktop/Humana Mays")
hm1 <- read.csv("2020_Competition_Training (1).csv")
holdout <- read.csv("2020_Competition_Holdout .csv")



dim(hm1)
hm <- hm1[, -which(colMeans(is.na(hm1)) > 0)]
hm <- hm %>% select(-contains("rx_"))
hm <- hm[, colSums(hm == 0)/nrow(hm) < .9, drop = FALSE]
hm <- hm %>% select(-contains("person_id"))
hm <- hm %>% select(-contains("submcc"))
mcc <- hm1[,c(grep("hcc_weighted_sum", colnames(hm1)), grep("^cms_.*", colnames(hm1)), 2,4,5,6,7,8,9,86,166:169,529:544,827:855)]
hm <- mcc %>%
  select_if(~ !any(is.na(.)))


holdoutTest <- holdout[names(holdout) %in% names(hm)]
holdoutTest <- holdoutTest %>% 
  select_if(~ !any(is.null(.)))

str(holdoutTest)

hm <- hm[names(hm) %in% names(holdoutTest)]
hm$transportation_issues <- hm1$transportation_issues

holdoutTest$zip_cd <- as.numeric(holdoutTest$zip_cd)
hm$zip_cd <- as.numeric(hm$zip_cd)

# -----------------------------
# Machine Learning (Supervised)
# -----------------------------

set.seed(345) #set random seed to generate the same results throughout the script

#make the prediction variable a factor type
hm$transportation_issues = as.factor(hm$transportation_issues)

# C) create a training and testing (holdout) set

trainIndex <- createDataPartition(hm$transportation_issues, p = 0.8, list = FALSE, times = 1)
datTrain <- hm[trainIndex,]
datTest <- hm[-trainIndex,]

# D) address potential issues related to class balance (e.g., downsampling, SMOTE)

table(hm$transportation_issues)
datTrainDS <- downSample(datTrain, datTrain$transportation_issues, list = TRUE)$x
table(datTrainDS$transportation_issues)
#do some preprocessing -- get rid of zero variance, and clean the data into tibble format

datTrain.processed <- datTrainDS
datTest.processed <- datTest
datTrain.clean <- datTrainDS
datTest.clean <- datTest
table(datTrain.processed$transportation_issues)
dim(datTrain.clean)

# Now we're basically ready to run the model, but first we will set a training control method 
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 2) 

#Set up tuning parameters: 

tuneGrid.rpart <- expand.grid(
  cp = seq(.01, .05, by = .005)
)

tuneGrid.gbm <- expand.grid(
  interaction.depth = c(1:3),
  n.trees = (1:3) * 25,
  shrinkage = seq(.05, .10, .01),
  n.minobsinnode = c(5, 10)
)

tuneGrid.rf <-  expand.grid(
  mtry = floor(sqrt(ncol(subset(datTrain.clean, select = -c(transportation_issues)))))
)


# Here is the code for each model:
library(parallel)
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

Humana.train.rpart <- train(
  y = datTrain.clean$transportation_issues,
  x = subset(datTrain.clean, select = -transportation_issues),
  method = "rpart",
  trControl = ctrl,
  tuneGrid = tuneGrid.rpart,
  na.action = na.pass)

Predictions.rpart <- predict(Humana.train.rpart, newdata = datTest.clean)
confusionMatrix(data = Predictions.rpart, reference = datTest.clean$transportation_issues, positive = "1")


Humana.train.gbm <- train(
  y = datTrain.clean$transportation_issues,
  x = subset(datTrain.clean, select = -transportation_issues),
  method = "gbm",
  trControl = ctrl,
  tuneGrid = tuneGrid.gbm,
  verbose = FALSE)

Humana.train.rf <- train(
  y = datTrain.clean$transportation_issues, 
  x = subset(datTrain.clean, select = -c(transportation_issues)),  
  method = "rf", 
  trControl = ctrl, 
  tuneGrid = tuneGrid.rf,
  ntree = 100, 
  na.action = na.pass)


stopCluster(cl)
# Let's look at model fitting results:

Humana.train.rpart
Humana.train.gbm
Humana.train.rf


plot(Humana.train.rpart)
plot(Humana.train.gbm)
plot(Humana.train.rf)


# To determine which model performed best using the training data, we can compare model performance statistics (accuracy, kappa) on the training data visually:

compareModels <- resamples(list(rpart = Humana.train.rpart,
                                gbm = Humana.train.gbm, 
                                C5.0 = Humana.train.rf))
bwplot(compareModels)
dotplot(compareModels) # all models are relatively close

# To evaluate performance on the testing data, first we need to generate predictions using each model:

Predictions.rpart <- predict(Humana.train.rpart, newdata = datTest.clean)
Predictions.gbm <- predict(Humana.train.gbm, newdata = datTest.clean)
Predictions.rf <- predict(Humana.train.rf, newdata = datTest.clean)


head(Predictions.rpart)
head(Predictions.gbm)
head(Predictions.C5.0)

# Now we can compare model performance on test data using confusion matrices 
rpart.CM <- confusionMatrix(data = Predictions.rpart, reference = datTest.clean$transportation_issues, positive = "1")
rf.CM <- confusionMatrix(data = Predictions.rf, reference = datTest.clean$transportation_issues, positive = "1")
gbm.CM <- confusionMatrix(data = Predictions.gbm, reference = datTest.clean$transportation_issues, positive = "1")


# Again, all models are relatively close using the test data holdout. To visualize model performance yet another way, we need to first predict test data class probabilities:

PredictionProbs.rpart <- predict(Humana.train.rpart, newdata = datTest.clean, type = "prob")
PredictionProbs.gbm <- predict(Humana.train.gbm, newdata = datTest.clean, type = "prob")
PredictionProbs.rf <- predict(Humana.train.rf, newdata = datTest.clean, type = "prob")

head(PredictionProbs.rpart)
head(PredictionProbs.gbm)
head(PredictionProbs.C5.0)

# Now let's compare models via receiver operating characteristic (ROC):

rpart.ROC <- roc(
  predictor = PredictionProbs.rpart[, 2],
  response = datTest.clean$transportation_issues,
  levels = levels(datTest.clean$transportation_issues))

gbm.ROC <- roc(
  predictor = PredictionProbs.gbm[, 2],
  response = datTest.clean$transportation_issues,
  levels = levels(datTest.clean$transportation_issues))

rf.ROC <- roc(
  predictor = PredictionProbs.rf[, 2],
  response = datTest.clean$transportation_issues,
  levels = levels(datTest.clean$transportation_issues))

# Using the ROCs computed above, we can look at area under the curve (AUC) to evaluate model performance on the test data:

auc(rpart.ROC)
auc(gbm.ROC)
auc(rf.ROC)

plot(rpart.ROC, print.auc = TRUE, col = "blue")
plot(gbm.ROC, print.auc = TRUE, col = "red", print.auc.y = .45, add = TRUE)
plot(rf.ROC, print.auc = TRUE, col = "green", print.auc.y = .4, add = TRUE)

# The above code generates a plot using base, but we can also use ggroc to plot the ROCs using ggplot2:

ggroc(list(rpart.ROC, gbm.ROC, rf.ROC), size = 1.5, alpha = .75) + 
  geom_abline(intercept = 1, slope = 1, size = 1.5, alpha = .2, linetype = 2) + 
  scale_color_manual(values = c("blue", "red", "green"), 
                     labels = c("rpart", "gbm", "C5.0")) + 
  theme_bw(base_size = 16)

#We can also plot variable importance for any of these models:
plot(varImp(Humana.train.gbm, scale = FALSE), top = 20)


topTerms <- varImp(Humana.train.gbm, metric = "splits", scale = FALSE)$importance %>%
  mutate(term = rownames(varImp(Humana.train.gbm, metric = "splits", scale = FALSE)$importance)) %>%
  arrange(desc(Overall)) %>%
  slice(5) %>%
  pull(term)
topTerms <- gsub("\\..*","",topTerms)
topTermsFormula <- paste("transportation_issues ~", paste(topTerms, collapse = " + "))
varImp.mod <- glm(formula = topTermsFormula, data = datTest, family = binomial)
Anova(varImp.mod)
plot(allEffects(varImp.mod), type = "response", rug = FALSE, main = "Signs and Symptoms Claims Effect Plot", xlab = "PMPM Signs and Symptoms Claims", ylab = "Predicted Probability of Transportation Issues")


testComparison <- bind_rows(rpart.CM$byClass,
                            gbm.CM$byClass,
                            rf.CM$byClass) %>%
  mutate(model = c("rpart", "gbm", "rf")) %>%
  select(model, everything())
testComparison %>%
  select(model, `Balanced Accuracy`) %>%
  arrange(desc(`Balanced Accuracy`))

ggplot(testComparison, aes(x = reorder(model, -`Balanced Accuracy`), y = `Balanced Accuracy`, fill = "red")) + 
  geom_bar(stat = "identity") +
  theme_bw(base_size = 16) + 
  labs(title = "Balanced Accuracy on Test Data",
       y = "Balanced Accuracy",
       x = "Model") +
  scale_y_continuous(limits = c(0, 1)) 

plot_model(varImp.mod, terms = c("cms_low_income_ind", "est_age"))


holdoutPredictions <- predict(Humana.train.gbm, newdata = holdoutTest)

