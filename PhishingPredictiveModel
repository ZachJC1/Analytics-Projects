
# ---------
# Read Data
# ---------

library(readxl)
library(dplyr)
library(lubridate)
library(caret)
library(quanteda)
library(textclean)
library(tibble)
library(parallel)
library(doParallel)
library(stringr)
library(janitor)
library(rpart)
library(pROC)
library(car)
library(effects)
library(data.table)
library(scales)
library(gbm)


# A) read in your data file(s)
setwd("C:/Users/zapcu/Downloads")

phish <- read.csv("PhishAnalysis.csv")

# B) show the structure (e.g., str, glimpse) and dimensions of your initial data sets

head(phish)
str(phish)
dim(phish)


# C) check for missing data

length(which(is.na(phish)))


# ------------------------------
# Data Manipulation/Organization
# ------------------------------

# A) organize data files


#check how many rows are na and omit na values
percent((nrow(phish) - nrow(na.omit(phish)))/nrow(phish))
dat <- na.omit(phish)


# B) manipulate data features

phish$Submit <- as.factor(phish$Submit)
phish$Submit <- as.factor(phish$Submit)

#remove click and action, because it's highly correlated with submit
phish <- subset(phish, select = -(Click))
phish <- subset(phish, select = -(action))


head(phish)
str(phish)


# -----------------------
# Run Predictive Analysis
# -----------------------

# A) set a random seed for analysis

set.seed(777)

# B) create a table of target variable to show the natural class balance

table(phish$Submit)

# C) create a training and testing (holdout) set

trainIndex <- createDataPartition(phish$Submit, p = 0.8, list = FALSE, times = 1)
phishTrain <- phish[trainIndex,]
phishTest <- phish[-trainIndex,]

# D) downsampling
phishTrainDS <- downSample(phishTrain, phishTrain$Submit, list = TRUE)$x
table(phishTrainDS$Submit)

#remove any variables that don't provide predictive value 
phish.preProc <- preProcess(phishTrainDS, method = c("zv")) 
phishTrain.processed <- predict(phish.preProc, phishTrain)
phishTest.processed <- predict(phish.preProc, phishTest) 

#preprocess the training set with dummy variables
dummy.preProc <- dummyVars( ~ ., data = phishTrain.processed %>% select(-Submit), fullRank = FALSE)
phishTrain.clean <- predict(dummy.preProc, phishTrain.processed) %>%
  as_tibble() %>%
  mutate(Submit = phishTrain.processed$Submit) %>%
  select(Submit, everything())

#do the same with the test set
phishTest.clean <- predict(dummy.preProc, phishTest.processed) %>%
  as_tibble() %>%
  mutate(Submit = phishTest.processed$Submit) %>%
  select(Submit, everything())


#set control grids for cross validation methods
ctrl <- trainControl(method = "cv", number = 5)
c5ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
ctrlRanger <- trainControl(method = "cv", number = 5, classProbs = TRUE)


#set the tuning grids for the models
tuneGrid.rpart <- expand.grid(
  cp = c(.01, .03, .05)
)

tuneGrid.gbm <- expand.grid(
  interaction.depth = c(1, 3, 5),
  n.trees = seq(100, 500, 100),
  shrinkage = c(.01, .1, .3),
  n.minobsinnode = c(5, 10, 15)
)

tuneGrid.C5.0 <-  expand.grid(
  winnow = TRUE,
  trials = 100,
  model = c("tree", "rules")
)

tuneGrid.rf <-  expand.grid(
  mtry = floor(sqrt(ncol(subset(phishTrain.clean, select = -c(Submit)))))
)

tuneGrid.ranger <-  expand.grid(
  mtry = floor(sqrt(ncol(subset(phishTrain.clean, select = -c(Submit))))),
  splitrule = "gini",
  min.node.size = 1
)

# H) train models

cl <- makeCluster(detectCores())
registerDoParallel(cl)

# #logistic regression 
#
start <- proc.time()

linear <- glm(Submit~., data=phishTrain.clean, family=binomial(link=logit))
test_pred_Linear <- predict(linear, phishTest.clean, type = "response")
glm.pred = rep("FALSE", length(test_pred_Linear))
glm.pred[test_pred_Linear > 0.5] = "TRUE"
glm.pred <- as.factor(glm.pred)

(linear.runTime <- proc.time() - start)

# rpart

start <- proc.time()

dat.train.rpart <- train(
  y = phishTrain.clean$Submit,
  x = subset(phishTrain.clean, select = -c(Submit)),
  method = "rpart",
  trControl = ctrl,
  tuneGrid = tuneGrid.rpart,
  na.action = na.pass)

(rpart.runTime <- proc.time() - start)

# gbm

start <- proc.time() 

dat.train.gbm <- train(
  y = phishTrain.clean$Submit, 
  x = subset(phishTrain.clean, select = -c(Submit)), 
  method = "gbm", 
  trControl = ctrl, 
  tuneGrid = tuneGrid.gbm, 
  verbose = FALSE)

(gbm.runTime <- proc.time() - start)

# C5.0

start <- proc.time() 

dat.train.C5.0 <- train(
  y = phishTrain.clean$Submit, 
  x = subset(phishTrain.clean, select = -c(Submit)), 
  method = "C5.0", 
  trControl = c5ctrl, 
  tuneGrid = tuneGrid.C5.0,
  na.action = na.pass)

(C5.0.runTime <- proc.time() - start)

# SVM

start <- proc.time() 

dat.train.svm <- train(
  y = phishTrain.clean$Submit, 
  x = subset(phishTrain.clean, select = -c(Submit)), 
  method = "svmLinear2", 
  trControl = ctrl,
  probability = TRUE)

(svm.runTime <- proc.time() - start)

# rf

start <- proc.time() 

dat.train.rf <- train(
  y = phishTrain.clean$Submit, 
  x = subset(phishTrain.clean, select = -c(Submit)),  
  method = "rf", 
  trControl = ctrl, 
  tuneGrid = tuneGrid.rf,
  ntree = 250, 
  na.action = na.pass)

(rf.runTime <- proc.time() - start)

# ranger

start <- proc.time() 

dat.train.ranger <- train(
  y = phishTrain.clean$Submit, 
  x = subset(phishTrain.clean, select = -c(Submit)),
  method = "ranger", 
  trControl = ctrlRanger, 
  tuneGrid = tuneGrid.ranger,
  importance = "impurity",
  num.trees = 500)

(ranger.runTime <- proc.time() - start)

stopCluster(cl)


# ------------------------------------
# Evaluate Predictive Analysis Results
# ------------------------------------


# A) a visual of important variables (i.e., see ?varImp)

plot(varImp(dat.train.rpart, scale = FALSE), 20)
plot(varImp(dat.train.gbm, scale = FALSE), 20)
plot(varImp(dat.train.C5.0, metric = "splits", scale = FALSE), 10, cex=2)
plot(varImp(dat.train.rf, scale = FALSE), 20)
plot(varImp(dat.train.ranger, scale = FALSE), 20)


#make predictions

Predictions.rpart <- predict(dat.train.rpart, newdata = phishTest.clean)
Predictions.gbm <- predict(dat.train.gbm, newdata = datTest.clean)
Predictions.C5.0 <- predict(dat.train.C5.0, newdata = datTest.clean)
Predictions.svm <- predict(dat.train.svm, newdata = datTest.clean)
Predictions.rf <- predict(dat.train.rf, newdata = datTest.clean)
Predictions.ranger <- predict(dat.train.ranger, newdata = datTest.clean)
Predictions.linear <- predict(linear, newdata= phishTest.clean)

#create confusion matrices
linear.CM <- confusionMatrix(glm.pred, datTest.clean$Submit)
rpart.CM <- confusionMatrix(data = Predictions.rpart, reference = phishTest$Submit, positive = "FALSE")
gbm.CM <- confusionMatrix(data = Predictions.gbm, reference = phishTest$Submit, positive = "FALSE")
C5.0.CM <- confusionMatrix(data = Predictions.C5.0, reference = phishTest$Submit, positive = "FALSE")
svm.CM <- confusionMatrix(data = Predictions.svm, reference = phishTest$Submit, positive = "FALSE")
rf.CM <- confusionMatrix(data = Predictions.rf, reference = phishTest$Submit, positive = "FALSE")
ranger.CM <- confusionMatrix(data = Predictions.ranger, reference = phishTest$Submit, positive = "FALSE")

#display confusion matrices
linear.CM
rpart.CM
gbm.CM
C5.0.CM
svm.CM
rf.CM
ranger.CM

#ROC curves

#generate probabilistically formatted predicitons
PredictionProbs.rpart <- predict(dat.train.rpart, newdata = datTest.clean, type = "prob")
PredictionProbs.gbm <- predict(dat.train.gbm, newdata = datTest.clean, type = "prob")
PredictionProbs.C5.0 <- predict(dat.train.C5.0, newdata = datTest.clean, type = "prob")
PredictionProbs.svm <- predict(dat.train.svm, newdata = datTest.clean, type = "prob")
PredictionProbs.rf <- predict(dat.train.rf, newdata = datTest.clean, type = "prob")
PredictionProbs.ranger <- predict(dat.train.ranger, newdata = datTest.clean, type = "prob")

#generate ROC curves 
rpart.ROC <- roc(
  predictor = PredictionProbs.rpart[, 2],
  response = datTest$Submit,
  levels = levels(datTest$Submit))

gbm.ROC <- roc(
  predictor = PredictionProbs.gbm[, 2],
  response = datTest$Submit,
  levels = levels(datTest$Submit))

C5.0.ROC <- roc(
  predictor = PredictionProbs.C5.0[, 2],
  response = datTest$Submit,
  levels = levels(datTest$Submit))

svm.ROC <- roc(
  predictor = PredictionProbs.svm[, 2],
  response = datTest$Submit,
  levels = levels(datTest$Submit))

rf.ROC <- roc(
  predictor = PredictionProbs.rf[, 2],
  response = datTest$Submit,
  levels = levels(datTest$Submit))

ranger.ROC <- roc(
  predictor = PredictionProbs.ranger[, 2],
  response = datTest$Submit,
  levels = levels(datTest$Submit))

#calculate area under the ROC curve
auc(rpart.ROC)
auc(gbm.ROC)
auc(C5.0.ROC)
auc(svm.ROC)
auc(rf.ROC)
auc(ranger.ROC)

#Display ROC curves, ordered by best AUC
AUCs <- c(auc(rpart.ROC), auc(gbm.ROC), auc(C5.0.ROC), auc(svm.ROC), auc(rf.ROC), auc(ranger.ROC))
bestOrder <- order(AUCs, decreasing = TRUE)
models <- paste0(c("rpart", "gbm", "C5.0", "svm", "rf", "ranger"), " (", round(AUCs, 2), ")")
ggroc(list(rpart.ROC, gbm.ROC, C5.0.ROC, svm.ROC, rf.ROC, ranger.ROC)[bestOrder], size = 1) + 
  geom_abline(intercept = 1, slope = 1, size = 2, alpha = .2, linetype = 2) + 
  scale_color_manual(values = c("purple", "red", "green", "blue", "orange", "black"),
                     labels = models[bestOrder]) + 
  theme_bw(base_size = 16) +
  labs(title = "Test Data ROCs",
       color = "Model (AUC)",
       x = "Specificity",
       y = "Sensitivity")

# D) Other interesting visualizations 

# run times
runTimes <- tibble(
  model = c("logistic", "rpart", "gbm", "C5.0", "svm", "rf", "ranger"),
  runtime = c(linear.runTime["elapsed"],
              rpart.runTime["elapsed"], 
              gbm.runTime["elapsed"], 
              C5.0.runTime["elapsed"], 
              svm.runTime["elapsed"], 
              rf.runTime["elapsed"], 
              ranger.runTime["elapsed"]))
ggplot(runTimes, aes(x = reorder(model, -runtime), y = runtime, fill = factor(runtime))) + 
  geom_bar(stat = "identity") +
  theme_bw(base_size = 16) + 
  labs(title = "Model Fit Time",
       caption = "Lower is better",
       y = "Time (Seconds)",
       x = "Model") +
  theme(legend.position = "none")


# balanced accuracy plotted with run time
testComparison <- bind_rows(linear.CM$byClass,
                            rpart.CM$byClass,
                            gbm.CM$byClass,
                            C5.0.CM$byClass,
                            svm.CM$byClass,
                            rf.CM$byClass,
                            ranger.CM$byClass) %>%
  mutate(model = c("logistic", "rpart", "gbm", "C5.0", "svm", "rf", "ranger")) %>%
  select(model, everything())
testComparison %>%
  select(model, `Balanced Accuracy`) %>%
  arrange(desc(`Balanced Accuracy`))
testComparisonWithRuntimes <- testComparison %>%
  left_join(runTimes)
ggplot(testComparisonWithRuntimes, aes(x = reorder(model, -`Balanced Accuracy`), y = `Balanced Accuracy`, fill = runtime)) + 
  geom_bar(stat = "identity") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=35, hjust=1)) + 
  labs(title = "Balanced Accuracy on Test Data",
       caption = "Higher balanced accuracy is better\nGreener bars indicate faster fitting models",
       fill = "Runtime (s)",
       y = "Balanced Accuracy",
       x = "Model") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_gradient(low = "green", 
                      high = "red")



# takes top (4) terms from best model, runs regression, shows effect plots

# varImp(dat.train.rpart, scale = FALSE)
# get rid of metric = "splits" if best model isn't C5.0
# varImp(dat.train.C5.0, metric = "splits", scale = FALSE)

topTerms <- varImp(dat.train.C5.0, metric = "splits", scale = FALSE)$importance %>%
  mutate(term = rownames(varImp(dat.train.C5.0, metric = "splits", scale = FALSE)$importance)) %>%
  arrange(desc(Overall)) %>%
  head(4) %>%
  pull(term)
topTerms <- gsub("\\..*","",topTerms)
topTermsFormula <- paste("Submit ~", paste(topTerms, collapse = " + "))
varImp.mod <- glm(formula = topTermsFormula, data = datTest.processed, family = binomial)
Anova(varImp.mod)
plot(allEffects(varImp.mod), type = "response")

#Test for interactions

AllInteractions <- glm(Submit~(Tenure+Contract+InternetService+MultipleLines)^2, data=datTest.processed, family=binomial)
Anova(AllInteractions)

#Tenure and contract have a significant interaction. Let's inspect:

TenureContract <- glm(Submit~Tenure*Contract, data = datTest.processed, family = binomial)
Anova(TenureContract)
plot(allEffects(TenureContract), axes=list(y=list(transform=exp, lab="prestige")))

ef <- effect(term = "Tenure:Contract", TenureContract, default.levels = 9) # 9 because the breaks are nicer
ef2 <- as.data.frame(ef)

ggplot(ef2, aes(Tenure, fit, col = factor(Contract))) +
  geom_line() +
  labs(y = 'Probability of Submit') +
  ylim(0, 1)
#####

