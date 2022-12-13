library(tidyverse)
library(data.table)
library(stargazer)
library(tidyverse)
library(tidymodels)
library(parsnip)
library(ranger)
library(tictoc)
library(imbalance)
library(cvAUC)
library(ROSE)
library(gridExtra)
library(mice)
library(xgboost)
library(kknn)
library(discrim)
library(kernlab)
library(ISLR)
library(MASS)
library(e1071)
library(gmodels)
library(neuralnet)
library(NeuralNetTools)
library(caret)
source("./Scripts/MachineLearningFunctions.R")

basePath = file.path("//researchhub/files/May/Credit Union Failure/SRDDfolder")

#-----------------------------------------------------------------------------------
# source data from files
dataRaw = data.table:: fread(file.path("./Data/CreditUnionMLData.csv"), stringsAsFactors = FALSE)

variables<-c("DEFAULT", "roa", "pctarrears", "loandeposit", "simplecapitalratio",
             "provlns", "logassets", "unsecassets", "CY_30E", "arrears312", "arrearsg12",
             "CPI", "GDP.Growth.Quarter.Last.Year", "Base.Rate", "Regunemployment")

DataML <- dataRaw[, variables, with = FALSE]
DataML <- na.omit(DataML)

#----------------------------------------------------------------------------------

sum(DataML$DEFAULT == 1)
sum(DataML$DEFAULT == 0)

tibble:: tibble(CountDefauls = sum(DataML$DEFAULT == 1), CountNonDefauls = sum(DataML$DEFAULT == 0))

#-----------------------------------------------------------------------------------
# prepare data for ML analysis
DataML <- PrepForRecipe(as.data.frame(DataML))

nrow_DataML <- tibble:: tibble(beforeSplit = nrow(DataML))

set.seed(12345)

train_test_split <- rsample:: initial_split(DataML, prop = 0.7)

train <- rsample:: training(train_test_split) # create training set 
test <- rsample:: testing(train_test_split) # create test set 

validation_data <- rsample:: mc_cv(train, prop = 0.8, times = 5) 

nrow_train_data <- tibble:: tibble(trainAftersplit = nrow(train))
nrow_test_data <- tibble:: tibble(testAftersplit = nrow(test))

cbind(nrow_DataML, nrow_train_data, nrow_test_data)

#---------------------------------------------------------------------------------------
# Create 'recipe' for pre-processing input data, and apply to one example split of data --------

simple_recipe <- function(dataset){
  recipes :: recipe(DEFAULT ~ ., data = dataset) %>%
    #step_shadow_missing(all_predictors()) %>%
    recipes:: step_center(all_numeric()) %>%
    recipes:: step_scale(all_numeric()) %>%
    recipes:: step_dummy(all_nominal(),- DEFAULT) %>%  ##Check this, potentially remove, may create a bias of categorise, check number of levels per variables. %>%
    recipes:: step_upsample(DEFAULT, ratio = 0.5)
}

train_recipe <- recipes:: prep(simple_recipe(train), training = train, retain = TRUE)
train_data <- train_recipe %>% juice #bake(simple_train_rec, new_data = train)
test_data <- recipes:: bake(train_recipe, new_data = test) 

tibble:: tibble(c(CountDefaulsTrain = sum(train_data$DEFAULT == 1), CountNonDefaultsTrain = sum(train_data$DEFAULT == 0)),
                c(CountDefaults = sum(test_data$DEFAULT == 1), CountNonDefaults = sum(test_data$DEFAULT == 0)) )

table <- rbind(c(CountDefaultsTrain = sum(train_data$DEFAULT == 1), CountNonDefaultsTrain = sum(train_data$DEFAULT == 0)), 
               c(CountDefaultsTest = sum(test_data$DEFAULT == 1), CountNonDefaultsTest = sum(test_data$DEFAULT == 0)))

rownames(table) <- c("training data", "test data")

#targazer:: stargazer(as.data.frame(train_data),type="text", digits=1, title = "Table 1: Summary Statistics of Credit Union Data", align = TRUE)
#----------------------------------------------------------------------------------------------
# run logistic regression as baseline model 
set.seed(1232)

logisticModel <- parsnip:: logistic_reg(mode = "classification", penalty = "glm") %>% 
  parsnip:: set_engine("glm") %>%
  parsnip:: fit(DEFAULT ~ ., data = train_data)

logisticModelResults <- modelResults(logisticModel, testData = test_data, "logisticModel")
logisticmodelConfMat <- logisticModelResults$confMatrix %>% autoplot(type = "heatmap")
logisticmodelStats <- tableGrob(logisticModelResults$modelStats)
logisticmodel_aucplot <- logisticModelResults$plot

grid.arrange(logisticmodelStats)

grid.arrange(logisticmodelConfMat, logisticmodel_aucplot,
             nrow=1,
             ncol =2,
             as.table=TRUE,
             heights=c(1.5))

plot <- logisticModelResults$modelAccuracy %>% 
  roc_curve(truth, .pred_1) %>% 
  autoplot

logisticModelResults$modelAccuracy %>% 
roc_curve(truth, .pred_1) %>%
  ggplot2:: ggplot(aes(x = 1 - specificity, y = sensitivity, fill = "LogistigModel", group = "1")) +
  ggplot2:: geom_path(aes( colour = unname(boe_cols)[1]), show.legend = F) +
  ggplot2:: geom_abline(lty = 4, colour = unname(boe_cols)[3], show.legend = FALSE, lwd = 1) +
  ggplot2:: scale_colour_manual(values = rep(unname(boe_cols),1)) + 
  ggplot2:: coord_equal() +
  ggplot2:: theme_minimal()
#----------------------------------------------------------------------------------------------
# run random forest alogrithm and test performance 

randomForest <- parsnip:: rand_forest(mtry = 10, trees = 150, min_n = 6, mode = "classification") %>% 
                parsnip:: set_engine("randomForest") %>%
                parsnip:: fit(DEFAULT ~ ., data = train_data)


randomForestResults <- modelResults(randomForest, testData = test_data, "RandomForest")
set.seed(12345)
RFmodelConfMat <- randomForestResults$confMatrix %>% autoplot(type = "heatmap")
RFmodelStats <- tableGrob(randomForestResults$modelStats)
RFmodel_aucplot <- randomForestResults$plot


grid.arrange(RFmodelConfMat, RFmodel_aucplot,
             nrow=1,
             ncol =2,
             as.table=TRUE,
             heights=c(1.5))


grid.arrange(RFmodelStats)

# run boosted trees alogrithm and test performance 

boostedTree <- parsnip:: boost_tree(mtry = 10, trees = 100, min_n = 6, mode = "classification") %>% 
  parsnip:: set_engine("xgboost", importance = 'impurity') %>%
  parsnip:: fit(DEFAULT ~ ., data = train_data)

boostedTreeResults <- modelResults(boostedTree, testData = test_data, "boostedTree")
set.seed(12345)
BTmodelConfMat <- boostedTreeResults$confMatrix %>% autoplot(type = "heatmap")
BTmodelStats <- tableGrob(boostedTreeResults$modelStats)
BTmodel_aucplot <- boostedTreeResults$plot

grid.arrange(BTmodelStats)

# run decision tree and test model performance  

decisionTree <- parsnip:: decision_tree(mode = "classification", cost_complexity = 10, min_n = 6) %>% 
  parsnip:: set_engine("C5.0", importance = 'impurity') %>%
  parsnip:: fit(DEFAULT ~ ., data = train_data)

decisionTreeResults <- modelResults(decisionTree, testData = test_data, "decisionTree")
set.seed(12345)
DTmodelConfMat <- decisionTreeResults$confMatrix %>% autoplot(type = "heatmap")
DTmodelStats <- tableGrob(decisionTreeResults$modelStats)
DTmodel_aucplot <- decisionTreeResults$plot

grid.arrange(DTmodelStats)

# run KNN model 

KNNmodel <- parsnip:: nearest_neighbor(mode = "classification",neighbors = 10) %>% 
  parsnip:: set_engine("kknn", importance = 'impurity') %>%
  parsnip:: fit(DEFAULT ~ ., data = train_data)

KNNTreeResults <- modelResults(KNNmodel, testData = test_data, "KNN")
set.seed(12345)
KNNmodelConfMat <- KNNTreeResults$confMatrix %>% autoplot(type = "heatmap")
KNNmodelStats <- tableGrob(KNNTreeResults$modelStats)
KNNmodel_aucplot <- KNNTreeResults$plot

grid.arrange(KNNmodelStats)

# run SVM models

SVMmodel <- parsnip:: svm_rbf(mode = "classification", rbf_sigma = 0.2) %>% 
  parsnip:: set_engine("kernlab", importance = 'impurity') %>%
  parsnip:: fit(DEFAULT ~ ., data = train_data)

SVMResults <- modelResults(SVMmodel, testData = test_data, "SVM")
set.seed(12345)
SVMmodelConfMat <- SVMResults$confMatrix %>% autoplot(type = "heatmap")
SVMmodelStats <- tableGrob(SVMResults$modelStats)
SVMmodel_aucplot <- SVMResults$plot

grid.arrange(SVMmodelStats)

#---------------------------------------------------------------
# run LDA models

LDAModel <- lda(DEFAULT ~ ., data = train_data) 

# Output models results using LDA
pred_raw = predict(LDAModel,test_data, type = "raw")
pred_probs = predict(LDAModel, test_data, type = "prob") 
pred_class = predict(LDAModel, test_data,type = "class")

##Building the predictions of the model and the data with probablity
LDAmodelAccuracy = tibble:: tibble(truth = test_data$DEFAULT, predicted = pred_class$class) %>% 
  cbind(pred_probs$posterior) %>% 
  dplyr:: mutate(imbalanceMethod = "logistic") %>% 
  dplyr:: rename(".pred_0" = "0", ".pred_1" = "1") %>% 
  dplyr:: mutate_if(is.factor, as.numeric) 
predictions = tibble:: tibble(truth = as.factor(test_data$DEFAULT), predicted = pred_class$class) 
LDAconfMatrix <- yardstick:: conf_mat(predictions, truth, predicted)
LDAconfMatrixStats <- summary(LDAconfMatrix)
LDAmodelAccuracy$truth <- as.factor(LDAmodelAccuracy$truth)
auc <- yardstick:: roc_auc(LDAmodelAccuracy , truth, predicted)
LDAmodelStats <- rbind(LDAconfMatrixStats, auc) %>% dplyr:: mutate(type = "LDA")
plot <- LDAmodelAccuracy %>% 
  roc_curve(as.factor(truth), predicted) %>% 
  autoplot

#----------------------------------------------------------------------------
# run naive bayes models

NaiveBayesModel <- naiveBayes(DEFAULT ~ ., data = train_data) 

pred_raw = predict(NaiveBayesModel,test_data, type = "raw")
pred_class = predict(NaiveBayesModel, test_data,type = "class")

##Building the predictions of the model and the data with probablity
NaiveBayesmodelAccuracy = tibble:: tibble(truth = test_data$DEFAULT, predicted = pred_class) %>% 
  cbind(pred_raw) %>% 
  dplyr:: mutate(imbalanceMethod = "NaiveBayes") %>% 
  dplyr:: rename(".pred_0" = "0", ".pred_1" = "1")

predictions = tibble:: tibble(truth = test_data$DEFAULT, predicted = pred_class)

NaiveBayesconfMatrix <- yardstick:: conf_mat(predictions, truth, predicted)
NaiveBayeconfMatrixStats <- summary(NaiveBayesconfMatrix)
NaiveBayesmodelAccuracy$truth <- as.factor(NaiveBayesmodelAccuracy$truth)
NaiveBayesmodelAccuracy$predicted <- as.numeric(NaiveBayesmodelAccuracy$predicted)

NaiveBayeauc <- yardstick:: roc_auc(NaiveBayesmodelAccuracy ,truth, predicted)
NaiveBayemodelStats <- rbind(NaiveBayeconfMatrixStats, NaiveBayeauc) %>% dplyr:: mutate(type = "NaiveBayes")
plot <- ModelAccuracy %>% 
  roc_curve(as.factor(truth), predicted) %>% 
  autoplot

#-------------------------------------------------------------------------------------------------
# run neurol networks model 

NeuralNetModel <- caret:: avNNet(DEFAULT ~ ., data = train_data, size=19, decay=0.1, repeats=1) 

# run neural network model with tuning gride
tuning.grid <- expand.grid(.size = 1:6, .decay = 0.1, .bag = FALSE)
NeuralNetModel.tuned <- caret::train(DEFAULT ~ ., data = train_data, method = "avNNet", trace = FALSE, 
                                     tuneGrid = tuning.grid, linout = TRUE, repeats = 1)

pred_raw = predict(NeuralNetModel.tuned, test_data, type = "raw")
pred_class = predict(NeuralNetModel.tuned, test_data, type = "prob")

class <- as.data.frame(pred_class) %>% dplyr::mutate(class = if_else(`0` > `1`, 0, 1), class = as.factor(class)) %>% 
  dplyr::select(class)
colnames(class)[1] <- "class"
## Building the predictions of the model and the data with probablity##
predictions = tibble::tibble(truth = test_data$DEFAULT, predicted = class$class)

NeuralNetModelAccuracy <- tibble::as_tibble(predictions) %>% dplyr::mutate_if(is.factor, as.numeric)
NeuralNetModelconfMatrix <- yardstick::conf_mat(predictions, truth, predicted)
NeuralNetModelconfMatrixStats <- summary(NeuralNetModelconfMatrix)
NeuralNetModelauc <- yardstick::roc_auc(NeuralNetModelAccuracy, as.factor(truth), predicted)
NeuralNetModelmodelStats <- rbind(NeuralNetModelconfMatrixStats, NeuralNetModelauc) %>% dplyr::mutate(type = "NeuralNetwork")
