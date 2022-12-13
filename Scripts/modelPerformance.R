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

source("./Scripts/prepForModel.R")


# generate summary statistics from the models 
# bind list of results together from model application

ModelResult <- dplyr:: bind_rows(logisticModelResults$modelStats, randomForestResults$modelStats,
                                 boostedTreeResults$modelStats, decisionTreeResults$modelStats, 
                                 KNNTreeResults$modelStats, SVMResults$modelStats, 
                                 LDAmodelStats, NaiveBayemodelStats, NeuralNetModelmodelStats) %>% 
  dplyr:: select(-.estimator) %>% 
  dplyr:: filter(.metric %in% c("accuracy", "sens", "spec", "bal_accuracy", "precision", "detection_prevalence", "roc_auc")) %>% 
  tidyr:: spread(key = .metric , value = .estimate) %>% 
  dplyr:: mutate_if(is.numeric, round, 2)

save(ModelResult, file =  "./Outputs/ModelResult.RData")

ModelAccuracy <- dplyr:: bind_rows(logisticModelResults$modelAccuracy, 
                                   LDAmodelAccuracy, 
                                   NaiveBayesmodelAccuracy, 
                                   NeuralNetmodelAccuracy)

save(ModelAccuracy, file =  "./Outputs/ModelAccuracy.RData")

Logistic <- MakeROCPlot(data = ModelAccuracy, modelType = "logisticModel", title = "Logistic model ROC")
NaiveBayes <- MakeROCPlot(data = ModelAccuracy, modelType = "NaiveBayes", title = "Naive Bayes model ROC")
LDA <- MakeROCPlot(data = ModelAccuracy, modelType = "LDA", title = "LDA model ROC")
NeuralNet <- MakeROCPlot(data = ModelAccuracy, modelType = "NeuralNet", title = "NeuralNet model ROC")

gridExtra:: grid.arrange(Logistic, NaiveBayes, LDA, NeuralNet)

#---------------------------------------------------------------------------------------------------
# model performance over resampling methods 
ModelPerformance <- lapply(X = c("boot", "boot632", "optimism_boot", "boot_all", "cv", "repeatedcv", "LOOCV", "LGOCV"), 
                           FUN = trainControlSample, 
                           data = test_data, numberFolds = 10, samplingMethod = "up", method = "lda") %>% 
  data.table:: rbindlist()


ModelPerformance <- tidyr:: spread(ModelPerformance, key = Meausure, value = Position)


#-------------------------------------------------------------------------------
# model performance over sampling methods 
ModelPerformance <- lapply(X = c("up", "down", "smote", "rose"),  FUN = trainControlSample, method = "nb", resamplingmethod = "boot",
                           traindata = train_data, testdata = test_data, numberFolds = 10) 

data = ModelPerformance %>% purrr:: map(1) %>% purrr:: invoke(rbind, .)

rebalancePerformance <- tidyr:: spread(data, key = Meausure, value = Position)

save(rebalancePerformance, file =  "./Outputs/rebalancePerformance.RData")


#-------------------------------------------------------------------------------
# model performance over model type
#"logicBag", "brnn", "ada", "C5.0", "kknn", "nb", "nnet"
ModelPerformance <- lapply(X = c("lda", "glm", "nb", "nnet"), 
                           FUN = trainControlSample, resamplingmethod = "boot", samplingMethod = "up", 
                           traindata = train_data, testdata = test_data, numberFolds = 5) #%>% data.table:: rbindlist(ModelPerformance)
  

AllModels <- ModelPerformance %>% purrr:: map(1) %>% purrr:: invoke(rbind, .) %>% tidyr:: spread(key = Meausure, value = Position)

save(AllModels, file =  "./Outputs/AllModels.RData")


ConfusionMatrices = ModelPerformance %>% purrr:: map(2) #%>% purrr:: invoke(rbind, .)
save(ConfusionMatrices, file =  "./Outputs/ConfusionMatrices.RData")

cm = ConfusionMatrices



#-------------------------------------------------------------------------------
# take neural network forward as best model and performc CV on the model to tune hyperparameters

fitControl <- trainControl(
  method = "repeatedcv", # resampling method
  number = 5, # k = 5 folds
  repeats = 5, 
  sampling = "up") # repeats 

tune.grid.neuralnet <- expand.grid(
  size = c(0, 1, 2, 5, 10), 
  decay  = c(0, 5, 10, 100)
)


tune.grid.neuralnet <-  expand.grid(size = seq(from = 1, to = 20, by = 2),
                                    decay = seq(from = 0.1, to = 1, by = 0.1))

nrow(tune.grid.neuralnet)

set.seed(825)

nnetFit <- caret:: train(DEFAULT ~ ., data = train_data, 
                         method = "nnet", 
                         trControl = fitControl, 
                         verbose = FALSE, 
                         ## Now specify the different degrees of freedom to apply 
                         tuneGrid = tune.grid.neuralnet)


save(nnetFit, file =  "./Outputs/nnetFit2.RData")

p1 <- ggplot2::ggplot(nnetFit)  +
  ggplot2:: scale_colour_manual(values = rep(unname(boe_cols),1)) + 
  ggplot2:: theme_minimal(base_size = 9) #+ 
#ggplot2:: labs(subtitle = paste0("Cross validation of neural network model over tuning grid")) 

p2 <- ggplot2:: ggplot(nnetFit, plotType = "level", scales = list(x = list(rot = 90))) + 
  ggplot2:: theme_minimal(base_size = 9) + 
  ggplot2:: scale_fill_gradient(low = "white", high = "darkred") 

grid.arrange(p1, p2, top = textGrob("Chart 4: Cross validation of neural network model over tuning grid",gp=gpar(fontsize=9,font=3))) #+ ggplot2:: labs(subtitle = paste0("Cross validation of neural network model over tuning grid")) 

