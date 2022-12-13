# helper funs

### ------ Functions to clean and summarise data
#

#' @title BOE colour pallete used for charting  

boe_cols <- c(
  red            = "#A51140",
  black          = "#1E1E1E",
  stone          = "#CAC0B6",
  dark_teal      = "#005E6E",
  dark_blue      = "#002A42",
  plum           = "#752864",
  mid_blue       = "#165788",
  maroon         = "#6C0721",
  purple         = "#4E3780",
  grey           = "#999999",
  green          = "#006663", 
  orange         = "#D55E00", 
  orange2        = "#E69F00", 
  blue           = "#56B4E9")

MakeROCPlot <- function(data, modelType, title){
  
  ModelAccuracy %>% 
    dplyr:: filter(imbalanceMethod ==  paste0(modelType)) %>% 
    yardstick:: roc_curve(truth, .pred_1) %>%
    ggplot2:: ggplot(aes(x = 1 - specificity, y = sensitivity, fill = paste0(modelType), group = "1")) +
    ggplot2:: geom_path(aes( colour = unname(boe_cols)[1]), show.legend = F, lwd =2) +
    ggplot2:: geom_abline(lty = 4, colour = unname(boe_cols)[3], show.legend = FALSE, lwd = 2) +
    ggplot2:: scale_colour_manual(values = rep(unname(boe_cols),1)) + 
    ggplot2:: coord_equal() +
    ggplot2:: theme_minimal(base_size = 16) + 
    ggplot2:: labs(subtitle = paste0(title)) 
  
  
}


PrepForRecipe <- function(df, ...){
  
  for(i in 2:nrow(df)) {
    
    if((nrow(df) %% i) == 0) {
      
      i
      
      break()
      
    }
    
  }

  df <- df[,which(!colnames(df) %in% c("FRN","FirmName", "Year"))]
  
  df$DEFAULT <- as.factor(df$DEFAULT)
  
  return(df)
  
}

simple_recipe <- function(df){
  
  recipe(Raise ~ ., data = df) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    step_dummy(all_nominal(),-Raise) %>%  ##Check this, potentially remove, may create a bias of categorise, check number of levels per variables. %>%
    step_upsample(Raise, ratio = 0.5)
  
  
}

yncount <- function(df){
  
  y <- countraise(df, "Yes") 
  
  print(y)
  
  n <- countraise(df, "No") 
  
  print(n)
  
} 

##Function to obtain the perdicted class from the model and test data along with
##Probabilty scores.
predictAndProb <- function(model, testData,imbalanceMethod){
  pred_raw = predict(model, new_data = testData, 
                     type = "raw")
  pred_probs = predict(model, new_data = testData,
                       type = "prob") 
  pred_class = predict(model, new_data = testData,
                       type = "class")
  
  ##Building the predictions of the model and the data with probablity##
  predictions = tibble:: tibble(truth = testData$DEFAULT,
                       predicted = pred_class$.pred_class) %>% 
    cbind(pred_probs) %>% 
    dplyr:: mutate(imbalanceMethod = imbalanceMethod)
  
  as_tibble(predictions)
}

##Code that extracts performance metrics after running the model, this is a single
#model not one that is cross validated.

modelResults <- function(model, testData, type){
  
  modelAccuracy <- predictAndProb(model, 
                                  testData, 
                                  type )
  
  confMatrix <- yardstick:: conf_mat(modelAccuracy, truth, predicted)
  
  confMatrixStats <- summary(confMatrix)
  
  auc <- yardstick:: roc_auc(modelAccuracy, truth, .pred_1)

  modelStats <- rbind(confMatrixStats, auc) %>% #oobPredictionErrorResults
    mutate(type = type)
  
  plot <- modelAccuracy %>% 
    yardstick:: roc_curve(truth, .pred_1) %>% 
    autoplot
  
  
  list(confMatrix = confMatrix, 
       modelStats = modelStats,
       modelAccuracy = modelAccuracy,
       plot = plot)
}


#------------------------------------------------------------------------------


trainControlSample <- function(traindata, testdata, samplingMethod, method, resamplingmethod, numberFolds = 10, repeats = 5){
  
  ctrl <- trainControl(method = paste0(resamplingmethod), number = numberFolds, verboseIter = FALSE,  repeats = repeats, sampling = paste0(samplingMethod))
  
  set.seed(42)
  model_trained <- caret::train(DEFAULT ~ .,
                                data = traindata,
                                method = paste0(method),
                                #preProcess = c("scale", "center"),
                                trControl = ctrl)
  
  finalPreds <- data.frame(actual = testdata$DEFAULT, predict(model_trained, newdata = testdata, type = "prob"))
  
  finalPreds$predict <- as.factor(ifelse(finalPreds$X1 > 0.5, 1, 0))
  cm_over <- confusionMatrix(finalPreds$predict, testdata$DEFAULT) 
  
  Results <- data.table:: data.table(cm_over$byClass)
  colnames(Results)[1] = "Position"
  Results <- Results[, `:=`(Meausure = names(cm_over$byClass), 
                            resamplingmethod = paste0(resamplingmethod), 
                            samplingMethod = paste0(samplingMethod), 
                            modeltype = paste0(method))]
  
  return(list(Results, cm_over$table))
  
  
}
