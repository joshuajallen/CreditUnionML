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
load(file.path(basePath, "./Outputs/nnetFit.RData"))

#------------------------------------------------------------------------------------------
#compute variable importance 
importance <- caret:: varImp(nnetFit, scale=FALSE)
# summarize importance
print(importance)

data <- tibble:: tibble(Importance = round(importance$importance$Overall, 1), Feature = row.names(importance$importance))
colnames(data) <- c("Importance", "Feature")

#p<- ggplot(data, aes(x= Importance, y= Feature, label=Importance)) + 
ggplot(data, aes(x=reorder(Feature, Importance), y= Importance, label=Importance)) + 
  geom_point(stat='identity', fill= "#A51140", size=12)  +
  geom_segment(aes(y = 0, 
                   x = Feature, 
                   yend = Importance, 
                   xend = Feature), 
               color = "#A51140") +
  geom_text(color="white", size=3) + 
  labs(title="", 
       subtitle="Relative feature importance", y = "Importance", x = NULL) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_flip() + 
  theme_minimal(base_size = 12) +     
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(), 
    panel.grid.major = element_blank()) 

control <- caret:: trainControl(method="repeatedcv", number=10, repeats=3)
model <- caret:: train(DEFAULT ~ ., data= train_data, method="nnet", trControl=control)

imp<-varImp(model)
plot(imp)

#----------------------------------------------------------------------------------
# define the control using a random forest selection function
control <- caret:: rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- caret:: rfe(DEFAULT~.,data = train_data, sizes=c(1:8), rfeControl=control)
save(results, file =  "./Outputs/RDEresults.RData")

# plot the results
ggplot(results, type=c("g", "o")) + 
  ggplot2:: geom_line(colour= "#A51140", size=1)  +
  ggplot2:: theme_minimal() + 
  ggplot2:: labs(title="", 
                 subtitle="Chart 5: Relative feature importance", y = "Importance", x = NULL) +
  ggplot2:: coord_flip() + 
  ggplot2:: theme_minimal(base_size = 16) +     
  ggplot2:: theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(), 
    panel.grid.major = element_blank()) 


               