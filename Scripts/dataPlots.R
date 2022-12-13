library(tidyverse)
library(scales)
library(ggplot2)
library(ggExtra)
library(ggcorrplot)
# exploratory analysis 
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
DataML <- PrepForRecipe(as.data.frame(DataML))

dataPlot <- tidyr:: gather(DataML, key = Feature, value = Position, -DEFAULT) %>% dplyr:: filter(Feature %in% c("logassets", "roa", "loandeposit", "pctarrears"))


#scatter plot

p <- ggplot2:: ggplot(dataPlot, ggplot2:: aes(x = DEFAULT, y = Position, group = Feature, colour = Feature)) +
  ggplot2:: geom_count() +
  geom_smooth(method = "loess", span = 0.3, se = FALSE) +
  ggplot2:: labs(title = NULL, subtitle = "Default rate against features",
                 y="Total assets", x=NULL)  +
  ggplot2:: theme_minimal() +
  ggplot2:: scale_color_manual(values = rep(unname(boe_cols), 100)) + 
  ggplot2:: facet_wrap(~Feature, scales = "free_y")


#hisogram 

p <- ggplot2:: ggplot(dataPlot, ggplot2:: aes(Position, fill = DEFAULT)) +
  # ggplot2:: geom_histogram(dplyr:: filter(dataPlot, DEFAULT ==1), ggplot2:: aes(Position)) +
  ggplot2:: geom_histogram(aes(y=..density.., fill = DEFAULT), bins = 30) +
  geom_density(alpha=.2, fill="#FF6666") +
  ggplot2:: labs(title = NULL, subtitle = "Default rate against features",
                 y="Total assets", x=NULL)  +
  ggplot2:: theme_minimal() +
  ggplot2:: scale_fill_manual(values = rep(unname(boe_cols), 100)) + 
  ggplot2:: facet_grid(.~Feature, scales = "free") + 
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())

p



#--------------------------------------------------------------------------
# box blot 

p <- ggplot2:: ggplot(dataPlot, ggplot2:: aes(x = DEFAULT, y = Position, fill = DEFAULT)) +
  ggplot2:: geom_boxplot() +
  ggplot2:: labs(title = NULL, subtitle = "Default rate against features",
                 y="Total assets", x=NULL)  +
  ggplot2:: theme_minimal() +
  ggplot2:: scale_fill_manual(values = rep(unname(boe_cols), 100)) + 
  ggplot2:: coord_flip() +
  ggplot2:: facet_wrap(.~Feature, scales = "free", ncol = 1) + 
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())

p


corr <- round(cor(dplyr:: select(DataML, -DEFAULT)), 1)

# Plot
ggcorrplot:: ggcorrplot(corr, hc.order = TRUE, 
                        type = "lower", 
                        lab = TRUE, 
                        lab_size = 3, 
                        method="circle", 
                        colors = c("#CAC0B6","white", "#006663"), 
                        title="Correlogram of Credit Union ML data", 
                        ggtheme=theme_bw)

