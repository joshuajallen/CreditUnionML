library(tidyverse)
library(data.table)
library(stargazer)
basePath = file.path("//researchhub/files/May/Credit Union Failure/SRDDfolder")

# source data from files
dataRaw = data.table:: fread(file.path(basePath, "appdatawin.csv"), stringsAsFactors = FALSE)

#select columns to keep
cols = colnames(dataRaw)[which(!grepl(pattern = "failure", x = colnames(dataRaw), ignore.case = TRUE))]
#prepare data 
dataClean = dataRaw[, match(cols, colnames(dataRaw)), with = FALSE][, FirmName := Firm.Name.x][,-"Firm.Name.x"]

variables<-c("roa", "pctarrears", "loandeposit", "simplecapitalratio",
             "provlns", "logassets", "unsecassets", "CY_30E", "arrears312", "arrearsg12",
             "CPI", "GDP.Growth.Quarter.Last.Year", "Base.Rate", "Regunemployment")

y <- dataClean[, variables, with = FALSE]

colnames(y)<- c("Return on Assets (%)","Arrears rate (%)", "Loan-deposit ratio (%)", "Risk-adjusted capital ratio (%)", 
                "Provisions-loans ratio (%)", paste0("Log assets(", "\u00A3", ")"), "Unsecured loans to assets (%)", 
                "Capital ratio (%)", "Arrears < 12 Months (%)", "Arrears > 12 Months (%)", "Inflation (%, CPI)", 
                "GDP Growth (Quarterly, %)",  "Bank Rate (%)", "Regional Unemployment Rate (%)")

stargazer:: stargazer(y, title="Summary Statistics", type="html", out = "./Outputs/summarystats.html", digits=1)

#------------------------------------------------------------------
# handle missing values 


dataCleanImputed = as.data.frame(dataClean) %>% 
  dplyr:: mutate(CY_30E = if_else(is.na(CY_30E) & DEFAULT == 1, case_when(DEFAULT == 1 ~ mean(CY_30E, na.rm = T)), CY_30E)) %>% 
  dplyr:: mutate(TotalAssets = if_else(is.na(TotalAssets) & DEFAULT == 1, case_when(DEFAULT == 1 ~ mean(TotalAssets, na.rm = T)), TotalAssets)) %>% 
  dplyr:: mutate(roa = if_else(is.na(roa) & DEFAULT == 1, case_when(DEFAULT == 1 ~ mean(roa, na.rm = T)), roa)) %>% 
  dplyr:: mutate(pctarrears = if_else(is.na(pctarrears) & DEFAULT == 1, case_when(DEFAULT == 1 ~ mean(pctarrears, na.rm = T)), pctarrears)) %>% 
  dplyr:: mutate(pctarrears = if_else(is.na(pctarrears) & DEFAULT == 1, case_when(DEFAULT == 1 ~ mean(pctarrears, na.rm = T)), pctarrears)) %>% 
  dplyr:: mutate(provcov = if_else(is.na(provcov) & DEFAULT == 1, case_when(DEFAULT == 1 ~ mean(provcov, na.rm = T)), provcov)) %>% 
  dplyr:: mutate(logassets = if_else(is.na(logassets) & DEFAULT == 1, case_when(DEFAULT == 1 ~ mean(logassets, na.rm = T)), logassets)) %>% 
  dplyr:: mutate(unsecassets = if_else(is.na(unsecassets) & DEFAULT == 1, case_when(DEFAULT == 1 ~ mean(unsecassets, na.rm = T)), unsecassets)) %>% 
  dplyr:: mutate(simplecapitalratio = if_else(is.na(simplecapitalratio) & DEFAULT == 1, case_when(DEFAULT == 1 ~ mean(simplecapitalratio, na.rm = T)), simplecapitalratio)) %>% 
  dplyr:: mutate(arrears312 = if_else(is.na(arrears312) & DEFAULT == 1, case_when(DEFAULT == 1 ~ mean(arrears312, na.rm = T)), arrears312)) %>% 
  dplyr:: mutate(arrearsg12 = if_else(is.na(arrearsg12) & DEFAULT == 1, case_when(DEFAULT == 1 ~ mean(arrearsg12, na.rm = T)), arrearsg12)) %>%  
  dplyr:: mutate(provlns = if_else(is.na(provlns) & DEFAULT == 1, case_when(DEFAULT == 1 ~ mean(provlns, na.rm = T)), provlns)) %>% 
  dplyr:: mutate(CY_30E = if_else(is.na(CY_30E) & DEFAULT == 1, case_when(DEFAULT == 1 ~ mean(CY_30E, na.rm = T)), CY_30E)) %>% 
  dplyr:: mutate(TotalAssets = if_else(is.na(TotalAssets) & DEFAULT == 0, case_when(DEFAULT == 0 ~ mean(TotalAssets, na.rm = T)), TotalAssets)) %>% 
  dplyr:: mutate(roa = if_else(is.na(roa) & DEFAULT == 0, case_when(DEFAULT == 0 ~ mean(roa, na.rm = T)), roa)) %>% 
  dplyr:: mutate(pctarrears = if_else(is.na(pctarrears) & DEFAULT == 0, case_when(DEFAULT == 0 ~ mean(pctarrears, na.rm = T)), pctarrears)) %>% 
  dplyr:: mutate(pctarrears = if_else(is.na(pctarrears) & DEFAULT == 0, case_when(DEFAULT == 0 ~ mean(pctarrears, na.rm = T)), pctarrears)) %>% 
  dplyr:: mutate(provcov = if_else(is.na(provcov) & DEFAULT == 0, case_when(DEFAULT == 0 ~ mean(provcov, na.rm = T)), provcov)) %>% 
  dplyr:: mutate(logassets = if_else(is.na(logassets) & DEFAULT == 0, case_when(DEFAULT == 0 ~ mean(logassets, na.rm = T)), logassets)) %>% 
  dplyr:: mutate(unsecassets = if_else(is.na(unsecassets) & DEFAULT == 0, case_when(DEFAULT == 0 ~ mean(unsecassets, na.rm = T)), unsecassets)) %>% 
  dplyr:: mutate(simplecapitalratio = if_else(is.na(simplecapitalratio) & DEFAULT == 0, case_when(DEFAULT == 0 ~ mean(simplecapitalratio, na.rm = T)), simplecapitalratio)) %>% 
  dplyr:: mutate(arrears312 = if_else(is.na(arrears312) & DEFAULT == 0, case_when(DEFAULT == 0 ~ mean(arrears312, na.rm = T)), arrears312)) %>% 
  dplyr:: mutate(arrearsg12 = if_else(is.na(arrearsg12) & DEFAULT == 0, case_when(DEFAULT == 0 ~ mean(arrearsg12, na.rm = T)), arrearsg12)) %>%  
  dplyr:: mutate(provlns = if_else(is.na(provlns) & DEFAULT == 0, case_when(DEFAULT == 0 ~ mean(provlns, na.rm = T)), provlns))  


write.csv(dataCleanImputed, file = "./Data/CreditUnionMLData.csv")
