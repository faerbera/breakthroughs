#Analyze press releases of breakthrough therapies for key characteristics. 
#Addi Faerber, 2018

#Data is Breakthrough Therapy spreadsheet on Google Drive: 
#Created by Addi Faerber and Rachna Shah

library(tidyverse)
library(lubridate)
library(googlesheets)
library(tableone)

source("scripts/functions.R")



#Import Data---------------------------------------------------------
breakthrough <- getGoogleSpreadsheet()

#Analysis---------------------------------------------------------

#Compare Press releases from breakthrough therapy designations to other press releases---------------
 
compPR <- breakthrough %>% 
  select(drugName, starts_with("comp_")) %>% 
  mutate(group = "comparison press release") %>% 
  rename(year = comp_year, 
         type = comp_type, 
         Phase = comp_Phase, 
         MentionBTD = comp_MentionBTD, 
         MentionTrials = comp_MentionTrials, 
         Quantifies = comp_Quantifies, 
         TrialData = comp_TrialData,
         Effec = comp_Effec, 
         Market = comp_Market)
  

originalPR <- breakthrough %>% 
  select(drugName, BTDdate, starts_with("claims_") ) %>% 
  mutate(claims_year = year(BTDdate)) %>% 
  mutate(claims_MentionBTD = TRUE) %>% #all of the BTD announcements mention the BTD
  mutate(group = "breakthrough therapy press release") %>% 
  mutate(type = "BTD Press Release") %>% 
  rename(year = claims_year, 
         Phase = claims_Phase, 
         MentionBTD = claims_MentionBTD, 
         MentionTrials = claims_MentionTrials, 
         Quantifies = claims_Quantifies, 
         TrialData = claims_TrialData,
         Effec = claims_Effec, 
         Market = claims_Market) %>% 
  select(-BTDdate) 

pr <- bind_rows(originalPR, compPR) %>% 
  mutate(Phase = as.factor(Phase))


rm(originalPR, compPR)

summary(pr)

#--Table summary of frequency of observations between brkathrough therapy designation press releases versus comparison
pr %>% 
  group_by(group, type) %>% 
  filter(type != "not found") %>% 
  summarize(n = n(), 
            MentionsTrials = mean(MentionTrials, na.rm = T), 
            QuantifiesResults = mean(Quantifies, na.rm = T), 
            EfficacyClaims = mean(Effec, na.rm = T), 
            MarketDataClaims = mean(Market, na.rm = T)) 
  
  
listVars <- c("year","MentionTrials","Phase","MentionBTD","Quantifies","TrialData","Effec","Market")
catVars <- c("Phase","MentionTrials","Quantifies","TrialData","Effec","Market","year","MentionBTD")
table1 <- CreateTableOne(vars = listVars, data = pr[which(pr$type != "not found"),], factorVars = catVars, strata=c("type"))
outputTable1 <- print(table1, showAllLevels = TRUE)
write.csv(outputTable1, "./output/table1.csv")


