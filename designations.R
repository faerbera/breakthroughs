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
desig <- breakthrough %>% 
  select(drugName, company, BTDdate, approvDate, approved, `Manuscript Category for Code`, accelerated, priority, fasttrack, class1 ) %>% 
  rename(indication = `Manuscript Category for Code`)

#Summary of Designations---------------------------------------------
require(tableone)
vars <- c("company", "BTDdate", "approvDate", "approved", "indication", "accelerated", "priority", "fasttrack", "class1")
catvars <- c( "company", "approved", "indication", "accelerated", "priority", "fasttrack", "class1")
designations <- CreateTableOne(vars = vars, factorVars = catvars, data = desig)
