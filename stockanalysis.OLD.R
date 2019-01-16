#Analyze stock data for key pharmaceutical companies
#Addi Faerber, 2018

#Data is Breakthrough Therapy spreadsheet on Google Drive: 
#1anzFAEoavV9ET1EdNhiu2NZElxHOKfFkG0Vz4qXHfqI
#Created by Addi Faerber and Rachna Shah

library(plyr)
library(tidyverse)
library(googlesheets)
library(quantmod)


#Import Data---------------------------------------------------------
#authenticate with Google Sheets - look for access key to auth
gs_ls()

stockKey <- "1anzFAEoavV9ET1EdNhiu2NZElxHOKfFkG0Vz4qXHfqI"
breakthroughKey <- "18rneVijVq1L1k0m6i0kcC7H_Knp5OV2KPwI7AHWI-1g"

#Pull data for stock values
stocks <- gs_key(x = stockKey) %>% 
  gs_read(ws = "Master", 
          na = c("","."),
          col_names = c("company","date","value","volume","rel_date"), 
          skip = 1)
 

#Pull breakthrough Data
drugs <- gs_key(x = breakthroughKey) %>% 
  gs_read(ws = "Master", range = cell_cols(1:61), na = c("",".") ) %>% 
  dplyr::rename(drug = 'Trade Name') %>% 
  dplyr::rename(company = `Manufacturer`) %>% 
  dplyr::rename(drugclass = `Category for Code`) %>% 
  dplyr::rename(ticker = `Manufacturer Stock Symbol`) %>% 
  mutate(date = as.Date(`Breakthrough Designation`,"%m/%d/%Y")) %>% 
  select(drug, company, date, drugclass, ticker) 




#Tidy Data---------------------------------------------------------
stocks <- stocks %>% 
  mutate(date = as.Date(date,"%m-%d-%Y")) %>% 
  filter(!is.null(value))

#Create new variables--------------------------------------------------------

#Create dataset for hepatitis C drugs
hepcdrugs <- drugs %>% 
  filter(drugclass == "hepatitis C")

lungdrugs <- drugs %>% 
  filter(drug %in% c("Zykadia","Alecensa","Tagrisso","Alunbrig","Keytruda","Tecentriq","Xalkori")) %>% 
  filter(drugclass == "lung cancer")


#the old crude way...
#hepcdrugs <- data.frame(date = c(as.Date("2013-04-25"),as.Date("2013-10-10"),as.Date("2013-10-22"),as.Date("2014-04-22"),as.Date("2014-06-30"),as.Date("2014-10-10"),as.Date("2016-09-30")), 
#                        drug = c("Daklinza", "Sovaldi", "Zepatier", "Epclusa", "Technivie", "Harvoni", "Mayvret"), 
#                        company = c("Bristol Myers-Squibb", "Gilead","Merck", "Gilead", "AbbVie", "Gilead", "AbbVie"))


#lungdrugs <- data.frame(date = c(as.Date("2013-03-15"),as.Date("2013-06-26")), 
#                        drug = c("Zykadia","Alecensa"), 
#                        company = c("Novartis","Roche"))

#Plot Horserace Diagrams---------------------------------------------------------

#Horserace function takes in a list of companies 
horserace <- function(drugs) {
#horserace prints a graph of stock values with dates of breakthrough therapy designations overlayed. 
  #input is a data frame with columns for the drug name, company and date of breakthrough designation.
  
  #create list of companies out of drug table
  companies <- unique(unlist(str_split(drugs$company,"([ /-])")))
  
  #filter out only the stock prices for companies in the list
  companydata <- stocks %>% 
  filter(str_detect(company, companies))
  

  chart <- ggplot()+
    geom_line(data = companydata, mapping = aes(x = date, y = value, color = company)) +
    geom_vline(data = drugs, mapping = aes(xintercept=date)) +
    geom_text(data = drugs, mapping = aes(x = date, y = 0, angle=90, vjust = 0, hjust = 0, label = drug)) +
    facet_grid(company ~ .) +
    theme_minimal()+
    theme(legend.position = "bottom") +
    labs(title = "Stock Values of Pharmaceutical Manufacturers with \nBreakthrough Designated Drugs")
  print(chart)
  return()
}


#Lung Cancer Horserace Graph
horserace(hepcdrugs)

#ALK+ Lung Cancer Therapies Graph
horserace(lungdrugs)

#Next task - Fix mapping of stock company names and breakthrough therapy company names. They have to match. Just fix it. 

#Calculate Market Rate of Return --------------------------------------

#General strategy is to get the company name and the date of the breakthrough therapy designation from the drugs data
#then pull the stock price data from -120 to +120. 
#calculate the market rate of return (day's closing stock value as % of stock value on day -120)
#and set index values of -120 to +120 to line up the data with other company's stocks. 
#breakthrough therapy designation is the unit of analysis, so companies may end up in the data multiple times. 

get240stocks <- function(drugs) {
  #The g240 stocks function takes in an object of one row of the breakthough drugs data frame, designation dates and companies, 
  #then pulls the stock values for -120 to +120 days around the designation
  #indexes the values as % of stock price at day -120
  #and returns a data frame containing the company, drug, date, stock value, indexed date (-120 to +120), and indexed stock value
  
  #select data for the company 120 days before and after the breakthrough therapy designation date
  #note the statistical window is 120 stock trading days. 
  #The stocks data is already in trading days... so I can just take a crude -120 +120 to get trading days
  testtab <- stocks %>% 
    filter(company == drugs$company) %>% 
    filter(between(date,as.Date(drugs$date-120),as.Date(drugs$date+120)))
  
  testtab <- testtab %>% 
    mutate(nlprice = (value - testtab[[1,"value"]]) / testtab[[1,"value"]]) %>% 
    mutate(indDate = date - drugs$date)
 
  return(testtab)
  
  
}

#Test creating a data vector for one drug: 
test <- data.frame(date = c(as.Date("2013-04-25"),as.Date("2013-10-10"),as.Date("2013-10-22"),as.Date("2014-04-22"),as.Date("2014-06-30"),as.Date("2014-10-10"),as.Date("2016-09-30")),
                   drug = c("Daklinza", "Sovaldi", "Zepatier", "Epclusa", "Technivie", "Harvoni", "Mayvret"), 
                   company = c("Bristol Myers Squibb", "Gilead","Merck", "Gilead", "AbbVie", "Gilead", "AbbVie"))
                   
#test <- data.frame(date = c(as.Date("2013-04-25")), 
#                   drug = c("Daklinza"), 
#                   company = c("Bristol Myers Squibb"))


analysis <- ddply(hepcdrugs, .(drug), function(x) get240stocks(x))


ggplot()+
  geom_line(data = analysis, mapping = aes(x = indDate, y = nlprice, color = company)) +
  facet_grid(drug ~ .) +
  theme_minimal()+
  theme(legend.position = "bottom") +
  labs(title = "Stock Values of Pharmaceutical Manufacturers with \nBreakthrough Designated Drugs")

