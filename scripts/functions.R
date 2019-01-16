#getGoogleSpreadsheet() Get Google Spreadsheet ---------------------------------------------------------
#authenticate with Google Sheets - look for access key to auth
getGoogleSpreadsheet <- function() {
  library(googlesheets)
  require(tidyverse)
  
  gs_ls()
  
  breakthroughKey <- "18rneVijVq1L1k0m6i0kcC7H_Knp5OV2KPwI7AHWI-1g"
  
  #Pull breakthrough Data
  raw <- gs_key(x = breakthroughKey) %>% 
    gs_read(ws = "Master", range = "A1:BV222",na = c("",".") ) 
  
  drugs <- raw %>% 
    rename(company = `Manufacturer`) %>% 
    rename(ticker = `Manufacturer Stock Symbol`) %>%
    rename(combo = `Combination Drug`) %>% 
    rename(supplement = `Supplement`) %>%
    rename(desig_date = `Breakthrough Designation`) %>%
    rename(indication = `Breakthrough Indication`) %>%
    rename(approv = `FDA Approval`) %>%
    rename(approv_ind_scope = `Is approved indication same, narrower or broader than breakthrough`) %>%
    rename(scope_line = `Changed nth line of therapy`) %>%
    rename(scope_pop = `Changed Population`) %>%
    rename(scope_combo = `Changed Combination`) %>%
    rename(drugclass = `Category for Code`) %>%
    rename(rescind = `Breakthough Rescinded`) %>%
    rename(submit = `Submitted to FDA`) %>%
    rename(ac = `Advisory Committee`) %>%
    rename(designate_b4_sub = `Designated before Submitted`) %>%
    rename(designat_approv = `Designation to Approval`) %>%
    rename(designat_submit = `Designation to Submission`) %>%
    rename(submit_approv = `Submission to Approval`) %>%
    rename(accelerated = `Accelerated Approval`) %>%
    rename(priority = `Priority Review`) %>%
    rename(fasttrack = `Fast Track`) %>%
    mutate(BTDdate = as.Date(desig_date,"%m/%d/%Y")) %>%   #convert breakthrough designation to date
    mutate(submit = as.Date(submit,"%m/%d/%Y")) %>% #convert date submitted to FDA into date
    mutate(approvDate = as.Date(approv,"%m/%d/%Y")) %>% #convert FDA approval to date
    mutate(NDA = as.integer(NDA)) %>% #convert NDA into int
    mutate(BLA = as.integer(BLA)) %>% #convert BLA into int
    mutate(designate_b4_sub = as.logical(designate_b4_sub)) %>% #convert Designated before Submitted into binary
    mutate(designat_submit = as.integer(designat_submit)) %>% #Convert designation to submission to int
    mutate(designat_approv = as.integer(designat_submit)) %>% #convert designtation to approval to int
    mutate(submit_approv = as.integer(submit_approv)) %>% #convert submission to approval to int
    mutate(accelerated = as.logical(accelerated)) %>% #convert accelerated approval to bin
    mutate(priority = as.logical(priority)) %>% #convert priority review to bin
    mutate(fasttrack = as.logical(fasttrack)) %>% #convert fast track to bin
    rename(flag = `Pure Cases`) %>% 
    mutate(promo = !is.na(`Promotional language narrow`)) %>%
    mutate(claims_Phase = as.character(`Data Column for Phase of Trial`)) %>% 
    mutate(claims_MentionTrials = recode(`Mentions trials supporting Breakthrough Designation`,yes=TRUE,Yes=TRUE, no=FALSE,No=FALSE, .missing = NA)) %>%
    mutate(claims_Quantifies = recode(`Shares results from trial`,yes=TRUE,Yes=TRUE, no=FALSE,No=FALSE, .missing = NA)) %>%
    mutate(claims_TrialData = recode(`Rachna - Claims about clinical trial findings`,yes=TRUE,Yes=TRUE, no=FALSE,No=FALSE, .missing = NA)) %>%
    mutate(claims_Effec = recode(`Rachna - Claims about the safety or effectiveness of the drug`,yes=TRUE,Yes=TRUE, no=FALSE,No=FALSE, .missing = NA)) %>%
    mutate(claims_Market = recode(`Rachna - Claims about the potential effects on the market`,yes=TRUE,Yes=TRUE, no=FALSE,No=FALSE, .missing = NA)) %>%
    rename(comp_year = `Comparison Year`) %>% 
    rename(comp_type = `Comparison: Previous or Next?`) %>% 
    mutate(comp_Phase = as.character(`Comparison Phase of Trial - Data Column`)) %>% 
    mutate(comp_MentionBTD = recode(`Comparison Mentions Breakthrough Therapy Designation`,yes=TRUE,Yes=TRUE, no=FALSE,No=FALSE, .missing = NA)) %>%
    mutate(comp_MentionTrials = recode(`Comparison Mentions trials`,yes=TRUE,Yes=TRUE, no=FALSE,No=FALSE, .missing = NA)) %>%
    mutate(comp_Quantifies = recode(`Comparison Quantifies specific results from trials`,yes=TRUE,Yes=TRUE, no=FALSE,No=FALSE, .missing = NA)) %>%
    mutate(comp_TrialData = recode(`Comparison claims about clinical trial findings`,yes=TRUE,Yes=TRUE, no=FALSE,No=FALSE, .missing = NA)) %>%
    mutate(comp_Effec = recode(`Comparison Claims about the safety or effectiveness of the drug`,yes=TRUE,Yes=TRUE, no=FALSE,No=FALSE, .missing = NA)) %>%
    mutate(comp_Market = recode(`Comparison Claims about the potential effects on the market`,yes=TRUE,Yes=TRUE, no=FALSE,No=FALSE, .missing = NA)) %>%
    mutate(pr = recode(`Press Release Link`, PR = TRUE, .missing = FALSE)) %>%
    rename(class1 = `Drug Class I`) %>%
    rename(class2 = `Drug Class II`) %>%
    rename(class3 = `Drug Class III`) %>%
    rename(class4 = `Drug Class IV`) %>% 
    mutate(approved = ifelse(is.na(approv),FALSE,TRUE)) %>%   #Create indicator variable for approved vs unapproved drugs
    mutate(initialApproval = ifelse(supplement==0,TRUE,FALSE)) %>% 
    # every product needs a name - Creates the drugName variable based on this order of names:
    # Trade Name/Brand Name
    # drug (Generic) name
    # development name/label
    mutate(drugName = ifelse(is.na(`Trade Name`),
                             ifelse(is.na(`Drug Name (Generic)`),`Development Name`,`Drug Name (Generic)`),
                             `Trade Name`))


  return(drugs)
}


#getStocks() Get Stock data ----------------------------------------------------------------
getStocks <- function(drug, start = as.Date("2012-01-01"), end = as.Date("2018-07-01")) {
  #Feed in a drug data object that has variables drugName, company, BTDdate, approvDate, drugclass, ticker
  #looks up stock data from a given start and stop date and a stock ticker symbol
  #and generates dataset containing daily stock values
  #for company, drug name, 
  #carries forward key dates for breakthrough designation and approval. 
  
  #Uses the tools in Quantmod, especially getSymbols
  library(quantmod)
  
  #select the stock tickers of companies (excluding duplicates, private companies and NAs)
  tryCatch
  getSymbols(drug$ticker, src = "yahoo", from = start, to = end, warnings = FALSE)
  
  stockData <- get(drug$ticker)
  stockData <- data.frame(index(stockData), coredata(stockData)) %>% #index is the date of the timeseries and coredata is the data
    mutate(company = drug$company) %>%
    mutate(drugName = drug$drugName) %>%
    mutate(BTDdate = drug$BTDdate) %>%
    mutate(approvDate = drug$approvDate) %>%
    rename(value = paste(drug$ticker,".Close", sep = "")) %>%
    rename(date = index.stockData.) %>%
    mutate(ticker = drug$ticker) %>%
    mutate(daysFromBTD = as.integer(date - BTDdate)) %>%
    mutate(daysFromApprov = as.integer(date - approvDate)) %>%
    mutate(UID = paste(company,drugName,as.character(BTDdate)),sep = "-") %>% 
    select(UID, company, drugName, BTDdate, approvDate, ticker, date, daysFromBTD, daysFromApprov, value)
  
  return(stockData)
  
}

#makeStockDataset() Make a full dataset of stock data for a given data frame of drugs------
#calls getStocks
makeStockDataset <- function(drugs) {
  stockData <- drugs %>% 
    rowwise() %>% 
    do(getStocks(.)) %>% 
    bind_rows() %>% 
    ungroup()
  
  return(stockData)
}

#CalcReturn() Calculate Market Rate of Return --------------------------------------

#General strategy is to get the company name and the date of the breakthrough therapy designation from the drugs data
#then pull the stock price data from -120 to +120. 
#calculate the market rate of return (day's closing stock value as % of stock value on day -120)
#and set index values of -120 to +120 to line up the data with other company's stocks. 
#breakthrough therapy designation is the unit of analysis, so companies may end up in the data multiple times. 


calcReturn <- function(stocks, eventDate, reldate, window) {
  #Calculate the % increase from a given day
  #based on an event date, relative days from event date and a range of days to calculate
  #See method of MacKinlay: Event Analysis in Economics and Finance, 
  #used in Overgaard, J Invest Med. 2000, Hwang PLoS One 2013, Rothenstein JNCI 2011.
  require("dplyr")
  
  eventDate <- enquo(eventDate)
  reldate <- enquo(reldate)


    
  subset <- stocks %>%
    group_by(company, drugName, !!eventDate) %>% 
    arrange(`date`, .by_group = TRUE) %>% 
    filter(!!reldate >= -window & !!reldate <= window) %>% 
    filter(!eval(!!reldate) %in% c(-2, -1, 0, 1, 2)) %>% 
    mutate(mktreturn = (value - first(value)) / first(value)) %>% 
    mutate(pre = cut(!!reldate,c(-window,0,window), include.lowest = TRUE, labels=c("pre","post"))) 
  
    
  return(subset)
}

#returnsTable() Generate a table of average changes in stock values before and after key date-------
returnsTable <- function(dataset, reldate) {
  #give a stock dataset, and the relative date (BTD or approval), 
  #isolate data to window 
  #calculate a table of statistics on the returns for each event
  reldate <- enquo(reldate)
  
  table <- dataset %>% 
    group_by(UID, pre) %>% 
    summarize(n = n(), 
              start = min(!!reldate), 
              end = max(!!reldate), 
              min = min(pcReturn), 
              mean = mean(pcReturn), 
              max = max(pcReturn), 
              sd = sd(pcReturn)) 
 print(table)
 return(table)
}

#horserace() Plot Horserace Diagrams---------------------------------------------------------

horserace <- function(stocks) {
  #horserace prints a graph of stock values with dates of breakthrough therapy designations overlayed. 
  #input is a data frame with columns for the drug name, company and date of breakthrough designation.
  
  tmpdrugs <- stocks %>% 
    group_by(drugName, company, BTDdate) %>% 
    count(drugName, company, BTDdate)
  
  chart <- ggplot()+
    geom_line(data = stocks, mapping = aes(x = date, y = value, color = company)) +
    geom_vline(data = tmpdrugs, mapping = aes(xintercept=BTDdate)) +
    geom_text(data = tmpdrugs, mapping = aes(x = BTDdate, y = 0, angle=90, vjust = 0, hjust = 0, label = drugName)) +
    facet_grid(company ~ ., scales = "free") +
    theme_minimal()+
    theme(legend.position = "none") +
    labs(title = "Stock Values of Pharmaceutical Manufacturers with \nBreakthrough Designated Drugs",
         subtitle = "Vertical lines indicate dates of Breakthrough Therapy Designations")
  print(chart)
  return()
}


#plotReturns() Plot Market Rate of Return -------------------------------------- 
#This plots
#the return rate (not the stock value) for 120 days before and after the event
#date of the drug (approval or breakthrough therapy designation)

plotReturns <- function(stockdata, reldate) {
  reldate <- enquo(reldate)
  
  chart <- stockdata %>% 
    ggplot()+
    geom_line(data = stockdata, mapping = aes(x = !!reldate, y = pcReturn)) +
    facet_grid(UID~., scales = "fixed") +
    geom_vline(mapping = aes(xintercept=0)) +
    theme_minimal()+
    theme(legend.position = "bottom") +
    labs(title = "Stock Values of Pharmaceutical Manufacturers with \nBreakthrough Designated Drugs")
  
  print(chart)
  return()
}


