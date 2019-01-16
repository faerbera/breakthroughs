#Tutorial on analysis of stock data with R
#https://www.datascience.com/blog/stock-price-time-series-arima
#Addi Faerber, 2018

library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(here)
here() # Should output current work directory