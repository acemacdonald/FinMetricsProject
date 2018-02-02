#------------------------------------------------------------------------------#
# File:        prepare.rstar.data.ea.R
#
# Description: This file (1) compiles and (2) prepares the data to be used in
#              HLW for the Euro Area.
#------------------------------------------------------------------------------#
rm(list = ls())
source("C:/FinMetricsProject/Code/utilities.R")

# Load time series library
if (!require("tis")) {install.packages("tis"); library('tis')}

# Set up seasonal adjustment
if (!require("seasonal")) {install.packages("seasonal"); library('seasonal')}


#------------------------------------------------------------------------------#
# Get Raw Data
#------------------------------------------------------------------------------#

gdp.rsa             <- read.csv("C:/FinMetricsProject/rawData/NAEXKP01ZAQ661S.csv")

price.index.rsa     <- read.csv("C:/FinMetricsProject/rawData/ZAFCPIALLQINMEI.csv")

price.index.rsa.core     <- read.csv("C:/FinMetricsProject/rawData/ZAFCPICORQINMEI.csv")

overnightrate.rsa       <- read.csv("C:/FinMetricsProject/rawData/IRSTCB01ZAM156N.csv")

#------------------------------------------------------------------------------#
# Prepare Data
#------------------------------------------------------------------------------#

# Set the start and end dates of the data used in the estimation

data.start <- c(1960,1)
data.end   <- c(2016,3)

colnames(gdp.rsa) <- c("DATE", "GDP")
View(gdp.rsa)

# Take log of real GDP
gdp.log <- log(gdp.rsa$GDP)
gdp.log <- cbind(gdp.rsa, gdp.log)
gdp.log <- gdp.log[-2]                     #Only logs the second column

#=================
# Inflation         #Still need to find out how to merge automatically
#==============

# Create an annualized inflation series using the price index
# First convert data.frame to time series object 

colnames(price.index.rsa) <- c("DATE","PriceIndex")
price.index.rsa$DATE <- as.Date((price.index.rsa$DATE),format="%Y-%m-%d")
price.index <- cbind(price.index.rsa, price.index.rsa$PriceIndex)


price.index.rsa <- xts(price.index.rsa, order.by = price.index.rsa$DATE)
price.index <- xts(price.index.rsa, order.by = as.POSIXct(price.index.rsa$DATE))


inflation <- 400*log(price.index/Lag(price.index, k=12))   #This does not run correctly

#Calculate inflation

inflation <- 400*(price.index/Lag(price.index, k=12))


View(inflation)

interest <- overnightrate.rsa





