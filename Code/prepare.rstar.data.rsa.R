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

data.start <- c(1961,1)
data.end   <- c(2016,1)

#------------------------------------------------------------------------------#
# Get Raw Data
#------------------------------------------------------------------------------#

gdp.rsa             <- read.csv("C:/FinMetricsProject/rawData/NAEXKP01ZAQ661S.csv")

price.index.rsa     <- read.csv("C:/FinMetricsProject/rawData/ZAFCPIALLQINMEI.csv")

inflation.rsa       <- read.csv("C:/FinMetricsProject/rawData/FPCPITOTLZGZAF.csv")

overnightrate.rsa   <- read.csv("C:/FinMetricsProject/rawData/IRSTCB01ZAM156N.csv")

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

# Define inflation

inflation <- inflation.rsa

quarter<-seq(as.Date("1961-01-01"), as.Date("2016-01-01"), by="quarter")
estinflation<-approx(inflation$FPCPITOTLZGZAF, n=length(quarter))

newdf<-data.frame(quarter, estinflation$y)

inflation.q <- newdf
colnames(inflation.q) <- c("DATE","Inflation")

inflation <- xts(inflation, order.by = as.POSIXct(inflation$DATE))

#Old method

inflation.expectations <- (inflation + lag(inflation, k=1) + lag(inflation, k=2) + lag(inflation, k=3))/4


inflation.num <- as.numeric(estinflation$y) 
inflation.num <- as.vector(inflation.num)

inflation.num.L1 <- lagpad(inflation.num,1)
inflation.num.L2 <- lagpad(inflation.num,2)
inflation.num.L3 <- lagpad(inflation.num,3)

#New method

inflation.expectations.new <- ((inflation.num + inflation.num.L1 + inflation.num.L2 + inflation.num.L3))/4
inflation.expectations.new


# Define ST rate

interest <- overnightrate.rsa

data.out <- window(cbind(gdp.log, inflation, inflation.expectations.new, interest),start = data.start, end = data.end)
write.table(data.out,file = 'inputData/rstar.data.us.csv', sep = ',',
            col.names = TRUE, quote = FALSE, na = '.', row.names = FALSE)
}



