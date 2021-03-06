prep.rstar.us <- function(){
  
#------------------------------------------------------------------------------#
# File:        prepare.rstar.data.us.R
#
# Description: This file (1) compiles and (2) prepares the data used in
#              HLW for the US.
#------------------------------------------------------------------------------#
Loc <- paste0(Root, "C:/FinMetricsProject/Code")

# Load time series library
if (!require("tis")) {install.packages("tis"); library('tis')}

#------------------------------------------------------------------------------#
# Get Raw Data
#------------------------------------------------------------------------------#

# Set the start and end dates of the data used in the estimation
data.start <- c(1960,1)
data.end   <- c(2016,3)

# Import data using the function getFRED() in utilities.R
# If the connection does not work, try the old URL:
# "https://research.stlouisfed.org/fred2/data/GDPC1.txt"
# NOTE: The getFRED() function requires the wget command line utility;
#       users can also manually download the text files.

gdp.us             <- read.csv("C:/FinMetricsProject/rawData/GDPC1.csv")

price.index.us     <- read.csv("C:/FinMetricsProject/rawData/PCEPILFE.csv")

ny.discount.us     <- read.csv("C:/FinMetricsProject/rawData/INTDSRUSM193N.csv")

fed.funds.us       <- read.csv("C:/FinMetricsProject/rawData/FEDFUNDS.csv")

#------------------------------------------------------------------------------#
# Prepare Data
#------------------------------------------------------------------------------#

# Take log of real GDP
gdp.log <- log(gdp.us$GDPC1)
gdp.log <- cbind(gdp.us, gdp.log)
gdp.log <- gdp.log[-2]

# Create an annualized inflation series using the price index
# First convert data.frame to time series object 

price.index.us$DATE <- as.Date(as.character(price.index.us$DATE),format="%Y-%m-%d")
price.index <- xts(price.index.us, order.by = as.POSIXct(price.index.us$DATE))

#Calculate inflation

inflation <- 400*log(price.index.us/lag(price.index.us,k=12))

# Inflation expectations measure: 4-quarter moving average of past inflation


inflation.expectations <- (inflation + lagpad(inflation,1) + lagpad(inflation,2) + Llagpad(inflation,3))/4

# Express interest rate data on a 365-day basis
ny.discount.eff <- 100*((1+ny.discount.us/36000)^365 -1)
fed.funds.eff   <- 100*((1+fed.funds.us/36000)^365 -1)

# NY Fed discount rate is used prior to 1965; thereafter, use the effective federal funds rate
interest <- mergeSeries(window(ny.discount.eff, end = c(1964,4)),window(fed.funds.eff, start = c(1965,1)))

#------------------------------------------------------------------------------#
# Output Data
#------------------------------------------------------------------------------#
data.out <- window(cbind(gdp.log, inflation, inflation.expectations, interest),start = data.start, end = data.end)
write.table(data.out,file = 'inputData/rstar.data.us.csv', sep = ',',
            col.names = TRUE, quote = FALSE, na = '.', row.names = FALSE)
}
