library(dplyr) # A powerful package for data manipulaion
library(data.table) # Includes useful tools and functions for manipulating data
library(fBasics) # Include useful tools to analyze financial, time-series data such as summary statistics
library(fPortfolio) # Includes useful tools for portfolio analysis
library(PerformanceAnalytics) # A powerful, popular package for portfolio analysis
library(quantmod) # Includes functions for optimization and also functions to pull data directly from Internet
library(xts) # Includes functions for time-series analysis
library(forecast)
library(jtools)
library(sandwich)
library(broom)
library(ggstance)
library(ggplot2)
library(quadprog)
library(restriktor)
library(tidyr)
library(reshape2)


# ASsign the target ticker/fund here
ticker <- "TRAIX"
getSymbols(ticker, from = "1999-01-01", to = "2019-02-28")
# If the periodicity needs to be changed, do so through the quantmod package below
Month.Fund <- monthlyReturn(TRAIX)

# Select the desired index fund tickers
index.list <- c("SPY", "IWM", "IEFA", "SCZ", "IEMG", "VNQ", "AGG", "TLT", "GSC", "BIL")
getSymbols(index.list, from = "1999-01-01", to = "2019-02-28")

Index.Returns <- data.frame()

for (i in seq(index.list)){
    nam <- paste("Month",index.list[i],sep=".")
    assign(nam, monthlyReturn(get(index.list[i])))
}

Total.Returns <- cbind(Month.Fund, Month.SPY, Month.IWM, Month.IEFA, Month.SCZ, Month.IEMG, Month.VNQ, Month.AGG, Month.TLT, Month.GSC, Month.BIL)

Total.Returns <- na.omit(Total.Returns)
colnames(Total.Returns) <- c(ticker, index.list)

Fund <- tslm(formula = TRAIX ~ ., data = Total.Returns)
plot_summs(Fund, robust = "HC3")

# This is the constraint for Restriktor. Currently set to all coefficients must sum to 100%
my.Const <- "SPY + IWM + IEFA + SCZ + IEMG + VNQ + AGG + TLT + GSC + BIL == 1"

Const.Coeff <- restriktor(Fund, constraints = my.Const)

# Store Coefficients & Standard errors into a variable
Estimates <- summary(Const.Coeff)$coefficients[,1:2]

barplot(Estimates[,1], col = "Blue")

Estimatesrev <- apply(Estimates, 2, rev)
Estimates.df <- data.frame(Estimatesrev)


