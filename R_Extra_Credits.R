# =========
# Setup
# =========
rm(list = ls())
install.packages(c(
  "quantmod",
  "PerformanceAnalytics",
  "PortfolioAnalytics",
  "ROI",
  "ROI.plugin.quadprog"
))

library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)

# Get Stock Prices --- only edit here later ---
tickers <- c("GOOGL", "XOM", "GLD", "CME", "MXUBY")   # placeholder
start_date <- "2025-01-01"                        # placeholder
rf_annual <- 0.04                                  # placeholder risk-free rate (4%)

# =========
# Functions
# =========

get_returns <- function(tickers, start_date) {
  # download prices
  suppressWarnings(getSymbols(tickers, from = start_date, src = "yahoo", auto.assign = TRUE))
  
  # merge adjusted prices
  prices_list <- lapply(tickers, function(sym) Ad(get(sym)))
  prices <- do.call(merge, prices_list)
  colnames(prices) <- tickers
  
  # compute daily returns
  rets <- na.omit(Return.calculate(prices, method = "log"))
  return(rets)
}

# =========
# Smoke test
# =========
rets <- get_returns(tickers, start_date)

# quick checks
print(dim(rets))          # 行=交易日数，列=股票数
print(head(rets, 3))      # 看前三行收益率

w_eq <- rep(1/ncol(rets), ncol(rets))
port_rets_eq <- rets %*% w_eq

rf_daily <- (1 + rf_annual)^(1/252) - 1
sharpe_eq <- (mean(port_rets_eq) - rf_daily) / sd(port_rets_eq)

sharpe_eq

