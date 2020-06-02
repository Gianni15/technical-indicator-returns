install.packages(c("quantmod", "PerformanceAnalytics"))
library(quantmod)
library(PerformanceAnalytics)

getSymbols("AAPL")
AAPL <- last(AAPL, "3 years")

prices <- Cl(AAPL)

# EMA
fast <- EMA(prices, n = 20)
slow <- EMA(prices, n = 100)

signal <- c()
signal[1:20] <- 0

for(i in 21:length(prices)){
  if(isTRUE(fast[i] > slow[i])) {
    signal[i] <- 1
  }
  else if (isTRUE(fast[i] < slow[i])) {
    signal[i] <- -1
  }
}

signal <- reclass(signal, prices)
trade <- Lag(signal)
ret1 <- dailyReturn(AAPL) * trade
names(ret1) <- "EMA"
charts.PerformanceSummary(ret1)

# EMA and RSI
n <- 14
rsi <- RSI(prices, n)

signal2 <- c()
signal2[1:100] <- 0

for(i in 101:length(prices)){
  if(isTRUE(fast[i] > slow[i])){
    signal2[i] <- 1
  }
  else if(isTRUE(rsi[i] > 70)){
    signal2[i] <- -1
  }
}

signal2 <- reclass(signal2, prices)
trade2 <- Lag(signal2)
ret2 <- dailyReturn(AAPL)*trade2
names(ret2) <- "EMA and RSI"
retall <- cbind(ret1, ret2)
charts.PerformanceSummary(retall, main = "EMA v. EMA and RSI",
                          color = bluemono)

# CCI and RSI
cci <- CCI(prices, n = 30, "SMA", c = 0.015)

signal3 <- c()
signal3[1:30] <- 0

for(i in 31:length(prices)) {
  if(isTRUE(cci[i] > 0)){
    signal3[i] <- 1
  }
  else if(isTRUE(rsi[i] > 70)){
    signal3[i] <- -1
  }
}

signal3 <- reclass(signal3, prices)
trade3 <- Lag(signal3)
ret3 <- dailyReturn(AAPL) * trade3
names(ret3) <- "CCI and RSI"
retall <- cbind(ret1, ret2, ret3)
charts.PerformanceSummary(retall, main = "EMA v. EMA and RSI v. CCI and RSI",
                          color = bluemono)