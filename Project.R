  
  install.packages("Quandl")
  library(Quandl)
  library(tidyquant)
  library(timetk)
  library(dplyr)
  
  ## Ingest Data
  price_data = tq_get('AAPL',
                      from = '2007-01-02',
                      to = '2011-11-30',
                      get = 'stock.prices')
  head(price_data)
  
  ## Return
  adjreturn = price_data %>% tq_transmute(select = adjusted,mutate_fun = periodReturn, period = 'daily', col_rename = 'return')
  # time series
  adjreturn_ts = ts(adjreturn$return)
  # plot and check stationary 
  plot.ts(adjreturn_ts, main='Apple Stock Prices(Adjust)', col=4)
  # Test Stationary ro not ? ANS Stationary
  install.packages("tseries")   
  library(tseries)
  adf.test(adjreturn_ts)
  
  ### auto.arima() ###
  install.packages("forecast")
  library(forecast)
  adjreturn_fit_auto = auto.arima(adjreturn_ts)
  
  # Check ACF & PACF
  acf(adjreturn_ts,lag.max = 10)
  pacf(adjreturn_ts,lag.max = 10)
  
  adjreturndiff3 = diff(adjreturn_ts, differences = 3)
  
  # Check ACF & PACF
  
  acf(adjreturndiff3,lag.max = 10)

  pacf(adjreturndiff3,lag.max = 10)
  
  ## Fit Model
  
  ml_arima_mg = arima(adjreturn_ts , order = c(0, 3, 2))
  
  residuals = residuals(ml_arima_mg)
 
  ml_arima__mg_fitted = adjreturn_ts - residuals

  ts.plot(adjreturn_ts , main="AAPL vs Fitted", ylab="Data")
  
  points(ml_arima__mg_fitted, type = "l", col = 2, lty = 2)
  
  
  ## Test Assumetion
  
  t.test(adjreturn_ts)
  
  Box.test(adjreturn_ts , lag=10, type='Ljung')
  
  Box.test(abs(adjreturn_ts), lag=10, type='Ljung')
  
  y = adjreturn_ts - mean(adjreturn_ts )
  
  Box.test(y^2, lag=10, type='Ljung')
  
  install.packages("MTS")
  
  library(MTS)
  
  archTest(y,10)
  
  ## GARCH
  install.packages("fGarch")
  library(fGarch)
  ## Data
  adjreturn_ts
  # norm
  
  library(fGarch)
  
  garchml1 = garchFit(~1+garch(1,1), data = adjreturn_ts, trace = F)
  
  summary(garchml1)
  
  vgarchml1 = volatility(garchml1)
  
  plot(garchml1)
  
  # student t
  
  garchml2 = garchFit(~1+garch(1,1), data = adjreturn_ts, trace = F,cond.dist = 'std')
  
  summary(garchml2)
  
  vgarchml2 = volatility(garchml2)
  
  # skew student t
  
  garchml3 = garchFit(~1+garch(1,1), data = adjreturn_ts, trace = F,cond.dist = 'sstd')
  
  summary(garchml3)
  
  vgarchml3 = volatility(garchml3)
  
  ## plot graph for comparison
  
  library(ggplot2)
  
  tdx = c(1:1238)/252+2007
  
  par(mfcol=c(3,1))
  
  plot(tdx,vgarchml1,xlab='Daily',ylab='Volatility',type='l',ylim=c(0,0.1))
  
  title(main='(a) Gaussian')
  
  plot(tdx,vgarchml2,xlab='Daily',ylab='Volatility',type='l',ylim=c(0,0.1))
  
  title(main='(b) Student-t')
  
  plot(tdx,vgarchml3,xlab='Daily',ylab='Volatility',type='l',ylim=c(0,0.1))
  
  title(main='(c) Skew Student-t')
  
  
  # Correlation
  
  cor(cbind(vgarchml1, vgarchml2, vgarchml3))
  
  ## ARMA-GARCH
  ## Data
  adjreturn_ts
  # norm
  
  armagarchml1 = garchFit(~arma(0,2)+garch(1,1), data = adjreturn_ts, trace = F)
  
  summary(armagarchml1)
  
  plot(armagarchml1)
  
  varmagarchml1 = volatility(armagarchml1)
  
  # student t
  
  armagarchml2 = garchFit(~arma(0,2)+garch(1,1), data = adjreturn_ts, trace = F,cond.dist = 'std')
  
  summary(armagarchml2)
  
  varmagarchml2 = volatility(armagarchml2)
  
  # skew student t
  
  armagarchml3 = garchFit(~arma(0,2)+garch(1,1), data = adjreturn_ts, trace = F,cond.dist = 'sstd')
  
  summary(armagarchml3)
  
  varmagarchml3 = volatility(armagarchml3)
  
  ## plot graph for compairson
  
  library(ggplot2)
  
  tdx = c(1:1238)/252+2007
  
  par(mfcol=c(3,1))
  
  plot(tdx,varmagarchml1,xlab='Daily',ylab='Volatility',type='l',ylim=c(0,0.1))
  
  title(main='(a) Gaussian')
  
  plot(tdx,varmagarchml2,xlab='Daily',ylab='Volatility',type='l',ylim=c(0,0.1))
  
  title(main='(b) Student-t')
  
  plot(tdx,varmagarchml3,xlab='Daily',ylab='Volatility',type='l',ylim=c(0,0.1))
  
  title(main='(c) Skew Student-t')
  
  
  # Correlation
  
  cor(cbind(varmagarchml1, varmagarchml2, varmagarchml3))
  
  armagarchml4 = garchFit(~arma(3,2)+garch(1,1), data = adjreturn_ts, trace = F)
  
  summary(armagarchml4)
  
  varmagarchml4 = volatility(armagarchml4)
  
  # student t
  
  armagarchml5 = garchFit(~arma(3,2)+garch(1,1), data = adjreturn_ts, trace = F,cond.dist = 'std')
  
  summary(armagarchml5)
  
  varmagarchml5 = volatility(armagarchml5)
  
  # skew student t
  
  armagarchml6 = garchFit(~arma(3,2)+garch(1,1), data = adjreturn_ts, trace = F,cond.dist = 'sstd')
  
  summary(armagarchml6)
  
  varmagarchml6 = volatility(armagarchml6)
  
  ## plot graph for compairson
  
  library(ggplot2)
  
  tdx = c(1:1238)/252+2007
  
  par(mfcol=c(3,1))
  
  plot(tdx,varmagarchml4,xlab='Daily',ylab='Volatility',type='l',ylim=c(0,0.1))
  
  title(main='(a) Gaussian')
  
  plot(tdx,varmagarchml5,xlab='Daily',ylab='Volatility',type='l',ylim=c(0,0.1))
  
  title(main='(b) Student-t')
  
  plot(tdx,varmagarchml6,xlab='Daily',ylab='Volatility',type='l',ylim=c(0,0.1))
  
  title(main='(c) Skew Student-t')
  
  
  # Correlation
  
  cor(cbind(varmagarchml4, varmagarchml5, varmagarchml6))
  
  
