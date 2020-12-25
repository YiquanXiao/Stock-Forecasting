library(FinTS) #for function `ArchTest()`
library(rugarch) #for GARCH models
library(tseries) # for `adf.test()`
library(dynlm) #for function `dynlm()`
library(vars) # for function `VAR()`
library(nlWaldTest) # for the `nlWaldtest()` function
library(lmtest) #for `coeftest()` and `bptest()`
library(broom) #for `glance(`) and `tidy()`
library(car) #for `hccm()` robust standard errors
library(sandwich)
library(knitr) #for `kable()` 
library(forecast) 
library(ggplot2)
library(pdfetch)
library(tsbox)
library(stats)
library(zoo)
library(vrtest)
library(quantmod)

# load data
# by doing this way, we load the newest updated data from Yahoo Finance
# Here, the data is from 2007/01/03 to 2020/11/15
startDate = as.Date("2007-01-03") #Specify period of time we are interested in
endDate = as.Date("2020-11-15")
getSymbols("IBM", from = startDate, to = endDate)
ibm <- IBM$IBM.Close


# check the first 6 rows of the data 
head(ibm)

# convert to time-series objects
ibmts <- ts(ibm, start = c(2007, 3), end = c(2020, 318), frequency = 365)
data.frame(ibmts)


# visuals
plot.ts(ibmts) + title("Time Series Plot of IBM")
"From the plot, we can see that this data has nonconstant variance"

hist(ibmts, main = "Histogram of IBM", freq = FALSE, col = "grey")
"From the histogram, we can see that this data is not normal"

plot.ts(diff(ibmts), ylab = "Change in IBM") + title("First difference of IBM")
"From this plot, we can see that the variance of the first difference of IBM is not constant either"

# Normality Test and Log Transformation
shapiro.test(ibmts[0: 5000])
"Based on the result, we have p-value < 2.2e-16, so we reject the null hypothesis, 
which means that the data is not normal. Thus, we need to take log"
ibm_log <- log(ibm)

# Check for stationarity of variance and mean
plot.ts(ibm_log) + title("Time Series plot of Log of IBM")

# check the variance
Auto.VR(ibm_log)
"Based on the result, the variance is not constant"

adf.test(ibm_log$IBM.Close, k = 3)
"Based on the result, we have p-value = 0.5774, so we will accept the null hypothesis, 
which means the data is not stationary. Thus, we need to take the first difference"
ibm_diff <- diff(ibm_log)
ibm_diff <- na.remove(ibm_diff)
adf.test(ibm_diff, k = 2)
"Now, the p-value < 0.01, which means ibm_diff is stationary"


# Determining AR and MA components
ibm_diff_acf <- acf(ibm_diff, main = "ACF of IBM", lag.max = 50)
# zoom in
ibm_diff_pacf <- pacf(ibm_diff, main = "PACF of IBM", lag.max = 50)
"There are two consecutive outside the blue line, which means 2 lags are needed"

# Estimating ARIMA
auto.arima(ibm_diff)
"The result suggest we need to use ARIMA(2, 0, 2). However, we have already taken
the first difference. So, we actually need to use ARIMA(2, 1, 2)"
arima212 <- arima(ibm_log, order = c(2, 1, 2))
summary(arima212)
# visualize arima212
tsdiag(arima212)
# check the residuals
checkresiduals(arima212)
# check the stability
autoplot(arima212)
"the red dots are in the circle, which means it is stable"

# Estimate mean equation r = beta + error
arimar_ibm <- arima212$residuals
ibm.mean <- dynlm(arimar_ibm ~ 1)
summary(ibm.mean)
"we get an insignificant number for mean, so we just need to focus on volatility"


# Determination of ARCH Effect
ehatsq.ibm <- ts(resid(ibm.mean)^2)
ibm.archef <- dynlm(ehatsq.ibm ~ L(ehatsq.ibm))
summary(ibm.archef)
"Based on the result, it is signifiant, which means that we need to use ARCH model"

# Arch Test
ibm.archef1 <- ArchTest(arimar_ibm, lags = 1, demean = TRUE)
ibm.archef1
"From the above result, we can see that there is a ARCH effect"

# Model Estimation
rIBM <- dailyReturn(IBM) 
ug_spec <- ugarchspec(mean.model=list(armaOrder=c(1,0)))
ugfit = ugarchfit(spec = ug_spec, data = rIBM, out.sample = 10)
ugfit

# Conditional variance with GARCH model
hhat <- ts(ugfit@fit$sigma^2)
plot.ts(hhat)

# Model Forecasting
ugfore <- ugarchforecast(ugfit, n.head = 10)
ugfore

# Model Performance
fpm(ugfore)
# MSE: mean squared error: 2.900529e-04
# MAE: mean absolute error: 1.570920e-02




# IBM.train <- IBM[1:(NROW(IBM)-10)]
# IBM.test <- IBM[(NROW(IBM)-9):NROW(IBM)]
# rIBM.train <- dailyReturn(IBM.train)
# rIBM.test <- dailyReturn(IBM.test)
# ugfit2 <- ugarchfit(spec = ug_spec, data = rIBM.train)
# ugfit2
# ugfore2 <- ugarchforecast(ugfit2, n.ahead = 10)
# ugfore2






