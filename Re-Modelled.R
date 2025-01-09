
library(quantmod)
library(gtrendsR)
library(dplyr)
library(ggplot2)
library(caret)
library(forecast)
library(tseries)
library(randomForest)
library(xgboost)
library(reshape2)
library(tidyr)
library(corrplot)
library(writexl)
library(zoo)
library(rugarch)
library(ggcorrplot)
library(e1071)
library(patchwork)

# Ensure all the packages are installed.

# install.packages("patchwork")
# install.packages("ggcorrplot")
#install.packages(c("quantmod", "gtrendsR", "dplyr", "ggplot2", "caret", "forecast", "tseries", "randomForest", "xgboost", "reshape2", "tidyr", "corrplot", "writexl", "rugarch", "zoo"))

## hello Artur, just a little comment in saying that the document (very closely) follows the word limit of 2500 (2483) words, given all tables, formulas, and captions are removed. This assignment has been the foundation for my new passion for data and econometrics and therefore is my basis of applying whatever means there is to ensure a great grade =]

#================== VIX, S&P 500, NASDAQ, Dow Jones Data Importing and Preparation ===============================#


#> fetching data from yahoo finance

getSymbols("^VIX", from = "2014-10-15", to = "2024-10-15", src = "yahoo")
getSymbols("^IXIC", from = "2014-10-15", to = "2024-10-15", src = "yahoo")
getSymbols("^GSPC", from = "2014-10-15", to = "2024-10-15", src ="yahoo")
getSymbols("^DJI", from = "2014-10-15", to = "2024-10-15", src ="yahoo")


#> extracting closing prices and removing NA values
VIXData <- na.omit(Cl(VIX))
NasdaqData <- na.omit(Cl(IXIC))
GSPCData <- na.omit(Cl(GSPC))
DJIData <- na.omit(Cl(DJI))

par(mfrow = c(2,2))
plot(VIXData, main = "VIX - VIX Volatility", ylab = "VIX")
plot(NasdaqData, main = "Nasdaq - Daily Returns", ylab = "IXIC")
plot(GSPCData, main = "S&P500 - Daily Returns", ylab = "GSPC")
plot(DJIData, main = "Dow Jones - Daily Returns", ylab = "DJI")


#============== VIX/NASDAQ/GSPC/DJI Summary Statistics ============#

summaryStatistics <- function(data) {
  stats <- c(
    Mean = mean(data),
    Median = median(data),
    SD = sd(data),
    Min = min(data),
    Max = max(data),
    Q1 = quantile(data, 0.25),
    Q3 = quantile(data, 0.75),
    Kurtosis = kurtosis(data),
    Skewness = skewness(data)
  )
  return(stats)
}

VIXSummary <- summaryStatistics(VIXData)
NasdaqSummary <- summaryStatistics(NasdaqData)
GSPCSummary <- summaryStatistics(GSPCData)
DJISummary <- summaryStatistics(DJIData)

VIXSummary
NasdaqSummary
GSPCSummary
DJISummary

#=================================================================================#


#============================= Google Trends Graphs ==============================#

# Fetch Google Trends data to obtain selected keyword trends
## S&P 500
## Recession
## Inflation
## Stock Market Crash
## Stock Market News

# Set a time limit on which to perform the analysis on, and keep it steady throughout the other data in which you'll gather.

# Plot.

terms <- c("S&P 500", "Recession", "Inflation", "Stock Crash", "Stock Market News")

gTrendsData <- gtrends(terms, gprop = "web", time = "2014-10-15 2024-10-15") 
interest_over_time <- gTrendsData$interest_over_time 
interest_over_time$hits <- as.numeric(gsub("<1", "0", interest_over_time$hits)) 

tidyData <- interest_over_time %>%
  select(date, keyword, hits) %>%
  mutate(date = as.Date(date), hits = hits / max(hits) * 100)

ggplot(tidyData, aes(x = date, y = hits, colour = keyword)) + geom_line(linewidth = 1) + labs(title = "Google Trends Data involving VIX Related Keywords (2014 - 2024)", 
  x = "Date", 
  y = "Normalised Interest (%)", 
  colour = "Terms") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
  theme(legend.position = "bottom")






#========== VIX MERGED ==========#

# Fetch Google Trends data
terms <- c("S&P 500", "Recession", "Inflation", "Stock Crash", "Stock Market News")
gTrendsData <- gtrends(terms, gprop = "web", time = "2014-10-15 2024-10-15")
interest_over_time <- gTrendsData$interest_over_time
interest_over_time$hits <- as.numeric(gsub("<1", "0", interest_over_time$hits))

# Normalise Google Trends hits
tidyData <- interest_over_time %>%
  select(date, keyword, hits) %>%
  mutate(date = as.Date(date), hits = hits / max(hits) * 100)

mergedData <- tidyData %>%
  rename(KeywordHits = hits, Keyword = keyword) %>%
  bind_rows(data.frame(date = index(VIXData), KeywordHits = as.numeric(VIXData), Keyword = "VIX"))

ggplot(mergedData, aes(x = date, y = KeywordHits, colour = Keyword, group = Keyword)) +
  geom_line(linewidth = 1.05) +  
  geom_line(data = filter(mergedData, Keyword == "VIX"), linewidth = 1, colour = "red") +  
  labs(title = "VIX and Google Trends Data (2014 - 2024)",
       x = "Date", y = "Normalised Value (%)", colour = "Keywords") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold")) +
  scale_colour_manual(values = c("VIX" = "red", 
                                 "S&P 500" = "#ff9b9b", 
                                 "Recession" = "#c59292", 
                                 "Inflation" = "#cb8282", 
                                 "Stock Crash" = "#ba5151", 
                                 "Stock Market News" = "#8a4e4e"))


#========

# Function for Min-Max Scaling

# normalise all values, calculating the min-max ranges (na.rm = TRUE meaning to ignore any non-assigned values)
min_max_scale <- function(x) {
  
  # scale it all to 100 to align the trends of nasdaq to the google trends keywords
  scaled <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  
  # return
  return(scaled * 100)  # Scale to [0, 100]
}

tidyData <- interest_over_time %>%
  select(date, keyword, hits) %>%
  mutate(date = as.Date(date), hits = min_max_scale(hits), 
         keyword = factor(keyword))

# (apply log first, then scale)
NasdaqDataLog <- log(NasdaqData)
NasdaqDataScaled <- data.frame(
  date = index(NasdaqDataLog), 
  hits = min_max_scale(as.numeric(NasdaqDataLog)),
  keyword = "Nasdaq (Log)"
)

mergedDataNasdaq <- tidyData %>%
  rename(KeywordHits = hits, Keyword = keyword) %>%
  bind_rows(data.frame(date = NasdaqDataScaled$date, 
                       KeywordHits = NasdaqDataScaled$hits, 
                       Keyword = "Nasdaq (Log)")) %>%
  arrange(date)

ggplot(mergedDataNasdaq, aes(x = date, y = KeywordHits, colour = Keyword, group = Keyword)) +
  geom_line(linewidth = 1.05) +  
  geom_line(data = filter(mergedDataNasdaq, Keyword == "Nasdaq (Log)"), linewidth = 1, colour = "red") +  
  labs(title = "Nasdaq (Logarithmic Scale) and Google Trends Data (2014 - 2024)",
       x = "Date", y = "Normalised Value [%]", colour = "Keywords") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold")) +
  scale_colour_manual(values = c("Nasdaq (Log)" = "red", 
                                 "S&P 500" = "#7CB9E8", 
                                 "Recession" = "#89CFF0", 
                                 "Inflation" = "#6699CC", 
                                 "Stock Crash" = "#318CE7", 
                                 "Stock Market News" = "#4B9CD3"))


## Seems to be an inverse-ish relationship between the logarithmic values of Nasdaq and the google trends data.


# Fix this code, there is a major inconsistency in the lines of each keyword (e.g. Comparing IXIC.Close to the GoogleTrends Keywords)
## FIXED

# Look into enhancing the nature of the data above by, instead of using the normalised value as the Y axis, you use the logarithmic values, so that comparisons between the rate of changes in each keyword is more accurate and consistent.



#======== GSPC   ===========#

# log-transform and normalize GSPC data
GSPCDataLog <- log(GSPCData) 
GSPCDataScaled <- data.frame(
  date = index(GSPCDataLog), 
  hits = min_max_scale(as.numeric(GSPCDataLog)), 
  keyword = "S&P 500 (Log)"
)

# combine Google Trends data with GSPC data
mergedDataGSPC <- tidyData %>%
  rename(KeywordHits = hits, Keyword = keyword) %>%
  bind_rows(data.frame(date = GSPCDataScaled$date, 
                       KeywordHits = GSPCDataScaled$hits, 
                       Keyword = "S&P 500 (Log)")) %>%
  arrange(date)

ggplot(mergedDataGSPC, aes(x = date, y = KeywordHits, colour = Keyword, group = Keyword)) +
  geom_line(linewidth = 1.05) +  
  geom_line(data = filter(mergedDataGSPC, Keyword == "S&P 500 (Log)"), linewidth = 1, colour = "purple") +  
  labs(title = "G (Logarithmic Scale) and Google Trends Data (2014 - 2024)",
       x = "Date", y = "Normalised Value [%]", colour = "Keywords") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold")) +
  scale_colour_manual(values = c("S&P 500 (Log)" = "purple", 
                                 "S&P 500" = "#7CB9E8", 
                                 "Recession" = "#89CFF0", 
                                 "Inflation" = "#6699CC", 
                                 "Stock Crash" = "#318CE7", 
                                 "Stock Market News" = "#4B9CD3"))



#============== Dow Jones ===========#

# log-transform and normalize DJI data again
DJIDataLog <- log(DJIData) 
DJIDataScaled <- data.frame(
  date = index(DJIDataLog), 
  hits = min_max_scale(as.numeric(DJIDataLog)), 
  keyword = "Dow Jones (Log)"
)

# combine Google Trends data with DJI data
mergedDataDJI <- tidyData %>%
  rename(KeywordHits = hits, Keyword = keyword) %>%
  bind_rows(data.frame(date = DJIDataScaled$date, 
                       KeywordHits = DJIDataScaled$hits, 
                       Keyword = "Dow Jones (Log)")) %>%
  arrange(date)

ggplot(mergedDataDJI, aes(x = date, y = KeywordHits, colour = Keyword, group = Keyword)) +
  geom_line(linewidth = 1.05) +  
  geom_line(data = filter(mergedDataDJI, Keyword == "Dow Jones (Log)"), linewidth = 1, colour = "black") +  
  labs(title = "Dow Jones (Logarithmic Scale) and Google Trends Data (2014 - 2024)",
       x = "Date", y = "Normalized Value [%]", colour = "Keywords") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold")) +
  scale_colour_manual(values = c("Dow Jones (Log)" = "black", 
                                 "S&P 500" = "#7CB9E8", 
                                 "Recession" = "#89CFF0", 
                                 "Inflation" = "#6699CC", 
                                 "Stock Crash" = "#318CE7", 
                                 "Stock Market News" = "#4B9CD3"))



#=============== all plots put together =================#


# Define the individual plots
# Plot 1: VIX and Google Trends Data
plot_VIX <- ggplot(mergedData, aes(x = date, y = KeywordHits, colour = Keyword, group = Keyword)) +
  geom_line(linewidth = 1.05) +  
  geom_line(data = filter(mergedData, Keyword == "VIX"), linewidth = 1, colour = "#A68F26") +  
  labs(title = "VIX (Logarithmic Scale) and Google Trends Data (2014 - 2024)",
       x = "Date", y = "Normalised Value (%)", colour = "Keywords") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold")) +
  scale_colour_manual(values = c("VIX (Log)" = "#A68F26", 
                                 "S&P 500" = "#7CB9E8", 
                                 "Recession" = "#89CFF0", 
                                 "Inflation" = "#6699CC", 
                                 "Stock Crash" = "#318CE7", 
                                 "Stock Market News" = "#4B9CD3"))

# Plot 2: Nasdaq (Logarithmic Scale) and Google Trends Data
plot_Nasdaq <- ggplot(mergedDataNasdaq, aes(x = date, y = KeywordHits, colour = Keyword, group = Keyword)) +
  geom_line(linewidth = 1.05) +  
  geom_line(data = filter(mergedDataNasdaq, Keyword == "Nasdaq (Log)"), linewidth = 1, colour = "darkgreen") +  
  labs(title = "Nasdaq (Logarithmic Scale) and Google Trends Data (2014 - 2024)",
       x = "Date", y = "Normalised Value [%]", colour = "Keywords") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold")) +
  scale_colour_manual(values = c("Nasdaq (Log)" = "darkgreen", 
                                 "S&P 500" = "#7CB9E8", 
                                 "Recession" = "#89CFF0", 
                                 "Inflation" = "#6699CC", 
                                 "Stock Crash" = "#318CE7", 
                                 "Stock Market News" = "#4B9CD3"))

# Plot 3: S&P 500 (Logarithmic Scale) and Google Trends Data
plot_GSPC <- ggplot(mergedDataGSPC, aes(x = date, y = KeywordHits, colour = Keyword, group = Keyword)) +
  geom_line(linewidth = 1.05) +  
  geom_line(data = filter(mergedDataGSPC, Keyword == "S&P 500 (Log)"), linewidth = 1, colour = "#893168") +  
  labs(title = "S&P 500 (Logarithmic Scale) and Google Trends Data (2014 - 2024)",
       x = "Date", y = "Normalised Value [%]", colour = "Keywords") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold")) +
  scale_colour_manual(values = c("S&P 500 (Log)" = "#893168", 
                                 "S&P 500" = "#7CB9E8", 
                                 "Recession" = "#89CFF0", 
                                 "Inflation" = "#6699CC", 
                                 "Stock Crash" = "#318CE7", 
                                 "Stock Market News" = "#4B9CD3"))

# Plot 4: Dow Jones (Logarithmic Scale) and Google Trends Data
plot_DJI <- ggplot(mergedDataDJI, aes(x = date, y = KeywordHits, colour = Keyword, group = Keyword)) +
  geom_line(linewidth = 1.05) +  
  geom_line(data = filter(mergedDataDJI, Keyword == "Dow Jones (Log)"), linewidth = 1, colour = "#ED5A8D") +  
  labs(title = "Dow Jones (Logarithmic Scale) and Google Trends Data (2014 - 2024)",
       x = "Date", y = "Normalised Value [%]", colour = "Keywords") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold")) +
  scale_colour_manual(values = c("Dow Jones (Log)" = "#ED5A8D", 
                                 "S&P 500" = "#7CB9E8", 
                                 "Recession" = "#89CFF0", 
                                 "Inflation" = "#6699CC", 
                                 "Stock Crash" = "#318CE7", 
                                 "Stock Market News" = "#4B9CD3"))

# arrange the four plots into a 2x2 grid using patchwork
(plot_VIX | plot_Nasdaq) / (plot_GSPC | plot_DJI)


#=================================================================================#

#============ TSA: VIX ================#

library(tseries)
library(forecast)

TSADataVIX <- VIXData$VIX
TSADataVIX <- na.omit(TSADataVIX)

adf_result <- adf.test(TSADataVIX)
cat("ADF Test Statistic Value:", adf_result$statistic, "\n")
cat("ADF Test p-value:", adf_result$p.value, "\n")

if (adf_result$p.value > 0.05){
  
  cat("The time-series data possesses non-stationarity features, which isn't optimal. Applying first fixes...\n")
  
  TSADataVIX <- diff(TSADataVIX)
  
  adf_result <- adf.test(TSADataVIX)
  cat("After differencing, ADF Test Statistic:",
      adf_result$statistic, "\n")
  cat("After differencing, ADF Test p-value:",
      adf_result$p.value, "\n")
} else {
  cat("The time-series data possesses stationarity features.")
}

arima_model <- Arima(TSADataNsdq, order = c(3, 0, 0))

summary(arima_model)

forecasted_values <- forecast(arima_model, h=10)
plot(forecasted_values)
checkresiduals(arima_model)
















#============ TSA: Nasdaq =============#

library(tseries)
library(forecast)

TSADataNsdq <- NasdaqData$IXIC
TSADataNsdq <- na.omit(TSADataNsdq)

adf_result <- adf.test(TSADataNsdq)
cat("ADF Test Statistic Value:", adf_result$statistic, "\n")
cat("ADF Test p-value:", adf_result$p.value, "\n")

if (adf_result$p.value > 0.05){
  
  cat("The time-series data possesses non-stationarity features, which isn't optimal. Applying first fixes...\n")
  
  TSADataNsdq <- diff(TSADataNsdq)
  
  adf_result <- adf.test(TSADataNsdq)
  cat("After differencing, ADF Test Statistic:",
      adf_result$statistic, "\n")
  cat("After differencing, ADF Test p-value:",
      adf_result$p.value, "\n")
} else {
  cat("The time-series data possesses stationarity features.")
}

arima_model <- Arima(TSADataNsdq, order = c(3, 1, 0))

summary(arima_model)

forecasted_values <- forecast(arima_model, h=10)
plot(forecasted_values)
checkresiduals(arima_model)


#============ TSA: S&P500 =============#


library(tseries)
library(forecast)

TSADataSP <- GSPCData$GSPC
TSADataSP <- diff(TSADataSP)
TSADataSP <- na.omit(TSADataSP)

adf_resultSP <- adf.test(TSADataSP)
cat("ADF Test Statistic Value:", adf_resultSP$statistic, "\n")
cat("ADF Test p-value:", adf_resultSP$p.value, "\n")

if (adf_resultSP$p.value > 0.05){
  
  cat("The time-series data possesses non-stationarity features, which isn't optimal. Applying first fixes...\n")
  
  TSADataSP <- diff(TSADataSP)
  
  adf_resultSP <- adf.test(TSADataSP)
  cat("After differencing, ADF Test Statistic:",
      adf_resultSP$statistic, "\n")
  cat("After differencing, ADF Test p-value:",
      adf_resultSP$p.value, "\n")
} else {
  cat("The time-series data possesses stationarity features.")
}

arima_model <- Arima(TSADataSP, order = c(2, 0, 0)) #(2,0,0) is optimal for SP500

summary(arima_model)

forecasted_values <- forecast(arima_model, h=10)
plot(forecasted_values)
checkresiduals(arima_model)


#============ TSA: DJI =============#

library(tseries)
library(forecast)

TSADataDJI <- DJIData$DJI
TSADataDJI <- na.omit(TSADataDJI)

adf_result <- adf.test(TSADataDJI)
cat("ADF Test Statistic Value:", adf_result$statistic, "\n")
cat("ADF Test p-value:", adf_result$p.value, "\n")

if (adf_result$p.value > 0.05){
  
  cat("The time-series data possesses non-stationarity features, which isn't optimal. Applying first fixes...\n")
  
  TSADataDJI <- diff(TSADataDJI)
  
  adf_result <- adf.test(TSADataDJI)
  cat("After differencing, ADF Test Statistic:",
      adf_result$statistic, "\n")
  cat("After differencing, ADF Test p-value:",
      adf_result$p.value, "\n")
} else {
  cat("The time-series data possesses stationarity features.")
}

arima_model <- Arima(TSADataDJI, order = c(3, 1, 0))

summary(arima_model)

forecasted_values <- forecast(arima_model, h=10)
plot(forecasted_values)


checkresiduals(arima_model)











# Work on a correlation matrix for the google trends keywords.

# Also see if you can make 4 graphs, each with a plot like the one above, put up a plot but with the others instead of VIX




#================== Time Series Analysis and Model Training ======================#

# VIX T.S.A Model

VIXData <- getSymbols("^VIX", src = "yahoo", auto.assign = FALSE)  
VIXData <- data.frame(date = index(VIXData), VIX = coredata(VIXData[, 4]))  

dataMergeVIX <- merge(VIXData, tidyData, by = "date")
dataMergeVIX <- na.omit(dataMergeVIX)

adfTestVIX <- adf.test(dataMergeVIX$VIX)
cat("ADF Test p-value for VIX:", adfTestVIX$p.value, "\n")

if (adfTestVIX$p.value > 0.05) {
  cat("VIX data is non-stationary. Applying first difference...\n")
  
  differenced_VIX <- diff(dataMergeVIX$VIX)  
  dataMergeVIX <- dataMergeVIX[-1, ]        
  dataMergeVIX$VIX <- differenced_VIX   
}

par(mfrow = c(1, 2))
acf(dataMergeVIX$VIX, main = "ACF of VIX")
pacf(dataMergeVIX$VIX, main = "PACF of VIX")
print(adfTestVIX)

# ------------------------------------------------------------------------------

# Nasdaq (IXIC) T.S.A Model

Nasdaq <- getSymbols("^IXIC", src = "yahoo", auto.assign = FALSE)  
NasdaqData <- data.frame(date = index(Nasdaq), IXIC = coredata(Nasdaq[, 4])) 

dataMergeNasdaq <- merge(NasdaqData, tidyData, by = "date")
dataMergeNasdaq <- na.omit(dataMergeNasdaq)

adfTestNasdaq <- adf.test(dataMergeNasdaq$IXIC)
cat("ADF Test p-value for Nasdaq:", adfTestNasdaq$p.value, "\n")

if (adfTestNasdaq$p.value > 0.05) {
  cat("Nasdaq data is non-stationary. Applying first difference...\n")
  
  differenced_IXIC <- diff(dataMergeNasdaq$IXIC)  
  dataMergeNasdaq <- dataMergeNasdaq[-1, ]        
  dataMergeNasdaq$IXIC <- differenced_IXIC    
}

par(mfrow = c(1, 2))
acf(dataMergeNasdaq$IXIC, main = "ACF of Nasdaq")
pacf(dataMergeNasdaq$IXIC, main = "PACF of Nasdaq")
print(adfTestNasdaq)


# ------------------------------------------------------------------------------


# S&P 500 T.S.A Model


SP500 <- getSymbols("^GSPC", src = "yahoo", auto.assign = FALSE)  
SP500Data <- data.frame(date = index(SP500), GSPC = coredata(SP500[, 4])) 

dataMergeSP500 <- merge(SP500Data, tidyData, by = "date")
dataMergeSP500 <- na.omit(dataMergeSP500)

adfTestSP500 <- adf.test(dataMergeSP500$GSPC)
cat("ADF Test p-value for S&P500:", adfTestNasdaq$p.value, "\n")

if (adfTestSP500$p.value > 0.05) {
  cat("S&P500 data is non-stationary. Applying first difference...\n")
  
  differenced_GSPC <- diff(dataMergeSP500$GSPC)  
  dataMergeSP500 <- dataMergeSP500[-1, ]        
  dataMergeSP500$GSPC <- differenced_GSPC    
}

par(mfrow = c(1, 2))
acf(dataMergeSP500$GSPC, main = "ACF of S&P500")
pacf(dataMergeSP500$GSPC, main = "PACF of S&P500")
print(adfTestSP500)




# ------------------------------------------------------------------------------


# DJI T.S.A Model


DJI <- getSymbols("^DJI", src = "yahoo", auto.assign = FALSE)  
DJIData <- data.frame(date = index(DJI), DJI = coredata(DJI[, 4])) 

dataMergeDJI <- merge(DJIData, tidyData, by = "date")
dataMergeDJI <- na.omit(dataMergeDJI)

adfTestDJI <- adf.test(dataMergeDJI$DJI)
cat("ADF Test p-value for DJI", adfTestNasdaq$p.value, "\n")

if (adfTestDJI$p.value > 0.05) {
  cat("DJI data is non-stationary. Applying first difference...\n")
  
  differenced_DJI <- diff(dataMergeDJI$DJI)  
  dataMergeDJI <- dataMergeDJI[-1, ]        
  dataMergeDJI$DJI <- differenced_DJI  
}

par(mfrow = c(1, 2))
acf(dataMergeDJI$DJI, main = "ACF of DJI")
pacf(dataMergeDJI$DJI, main = "PACF of DJI")
print(adfTestDJI)







plot(GSPCData$GSPC.Close, DJIData$DJI.Close, 
     main = "S&P 500 vs Dow Jones Closing Prices",
     xlab = "S&P 500 (Closing Price)", 
     ylab = "Dow Jones (Closing Price)")





correlationMatrix <- cor(combinedData %>% select(GSPC.Close, DJI.Close, IXIC.Close, VIX.Close))

library(corrplot)
corrplot(correlationMatrix, method = "number")



