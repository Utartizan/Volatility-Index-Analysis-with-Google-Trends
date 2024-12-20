
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

install.packages("ggcorrplot")
# Ensure all the packages are installed.
#install.packages(c("quantmod", "gtrendsR", "dplyr", "ggplot2", "caret", "forecast", "tseries", "randomForest", "xgboost", "reshape2", "tidyr", "corrplot", "writexl", "rugarch", "zoo"))



#================== VIX, S&P 500, NASDAQ, Dow Jones Data Importing and Preparation ===============================#


#> fetching data from yahoo finance

getSymbols("^VIX", from = "2014-10-15", to = "2024-10-15", src = "yahoo")
getSymbols("^IXIC", from = "2014-10-15", to = "2024-10-15", src = "yahoo")
getSymbols("^GSPC", from = "2014-10-15", to = "2024-10-15", src ="yahoo")
getSymbols("^DJI", from = "2014-10-15", to = "2024-10-15", src ="yahoo")


#> extracting closing prices and removing NA values
VIXData <- na.omit(CL(VIX))
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
    Q3 = quantile(data, 0.75)
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

#========== ALTERNATIVE ==========#

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

NasdaqDataLog <- log(NasdaqData)

# Combine Google Trends with Nasdaq data
mergedDataNasdaq <- tidyData %>%
  rename(KeywordHits = hits, Keyword = keyword) %>%
  bind_rows(data.frame(date = index(NasdaqDataLog), KeywordHits = as.numeric(NasdaqDataLog), Keyword = "IXIC")) %>%
  arrange(date)

ggplot(mergedDataNasdaq, aes(x = date, y = KeywordHits, colour = Keyword, group = Keyword)) +
  geom_line(size = 1.05) +  
  geom_line(data = filter(mergedDataNasdaq, Keyword == "IXIC"), size = 1, colour = "red") +  
  labs(title = "Nasdaq (Logarithmic Scale) and Google Trends Data (2014 - 2024)",
       x = "Date", y = "Log(Nasdaq Index) / Normalized Interest (%)", colour = "Keywords") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold")) +
  scale_colour_manual(values = c("IXIC" = "red", 
                                 "S&P 500" = "#7CB9E8", 
                                 "Recession" = "#89CFF0", 
                                 "Inflation" = "#6699CC", 
                                 "Stock Crash" = "#318CE7", 
                                 "Stock Market News" = "#4B9CD3"))


# Fix this code, there is a major inconsistency in the lines of each keyword (e.g. Comparing IXIC.Close to the GoogleTrends Keywords)

# Look into enhancing the nature of the data above by, instead of using the normalised value as the Y axis, you use the logarithmic values, so that comparisons between the rate of changes in each keyword is more accurate and consistent.



#======== GSPC


# Normalise Google Trends hits
tidyData <- interest_over_time %>%
  select(date, keyword, hits) %>%
  mutate(date = as.Date(date), hits = hits / max(hits) * 100)

mergedDataTwo <- tidyData %>%
  rename(KeywordHitsOne = hits, Keyword = keyword) %>%
  bind_rows(data.frame(date = GSPCData$date, KeywordHitsOne = GSPCData$GSPC.Close, Keyword = "GSPC"))

ggplot(mergedDataOne, aes(x = date, y = KeywordHitsOne, colour = Keyword, group = Keyword)) +
  geom_line(linewidth = 1.05) +  
  geom_line(data = filter(mergedDataOne, Keyword == "IXIC"), linewidth = 1, colour = "green") +  
  labs(title = "Nasdaq and Google Trends Data (2014 - 2024)",
       x = "Date", y = "Normalised Value (%)", colour = "Keywords") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold")) +
  scale_y_continuous(labels = scales::percent_format(scale = 0.8)) +
  scale_colour_manual(values = c("IXIC" = "green", 
                                 "S&P 500" = "#7CB9E8", 
                                 "Recession" = "#89CFF0", 
                                 "Inflation" = "#6699CC", 
                                 "Stock Crash" = "#318CE7", 
                                 "Stock Market News" = "#4B9CD3"))

#=================================================================================#



# Work on a correlation matrix for the google trends keywords.

# Also see if you can make 4 graphs, each with a plot like the one above, put up a plot but with the others instead of VIX




#================== Time Series Analysis and Model Training ======================#

# VIX Model T.S.A

VIXData <- data.frame(date = index(VIXData), VIX = coredata(VIXData))
dataMerge <- merge(VIXData, tidyData, by = "date")
dataMerge <- na.omit(dataMerge)

print(head(VIXData))

adfTest <- adf.test(dataMerge$VIX)
cat("ADF Test p-value:", adfTest$p.value, "\n")

acfPlot <- acf(dataMerge$VIX, plot = TRUE)
pacfPlot <- pacf(dataMerge$VIX, plot = TRUE)

par(mfrow = c(1, 2))
plot(acfPlot, main = "ACF of VIX")
plot(pacfPlot, main = "PACF of VIX")


# Nasdaq Model T.S.A
# ONLY RUN ONCE, if you have to run again, restart

NasdaqData <- data.frame(date = index(NasdaqData), Nasdaq = coredata(NasdaqData))
dataMergeOne <- merge(NasdaqData, mergedDataOne, by = "date")
dataMergeOne <- na.omit(dataMergeOne)

print(head(NasdaqData))

adfTestOne <- adf.test(dataMergeOne$IXIC)
cat("ADF Test p-value", adfTestOne$p.value, "\n")

acfPlotOne <- acf(dataMergeOne$IXIC, plot = TRUE)
pacfPlotOne <- pacf(dataMergeOne$IXIC, plot = TRUE)

par(mfrow = c(1, 2))
plot(acfPlotOne, main = "ACF of IXIQ (Nasdaq)")
plot(pacfPlotOne, main = "PACF of IXIQ (Nasdaq)")


# GSPC Model T.S.A
# ONLY RUN ONCE, if you have to run again, restart
GSPCData <- data.frame(date = index(GSPCData), GSPC = coredata(GSPCData))
dataMergeTwo <- merge(GSPCData, mergedDataTwo, by = "date")
dataMergeTwo <- na.omit(dataMergeTwo)

print(head(GSPCData))



#=================================================================================#









#============================== Forecast: RF Model ===============================#

VIXData$MA <- rollmean(VIX_data, k = 10, fill = NA)
VIXData <- na.omit(VIX_data)   

trainPct <- 0.8
trainEnd <- round(nrow(VIXData) * trainPct)

trainData <- VIXData[1:trainEnd, ]
testData <- VIXData[(trainEnd + 1):nrow(VIXData), ]

trainX <- as.data.frame(trainData[, -1])   
trainY <- as.numeric(trainData[, 1]) 

testX <- as.data.frame(testData[, -1])
testY <- as.numeric(testData[, 1])

rf_model <- randomForest(trainX, trainY, ntree = 200)  
predictions <- predict(rf_model, testX)

future_days <- 30
future_dates <- seq(from = max(index(VIXData)), by = "days", length.out = future_days)

last_row <- testX[nrow(testX), , drop = FALSE]
future_predictions <- numeric(future_days)

for (i in 1:future_days) {
  future_predictions[i] <- predict(rf_model, last_row)
  last_row[1, 1] <- future_predictions[i]  # Replace last value with the new prediction
}

future_data <- data.frame(Date = future_dates, Price = future_predictions)

# test 

ggplot() +
  geom_line(data = data.frame(Date = index(VIXData), Price = as.numeric(VIXData[,1])),
            aes(x = Date, y = Price, color = "Historical VIX"), size = 1) +
  geom_line(data = data.frame(Date = index(testData), Price = predictions),
            aes(x = Date, y = Price, color = "Random Forest Best of Fit"), size = 1) +
  geom_line(data = future_data, aes(x = Date, y = Price, color = "Future Predictions"), size = 1) +
  labs(title = "VIX Data and Forecast",
       x = "Date", y = "VIX Value", color = "Legend") +
  scale_color_manual(values = c("Historical VIX" = "#820263", "Random Forest Best of Fit" = "#FFA632", "Future Predictions" = "#44884E")) +
  theme_linedraw()


#=================================================================================#











#============================= Garch Implementation ==============================#

library(rugarch)

garchSpec <- ugarchspec(variance.model = list(model = "sGARCH",
                        garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(1, 1),
                        include.mean = TRUE),
                        distribution.model = "std")

garchFit <- ugarchfit(spec = garchSpec, data = dataMerge$VIX)

garchForecast <- ugarchforecast(garch_fit, n.ahead = 30)

plot(garchForecast, which = 1)



garchSpec1 <- ugarchspec(variance.model = list(model = "sGARCH",
                                              garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(1, 1),
                                          include.mean = TRUE),
                        distribution.model = "sstd")



garchFit1 <- ugarchfit(spec = garchSpec, data = dataMerge$VIX)

garchForecast1 <- ugarchforecast(garch_fit, n.ahead = 30)

plot(garchForecast1, which = 1)


#=================================================================================#






#=========================== Multivariate Regression =============================#

dataMerge <- data.frame(date = index(VIXData), VIX = as.numeric(VIXData))

#== Debug this later ==#
dataMerge <- dataMerge %>%
  arrange(date) %>%
  mutate(
    VIX_Lag1 = lag(VIX, 1),
    VIX_Lag2 = lag(VIX, 2)
  )

#======================#

dataMerge <- na.omit(dataMerge)

print(head(dataMerge))

trainPct <- 0.8
trainEnd <- round(nrow(dataMerge) * trainPct)
trainData <- dataMerge[1:trainEnd, ]
testData <- dataMerge[(trainEnd + 1):nrow(dataMerge), ]
  
reg_model <- lm(VIX ~ VIX_Lag1 + VIX_Lag2, data = dataMerge)
summary(reg_model)
reg_predictions <- predict(reg_model, newdata = testData)

plot(dataMerge$date, dataMerge$VIX, type = "l", col = "blue", main = "Multivariate Regression Fit")
lines(dataMerge$date, reg_predictions, col = "red")


#=================================================================================#
