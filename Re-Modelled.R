
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
  geom_line(linewidth = 1.05) +  
  geom_line(data = filter(mergedDataNasdaq, Keyword == "IXIC"), linewidth = 1, colour = "red") +  
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



#======== GSPC (needs fixing)


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


#=================================================================================#



# =========================== Data Preparation for Random Forest =========================== #

combinedData <- dataMergeVIX %>%
  left_join(dataMergeNasdaq, by = "date") %>%
  left_join(dataMergeSP500, by = "date") %>%
  left_join(dataMergeDJI, by = "date") %>%
  na.omit()  # Remove rows with NA values

# Ensure the data is stationary (if not already)
combinedData <- combinedData %>%
  mutate(
    VIX_diff = diff(c(0, VIX)),  # First difference of VIX
    IXIC_diff = diff(c(0, IXIC)),  # First difference of Nasdaq
    GSPC_diff = diff(c(0, GSPC)),  # First difference of S&P 500
    DJI_diff = diff(c(0, DJI))     # First difference of Dow Jones
  ) %>%
  select(date, VIX_diff, IXIC_diff, GSPC_diff, DJI_diff, everything()) %>%
  filter(date != min(date))  # Remove first row created by differencing

# Use the normalized Google Trends data and merge it with the financial data
finalData <- combinedData %>%
  left_join(tidyData %>%
              spread(keyword, hits), by = "date") %>%
  na.omit()  # Remove NA values to ensure compatibility

# ============================ Random Forest Training ============================ #

# Define target variable and predictors
# Here, we use `VIX_diff` as the target variable (to predict VIX movement)
target <- finalData$VIX_diff
predictors <- finalData %>%
  select(-date, -VIX_diff) %>%  # Remove target and non-predictor columns
  as.data.frame()

# Split data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(target, p = 0.6, list = FALSE)
trainPredictors <- predictors[trainIndex, ]
trainTarget <- target[trainIndex]
testPredictors <- predictors[-trainIndex, ]
testTarget <- target[-trainIndex]

# Train a Random Forest model
set.seed(123)
rfModel <- randomForest(
  x = trainPredictors,
  y = trainTarget,
  ntree = 250,         # Number of trees
  mtry = floor(sqrt(ncol(trainPredictors))),  # Number of variables randomly selected
  importance = TRUE    # Calculate variable importance
)

# Print Random Forest model summary
print(rfModel)

# ============================ Random Forest Evaluation ============================ #

# Predict on test data
predictions <- predict(rfModel, newdata = testPredictors)

# Evaluate the model's performance
rfMetrics <- postResample(predictions, testTarget)
cat("Random Forest RMSE:", rfMetrics["RMSE"], "\n")
cat("Random Forest R-squared:", rfMetrics["Rsquared"], "\n")

# Feature Importance
featureImportance <- importance(rfModel)
featureImportanceDF <- data.frame(Variable = rownames(featureImportance),
                                  Importance = featureImportance[, "IncNodePurity"])
featureImportanceDF <- featureImportanceDF[order(-featureImportanceDF$Importance), ]

# Plot Feature Importance
ggplot(featureImportanceDF, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Feature Importance in Random Forest",
    x = "Features",
    y = "Importance"
  ) +
  theme_minimal()

# ============================ Predicting Future Values ============================ #

newPredictors <- testPredictors  # Use test data for demonstration
futurePredictions <- predict(rfModel, newdata = newPredictors)

# Add predictions to a dataframe
results <- data.frame(
  Actual = testTarget,
  Predicted = predictions
)
results$Residuals <- results$Actual - results$Predicted

# Visualize actual vs predicted values
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(colour = "blue") +
  geom_abline(intercept = 0, slope = 1, colour = "red", linetype = "dashed") +
  labs(
    title = "Actual vs Predicted VIX Movements",
    x = "Actual VIX Movement",
    y = "Predicted VIX Movement"
  ) +
  theme_minimal()




#============ Testing Garch Implementatino Again ============#

library(rugarch)

garchSpec <- ugarchspec(variance.model = list(model = "sGARCH",
                        garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(1,1),
                        include.mean = TRUE),
                        distribution.model = "sstd")
                        
garchFitVIX <- ugarchfit(spec = garchSpec, data = dataMergeVIX$VIX.Close)
garchFitIXIC <- ugarchfit(spec = garchSpec, data = dataMergeNasdaq$IXIC.Close)
garchFitSP500 <- ugarchfit(spec = garchSpec, data = dataMergeSP500$GSPC.Close)
garchFitDJI <- ugarchfit(spec = garchSpec, data = dataMergeDJI$DJI.Close)


garchForecastVIX <- ugarchforecast(garchFitVIX, n.ahead = 30)
garchForecastIXIC <- ugarchforecast(garchFitIXIC, n.ahead = 30)
#garchForecastSP500 <- ugarchforecast(garchFitSP500, n.ahead = 30)
garchForecastDJI <- ugarchforecast(garchFitDJI, n.ahead = 30)


par(mfrow=c(2,2))
plot(garchForecastVIX, which = 1)
plot(garchForecastIXIC, which = 1)
#plot(garchForecastSP500, which = 1)
plot(garchForecastDJI, which = 1)



#seems to be an issue with equating the garchforecast values under the S&P500 variable.... try fix later, good work on making the other 3 work.

#also figure out a way to bring back the yellow-highlighted range for the prediction.



