#==================================================================================#
set.seed(123) # ?????

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

#install.packages(c("quantmod", "gtrendsR", "dplyr", "ggplot2", "caret", "forecast", "tseries", "randomForest", "xgboost", "reshape2", "tidyr", "corrplot"))

#================== Data Importing and Preparation ===============================#

# Fetching VIX data
getSymbols("^VIX", from = "2014-10-15", to = "2024-10-15", src = "yahoo")
  StockData <- Cl(VIX)
  StockData <- na.omit(StockData)
  plot(StockData, main = "VIX - Volatility for VIX Stock Market", ylab = "VIX")

#================== Time Series Analysis and Model Training ======================#

# Reshaping data for comparative analysis
VIX_data <- data.frame(date = index(StockData), VIX = coredata(StockData))
  dataMerge <- merge(VIX_data, tidy_data, by = "date")
  dataMerge <- na.omit(dataMerge)

# Time Series Decomposition and Stationarity Test
adfTest <- adf.test(dataMerge$VIX)
  cat("ADF Test p-value:", adfTest$p.value, "\n")
  
  acf_plot <- acf(dataMerge$VIX, plot = TRUE)
  pacf_plot <- pacf(dataMerge$VIX, plot = TRUE)

# Plot ACF and PACF graphs
par(mfrow = c(1, 2))
  plot(acf_plot, main = "ACF of VIX")
  plot(pacf_plot, main = "PACF of VIX")


#======================== Machine Learning Models ================================#

#============ARIMA Model============#
# Using the ARIMA Model
arimaModel <- auto.arima(train_data$VIX) # <- Minimising AIC (selecting the best ARIMA model for the time series)
  arimaForecast <- predict(arimaModel, n.ahead = nrow(test_data)) #generate a forecast from arimaModel, specifies number of 'steps' to store in 'arimaForecast' (confidence intervals)
  arimaPredict <- arimaForecast$pred #extracts predicted values, figures, intervals, and assigns i to 'arimaPredict'
  
# Error metrics for Arima model
arimaRMSE <- RMSE(arimaPredict, test_data$VIX) #root mean squared error, the lower the more accurate
  arimaMAE <- MAE(arimaPredict, test_data$VIX) #mean absolute error, calculating average of the absolute errors between predictions and actual values
  arimaMAPE <- mean(abs((test_data$VIX - arimaPredict) / test_data$VIX)) * 100 #mean absolute percentage error, the lower the better

#============Random Forest============#
# Using the Random Forest Model
colnames(train_data)[colnames(train_data) == "VIX.Close"] <- "VIX"
  rfModel <- randomForest(VIX ~ ., data = train_data[, colnames(train_data) != "date"], ntree = 500)
  rfPredictions <- predict(rfModel, test_data[,-1])

# Error metrics for Random Forest model
rfRMSE <- RMSE(rfPredictions, test_data$VIX)
  rfMAE <- MAE(rfPredictions, test_data$VIX)
  rfMAPE <- mean(abs((test_data$VIX - rfPredictions) / test_data$VIX)) * 100

#============XGBoost Model============#
# Using the XGBoost Model - Removing qualitative columns
trainMatrix <- as.matrix(sapply(train_data[, -c(1, 2)], as.numeric))
  testMatrix <- as.matrix(sapply(test_data[, -c(1, 2)], as.numeric))
  
# Sorting out any potential missing values
trainMatrix[is.na(trainMatrix)] <- 0
  testMatrix[is.na(testMatrix)] <- 0
  
xgbTrain <- xgb.DMatrix(data = trainMatrix, label = train_data$VIX)
  xgbTest <- xgb.DMatrix(data = testMatrix)
  xgbModel <- xgboost(data = xgbTrain, nrounds = 100, objective = "reg:squarederror", verbose = 0)
  xgbPredict <- predict(xgbModel, xgbTest)
  
# Error metrics for XGBoost model
xgbRMSE <- RMSE(xgbPredict, test_data$VIX)
  xgbMAE <- MAE(xgbPredict, test_data$VIX)
  xgbMAPE <- mean(abs((test_data$VIX - xgbPredict) / test_data$VIX)) * 100


#================== Visualisation of Results =====================================#

# Plotting the RMSE, MAE, and MAPE results
errorMetrics <- data.frame(
  Model = c("ARIMA", "Random Forest", "XGBoost"),
  RMSE = c(arimaRMSE, rfRMSE, xgbRMSE),
  MAE = c(arimaMAE, rfMAE, xgbMAE),
  MAPE = c(arimaMAPE, rfMAPE, xgbMAPE)
)

# Plotting the results
ggplot(melt(errorMetrics, id.vars = "Model"), aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Error Metrics Across Differing ML Algorithms", x = "Model", y = "Error") +
  theme_light()

#========== Insights/Comments ===========#

# ARIMA: Relatively low RMSE and MAE, however it possesses a very high MAPE. 
# Meaning it struggles with relative accuracy regarding VIX values
#
# Random Forest: Lower MAPE, MAE, and RMSE compared to Arima and XGBoost, suggesting a higher level of 
# accuracy. This indicates that it produces the most accurate forecasts in terms of absolute error
#
# XGBoost: Relatively low RMSE and MAE, lower to that of ARIMA's but not Random Forest's, meaning they 
# have limitations on accurately capturing time-series patterns compared to that of RandomForest's
# 
# It may be important to clarify that machine-learning models can struggle to handle data points with
# incredible amounts of accuracy without fine-tuning (how do i even do this?)

#=================================================================================#

plot(test_data$date, test_data$VIX, type = "l", col = "black", lwd = 2,
     ylim = range(c(10, 45)), # Adjust the y-axis range here
     xlab = "Date", ylab = "VIX", main = "VIX Actual vs Predictions")

lines(test_data$date, arimaPredict, col = "blue", lwd = 2)
lines(test_data$date, rfPredictions, col = "red", lwd = 2)
lines(test_data$date, xgbPredict, col = "green", lwd = 2)
legend("topright", legend = c("Actual", "ARIMA", "Random Forest", "XGBoost"),
       col = c("black", "blue", "red", "green"), lty = 1, lwd = 2)

#=================Splitting into Training and Testing Datasets ===================#

  # Train-test split
  train_index <- 1:floor(0.8 * nrow(dataMerge)) #setting 80% of value of 'dataMerge' to be served as training data, stored as 'train_index'
  train_data <- dataMerge[train_index, ] #store the result in 'train_data'
  test_data <- dataMerge[-train_index, ] #store the non-trained data (a.k.a the 20%) and assign it as 'test_data'

#verification to ensure they're all the same lengths for efficient analysis. all have to be equal to work
  # essentially this is debugging material
  # not a part of any of the findings
  length(test_data$date) 
  length(test_data$VIX)
  length(arimaPredict)
  length(rfPredictions)
  length(xgbPredict)

# running the ADF test
  adfTest <- adf.test(train_data$VIX)
  print(adfTest)

# Running the Augmented Dickey-Fuller Test, which is
# a test utilised to see whether or not a time-series
# possesses a nature of stationarity or non-stationarity
#
# Running the test brings forth an ADF value of -3.4756,
# a Lag Order of 6, and a p-value of 0.04535, which 
# means it fits into the requirements of fulfilling the
# alternative hypothesis, which is below the set 
# statistical significance value of 0.05.
#
# This alone can suggest that there is sufficient 
# evidence within the data findings to suggest that
# the findings of VIX data can be enhanced with Google
# Trends data.

#==================== DATA ENHANCED WITH GOOGLE TRENDS DATA =====================#

# Fetching Google Trends data for VIX-related keywords
terms <- c("S&P 500", "Recession", "Inflation", "Stock Crash", "Stock Market News") #define the specific terms
  gTrendsData <- gtrends(terms, gprop = "web", time = "2014-10-15 2024-10-15") #set the specifics for the trends, type (web), date boundaries, etc
  interest_over_time <- gTrendsData$interest_over_time #find out the interest_over_time data for the terms which is then assigned to interest_over_time
  interest_over_time$hits <- as.numeric(gsub("<1", "0", interest_over_time$hits)) #Include all the interest over time so long as it's above the set mathematical limits

# Normalising and plotting the Google Trends data
tidy_data <- interest_over_time %>%
  select(date, keyword, hits) %>%
  mutate(hits = hits / max(hits) * 100)

# Utilising GGPlot to plot the Google Trends data
ggplot(tidy_data, aes(x = date, y = hits, colour = keyword)) +
  geom_line(linewidth = 1) +
  labs(title = "Google Trends Data involving VIX Related Keywords (2014 - 2024)", 
       x = "Date", 
       y = "Normalised Interest (%)", 
       colour = "Terms") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
  theme(legend.position = "bottom")

# 1. Line Plot of VIX and Google Trends Terms Over Time

coloursTerms <- c("VIX.Close" = "red", 
                  "S&P 500" = "#ababab", 
                  "Recession" = "#8a8a8a", 
                  "Inflation" = "#6a6a6a", 
                  "Stock Crash" = "#3f3f3f", 
                  "Stock Market News" = "#171717")

ggplot(tidy_data_long, aes(x = date, y = value, color = variable)) +
  geom_line(data = subset(tidy_data_long, variable != "VIX.Close"), 
            linewidth = 1) +  # Thinner lines for other terms
  
  # Add VIX.Close on top with a thicker line
  geom_line(data = subset(tidy_data_long, variable == "VIX.Close"), 
            linewidth = 1.25) +  # Thicker line for VIX.Close

  labs(title = "VIX and Google Trends Terms Over Time",
       x = "Date", 
       y = "Value",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = coloursTerms) +
  theme(legend.position = "right")


#============================= Correlation Heatmap ===============================#

# 2. Calculate correlations and plot as heatmap
cor_data <- combined_data[, -1]  # Exclude the date column for correlation calculation
  cor_matrix <- cor(cor_data, use = "complete.obs")
  corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black",
           title = "",
           addCoef.col = "purple", number.cex = 0.9)
  mtext("Correlation Between VIX and Google Trends Terms", side = 3, line = 2.1, cex = 1.5)
  
#================= Scatter Plot of VIX vs Google Trends Terms ====================#

# 3. Scatter plots between VIX and each Google Trends term
plot_list <- lapply(names(combined_data)[-1], function(term) {  # Iterate through each Google Trends term
  ggplot(combined_data, aes_string(x = term, y = "VIX")) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = paste("VIX vs", term),
         x = term,
         y = "VIX") +
    theme_minimal()
})

# Display each scatter plot
for (plot in plot_list) {
  print(plot)
}



