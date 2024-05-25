library(readxl)
library(dplyr)
library(lubridate)


Actuals_NA1_1_ <- read_excel("C:/Users/user/Downloads/Actuals_NA1 (1).xlsx")

#Best Performing Product for each year

Actuals_NA1_1_$PERIOD_DATE <- as.Date(Actuals_NA1_1_$PERIOD_DATE)
best_performing <- Actuals_NA1_1_ %>%
  group_by(year = year(PERIOD_DATE), MPG_ID) %>%
  summarise(total_amount = sum(AMOUNT)) %>%
  top_n(1, total_amount) %>%
  arrange(year) 
print(best_performing)

# 5 Best Performing  product for each year

Actuals_NA1_1_$PERIOD_DATE <- as.Date(Actuals_NA1_1_$PERIOD_DATE)
best_performing_5 <- Actuals_NA1_1_ %>%
  group_by(year = year(PERIOD_DATE), MPG_ID) %>%
  summarise(total_amount = sum(AMOUNT)) %>%
  group_by(year) %>%
  top_n(5, total_amount) %>%
  arrange(year, desc(total_amount)) 
print(best_performing_5)

#Revenue
# Load required libraries
library(dplyr)
library(lubridate)

# Assuming 'data' is your dataset with columns: 'APP_NAME', 'MPG_ID', 'ACCOUNT_ID', 'MARKET', 'CHANNEL_ID', 'CURRENCY_ID', 'PERIOD_DATE', and 'AMOUNT'

# Convert 'PERIOD_DATE' to year
data$YEAR <- lubridate::year(data$PERIOD_DATE)

# Calculate revenue for each year
revenue_each_year <- data %>%
  group_by(YEAR) %>%
  summarise(REVENUE = sum(AMOUNT))

# View the resulting data frame
print(revenue_each_year)

#OR
# Load necessary library
library(ggplot2)

# Convert 'PERIOD_DATE' to year
data$YEAR <- lubridate::year(data$PERIOD_DATE)

# Calculate revenue for each year
revenue_each_year <- data %>%
  group_by(YEAR) %>%
  summarise(REVENUE = sum(AMOUNT))
print(revenue_each_year)

#PIE CHART for Best Performing product

library(ggplot2)

# Define custom colors
custom_colors <- c("#FF9999", "#66CCCC", "#FFCC99", "#99CC66", "#FFCC66")

# Convert MPG_ID to factor
best_performing$MPG_ID <- factor(best_performing$MPG_ID)

# Plot a pie chart with custom colors
pie_chart <- ggplot(best_performing, aes(x = "", y = total_amount, fill = MPG_ID)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Best Performing Products") +
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  theme_void()

# Display the pie chart
print(pie_chart)



# Load necessary libraries
library(ggplot2)

# Sample data
best_performing <- data.frame(
  MPG_ID = c(1, 2, 3, 4),
  Performance = c(20, 30, 15, 35)
)

# Define custom colors
custom_colors <- c("#FF9999", "#66CCCC", "#FFCC99", "#99CC66", "#FFCC66")

# Convert MPG_ID to factor
best_performing$MPG_ID <- factor(best_performing$MPG_ID)

# Create pie chart
pie_chart <- ggplot(best_performing, aes(x = "", y = Performance, fill = MPG_ID)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = custom_colors) +
  theme_void() +
  theme(legend.position = "right") + 
  labs(fill = "MPG ID")

# Display pie chart
print(pie_chart)



library(ggplot2)

# Plot a pie chart
ggplot(best_performing_5, aes(x = "", y = total_amount, fill = factor(MPG_ID))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Total Amount by MPG_ID (Pie Chart)",
       fill = "MPG_ID") +
  theme_void()

library(ggplot2)

#Bar Plot



library(ggplot2)

# Plot
ggplot(best_performing_5, aes(x = factor(year), y = total_amount, fill = factor(MPG_ID))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Amount by Year and MPG_ID",
       x = "Year",
       y = "Total Amount") +
  theme_minimal()

#Pie Chart on Revenue
# Create a pie chart for historical revenue data with exact values as labels
ggplot(revenue_each_year, aes(x = "", y = REVENUE, fill = as.factor(YEAR), label = sprintf("%.2f", REVENUE))) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = 0) +
  labs(title = "Historical Revenue Data",
       fill = "Year") +
  theme_minimal()

#Time Series Forecating
#Arima Model


library(readxl)
library(dplyr)
library(lubridate)
library(forecast)

# Read the data from the Excel file
data <- read_excel("C:/Users/user/Downloads/Actuals_NA1 (1).xlsx")

# Check if data is empty
if (nrow(data) == 0) {
  stop("Error: The dataset is empty. Please check if the file path is correct or if the data is in the expected format.")
}

# Convert 'PERIOD_DATE' to Date
data$PERIOD_DATE <- as.Date(data$PERIOD_DATE)

# Filter data for MPG_ID = 592010
filtered_data <- data %>%
  filter(MPG_ID == 592010)

# Convert 'PERIOD_DATE' to year
filtered_data$YEAR <- year(filtered_data$PERIOD_DATE)

# Calculate revenue for each year
revenue_each_year <- filtered_data %>%
  group_by(YEAR) %>%
  summarise(REVENUE = sum(AMOUNT))

# Convert 'YEAR' to a time series object
ts_data <- ts(revenue_each_year$REVENUE, start = min(revenue_each_year$YEAR), frequency = 1)

# Fit ARIMA model
arima_model <- auto.arima(ts_data)

# Make predictions for the next 2 years
forecast_values <- forecast(arima_model, h = 2)

# Print the forecasted values
print(forecast_values)

# Plot the historical revenue data
plot(ts_data, main = "Historical and Forecasted Revenue Data (MPG_ID = 592010)", 
     xlab = "Year", ylab = "Revenue", xlim = c(min(revenue_each_year$YEAR), max(revenue_each_year$YEAR) + 2), 
     ylim = c(min(ts_data), max(ts_data) * 1.2))

# Add the forecasted values to the plot
lines(forecast_values$mean, col = "blue", lty = 2)

# Add legend
legend("topright", legend = c("Historical Data", "Forecast"), col = c("black", "blue"), lty = c(1, 2))

