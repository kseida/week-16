# Install necessary packages (only run if not already installed)
packages <- c("tseries", "forecast", "TTR", "ggplot2", "dplyr", "tidyr", "lubridate")
installed <- packages %in% installed.packages()
i
if (any(!installed)) {
  install.packages(packages[!installed])
}
install.packages("readr")

# Load the libraries
library(tseries)
library(forecast)
library(TTR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

# load packages
soybean = read_csv("soybean-prices-historical-chart-data.csv")
head(soybean, 3)

cpi = read_csv("historical-cpi-u-202503.csv")
head(cpi, 3)

# Transform cpi data into longer table 
cpi <- cpi %>% pivot_longer(cols = -Year, names_to = "Month", values_to = "cpi")
# cpi_raw_long <- cpi %>% pivot_longer(cols = -Year)

# Convert month names 
cpi <- cpi %>% mutate(Month = match(gsub("\\.", "", Month), month.abb))  # Convert month names to numbers

# Create date column
cpi <- cpi %>% mutate(date = as.Date(paste(Year, Month, 1, sep = "-")))

# Format date as m/1/yyyy
cpi <- cpi %>% mutate(date = format(date, "%m/1/%Y"))

# Drop the Year and Month columns and ensure chronological order
cpi <- cpi %>% select(date, cpi) %>% arrange(as.Date(date, format = "%m/%d/%Y"))  

head(cpi)

# Transform the Soybean date column in Date format using the as.Date() function
soybean$date <- as.Date(soybean$date, format = "%m/%d/%Y")

# calculate the monthly average from your daily soybean price data 
soybean <- soybean %>%
  mutate(date = floor_date(date, "month")) %>%        # Set each date to the first of the month
  group_by(date) %>%
  summarize(price = mean(value, na.rm = TRUE)) %>%    # Calculate monthly average
  ungroup()

# Filter for the from data from `January 1969` to `March 2025`
# Filter for the from data from `1969-01-01` to `2025-03-31`
soybean <- soybean %>% filter(date >= as.Date("01/01/1969", format = "%m/%d/%Y") & date <= as.Date("03/31/2025", format = "%m/%d/%Y"))

head(soybean)

# Transform the CPI date column in Date format using the as.Date() function
cpi$date <- as.Date(cpi$date, format = "%m/%d/%Y")

# Filter for the from data from `1969-01-01` to `2025-03-31`
cpi <- cpi %>% filter(date >= as.Date("01/01/1969", format = "%m/%d/%Y") & date <= as.Date("03/31/2025", format = "%m/%d/%Y"))

head(cpi)

# Merge datasets
soybeans <- merge(cpi, soybean, by = "date")
head(soybeans)

# add real price column
soybeans$real_price = (soybeans$price)/(soybeans$cpi/100)

ggplot(soybeans, aes(x=date)) + 
  geom_line(aes(y = real_price), color = "goldenrod") + 
  geom_line(aes(y = price), color="steelblue", linetype="twodash") +
  labs(x = "Date (m/d/y)", y = "Price ($)") +
  theme_minimal()

# Your task: Set nominal prices as a time series. Replace the placeholders (including the brackets) with the appropriate arguments.
price.ts <- ts(soybeans$price, start=c(1969, 01), end=c(2025, 03), frequency= 10)

# Decompose nominal price time-series
price_components <- decompose(price.ts, type="additive")
plot(price_components)
price_components$figure

# Your task: Plot the seasonally adjusted time-series using `plot.ts()`.
# The `plot.ts()` function is specifically for time-series objects and works similarly to the `plot()` function.
plot.ts(price.ts, 
        xlab = "Year", 
        ylab = "Nominal Price ($)", 
        main = "Adjusted Seasonal Soybean Prices Over Time",
        col = "lightpink3")

# Your task: Take a 3-, 6-, and 12-month simple moving average of price using the `SMA()` function below (arguments in brackets).
price_sma3 <- SMA(price.ts, n=3) # 3-month simple moving average
price_sma6 <- SMA(price.ts, n=12)    # 12-month simple moving average
price_sma12 <- SMA(price.ts, n=48)    # 48-month simple moving average

# Graph each moving average series in a separate graph using `plot.ts()`. You can use the `par(mfrow=c([# of rows], [# of columns]))` command to tell R to group the three graphs together.
par(mfrow=c(3, 1))  # Arrange plots in 3 rows
# Example: Plot the 3-month moving average
plot.ts(price_sma3, main="Soybeans Price - 3 Month SMA", xlab="", ylab="SMA Price ($)")
# Your task: Plot the 12-month moving average
plot.ts(price_sma6, main="Soybeans Price - 12 Month SMA", xlab="", ylab="SMA Price ($)")
# Your task: Plot the 48-month moving average
plot.ts(price_sma12, main="Soybeans Price - 48 Month SMA", xlab="", ylab="SMA Price ($)")
par(mfrow=c(1, 1))  # Reset to default layout

# Take the first difference of the monthly soybeans price time-series using the `diff()` function.
price_diff.ts <- diff(price.ts, differences=1)

# Your task: Graph the differenced time-series using `plot.ts()`
plot.ts(price_diff.ts, 
        xlab = "Year", 
        ylab = "Nominal Price Change ($)", 
        main = "Month-Over-Month Change in Soybean Prices Over Time",
        col = "skyblue2")

# Your task: Log-transform the soybeans price time-series using the `log()` function.
# This helps to stabilize variance and make the data more suitable for analysis.
log_price.ts <- log(price.ts)

# Your task: Take the first difference of the log-transformed soybeans price time-series using the `diff()` function.
# This essentially calculates the month-over-month percentage change. (*Why is this the case?*)
log_price_diff.ts <- diff(log_price.ts, differences=2)

# Your task: Plot the log differenced time-series using the `plot.ts()` function. Change the title to "Month-over-Month % Change in Soybeans Prices Over Time", x axis lable to "", and y axis lable to "% Price Change"
plot.ts(log_price_diff.ts, 
        xlab = "", 
        ylab = "% Price Change", 
        main = "Month-over-Month % Change in Soybeans Prices Over Time",
        col = "palegreen4")

# Run auto-regressive model with 3 lags (AR(3))
ar <- ar(log_price_diff.ts, order.max=5)
ar
checkresiduals(ar, lag.max=60)

# Forecast prices 6 months into the future based on the AR(3) model
forecast <- forecast(ar, h=36)
forecast
autoplot(forecast, include=36, xlab="", ylab="Soybeans price - monthly % returns")
