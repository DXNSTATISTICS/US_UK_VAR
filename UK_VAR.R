
# Load necessary libraries
library(fredr)
library(vars)
library(tseries)
library(tidyverse)

KEY = "insert FREDR key here"
fredr_set_key(KEY)

DIRECTORY = "INSERT DIRECTORY PATH FOR SAVING GRAPHS"
setwd(dir = DIRECTORY)

#UK Data

# Fetch CPI data with a defined start and end date
uk_cpi <- fredr(
  series_id = "GBRCPIALLMINMEI",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2023-11-01")
)

# Download U.K. M1 Money Supply - Monthly
uk_m1 <- fredr(
  series_id = "MANMM101GBM189S",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2023-11-01")
)

# Combine CPI and M1 into one dataset
gbp <- inner_join(uk_cpi, uk_m1, by = "date")
glimpse(gbp)

uk_cpi <- ts(data = gbp$value.x, start = c(2000,1), frequency = 12)
uk_m1 <- ts(data = gbp$value.y, start = c(2000,1), frequency = 12)

# Assuming `uk_m1` and `uk_cpi` are already loaded as time series objects
# Combine CPI and M1 into one dataset for analysis
data <- cbind(uk_m1, uk_cpi)

# Step 1: Check for stationarity using the Augmented Dickey-Fuller (ADF) test
adf_m1 <- adf.test(uk_m1)
adf_cpi <- adf.test(uk_cpi)

# Print ADF test results
print(adf_m1)
print(adf_cpi)

# Step 2: Make the series stationary by differencing if needed
if (adf_m1$p.value > 0.05) {
  m1_diff <- diff(uk_m1)
} else {
  m1_diff <- uk_m1
}

if (adf_cpi$p.value > 0.05) {
  cpi_diff <- diff(uk_cpi)
} else {
  cpi_diff <- uk_cpi
}

# Combine the differenced series into a new dataset
stationary_data <- cbind(m1_diff, cpi_diff)

# Step 3: Subset the data for training (accounting for differencing lag)
training_data <- window(stationary_data, start = c(2000, 2), end = c(2023, 11))

# Step 4: Select the lag length for the VAR model using information criteria
lag_selection <- VARselect(training_data, lag.max = 10, type = "const")
optimal_lag <- lag_selection$selection["AIC(n)"]

# Step 5: Fit the VAR model using the selected lag length
var_model <- VAR(training_data, p = optimal_lag, type = "const")

# Step 6: Forecast CPI for the period beyond 2023-11
forecast_horizon <- 60  # Adjust forecast horizon as needed
forecast_results <- predict(var_model, n.ahead = forecast_horizon)

# Step 7: Extract forecasted differenced CPI values and confidence intervals
cpi_diff_forecast <- forecast_results$fcst$cpi_diff[, "fcst"]
cpi_diff_forecast_upper <- forecast_results$fcst$cpi_diff[, "upper"]
cpi_diff_forecast_lower <- forecast_results$fcst$cpi_diff[, "lower"]

# Step 8: Reconstruct the forecasted CPI series from the differenced data
# Use the last actual value before the forecast period
last_actual_cpi_value <- tail(uk_cpi, 1)

cpi_forecast <- cumsum(c(last_actual_cpi_value, cpi_diff_forecast))
cpi_forecast_upper <- cumsum(c(last_actual_cpi_value, cpi_diff_forecast_upper))
cpi_forecast_lower <- cumsum(c(last_actual_cpi_value, cpi_diff_forecast_lower))

# Step 9: Plot the actual CPI values and forecasted CPI with shaded confidence intervals
plot(window(uk_cpi, start = c(2000, 1)), col = "black", lwd = 2, 
     main = "UK: Actual vs Forecasted CPI with Confidence Intervals",
     xlab = "Year", ylab = "CPI", xlim = c(2000, 2030), 
     ylim = range(c(uk_cpi, cpi_forecast, cpi_forecast_upper, cpi_forecast_lower), na.rm = TRUE) * c(0.9, 1.1))

# Add the shaded confidence interval
polygon(c(time(ts(cpi_forecast_upper[-1], start = c(2023, 12), frequency = 12)),
          rev(time(ts(cpi_forecast_lower[-1], start = c(2023, 12), frequency = 12)))),
        c(cpi_forecast_upper[-1], rev(cpi_forecast_lower[-1])),
        col = rgb(0.1, 0.5, 0.8, 0.3), border = NA)

# Add forecasted CPI line
lines(ts(cpi_forecast[-1], start = c(2023, 12), frequency = 12), col = "red", lty = 2, lwd = 2)

# Add actual CPI line (for completeness)
lines(window(uk_cpi, start = c(2000, 1)), col = "black", lwd = 2)

# Add a vertical dotted line at the forecast start
abline(v = c(2023 + 11/12), col = "darkgray", lty = "dotted", lwd = 2)

# Add a grid for visual clarity
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# Add a legend
legend("topleft", inset = 0.02, legend = c("Actual CPI", "Forecasted CPI", "Confidence Interval"),
       col = c("black", "red", rgb(0.1, 0.5, 0.8, 0.3)), lty = c(1, 2, NA), lwd = c(2, 2, NA), 
       pch = c(NA, NA, 15), pt.cex = 2, bty = "n", pt.bg = rgb(0.1, 0.5, 0.8, 0.3))

# Fine-tune the axis limits for clarity
axis(1, at = seq(2000, 2030, by = 5))

#################################################
# Number of years
years <- 5

# Initial and final values
initial_value <- cpi_forecast[1]
final_value <- cpi_forecast[length(cpi_forecast)]

# Average yearly growth rate
average_yearly_growth_rate <- ((final_value / initial_value)^(1 / years)) - 1

# Convert to percentage
average_yearly_growth_rate_percentage <- average_yearly_growth_rate * 100

# Print the result
print(average_yearly_growth_rate_percentage)

# Calculate yearly growth rates
num_years <- 5  # Specify the number of years
yearly_growth_rates <- numeric(num_years)

for (i in 1:num_years) {
  start_value <- cpi_forecast[(i - 1) * 12 + 1]  # Start of year
  end_value <- cpi_forecast[i * 12]  # End of year
  yearly_growth_rate <- (end_value / start_value) - 1
  yearly_growth_rates[i] <- yearly_growth_rate
}

# Convert to percentage
yearly_growth_rates_percentage <- yearly_growth_rates * 100

# Print the result
print(yearly_growth_rates_percentage)