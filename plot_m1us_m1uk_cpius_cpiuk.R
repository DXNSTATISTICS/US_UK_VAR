# Load necessary libraries
library(fredr)

KEY = "insert FREDR key here"
fredr_set_key(KEY)

DIRECTORY = "INSERT DIRECTORY PATH FOR SAVING GRAPHS"
setwd(dir = DIRECTORY)


# Download U.S. CPI (Consumer Price Index for All Urban Consumers: All Items) - Monthly
us_cpi <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("2000-01-01")
)
#the United States does not have a separate official Consumer Price Index (CPI) that covers all households, including rural areas.

# Download U.K. CPI (Consumer Price Index: All Items for the United Kingdom) - Monthly
uk_cpi <- fredr(
  series_id = "GBRCPIALLMINMEI",
  observation_start = as.Date("2000-01-01")
)

# Download U.S. M1 Money Stock - Monthly
us_m1 <- fredr(
  series_id = "M1NS",
  observation_start = as.Date("2000-01-01")
)


# Download U.K. M1 Money Supply - Monthly
uk_m1 <- fredr(
  series_id = "MANMM101GBM189S",
  observation_start = as.Date("2000-01-01")
)

# Convert uk_m1 to billions if necessary
uk_m1$value <- uk_m1$value / 1e6  # Adjust only if units were in trillions; modify divisor accordingly

dolla <- inner_join(us_cpi, us_m1, by = "date")
gbp<- inner_join(uk_cpi, uk_m1, by = "date")


us_cpi <- ts(data = dolla$value.x, start = c(2000,1), frequency = 12)
us_m1 <- ts(data = dolla$value.y, start = c(2000,1), frequency = 12)
uk_cpi <- ts(data = gbp$value.x, start = c(2000,1), frequency = 12)
uk_m1 <- ts(data = gbp$value.y, start = c(2000,1), frequency = 12)


# Enable scientific notation globally
options(scipen = 0)  # Lower values increase the likelihood of scientific notation

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2) + 0.1)  # Adjust margins for more space in each pane


# Plot each time series in a separate panel
plot(us_m1, col = "black", lwd = 2, main = "US M1 Money Supply", xlab = "Year", ylab = "M1")
plot(us_cpi, col = "black", lwd = 2, main = "US CPI", xlab = "Year", ylab = "CPI")
plot(uk_m1, col = "black", lwd = 2, main = "UK M1 Money Supply", xlab = "Year", ylab = "M1")
plot(uk_cpi, col = "black", lwd = 2, main = "UK CPI", xlab = "Year", ylab = "CPI")

# Reset the plotting layout to default
par(mfrow = c(1, 1))
