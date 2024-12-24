library(fredr)
library(tidyverse)


KEY = "insert FREDR key here"
fredr_set_key(KEY)

DIRECTORY = "INSERT DIRECTORY PATH FOR SAVING GRAPHS"
setwd(dir = DIRECTORY)

# Fetch the inflation data from 2000 to the present
us_inflation_data <- fredr(
  series_id = "FPCPITOTLZGUSA",
  observation_start = as.Date("2000-01-01")
)
glimpse(us_inflation_data)


# Download U.S. M1 Money Stock - Monthly
us_m1 <- fredr(
  series_id = "M1NS",
  observation_start = as.Date("2000-01-01")
)

inflation_us <- us_inflation_data %>%
  select(date, value) %>%
  rename(value1 = value)

#####################################################################################
# Merge the datasets by date
df <- inner_join(us_m1, inflation_us, by = "date") %>%
  select(date, M1 = value, Inflation = value1)

glimpse(us_m1)

# Define a scaling factor for inflation to match the visual range of M1
library(plotly)


# Generate a sequence of years from 2000 to 2024
years <- seq(as.Date("2000-01-01"), as.Date("2024-01-01"), by = "year")

# Create the plot
p <- plot_ly(df, x = ~date) %>%
  add_lines(y = ~M1, name = "M1 Money Supply", yaxis = "y1", line = list(color = '#1a3d7a')) %>%
  add_markers(y = ~M1, yaxis = "y1", marker = list(color = '#1a3d7a', size = 6), showlegend = FALSE) %>%
  add_lines(y = ~Inflation, name = "Inflation Rate", yaxis = "y2", line = list(color = 'gray')) %>%
  add_markers(y = ~Inflation, yaxis = "y2", marker = list(color = 'gray', size = 6), showlegend = FALSE) %>%
  layout(
    title = list(
      text = "M1 Money Supply and Inflation Rate USD in the USA",
      x=0.1,
      xanchor = "left"
    ),
    xaxis = list(
      title = "Year",
      tickangle = 90,  # Rotate x-axis labels
      tickformat = "%Y"  # Ensure year formatting
    ),
    yaxis = list(
      title = "M1 Money Supply (Dollars)",  # Update label to trillions
      tickformat = ".1f",  # Format numbers to one decimal place
      tickprefix = "",  # Remove "K" prefix
      ticksuffix = " B",  # Add "T" suffix for trillions
      side = "left",
      showgrid = FALSE
    ),
    yaxis2 = list(
      title = "Inflation Rate (%)",
      tickformat = ".2f",  # Format numbers to one decimal place
      side = "right",
      overlaying = "y",
      showgrid = FALSE,
      title_standoff = 20  # Adds space between the label and tick values
    ),
    margin = list(r = 80),  # Increase the right margin to provide space for y-axis label
    legend = list(x = 0.5, y = -0.25, 
                  xanchor = "center",
                  yanchor = "top",
                  orientation = "h")
  )

# Display the plot
p

# Save the plot as a PNG
export(p, file = "US_M1_and_Inflation.png", width = 8, height = 4.5, dpi = 300)
