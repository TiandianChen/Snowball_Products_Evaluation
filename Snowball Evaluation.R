###############Analyzing the Performance and Risk Factors of Snowball Investment Strategies###############
# Load the 'readr' library for reading and parsing data from files.
library(readxl)
# Load the 'dplyr' library for data manipulation and transformation.
library(dplyr)
# Load the 'lubridate' library for working with dates and times.
library(lubridate)
# Load the 'ggplot2' library for creating data visualizations using the Grammar of Graphics.
library(ggplot2)


# Read the CSV file into a data frame
data <- read_xlsx("/Users/chentiandian/Documents/DA_Project/Snowball_Evaluation/CSI500_20220526.xlsx")

# Adding a new column 'ID' as a primary key with date values
# The 'ID' values are assigned in descending order of row numbers, 
# ensuring the newest date has the largest number
data$ID <- c(length(data$date):1)

data <- data[order(data$ID),]
data <- data[, c("ID", "date", "price")]

data$date <- as.Date(data$date, format = "%Y-%m-%d")  # Use '-' as the separator if it's in "YYYY-MM-DD" format

#=======================================================================#
# Function: calculate_monthly_observation_date_theoretical
# Description: Calculates the ith theoretical monthly observation date for a snowball product, 
#              considering the product lock-in period, starting from a specified initial date
#              Note: The calculated observation date may represent a non-trading day.
# Parameters:
#   - d: The initial date of the product.
#   - lock_in_period: The product lock-in period in months.
#   - i: The ith monthly observation date to check.
# Returns:
#   - observation_date: The calculated observation date or NA if the input date is invalid or a non-trading day.

calculate_monthly_observation_date_theoretical <- function(d, lock_in_period, i) {
  
  # Check if the input initial date is invalid or a non-trading day.
  if (is.na(d) | is_trading_day(d) == FALSE) {
    observation_date <- NA  # If NA, set the observation date to NA
  } else {
    # If not NA, calculate the observation date by adding months using %m+%
    observation_date <- d %m+% months(lock_in_period - 1 + i)
  }
  
  # Return the calculated observation date
  observation_date
}
#=======================================================================#



#=======================================================================#
# Function: calculate_monthly_observation_date_actual
# Description: Calculates the actual ith monthly observation date (a trading day) for a snowball product, considering the product lock-in period, starting from a specified initial date.
# Parameters:
#   - d: The initial date of the product.
#   - lock_in_period: The product lock-in period in months.
#   - i: The ith monthly observation date to check.
# Returns:
#   - observation_date: The calculated actual observation date (a trading day) for the ith month after the product lock-in period.
#                       If the product start date is in the first half of the month, the function iteratively checks consecutive days,
#                       starting from the theoretical observation date, until it finds the first trading day.
#                       If the product start date is in the second half of the month, the function iteratively checks consecutive days,
#                       starting from the theoretical observation date, until it finds the first trading day, with a one-day backward shift.

calculate_monthly_observation_date_actual <- function(d, lock_in_period, i) {
  q <- -1
  
  # Check if the day of the month is in the first half
  if (day(d) >= 1 & day(d) <= 15) {
    repeat {
      q <- q + 1
      theoretical_monthly_observation_date <- calculate_monthly_observation_date_theoretical(d, lock_in_period, i) + q
      # Check if the theoretical date is a trading day
      if (is_trading_day(theoretical_monthly_observation_date) == TRUE) {
        actual_monthly_observation_date <- theoretical_monthly_observation_date
        break
      }
    }
  } 
  
  # Check if the day of the month is in the second half
  else {
    repeat {
      q <- q + 1
      theoretical_monthly_observation_date <- calculate_monthly_observation_date_theoretical(d, lock_in_period, i) - q
      # Check if the theoretical date is a trading day
      if (is_trading_day(theoretical_monthly_observation_date) == TRUE) {
        actual_monthly_observation_date <- theoretical_monthly_observation_date
        break
      }
    }
  }
  
  actual_monthly_observation_date
}

# Helper Function: is_trading_day
# Description: Check if a given date is a trading day.
# Parameters:
#   - d: The date to check.
# Returns:
#   - logical: TRUE if the date is a trading day, FALSE otherwise.

is_trading_day <- function(d) {
  return(d %in% data$date)
}
#--------------------------------------------------#
# Example Usage
# Test with the example product: product start date is 2019-11-29 (a trading day), with a lock-in-period of 3 months
product_start_date <- ymd("2019-11-29")
is_trading_day(product_start_date)

# Calculate the first theoretical monthly observation date for the product
first_observation_theoretical <- calculate_monthly_observation_date_theoretical(product_start_date, 3, 1)
print(first_observation_theoretical)
is_trading_day(first_observation_theoretical)

# Calculate the first actual monthly observation date for the product
first_observation_actual <- calculate_monthly_observation_date_actual(product_start_date, 3, 1)
print(first_observation_actual)
is_trading_day(first_observation_actual)
#--------------------------------------------------#
#=======================================================================#



#=======================================================================#
# Function: if_knock_out
# Description: Checks if the upper threshold is breached on a monthly observation date before the product maturity.
# Parameters:
#   - d: Initial date of the product.
#   - r: Ratio between the upper threshold price and the initial reading price.
#   - product_tenure: Product maturity period in months.
#   - lock_in_period: Product lock-in period in months.
# Returns:
#   - 1 if the product successfully knocks out before maturity, 0 otherwise.

if_knock_out <- function(d, r, product_tenure, lock_in_period) {
  # Get the initial reading price based on the initial date of the product
  base_price <- data$price[data$date == d]
  
  # Loop through monthly observation dates within the applicable period
  for (n in 1:(product_tenure - lock_in_period + 1))  {
    # Get the price at the current monthly observation date
    monthly_observation_date_price <- data$price[data$date == calculate_monthly_observation_date_actual(d, lock_in_period, n)]
    
    # Check if the upper threshold is breached
    if (monthly_observation_date_price >= base_price * r) {
      return(1)  # Product successfully knocks out
    }
  }
  
  return(0)  # Product did not knock out before maturity
}
#=======================================================================#
#=======================================================================#
# 这边到时候再补充一个input function #
ProductTenure <- 24
LowerThresholdRatio <- 0.8
UpperThresholdRatio <- 1
LockInPeriod <- 3
num_rows <- nrow(data)
#=======================================================================#
#-----------------------------------------------------------------------#
# Column: knock_out_result
# Description: Indicates if the product has knocked out before maturity, 
# excluding those with a start date to the latest dataset date difference 
# less than the predetermined maturity duration.
# Column values: 
#   - 1 if there is a knock-out event
#   - 0 otherwise
#   - NA if the product start date is too recent for the predetermined product tenure

for(i in 1:num_rows) {
  
  # Check if the date in the current row is within the specified range
  if(data$date[i] <= data$date[num_rows] - months(ProductTenure)) {
    
    # If within the range, calculate knock-out result using if_knock_out function
    data$knock_out_result[i] <- if_knock_out(data$date[i], UpperThresholdRatio, ProductTenure, LockInPeriod)
  }
  else {
    
    # If outside the specified range, set knock_out_result to NA
    data$knock_out_result[i] <- NA
  }
}
#-----------------------------------------------------------------------#



#=======================================================================#
# Function: if_neither_knock_in_nor_knock_out
# Description: Checks if neither knock-in nor knock-out occurs before the product maturity.
# Parameters:
#   - d: Initial date of the product.
#   - r_knock_out: Ratio between the upper threshold price and the initial reading price.
#   - r_knock_in: Ratio between the lower threshold price and the initial reading price.
#   - product_tenure: Product maturity period in months.
#   - lock_in_period: Product lock-in period in months.
# Returns:
#   - 1 if neither knock-in nor knock-out occurs on any observation date, 0 otherwise.

if_neither_knock_in_nor_knock_out <- function(d, r_knock_in, r_knock_out, product_tenure, lock_in_period) {
  base_price <- data$price[data$date == d]
  first_knock_out_observation <- calculate_monthly_observation_date_actual(d, lock_in_period, 1)
  last_knock_out_observation <- calculate_monthly_observation_date_actual(d, lock_in_period, product_tenure - lock_in_period + 1)
  first_knock_out_observation_ID <- data$ID[data$date == first_knock_out_observation]
  last_knock_out_observation_ID <- data$ID[data$date == last_knock_out_observation]
  
  neither_knock_in_nor_knock_out <- 1  # Assume neither knock-in nor knock-out until proven otherwise
  
  for (i in first_knock_out_observation_ID:last_knock_out_observation_ID) {
    if(data$price[i] < r_knock_in * base_price | data$price[i] >= r_knock_out * base_price) {
      neither_knock_in_nor_knock_out <- 0
      break # Exit the loop as soon as a knock-in or knock-out event is found
    }
  }
  
  return(neither_knock_in_nor_knock_out)
}
#=======================================================================#
#-----------------------------------------------------------------------#
# Column: neither_knock_in_nor_knock_out_result
# Description: Indicates if neither a knock-in nor a knock-out event occurred 
# before the product matures, excluding those with a start date difference to 
# the latest dataset date less than the predetermined maturity duration.
# Column values: 
#   - 1 if there is neither a knock-in nor a knock-out event
#   - 0 otherwise
#   - NA if the product start date is too recent for the predetermined product tenure

for(i in 1:nrow(data)) {
  
  # Check if the date in the current row is within the specified range
  if(data$date[i] <= data$date[num_rows] - months(ProductTenure)) {
    data$neither_knock_in_nor_knock_out_result[i] <- if_neither_knock_in_nor_knock_out(data$date[i], LowerThresholdRatio, UpperThresholdRatio, ProductTenure, LockInPeriod)
  }
  else {
    
    # If outside the specified range, set neither_knock_in_nor_knock_out_result to NA
    data$neither_knock_in_nor_knock_out_result[i] <- NA
  }
}
#-----------------------------------------------------------------------#



#=======================================================================#