library(tidyverse)
library(lubridate)

# For calibrating parameter values:
source("adaptive_matching_functions.R")

# Load and reformat data --------
hias_anonymized =
    read_csv("Data_raw/HIAS_Anonymous_with_Emp.csv") %>%
    rename(
        # Rename columns with spaces
        Affiliate = `Affiliate Code`,
        Arrival = `Arrival month/year`,
        Size = `Case Size`,
        Adult = `Age (25-54)`
    ) %>%
    mutate(Arrival = ymd(Arrival, truncated  = 2), # convert date to standard date format
           Size = 1) # Setting case size to 1

## Filter out small affiliates and older data  -----
cutoff_n = 150

larger_affiliates = hias_anonymized %>%
    group_by(Affiliate) %>%
    summarize(n = n()) %>%
    filter(n > cutoff_n)

hias_anonymized = hias_anonymized %>%
    filter(Affiliate %in% larger_affiliates$Affiliate,
           # subset data to larger affiliates
           year(Arrival) >= 2011) %>%
    mutate(
        # Add variables in format used by the matching algorithm
        U = interaction(Male, English, Adult),
        V = factor(Affiliate),
        y = as.integer(Employed)
    )

levels(hias_anonymized$U) = 1:8
levels(hias_anonymized$V) = 1:17

# Store processed data
hias_anonymized %>%
    write_csv("Data_processed/HIAS_Anonymous_Prepared.csv")


# Calculate posterior means from data, to use as calibrated values for the simulation ------
calibration_parameters =
    hias_anonymized %>%
    coefficient_posterior()

theta_calibrated = extract(calibration_parameters)$beta %>%
    plogis() %>%
    colMeans() %>% as_tibble() %>%
    mutate(
        U = rep(1:8, 17),
        V = rep(1:17, each = 8),
        name = paste("beta_", U, "_", V, sep = "")
    )

# Add success columns; just for reference (data not used in simulation) 
X = predictor_matrix(hias_anonymized)
theta_calibrated = theta_calibrated %>%
    mutate(
        n = as.vector(colSums(X)),
        s = as.vector(hias_anonymized$y %*% X),
        succes_rate = s / n
    ) %>%
    rename(theta = value)

# Store calibrated parameter values
theta_calibrated  %>%
    write_csv("Data_processed/theta_calibrated.csv")


# Create capacity data ------
# Quarterly capacity is calculated as actual matches of NUST = 1 arrivals
affiliate_capacities = hias_anonymized %>%
    filter(NUST == 1) %>%
    mutate(Year = year(Arrival),
           Quarter = quarter(Arrival)) %>%
    group_by(Year, Quarter, V) %>%
    summarize(quarterly_capacity = sum(Size)) %>%
    mutate(monthly_capacity = ceiling(quarterly_capacity / 3))

affiliate_capacities %>%
    write_csv("Data_processed/affiliate_capacities.csv")
