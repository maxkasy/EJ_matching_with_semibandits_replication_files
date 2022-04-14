library(tidyverse)
library(lubridate)
library(furrr)
source("adaptive_matching_functions.R")
future::plan(multisession)

# Whether to do matching by months or quarters
period = "quarters"
# Number of simulations
n_simulations = 32

# Load and prepare data for simulation
theta_calibrated =
    read_csv("Data_processed/theta_calibrated.csv") %>%
    select(U, V, theta) %>%
    mutate(U = factor(U), V = factor(V))

hias_data =
    read_csv("Data_processed/HIAS_Anonymous_Prepared.csv") %>%
    mutate(
        U = factor(U),
        V = factor(V),
        Simulated_V = V, # Imputing simulated affiliate for everyone (some will be over-written)
        Optimal_V = V
    ) %>%
    left_join(theta_calibrated, by = c("U", "V")) %>%
    mutate(theta_original = theta)

hias_data = hias_data %>%
    mutate(
        Simulated_Y =  rbinom(nrow(hias_data), 1,
                              p = theta),
        y = Simulated_Y,
        Successful_match = 0 # to keep track of whether simulated matching was successful
    ) 

affiliate_capacities =
    read_csv("Data_processed/affiliate_capacities.csv") %>%
    mutate(V = factor(V, levels = 1:17)) %>%
    mutate(
        Capacity =
            case_when(
                period == "months" ~ monthly_capacity,
                period == "quarters" ~ quarterly_capacity
            )
    )

start_date = min(hias_data$Arrival)
# Excluding 2020 to avoid issues of incomplete data / Covid
end_date = "2019-10-01"


arrival_dates = seq(ymd(start_date), ymd(end_date), by = period)


future_map(1:n_simulations, ~ {
    for (t in seq(5, length(arrival_dates))) {
        # Prior cases, regardless of NUST status
        # Note that t-1 in the filter argument implies that employment is observed with 1 period of delay
        prior_cases_simulated = hias_data %>%
            filter(Arrival < arrival_dates[t - 1]) %>%
            mutate(V = Simulated_V,
                   y = Simulated_Y)
        
        # Current NUST==1 cases for which a counterfactual treatment will be simulated
        # Only NUST == 1 can be reassigned
        current_case_indices = ((hias_data$Arrival >= arrival_dates[t]) &
                                    (hias_data$Arrival < arrival_dates[t + 1]) &
                                    (hias_data$NUST == 1)
        ) %>%
            which() # converting logical subsetting to integer indices
        
        current_cases = hias_data[current_case_indices,]
        
        # Available capacity at affiliates this month,
        # based on annual capacities
        current_capacity = affiliate_capacities %>%
            filter(Year == year(as_date(arrival_dates[t])),
                   Quarter == quarter(as_date(arrival_dates[t])))
        
        # Calculate posterior based on prior cases with simulated assignment
        # Assign current cases to affiliates, subject to capacity constraints
        simulated_matching = thompson_matching(
            prior_cases_simulated,
            U = current_cases,
            V = current_capacity,
            method = "lpSolve"
        )
        
        matching = simulated_matching$matching %>%
            left_join(theta_calibrated, by = c("U", "V")) %>%
            mutate(Simulated_Y =  rbinom(nrow(simulated_matching$matching), 1,
                                         p = theta))
        
        # Printing output during simulation
        cat(
            "t: " ,
            t,
            "\nDate: ",
            year(as_date(arrival_dates[t])),
            " ",
            month(as_date(arrival_dates[t])),
            "\nCases: ",
            nrow(current_cases),
            "\nMatches: ",
            nrow(matching),
            "\n\n\n"
        )
        
        # For NUST==1 observations, impute Thompson matchings, and simulated outcomes
        current_case_indices_matched =
            current_case_indices[matching$i]
        
        hias_data[current_case_indices_matched, "Simulated_V"] =
            matching$V
        hias_data[current_case_indices_matched, "Simulated_Y"] =
            matching$Simulated_Y
        hias_data[current_case_indices_matched, "Successful_match"] = 1
        
        # Impute the oracle optimum matching and corresponding theta
        U = current_cases
        V = current_capacity
        oracle_optimal_matching =
            optimal_matching_implementation(
                predictions_all_combinations(theta_calibrated$theta, U$U, V$V),
                nrow(U),
                nrow(V),
                size = U$Size,
                capacity = V$Capacity,
                method = "lpSolve"
            )
        current_case_indices_matched_optimal =
            current_case_indices[oracle_optimal_matching$i]
        hias_data[current_case_indices_matched, "Optimal_V"] =
            oracle_optimal_matching$matching$V
    }
    
    # Store simulation output in the appropriate subfolder
    # One file for each simulation run
    file_name = paste("Simulation_output/",
                      .x,
                      "_hias_simulated_allocation.csv",
                      sep = "")
    hias_data %>% write_csv(file_name)
})
