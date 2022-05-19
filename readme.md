# Replication code for "Matching with semi-bandits"
by Maximilian Kasy and Alexander Teytelboym


## Overview

This archive provides all code needed to replicate the calibrated simulations in "Matching with semi-bandits."

## Code-files

To execute all our code sequentially, source **master.R**. 
This file also has a (commented) line installing the required **R** packages.

### Generic

The following two files contain code that might be useful for others who wish to implement our proposed adaptive matching procedure:

1. **additive_effects_logit_aggregated.stan**: This is the **Stan** file specifying  our generic hierarchical Bayesian model for binary outcomes, as described in Table 1 in the manuscript.
1. **adaptive_matching_functions.R**: This file contains all the R code (functions) for (i) sampling from the posterior for the hierarchical Bayesian model using Hamiltonian Monte Carlo as implemented in Stan, 
(ii) solving for the optimal matching for given model parameters and capacity constraints, using **lpSolve** for integer programming, and 
(iii) implementing Thompson sampling for matching by combining (i) and (ii).
 

### Application specific

The following three scripts contain code that is specific to our empirical application and the calibrated simulations based on it. To replicate our results, execute these three scripts sequentially:

1. **dataprep.R**: This script reads *HIAS_Anonymous_with_Emp.csv* in the folder *Data_raw/*, and produces three processed data files in *Data_processed/*, *HIAS_Anonymous_Prepared.csv*, *affiliate_capacities.csv*, and *theta_calibrated.csv*.
1. **simulation.R**: This script runs the main simulations, using the processed data files, and stores the output in *Simulation_output/*, with one file for each simulation iteration.
1. **summarize_simulations.R**: This file reads the simulation output, and produces the figures shown in the paper, storing them in the folder *Figures/*.

Total running time is about about 6 hours on our laptop, a Macbook Pro with 8 CPUs. Most of this time is spent by running the main simulations in **simulation.R**, while the other two scripts execute in less than one minute.


## Data-files


1. Raw:
    - **HIAS_Anonymous_with_Emp.csv**: Our source data as obtained from HIAS.
2. Processed:
    - **HIAS_Anonymous_Prepared.csv**: The sequence of arriving refugee families who need to be matched to an affiliate.
    - **affiliate_capacities.csv**: The available number of slots for a given affiliate and time period.
    - **theta_calibrated.csv**: Our calibrated parameter estimates, based on fitting the hierarchical Bayesian model with actual employment outcomes. 


Our code runs from start to end with these files placed in the appropriate subfolders, as described above.
However, for data-privacy reasons we cannot share the raw data (*HIAS_Anonymous_with_Emp.csv*), or the individual-level processed data (*HIAS_Anonymous_Prepared*); we have provided temporary access to the data-editors of Econometrics Journal to verify reproducibility.

The original *HIAS_Anonymous_with_Emp.csv* is a csv file that includes the following variables:

- Case Size, Male, English, Age (25-54), NUST, Employed: Binary.
-Arrival month/year: Date in yyyy-mm format.
- Affiliate Code: Integer identifier for the affiliate.

The derived *HIAS_Anonymous_Prepared.csv* is a csv file that includes the following variables which are used for the simulations:

- U and V: Integer identifiers for refugee type and affiliate.
- Employed: Binary outcome variable. 
- NUST: Binary variable indicating whether a given individual might be reassigned.
- Arrival: Date in yyyy-mm-dd format.

## Dependencies

Our code uses **R** and **Stan** to implement adaptive matching and the corresponding calibrated simulations.

In R we use the following packages (and their dependencies), which need to be installed prior to running our code:
- tidyverse (for data processing)
- lubridate (for processing date variables)
- furrr (for parallel computing)
- rstan (for using Stan from R)
- lpSolve (for integer programming)
- patchwork (for producing figures combinging multiple plots)

### Versions
On our system, we used the following versions of these packages:

R version 4.1.2 (2021-11-01)  
Platform: x86_64-apple-darwin17.0 (64-bit)  
Running under: macOS Monterey 12.2.1
  
**Packages**:  
lpSolve_5.6.15, rstan_2.21.3, StanHeaders_2.21.0-7,
lubridate_1.8.0, forcats_0.5.1, stringr_1.4.0, 
dplyr_1.0.8,    purrr_0.3.4,    readr_2.1.2,
tidyr_1.2.0,    tibble_3.1.6,   ggplot2_3.3.5,
tidyverse_1.3.1


## Output

The code ultimately produces the following figures, as shown in the manuscript:

![Simulated trajectories](Figures/Simulated_expected_trajectories.png?raw=true)

![Simulated trajectories](Figures/Simulated_expected_trajectories_bytype.png?raw=true)

![Aggreggated simulation outcomes by type](Figures/Aggregate_bytype_casecount.png?raw=true)

![Aggreggated simulation outcomes by type](Figures/Aggregate_by_affiliate.png?raw=true)

![Aggreggated simulation outcomes by type](Figures/Realized_and_expected_trajectories.png?raw=true)

