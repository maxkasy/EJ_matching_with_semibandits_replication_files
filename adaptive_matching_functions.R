library(tidyverse)
library(rstan) # For MCMC
library(lpSolve) # For finding the optimal matching (integer programming)

# Compile model on start-up to avoid repeated re-compilation
# Store it, so it can be re-used next time, if the stan file has not changed
rstan_options(auto_write = TRUE)
logit_aggregated_stan_model = stan_model("additive_effects_logit_aggregated.stan")


# Take data frame with factors U and V and form a matrix of dummies
# for all interactions of U, V
predictor_matrix = function(data) {
    model.matrix( ~ U:V - 1, data)
}

# Use Stan to sample from posterior for the logistic random effects regression model
coefficient_posterior = function(data) {
    X = predictor_matrix(data)
    
    data_list = list(
        dim_U = length(levels(data$U)),
        dim_V = length(levels(data$V)),
        n = as.vector(colSums(X)), # number of trials and successes for each match type
        s = as.vector(data$y %*% X)
    )
    
    sampling(
        logit_aggregated_stan_model,
        data = data_list,
        show_messages = F, # suppress output
        refresh = 0, # suppress output
        seed = 12345,
        control = list(adapt_delta = 0.95) # to reduce "divergent transitions" in MCMC
    )
}


# Create predicted outcomes for all possible matches of elements of the factors  U and V
# Create data frame with all combinations of U and V, and i,j as indices
predictions_all_combinations = function(beta, U, V) {
    all_combinations = merge(tibble(U = U) %>% mutate(i = row_number()),
                             tibble(V = V) %>% mutate(j = row_number()))
    
    # add predictions from logit model
    all_combinations %>%
        mutate(yhat = plogis(predictor_matrix(all_combinations) %*% beta))
}

# A matching is a tibble with columns i and j, each row corresponding to a matched pair
evaluate_matching = function(matching,
                             predictions_all) {
    # select rows of all predictions correspnding to the rows in matching
    left_join(matching, predictions_all, by = c("i", "j")) %>%
        summarise(average = mean(yhat)) %>%  # return average predicted outcome
        as.numeric()
}



# Find a matching that maximizes predicted average outcomes, using integer programming
optimal_matching_implementation = function(predictions_all,
                                           m,
                                           n,
                                           size = rep(1, m), ##family size vector - 1 corresponds to LP
                                           capacity = rep(1, n), ##location capacity vector - 1 corresponds to LP
                                           method = "lpSolve") {
    # sort first by j, within j by i, then extract yhat
    predictions_all = predictions_all %>%
        arrange(j, i)
    obj = predictions_all$yhat / m
    
    ##################################
    #Set up the constraint matrix
    #Sizes submatrix
    size_mat <- kronecker(diag(n),
                          matrix(size, nrow = 1))
    # match refugees only once submatrix
    match_once = kronecker(matrix(rep(1, n), nrow = 1),
                           diag(m))
    # combine the constraints in one matrix
    constr_mat <- rbind(size_mat, match_once)
    # right hand side of the constraints - to be replaced?
    cap <- c(capacity, rep(1, m))
    
    if (method == "lpSolve") {
        solve = lp(
            direction = "max",
            objective.in = obj,
            const.mat = constr_mat,
            const.dir = rep("<=", (n + m)),
            const.rhs = cap,
            all.bin = TRUE
        )
        
        output = list(
            matching = predictions_all[as.logical(solve$solution), ] %>%
                arrange(i, j),
            predicted_average = solve$objval
        )
    } else if (method == "Gurobi") { # Allow for use of alternative solver; not used in our simulations
        model <- list()
        model$A          <- constr_mat
        model$obj        <- obj
        model$modelsense <- 'max'
        model$rhs        <- cap
        model$sense      <- rep("<=", (n + m))
        model$vtype      <- 'B'
        
        params <- list(OutputFlag = 0)
        
        result <- gurobi(model, params)
        
        output = list(
            matching = predictions_all[as.logical(result$x), ] %>%
                arrange(i, j),
            predicted_average = result$objval
        )
    }
    
    output
}



# Putting it all together
thompson_matching = function(prior_data, U, V, method = "lpSolve") {
    posterior = coefficient_posterior(prior_data)
    
    # need to extract one draw from posterior here
    betadraws = (posterior %>%
                     rstan::extract(permuted = TRUE))[["beta"]] #extract the simulation draws
    # extract the first draw for each coefficient (draws are permuted)
    # alpha is intercept, beta the other components
    betadraw = betadraws[1, ]
    
    predictions_all = predictions_all_combinations(betadraw, U$U, V$V)
    
    optimal_matching =
        optimal_matching_implementation(
            predictions_all,
            nrow(U),
            nrow(V),
            size = U$Size,
            capacity = V$Capacity,
            method = method
        )
    
    # estimated coefficients - not used by the algorithm, but for reporting results
    optimal_matching$beta_hat = colMeans(betadraws)
    
    optimal_matching
}
