library(foreach)
library(doParallel)

# Set seed for simulations
set.seed(2023)

# Define the number of cores to use for parallel processing
num_cores <- parallel::detectCores()

# Register the parallel backend using doParallel
cl <- makeCluster(num_cores)
registerDoParallel(cl)

#Evaluate when probability of HR<1 is 95% or higher, given alternative hypothesis is true

prob <- rep(0, length(sim_datasets_acla))

execution_time <- system.time({
  x <- foreach(i = 1:length(sim_datasets_acla), .combine="c", .packages = c("rjags")) %dopar% {
    ##############################################
    #Bayesian analysis overall survival
    ##############################################
    #censored data
    censored<- c(ifelse(sim_datasets_acla[[i]]["event"] == 0, TRUE, FALSE))
    is_censored <- censored*1
    time <- sim_datasets_acla[[i]]["time"][,1]
    time[censored] <- NA
    
    t_cen <- rep(0, length(censored))
    t_cen[censored] <- unlist(sim_datasets_acla[[i]]["time"])[censored]
    group <- c(ifelse(sim_datasets_acla[[i]]["trt"]== 1, TRUE, FALSE))
    n_dat <- nrow(sim_datasets_acla[[i]])
    
    # put the data together for JAGS
    jags_dat <- list(time = time + 0.001,
                     t_cen = t_cen + 0.001,
                     is_censored = is_censored,
                     group = group,
                     n = n_dat
    )
    
    
    #which paramaters to monitor
    params_to_monitor <- c("p")
    
    # Running the model
    model <- jags.model(file = "weibull_survival_model.txt", jags_dat, 
                        n.chains = 1, n.adapt= 1000, quiet = TRUE)
    update(model, 1000); #1000 samples burn in
    mcmc_samples_acla <- coda.samples(model, params_to_monitor, n.iter= 2000)
    
    prob[i] <- mean(sapply(mcmc_samples_acla, mean))
  }
})

# Print the execution time
print(execution_time)

#posterior probabilities in matrix by interim analysis and trial
x_matrix <- matrix(x, nrow = 10000, ncol = 6, byrow = TRUE)

#apply stopping criteria, if pr > 0.95 stop for efficacy
new_mat <- x_matrix

new_mat <- apply(new_mat, 1, function(row) {
  for (j in 2:(length(row))) {
    if (row[j - 1] > 0.95 || is.na(row[j - 1])) {
      row[j] <- NA
    }
  }
  return(row)
})

new_mat <- t(new_mat)

#power after n = 80
sum(new_mat[,1] > 0.95) / 10000

#power after n = 90
(sum(new_mat[,1] > 0.95) + sum(new_mat[,2] > 0.95, na.rm = T)) / 10000

#power after n = 100
(sum(new_mat[,1] > 0.95) + sum(new_mat[,2] > 0.95, na.rm = T) + sum(new_mat[,3] > 0.95, na.rm = T)) / 10000

#power after n = 110
(sum(new_mat[,1] > 0.95) + sum(new_mat[,2] > 0.95, na.rm = T) + sum(new_mat[,3] > 0.95, na.rm = T) + 
    sum(new_mat[,4] > 0.95, na.rm = T)) / 10000

#power after n = 120
(sum(new_mat[,1] > 0.95) + sum(new_mat[,2] > 0.95, na.rm = T) + sum(new_mat[,3] > 0.95, na.rm = T) + 
    sum(new_mat[, 4] > 0.95, na.rm = T) + sum(new_mat[, 5] > 0.95, na.rm = T)) / 10000

#power after n = 126
(sum(new_mat[,1] > 0.95) + sum(new_mat[,2] > 0.95, na.rm = T) + sum(new_mat[,3] > 0.95, na.rm = T) + 
    sum(new_mat[, 4] > 0.95, na.rm = T) + sum(new_mat[, 5] > 0.95, na.rm = T) + sum(new_mat[, 6] > 0.95, na.rm = T)) / 10000


