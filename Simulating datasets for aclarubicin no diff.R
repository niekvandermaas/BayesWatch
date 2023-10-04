set.seed(2023)  #Use the same seed for each scenario
library(doParallel)
library(foreach)
library(rpact)
source("simulate_arrival_times function.R")
source("allocation_fun.R")
source("int_dat_fun.R")

# Define a function for one simulated dataset
sim_run <- function() {
  #Settings of simulation study
  d_pt_month <- 126/36
  cens_start<- 12 
  cens_end <- 42 #All patients are censored between 12 and 42 months, using a uniform distribution
  n_patients <- 126 #Number of patients in simulated data set
  interim_analysis <- seq(80, 120, 10)
  pw_survival_time_c <- list( "0 - <12" = 0.144, ">=12" = 0.053) #piecewise hazard values for control group
  pw_survival_time_t <- list( "0 - <12" = 0.144, ">=12" = 0.053) #piecewise hazard values for treatment group alt hyp =  "0 - <12" = 0.062, ">=12" = 0.050
  
  # Assigning interim look and final look
  number_look <- c(interim_analysis, n_patients)
  
  #Simulate the data set
  trt <- allocation_fun(N_total = n_patients, block = 12, allocation = c(1, 2))
  time <- rep(NA, n_patients)
  
  #Generate censoring dates
  cens_date <- cens_start+(cens_end-cens_start)* runif(n_patients)
  
  
  for (i in 1:sum(!trt)){
    #Control group of current trial
    time[which(!trt)[i]]<- min(cens_date[i],rpwexp(n = 1, s = pw_survival_time_c))
  } 
  
  for (j in 1:sum(trt)){
    #Intervention group of current trial
    time[which(!!trt)[j]]<- min(cens_date[j],rpwexp(n = 1, s = pw_survival_time_t))
  } 
  
  
  dat_tot <- data.frame(
    id = 1:n_patients,
    trt = trt, 
    acc_time = simulate_arrival_times(d_pt_month, n_patients), 
    time = time,
    event = time<cens_date
  )
  
  int_list <- list()
  
  for (i in 1:length(number_look)){
    int_list[[i]] <- int_dat_fun(dat_tot, n_int = number_look[i])
  }
  
  
  return(int_list)
}

# Set seed for simulations
set.seed(2023)

# Define the number of cores to use for parallel processing
num_cores <- parallel::detectCores()

# Register the parallel backend using doParallel
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Measure the execution time
execution_time <- system.time({
  # Perform 10000 replicates in simulation study using parallel processing
  sim_datasets_acla_no_diff <- foreach(i = 1:10000, .combine = "c", .packages = c("rpact", "tidyverse")) %dopar% {
    sim_run()
  }
})

# Stop the parallel backend
stopCluster(cl)

# Print the execution time
print(execution_time)