set.seed(2023)  #Use the same seed for each scenario
library(doParallel)
library(foreach)
source("~/r_projects/Simulatie controle arm/simulate_arrival_times function.R")
source("~/r_projects/Simulatie controle arm/allocation_fun.R")
source("~/r_projects/Simulatie controle arm/int_dat_fun.R")
source("~/r_projects/Simulatie controle arm/rpwexp.R") # from nphsim package

# Define a function for one simulated dataset
sim_run <- function(n_patients, #Number of patients in simulated data set
                    pw_survival_time_c, #piecewise hazard values for control group
                    pw_survival_time_e, #piecewise hazard values for treatment group
                    pw_intervals, #time cutpoint at 12 months for pw exp model
                    interim_analysis, #number of patients at interim look
                    acc_pt, #accrual per time unit (months)
                    prop_loss_to_fu, #proportion lost to follow up 
                    total_time #total time of the study
                    ) {
  # Assigning interim look and final look
  number_look <- c(interim_analysis, n_patients)
  
  #Simulate the data set
  trt <- allocation_fun(n_total = n_patients, block = 12, allocation = c(1, 2))
  time <- rep(NA, n_patients)
  
  for (i in 1:sum(!trt)){
    #Control group of current trial
    time[which(!trt)[i]]<- min(total_time, rpwexp(n = 1, rate = pw_survival_time_c, intervals = pw_intervals))
  } 
  
  for (j in 1:sum(trt)){
    #Intervention group of current trial
    time[which(!!trt)[j]]<- min(total_time, rpwexp(n = 1, rate = pw_survival_time_e, intervals = pw_intervals))
  } 
  
  # Simulate loss to follow-up
  n_loss_to_fu <- ceiling(prop_loss_to_fu * n_patients)
  loss_to_fu   <- rep(FALSE, n_patients)
  loss_to_fu[sample(1:n_patients, n_loss_to_fu)] <- TRUE
  
  # Creating a new data frame for all the variables
  dat_tot <- data.frame(
                    id = 1:n_patients,
                    trt = trt, 
                    acc_time = simulate_arrival_times(acc_pt, n_patients), 
                    time = time,
                    event = time<total_time,
                    loss_to_fu = loss_to_fu
  )
  
  # lost to follow up is uniformly distributed
  dat_tot$time[dat_tot$loss_to_fu]  <- runif(n_loss_to_fu, 0, dat_tot$time[dat_tot$loss_to_fu])
  dat_tot$event[dat_tot$loss_to_fu] <- rep(0, n_loss_to_fu)
  
  
  int_list <- list()
  
  for (i in 1:length(number_look)){
    int_list[[i]] <- int_dat_fun(dat_tot, n_int = number_look[i], n_max = n_patients)
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
  sim_datasets_acla <- foreach(i = 1:10000, .combine = "c", .packages = c("tidyverse")) %dopar% {
    # Simulation study settings
    sim_run(n_patients = 126, #total of 126 patients
            pw_survival_time_c = c(0.144, 0.053), #hazard rates for control treatment
            pw_survival_time_e = c(0.062, 0.050), # hazard rates for experimental treatment
            pw_intervals = c(0,12), #cutpoint is at 12 months or 1 year
            interim_analysis = seq(80, 120, 10), #interim look at 80, 90, 100, 110 and 120 patients
            acc_pt = 126/36, # accrual rate per time unit (months)
            prop_loss_to_fu = 0.10, #proportion loss to follow-up
            total_time = 42 #total study time
            )
  }
})

# Stop the parallel backend
stopCluster(cl)

# Print the execution time
print(execution_time)
