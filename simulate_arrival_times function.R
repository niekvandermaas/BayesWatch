simulate_arrival_times <- function(acc_pt, n_patients) {
  
  a_times <- cumsum(rexp(n_patients, acc_pt))
  return(a_times)
  
}

