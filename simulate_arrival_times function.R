simulate_arrival_times <- function(d_pt_month, n_patients) {
  
  a_times <- cumsum(rexp(n_patients, d_pt_month))
  return(a_times)
  
}

