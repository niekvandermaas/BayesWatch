simulate_arrival_times <- function(d_pt_month, n_patients) {
  
  v_times <- cumsum(rexp(n_patients, d_pt_month))
  return(v_times)
  
}

