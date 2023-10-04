library(tidyverse)
int_dat_fun <- function(dat, n_int, n_max) { 
  if (n_int < n_max) {
  new_dat <- dat %>% 
  slice_head(n = n_int) %>%
  mutate(time_int = max(acc_time) - acc_time,
         time_int_2 = pmin.int(time, time_int),
         event_2 = ifelse(time_int_2 == time & event == TRUE, TRUE, FALSE)) %>%
    select(id, trt, acc_time, time_int_2, event_2) %>% rename(time = time_int_2, event = event_2)
  
  return(new_dat)} else {return(dat)}
}

