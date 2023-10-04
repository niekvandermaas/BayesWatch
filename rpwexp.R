rpwexp <- function(n, rate = 1, intervals = NULL, cumulative = FALSE) {
  
  # Check if intervals argument is NULL
  if (is.null(intervals)) {
    if (cumulative) {
      return(cumsum(rexp(n, rate[1])))
    } else {
      return(rexp(n, rate[1]))
    }
  }
  
  k <- length(rate)
  
  # Check if there's only one rate value
  if (k == 1) {
    if (cumulative) {
      return(cumsum(rexp(n, rate)))
    } else {
      return(rexp(n, rate))
    }
  }
  
  # Check if the length of intervals is sufficient
  if (length(intervals) < k - 1) {
    stop("length(intervals) must be at least length(rate) - 1")
  }
  
  tx <- 0
  j <- 1
  times <- array(0, n)
  timex <- cumsum(intervals)
  indx <- array(TRUE, n)
  
  for (i in 1:k) {
    nindx <- sum(indx)
    
    # Break the loop if all items have been assigned times
    if (nindx == 0) break
    
    increment <- rexp(nindx, rate[i])
    
    if (cumulative) {
      times[indx] <- tx + cumsum(increment)
    } else {
      times[indx] <- tx + increment
    }
    
    if (i < k) {
      tx <- timex[i]
      indx <- (times > timex[i])
    }
  }
  
  return(times)
}
