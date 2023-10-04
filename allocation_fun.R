allocation_fun <- function(n_total, block = 2, allocation = c(1, 1)) {
  
  # Create empty sampling vector
  sampling <- integer(0)
  
  # Calculate the number of blocks
  num_blocks <- n_total %/% sum(block)
  
  # Calculate the remaining items
  remaining_items <- n_total - (num_blocks * sum(block))
  
  # Create a vector of block sizes
  block_sizes <- rep(block, num_blocks)
  
  # Add the remaining items to the last block
  block_sizes[length(block_sizes)] <- block_sizes[length(block_sizes)] + remaining_items
  
  # Loop through each block size
  for (block_size in block_sizes) {
    
    # Create items based on allocation
    items <- rep(rep(0:1, times = allocation), each = block_size/sum(allocation))
    
    # Append the items to the sampling vector
    sampling <- c(sampling, sample(items))
  }
  
  return(sampling)
}

