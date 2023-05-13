calc_mse <- function(left, right) {
  
  # calculate the MSE for each node
  mse_left <-sum((left - mean(left)) ^ 2)
  mse_right <-sum((right - mean(right)) ^ 2)
  
  # calculate the overall mse
  mse_combined <- mse_left + mse_right
  
  # return
  return(
    list(
      mse_left = mse_left,
      mse_right = mse_right,
      mse_combined = mse_combined
    )
  )
    
}

calc_mse(left = c(0, 0, 1, 2), right = c(3))
calc_mse(left = c(0, 0, 1), right = c(2, 3))
calc_mse(left = c(0, 0), right = c(1, 2, 3))




