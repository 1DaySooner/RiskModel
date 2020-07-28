# Summarise a single column of mcmc output
summarise_mcmc_single <- function(x, q = c(0.025, 0.975)){
  
  # create output dataframe to populate:
  output <- c(mode=numeric(), mean=numeric(), median=numeric()) 
    
  # compute kernel density and normalise
  d <- density(x)
  d$y <- d$y/sum(d$y)
  
  # get mode
  x_mode <- d$x[which.max(d$y)]
  x_mean = mean(x)
  x_median = median(x)
  
  return(c(mode = x_mode, mean = x_mean, median = x_median, quantile(x, q))) #Modified to allow more qunatile outputs.
}

# Summarise multiple columns of MCMC output
summarise_mcmc <- function(x, q = c(0.025, 0.975)){
  if(is.vector(x)){
    x <- matrix(x)
  }
  names = names(summarise_mcmc_single(x[,1], q=q))
  out_df = data.frame(t(apply(x, 2,  summarise_mcmc_single, q = q)))
  names(out_df) <- names
  return(out_df)
}
