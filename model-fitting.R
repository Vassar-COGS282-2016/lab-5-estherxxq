# Here's a (simulated) experiment, with a single subject and 500 categorization trials.
all.data <- read.csv('experiment-data.csv')
source('memory-limited-exemplar-model.R')
rm(sample.data.set)
rm(sample.training.data)

# Use optim() to fit the model to this data.
# Note: In optim() you can tell it to display updates as it goes with:
# optim( ... , control=list(trace=4))

# below is a past example
accumulator.model.rmse <- function(parameters){
  rate <- parameters[1]
  noise <- parameters[2]
  threshold <- parameters[3]
  
  # check if the rate and the noise and the threshold - they should all be positive
  if (rate > 0 && noise > 0 && threshold >0) {
    simulated <- 
      replicate(1000, {simple.accumulator(rate, noise, threshold)})
    
    rmse <- sqrt(mean((rt.data - simulated)^2))
    
    return(rmse)
  } else {
    return(NA)
  }
}

fit.accumulator <- optim(c(1,3,100), accumulator.model.rmse, method="Nelder-Mead")
fit.accumulator$par
fit.accumulator$value


# Now try fitting a restricted version of the model, where we assume there is no decay.
# Fix the decay.rate parameter to 1, and use optim to fit the sensitivity parameter.
# Note that you will need to use method="Brent" in optim() instead of Nelder-Mead. 
# The brent method also requires an upper and lower boundary:
# optim( ..., upper=100, lower=0, method="Brent")

# What's the log likelihood of both models? (see the $value in the result of optiom(),
# remember this is the negative log likeihood, so multiply by -1.

# What's the AIC and BIC for both models? Which model should we prefer?

#### BONUS...
# If you complete this part I'll refund you a late day. You do not need to do this.

# Use parametric bootstrapping to estimate the uncertainty on the decay.rate parameter.
# Unfortunately the model takes too long to fit to generate a large bootstrapped sample in
# a reasonable time, so use a small sample size of 10-100 depending on how long you are
# willing to let your computer crunch the numbers.

# Steps for parametric bootstrapping:
# Use the best fitting parameters above to generate a new data set (in this case, that means
# a new set of values in the correct column for all.data).
# Fit the model to this new data, record the MLE for decay.rate.
# Repeat many times to get a distribution of decay.rate values.
# Usually you would then summarize with a 95% CI, but for our purposes you can just plot a
# histogram of the distribution.

