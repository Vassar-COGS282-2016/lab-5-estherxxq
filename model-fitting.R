# Here's a (simulated) experiment, with a single subject and 500 categorization trials.
all.data <- read.csv('experiment-data.csv')
source('memory-limited-exemplar-model.R')
rm(sample.data.set)
rm(sample.training.data)

# Use optim() to fit the model to this data.
# Note: In optim() you can tell it to display updates as it goes with:
# optim( ... , control=list(trace=4))

memory.limited.log.likelihood <- function(parameters){
  sensitivity <- parameters[1]
  decay.rate <- parameters[2]
  
  if (sensitivity > 0 && decay.rate > 0 && decay.rate < 1){
    log.likelihood <- exemplar.memory.log.likelihood(all.data, sensitivity, decay.rate)
    
    return(-log.likelihood)
  } else {
    return(NA)
  }
}

fit.log.likelihood <- optim(c(2,0.6), memory.limited.log.likelihood, method="Nelder-Mead", 
                            control = list(trace=4))
fit.log.likelihood$par
# 5.1525614 0.6272553
fit.log.likelihood$value
# 187.5985 (so log likelihood = -187.5985)

# Now try fitting a restricted version of the model, where we assume there is no decay.
# Fix the decay.rate parameter to 1, and use optim to fit the sensitivity parameter.
# Note that you will need to use method="Brent" in optim() instead of Nelder-Mead. 
# The brent method also requires an upper and lower boundary:
# optim( ..., upper=100, lower=0, method="Brent")

decay.fixed.log.likelihood <- function(parameters){
  sensitivity <- parameters[1]
  
  decay.rate <- 1
  
  if (sensitivity > 0 && decay.rate == 1){
    log.likelihood <- exemplar.memory.log.likelihood(all.data, sensitivity, decay.rate)
    
    return(-log.likelihood)
  } else {
    return(NA)
  }
}

fit.decay.fixed <- optim(c(5), decay.fixed.log.likelihood, method="Brent", upper = 100, 
                         lower = 0)
fit.decay.fixed$par
#3.862599
fit.decay.fixed$value
#248.5161 (so log likelihood = -248.5161)

# What's the log likelihood of both models? (see the $value in the result of optiom(),
# remember this is the negative log likeihood, so multiply by -1.

# for the Nelder-Mead model, log likelihood = -187.5985
# for the Brent model, log likelihood = -248.5161

# What's the AIC and BIC for both models? Which model should we prefer?

AIC.NM <- 2 * length(fit.log.likelihood$par) - 2 * (-fit.log.likelihood$value)
# 379.197
AIC.Brent <- 2 * length (fit.decay.fixed$par) - 2 * (-fit.decay.fixed$value)
# 499.0321
exp((AIC.NM - AIC.Brent)/2)
# the Brent model is 9.50892e-27 times as likely as the Nelder-Mead model
# the NM model is 1/9.50892e-27 = 1.051644e+26 times better than the Brent model
# which is A LOT BETTER

BIC.NM <- length(fit.log.likelihood$par) * log(nrow(all.data)) - 2 * (-fit.log.likelihood$value)
# 387.6262
BIC.Brent <- length(fit.decay.fixed$par) * log(nrow(all.data)) - 2 * (-fit.decay.fixed$value)
# 503.2467
BIC.Brent - BIC.NM
# 115.6205
# the difference is WAY larger than 10: super strong evidence that the NM model is a better fit than Brent

#### BONUS...
# If you complete this part I'll refund you a late day. You do not need to do this.

# Use parametric bootstrapping to estimate the uncertainty on the decay.rate parameter.
# Unfortunately the model takes too long to fit to generate a large bootstrapped sample in
# a reasonable time, so use a small sample size of 10-100 depending on how long you are
# willing to let your computer crunch the numbers.

# Steps for parametric bootstrapping:
# Use the best fitting parameters above to generate a new data set (in this case, that means
# a new set of values in the correct column for all.data).
  
  #this function calculates the probability for the correct output for all.data      
  pr.generator <- function(all.data, sensitivity, decay.rate){
    ad <- all.data
    
    ad$pr.correct <- 1:nrow(ad)
    # the probability of the first row is 0.5
    ad$pr.correct[1] <- 0.5
    # calculate the probability of the following rows
    x = 2
    while (x <= nrow(ad)) {
      ad$pr.correct[x] <- exemplar.memory.limited(ad[1:x-1,], ad$x[x], ad$y[x], ad$category[x], sensitivity, decay.rate)
      x = x + 1
    }
    return(ad$pr.correct)
  }
  
  #This function generates a responses based on the probability of correct response
  correct.generator <- function(x){
    if (rbinom(1, 1, x)==1) {
      correct <- "TRUE"
    } else {correct <- "FALSE"}
    return(correct)
  }
  
  # use the same set of all.data in preparation to fit the likelihood model
  # pr.correct is generated for this "new" set of data; responses will be generated later
  new.data <- all.data
  new.data$pr.correct <- pr.generator(all.data, fit.log.likelihood$par[1], 
                                      fit.log.likelihood$par[2])

# Fit the model to this new data, record the MLE for decay.rate.
# Repeat many times to get a distribution of decay.rate values.
  
  # Use a while loop to:
  # 1. generate a set of response correctness data, based on the pr.correct
  # 2. fit the NM model using this set of data
  # 3. record the results (we only need decay rate but I wrote down sensitivity and negative log likelihood anyway)
  # the loop keep running until it reaches n (sample size)

  n <- 53
  best.sensitivity <- c(1:n)
  best.decay.rate <- c(1:n)
  best.neg.log.likelihood <- c(1:n)
  
  x = 1
  while (x <= n) {
    # generate a new set of responses to overwrite the existing responses
    new.data$correct<- mapply(function(x) {
      correct.generator(x)
    }, all.data$pr.correct)
    
    # Find the fittest value using a Nelder-Mead model
    bootstraping.fit <- optim(c(5,0.5), memory.limited.log.likelihood, method="Nelder-Mead", 
                                control = list(trace=4))
    # record the values
    best.sensitivity[x] <- bootstraping.fit$par[1]
    best.decay.rate[x] <- bootstraping.fit$par[2]
    best.neg.log.likelihood[x] <-bootstraping.fit$value
    
    # run the next sample
    x = x + 1
  }
  
  print(best.decay.rate)
  print(best.neg.log.likelihood)
  print(best.sensitivity)
# Usually you would then summarize with a 95% CI, but for our purposes you can just plot a
# histogram of the distribution.

  hist(best.decay.rate, xlim = c(0:1))
