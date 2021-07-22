model_name <- "transduction model"
model_state.names <- c("Be","Bt","Bet","Pl", "Pe", "Pt")
model_theta.names <- c("beta", "L", "alpha", "tau")

#this is empty, it's flexibly filled in by using the choose_model() function
#please see README and transduction_model_functions.R script
model_simulateDeterministic <- function(theta,init.state,times) {
  
}


## function to compute log-prior
model_prior <- function(theta, log = TRUE) {
  
  #log.prior.L <- dunif(theta[["L"]], min = 1, max = 500, log = TRUE)
  log.prior.L <- dnorm(theta[["L"]], mean = 40, sd = 7, log = TRUE)
  log.prior.beta <- dunif(theta[["beta"]], min = 1, max = 1e20, log = TRUE)
  #log.prior.gamma <- dunif(theta[["gamma"]], min = 1e1, max = 1e5, log = TRUE)
  log.prior.alpha <- dunif(theta[["alpha"]], min = 1, max = 1e10, log = TRUE)
  
  #log.prior.tau <- dunif(theta[["tau"]], min = 0.01, max = 0.98, log = TRUE)
  log.prior.tau <- dnorm(theta[["tau"]], mean = 0.67, sd = 0.07, log = TRUE)
  
  log.sum <- log.prior.L + log.prior.beta + log.prior.alpha + log.prior.tau
  
  return(ifelse(log, log.sum, exp(log.sum)))
}

## function to compute the likelihood of one data point
model_pointLike <- function(data.point, model.point, theta, log = FALSE){
  
  dpoisBet = dpois(x = data.point[["Bet"]],
                   lambda = model.point[["Bet"]],
                   log = log)
  if(is.infinite(dpoisBet)) dpoisBet = -1e7
  
  
  dpoisPl = dpois(x = round(data.point[["P"]]/(10^(max(floor(log10(data.point[["P"]])),1)-1))),
                  lambda = model.point[["Pl"]]/(10^(max(floor(log10(data.point[["P"]])),1)-1)),
                  log = log)

  ## the prevalence is observed through a Poisson process
  return(sum(2*dpoisBet, dpoisPl))
}


## create deterministic SIR fitmodel
model <- fitR::fitmodel(
  name = model_name,
  state.names = model_state.names,
  theta.names = model_theta.names,
  simulate = model_simulateDeterministic,
  dprior = model_prior,
  dPointObs = model_pointLike)

saveRDS(model, here::here("Model", "transduction_model.rds"))
