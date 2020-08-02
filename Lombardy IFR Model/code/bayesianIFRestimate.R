#################################################################
# Bayesian analysis to get credible intervals for IFR estimates
#################################################################

# Parameters:

# delta: 7x1 vector of baseline fatality rate by age range
# deltaCovid: 7x1 vector of COVID-19 IFR by age range
# theta_i: 9x1 proportion infected by COVID-19 in each municipality 

###########################################
# Begin by loading and cleaning deaths data
source(file = "code/loadCleanData.R")

####################################
# Model by town
####################################
set.seed(0) # Set seed to assure replicability

dataLikelihoodTown <- merge(demographicData, 
                            allDeathsByYearTown, 
                            by.x = c("Denominazione", "ageRange"), 
                            by.y = c("NOME_COMUNE", "ageRange"), 
                            all.x = T)
dataLikelihoodTown[is.na(dataLikelihoodTown),] <- 0

model = function(){
  #priors
  deltaCovid[1] ~ dunif(0.0,.3)
  deltaCovid[2] ~ dunif(0.0,.3)
  deltaCovid[3] ~ dunif(0.0,.3)
  deltaCovid[4] ~ dunif(0.0,.3)
  deltaCovid[5] ~ dunif(0.0,.3)
  deltaCovid[6] ~ dunif(0.0,.3)
  deltaCovid[7] ~ dunif(0.0,.3)
  
  delta[1] ~ dunif(0.0,.1)
  delta[2] ~ dunif(0.0,.1)
  delta[3] ~ dunif(0.0,.1)
  delta[4] ~ dunif(0.0,.1)
  delta[5] ~ dunif(0.0,.1)
  delta[6] ~ dunif(0.0,.1)
  delta[7] ~ dunif(0.0,.1)
  
  theta_i[1] ~ dbeta(3,2)
  theta_i[2] ~ dbeta(3,2)
  theta_i[3] ~ dbeta(3,2)
  theta_i[4] ~ dbeta(3,2)
  theta_i[5] ~ dbeta(3,2)
  theta_i[6] ~ dbeta(3,2)
  theta_i[7] ~ dbeta(3,2)
  theta_i[8] ~ dbeta(3,2)
  theta_i[9] ~ dbeta(3,2)
  
  # likelihood over the 7 age groups (j) and 8 towns (i)
  for (i in 1:9){
    for (j in 1:7){
      totDeathsTown15[(i-1)*7 + j] ~ dbin(delta[j], tot2015[(i-1)*7 + j])
      totDeathsTown16[(i-1)*7 + j] ~ dbin(delta[j], tot2016[(i-1)*7 + j])
      totDeathsTown17[(i-1)*7 + j] ~ dbin(delta[j], tot2017[(i-1)*7 + j])
      totDeathsTown18[(i-1)*7 + j] ~ dbin(delta[j], tot2018[(i-1)*7 + j])
      totDeathsTown19[(i-1)*7 + j] ~ dbin(delta[j], tot2019[(i-1)*7 + j])
      totDeathsTown20[(i-1)*7 + j] ~ dbin(delta[j] + deltaCovid[j]*theta_i[i], tot2019[(i-1)*7 + j])
    }
  }
}

model.file="model.txt"
write.model(model, model.file)

# Initial seed to insure reproducibility
inits<-list(.RNG.name="base::Super-Duper", .RNG.seed=1)

# what parameters we want to track
params = c("deltaCovid",
           "delta",
           "theta_i")

## hyperparameters
# number of iterations
ni = 10000
# burn in interval
nb = 1000
# thinning interval
nt = 1
# number of chains
nc = 10

# compile model
jmod = jags.model(file = model.file, data = dataLikelihoodTown, n.chains = nc, inits = inits, n.adapt = 1000)

# iterate through jmod for the extent of the burn-in
update(jmod, n.iter=nb, by=1)

# draw samples from the posterior for params, given MCMC hyperparameters
postTown = coda.samples(jmod, params, n.iter = ni, thin = nt, seed = 0)

MCMCtrace(postTown,
          type = 'density',
          ind = TRUE)

# objectively assess convergence with gelmans diagnostic
gelman.diag(postTown)

# get summary of posterior samples
MCMCsum <- MCMCsummary(postTown, params = c('delta','theta_i', "deltaCovid"), digits=4, HPD = T,func = function(x) posterior.mode(x))

IFRbyAge <- MCMCsummary(postTown, params = c("deltaCovid"), digits=2, probs = c(0.025, 0.25,.5,.75,.975))
ageRanges <- unique(dataLikelihoodTown$ageRange)
IFRbyAge$`Age Range` <- ageRanges
names(IFRbyAge)[5] <- "Infection Fatality Rate"
IFRbyAge <- as.data.table(IFRbyAge)

#######################################################################################################################################################
# Run Model for each potential proportion of population infected, in order to make plot with changing population infected, fixing proportion infected
#######################################################################################################################################################

## hyperparameters
# number of iterations
ni = 1000
# burn in interval
nb = 100
# thinning interval
nt = 1
# number of chains
nc = 3

params <- c("deltaCovid", "delta")

infectedProportions <- seq(0.01, 1, 0.01)

graphDataAll <- data.table()

for(propInfected in infectedProportions){
  
  dataLikelihoodTown[, prop := propInfected]
  print(unique(dataLikelihoodTown$prop))
  model = function(){
    #priors
    deltaCovid[1] ~ dunif(0.0,.3)
    deltaCovid[2] ~ dunif(0.0,.3)
    deltaCovid[3] ~ dunif(0.0,.3)
    deltaCovid[4] ~ dunif(0.0,.3)
    deltaCovid[5] ~ dunif(0.0,.3)
    deltaCovid[6] ~ dunif(0.0,.3)
    deltaCovid[7] ~ dunif(0.0,.3)
    
    delta[1] ~ dunif(0.0,.1)
    delta[2] ~ dunif(0.0,.1)
    delta[3] ~ dunif(0.0,.1)
    delta[4] ~ dunif(0.0,.1)
    delta[5] ~ dunif(0.0,.1)
    delta[6] ~ dunif(0.0,.1)
    delta[7] ~ dunif(0.0,.1)
    
    #likelihood over the 7 age groups (j) and 8 towns (i)
    for (i in 1:9){
      for (j in 1:7){
        totDeathsTown15[(i-1)*7 + j] ~ dbin(delta[j], tot2015[(i-1)*7 + j])
        totDeathsTown16[(i-1)*7 + j] ~ dbin(delta[j], tot2016[(i-1)*7 + j])
        totDeathsTown17[(i-1)*7 + j] ~ dbin(delta[j], tot2017[(i-1)*7 + j])
        totDeathsTown18[(i-1)*7 + j] ~ dbin(delta[j], tot2018[(i-1)*7 + j])
        totDeathsTown19[(i-1)*7 + j] ~ dbin(delta[j], tot2019[(i-1)*7 + j])
        totDeathsTown20[(i-1)*7 + j] ~ dbin(delta[j] + (deltaCovid[j]*prop[j]), tot2019[(i-1)*7 + j])
      }
    }
  }
  
  model.file="model.txt"
  write.model(model, model.file)
  
  # compile model
  jmod = jags.model(file = model.file, data = dataLikelihoodTown, n.chains = nc, inits = inits, n.adapt = 1000)
  
  # iterate through jmod for the extent of the burn-in
  update(jmod, n.iter=nb, by=1)
  
  # draw samples from the posterior for params, given MCMC hyperparameters
  post = coda.samples(jmod, params, n.iter = ni, thin = nt, seed = 0)
  
  graphData <- as.data.table(MCMCsummary(post)[8:14, 3:5])
  graphData[, ageRange := ageRanges]
  graphData[, prop := propInfected]
  graphDataAll <- rbind(graphDataAll, graphData)
  }
  

