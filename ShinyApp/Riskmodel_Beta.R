#
# This file will preprocess the IFR / Hopitalized fits from other papers, then simulate.

# Study / Analysis Characteristics: (Set in function)
Age_Range = '20 to 29'
Pctile = '95%'
Participants = 35

all_probs = read.csv(file = "app_dataset.csv")

all_probs[,'probability'] = paste0(all_probs[,'probability']*100,"%")

# Data is together. Now pulling is easy!
require(dplyr)


IndivRisk = function(Age_Range='20 to 29', Pctile='95%', gender='f', outcome='death', sta="all", population="best", Therapy=0){
  if (gender == 'b'){gender = c('m','f')}
  if(Age_Range == '20 to 39'){Age_Range = c('20 to 29','30 to 39')}
  relevant_probs <- all_probs %>%
  filter(ages %in% Age_Range, probability  %in%  Pctile, gend %in% gender, outc %in% outcome, status %in% sta, case %in% population) %>%
  mutate(therapy=Therapy, value=value*(1-Therapy))
  return(relevant_probs)
}

IndivRiskPull = function(Age_Range='20 to 29', Pctile='95%', gender='f', outcome='death', sta="all", population="best", Therapy=0){
  return(mean(unlist(IndivRisk(Age_Range, Pctile, gender, outcome, sta, population, Therapy)['value'])))
}

# To be created based on study information.
DoseResponse = function(Dose, Risk){ #Returns a value in (0,1) for the new risk. This can be customized.
  # Doses are likely concentrations of 10^4, 10^5, 10^6, and/or 10^7.
  # The Risk is the population level risk of a given impact. 
  return(Risk)
}

# Generally, we can define a study as a set of doses a dose response function, for each group of people. 
# The current version has a constant response, but the above function will allow this to vary.

# Studies should also allow for contingent groups. That needs to be implemented.

# Let's define a study as a list.
Study_Definition_Example = list(Groups=list(list(Risk=0.001, Count=5,Dose=10), list(Risk=.005, Count=7, Dose=100)), DR_function = function(Dose, Risk){return(Risk)})
# This doesn't allow for conditionals, etc. 
# TODO: Implement conditional branches.

# Given a list of things about a study, we want to create that study definition.

Make_Study <- function(Participants=1, Age_Range='20 to 29', Pctile='95%', gender='f', outcome='death', sta="all", population="best", Therapy=0, dose=0, simulate=FALSE){
  Indiv_Risk = IndivRisk(Age_Range, Pctile, gender, outcome, sta, population, Therapy)
  groups = dim(Indiv_Risk)[1]
  if (simulate == TRUE){
    warning("Simulation is not yet fully implemented for Make_Study.")
    simplex = runif(groups,0,Participants) #But this doesn't sum correctly
    group_sizes = round(simplex*Participants/sum(simplex),0)
    while (sum(group_sizes) < Participants){
      add_to = sample(1:groups)[1]
      group_sizes[add_to] = group_sizes[add_to] + 1
      # print("Added")
      }
    while (sum(group_sizes) > Participants){
      take_from = sample(1:groups)[1]
      if (group_sizes[take_from]>0){group_sizes[take_from] = group_sizes[take_from] - 1}
      # print("Subtracted")
      }
    } 
  else {
    group_sizes = rep(Participants/groups,groups)
    Study = list(Groups=list(), DR_function = function(...){return(1)})
    for (i in 1:dim(Indiv_Risk)[1]){
      Study$Groups[[i]] = list(Risk=Indiv_Risk[i,'value'], Count=group_sizes[i], Dose=dose)
      }
    return(Study)
    }
  }

StudyRisk <- function(Participants=1, Age_Range='20 to 29', Pctile='95%', gender='f', outcome='death', sta="all", population="best", Therapy=0, average=TRUE){
  if (length(outcome)>1){
    warning("Too many outcomes here.")
  }
  Study_Characteristics = Make_Study(Participants, Age_Range, Pctile, gender, outcome, sta, population, Therapy)
  
  prob_none = 1
  for(i in 1:length(Study_Characteristics$Groups)){
    prob_none = prob_none*((1-Study_Characteristics$Groups[[i]]$Risk)^(Study_Characteristics$Groups[[i]]$Count))
  }
  Risk_of_One_or_More = 1-prob_none
  return(Risk_of_One_or_More)
}

#Assumes that per-gender and per-age percentiles are perfectly correlated, i.e. we pick a single random number for the full study for each simulation across all ages and genders. We then pick the 95th percentile of these.

Simulate_StudyRisks = function(Simulations = 1000, Participants=1, Age_Range='20 to 29', gender='f', sta="all", population="best", Therapy = 0, qtile=c(0.95)){
  P_per_sim = runif(Simulations, min=0, max=100) #Pick percentiles from the overall population risk estimate to simulate
  death_probs=c()
  hosp_probs=c()
  for(i in 1:Simulations){
    P=paste0(as.character(round(P_per_sim[i])),"%")
    death_probs[i]= StudyRisk(Participants=Participants, Age_Range=Age_Range, gender=gender, outcome='death', Therapy=Therapy, sta=sta, population=population, Pctile = P)
    hosp_probs[i] = StudyRisk(Participants=Participants, Age_Range=Age_Range, gender=gender, outcome='hosp', Therapy=Therapy, sta=sta, population=population, Pctile = P)
  }
  return(c(Death_qtile=quantile(death_probs, qtile), Hosp_qtile = quantile(hosp_probs, qtile)))
}



#SUPER inefficient! (This takes a long time to run. 20 seconds per million is too slow.)
Simulate_Studies =  function(Simulations = 1000, Participants=15, Age_Range='20 to 29', gender='f', sta="all", population="best", Therapy=0, weights='Even'){
  if(gender=='b'){
    subgroups_g = c('m','f') 
  } else {
    subgroups_g = c(gender)
  }
  if(Age_Range == '20 to 39'){
    subgroups_a = c('20 to 29','30 to 39')
  } else {
    subgroups_a = c(Age_Range)
  }
  outcomes = matrix(rep(0,2*Simulations), nrow=Simulations,ncol=2)
  
  P_per_sim = runif(Simulations, min=0, max=100)
  for(i in 1:Simulations){
    genders = sample(subgroups_g, Participants, replace =T)
    ageranges =  sample(subgroups_a, Participants, replace =T)
    P=paste0(as.character(round(P_per_sim[i])),"%")
    outcome_probs = runif(Participants)
    hospitalizations = 0
    deaths = 0
    for(p in 1:Participants){
      d=IndivRiskPull(Age_Range=ageranges[p], Pctile=P, gender=genders[p], Therapy=Therapy, sta=sta, population=population, outcome='death')
      h=IndivRiskPull(Age_Range=ageranges[p], Pctile=P, gender=genders[p], Therapy=Therapy, sta=sta, population=population, outcome='hosp')
      if(outcome_probs[p]<d){deaths = deaths+1} else if(outcome_probs[p]<h){hospitalizations = hospitalizations+1}
    }
  outcomes[i,]=c(hospitalizations, deaths)
  colnames(outcomes)=c('non-fatal serious', 'deaths')
  }
  return(outcomes)
  }

# start_time = Sys.time()
# Outcomes = Simulate_Studies(Simulations=5000, 35,'20 to 39', 'b') #Under 2 minutes. 
# end_time = Sys.time()
# print(end_time-start_time) #~20 Seconds per million people simulated.
#Note: Need to simulate FAR too many people in the given age ranges to get reasonable data for CIs.


nonzeros <- function(valueset){
  sum(valueset!=0)
}

# We can simulate by picking a p(death) from the distributions. But there IS between-person correlation - the estimates are of the population-level average, so (I think) we should simulate across all people using the same probaility, and which leads to 2-levels of uncertainty in the .
