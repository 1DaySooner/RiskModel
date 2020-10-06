
#
# This file will preprocess the IFR / Hopitalized fits from other papers, then simulate.

# Study / Analysis Characteristics: (Set in function)
Age_Range = '20 to 29'
Pctile = '95%'
Participants = 35


# Note: Both provide a fit for each of IFR and Hospitalization by age for all infections (including asymptomatics.) 



# This will be the code to preprocess inputs.

# Ideally, structure the data into a single dataframe to pull numbers from.
# The final version will need to create a combined posterior estimate over multiple studies.

# We will also need to correct for lack of cormorbidities based on studies with that information.


# For now, use the France study as our basis for estimates.
# Load data from files.

Male_probDeath <- cbind(gend = "m", outc="death", read.csv("France_Male_p_death_by_age_range.csv", row.names=1, check.names=FALSE))
Male_probDeath$ages <- rownames(Male_probDeath)
Female_probDeath <- cbind(gend = "f", outc="death", read.csv("France_Female_p_death_by_age_range.csv", row.names=1, check.names=FALSE))
Female_probDeath$ages <- rownames(Female_probDeath)
Male_probHosp <- cbind(gend = "m", outc="hosp", read.csv("France_Male_p_hospitalization_by_age_range.csv", row.names=1, check.names=FALSE))
Male_probHosp$ages <- rownames(Male_probHosp)
Female_probHosp <- cbind(gend = "f", outc="hosp", read.csv("France_Female_p_hospitalization_by_age_range.csv", row.names=1, check.names=FALSE))
Female_probHosp$ages <- rownames(Female_probHosp)
Male_probICU <- cbind(gend = "m", outc="ICU", read.csv("France_Male_p_death_by_age_range.csv", row.names=1, check.names=FALSE))
Male_probICU$ages <- rownames(Male_probICU)
Female_probICU <- cbind(gend = "f", outc="ICU", read.csv("France_Female_p_death_by_age_range.csv", row.names=1, check.names=FALSE))
Female_probICU$ages <- rownames(Female_probICU)

participants <- 10:100

problist = paste0(formatC(c(0:100)),"%")
agelist = c("0 to 19", "20 to 29", "30 to 39", "40 to 49", "50 to 59", "60 to 69", "70 to 79", "80 to 100")

all_probs <- 
  rbind(
    reshape(Male_probDeath, direction='long', v.name="value", varying=problist, times=problist, idvar="ages"),
    reshape(Female_probDeath, direction='long', v.name="value", varying=problist, times=problist, idvar="ages"),
    reshape(Male_probHosp, direction='long', v.name="value", varying=problist, times=problist, idvar="ages"),
    reshape(Female_probHosp, direction='long', v.name="value", varying=problist, times=problist, idvar="ages"),
    reshape(Male_probICU, direction='long', v.name="value", varying=problist, times=problist, idvar="ages"),
    reshape(Female_probICU, direction='long', v.name="value", varying=problist, times=problist, idvar="ages")
  )

names(all_probs)[4] <- "probability"
row.names(all_probs) <- NULL

all_probs = as.data.frame(all_probs)

# Data is together. Now pulling is easy!

IndivRisk = function(Age_Range='20 to 29', Pctile='95%', gender='f', outcome='death', Therapy=0){
  if (gender == 'b'){gender = c('m','f')}
  if(Age_Range == '20 to 39'){Age_Range = c('20 to 29','30 to 39')}
  relevant_probs <- all_probs %>%
  filter(ages %in% Age_Range, probability  %in%  Pctile, gend  %in% gender, outc  %in% outcome) %>%
  mutate(therapy=Therapy, value=value*(1-Therapy))
  return(relevant_probs)
}

IndivRiskPull = function(Age_Range='20 to 29', Pctile='95%', gender='f', outcome='death', Therapy=0){
  return(mean(unlist(IndivRisk(Age_Range, Pctile, gender, outcome, Therapy)['value'])))
}

# To be done.
DoseResponse = function(Dose, Risk){ #Returns a value in (0,1) for the new risk. This can be customized.
  # Doses are likely concentrations of 10^4, 10^5, 10^6, and/or 10^7.
  # The Risk is the population level risk of impact. 
  
}

#Always assumes evenly weighted group sizes across groups.
StudyRisk <- function(Participants=1, Age_Range='20 to 29', Pctile='95%', gender='f', outcome='death', Therapy=0, average=TRUE){
  if (length(outcome)>1){
    warning("Too many outcomes here.")
  }
  Indiv_Risk = IndivRisk(Age_Range, Pctile, gender, outcome, Therapy)
  risks = unlist(Indiv_Risk['value'])
  groups = dim(Indiv_Risk)[1] #Only if it's only 1 outcome...
  if(average){ #Number of people per group is equal, despite fractional people.
    group_sizes = rep(Participants/groups,groups)
  } else { #Generate groups.
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
  
  prob_none = 1
  for(i in 1:groups){
    prob_none = prob_none*((1-risks[i])^(group_sizes[i]))
  }
  Risk_of_One_or_More = 1-prob_none
  return(Risk_of_One_or_More)
}

#Assumes that per-gender and per-age percentiles are perfectly correlated, i.e. we pick a single random number for the full study for each simulation across all ages and genders. We then pick the 95th percentile of these.

Simulate_StudyRisks = function(Simulations = 1000, Participants=1, Age_Range='20 to 29', gender='f',Therapy = 0, qtile=c(0.95)){
  P_per_sim = runif(Simulations, min=0, max=100) #Pick percentiles from the overall population risk estimate to simulate
  death_probs=c()
  hosp_probs=c()
  for(i in 1:Simulations){
    P=paste0(as.character(round(P_per_sim[i])),"%")
    death_probs[i]= StudyRisk(Participants=Participants, Age_Range=Age_Range, gender=gender, outcome='death', Therapy=Therapy, Pctile = P)
    hosp_probs[i] = StudyRisk(Participants=Participants, Age_Range=Age_Range, gender=gender, outcome='hosp', Therapy=Therapy, Pctile = P)
  }
  return(c(Death_qtile=quantile(death_probs, qtile), Hosp_qtile = quantile(hosp_probs, qtile)))
}



#SUPER inefficient! (This takes a long time to run. 20 seconds per million is too slow.)
Simulate_Studies =  function(Simulations = 1000, Participants=15, Age_Range='20 to 29', gender='f', Therapy=0, weights='Even'){
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
      d=IndivRiskPull(Age_Range=ageranges[p], Pctile=P, gender=genders[p], Therapy=Therapy, outcome='death')
      h=IndivRiskPull(Age_Range=ageranges[p], Pctile=P, gender=genders[p], Therapy=Therapy, outcome='hosp')
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
