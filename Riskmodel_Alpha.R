
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

Male_probDeath <- read.csv("France_Male_p_death_by_age_range.csv", row.names=1, check.names=FALSE)
Female_probDeath <- read.csv("France_Female_p_death_by_age_range.csv", row.names=1, check.names=FALSE)
Male_probHosp <- read.csv("France_Male_p_hospitalization_by_age_range.csv", row.names=1, check.names=FALSE)
Female_probHosp <- read.csv("France_Female_p_hospitalization_by_age_range.csv", row.names=1, check.names=FALSE)
Male_probICU <- read.csv("France_Male_p_death_by_age_range.csv", row.names=1, check.names=FALSE)
Female_probICU <- read.csv("France_Female_p_death_by_age_range.csv", row.names=1, check.names=FALSE)

# Not currently used:

# China_probICU <- read.csv("China_p_death_by_age_range.csv", row.names=1, check.names=FALSE)
# China_probDeath <- read.csv("China_p_severe_by_age_range.csv", row.names=1, check.names=FALSE)


# Note: we are estimating risks from intentional infections. Our 95% assurance level is 1-directional, i.e. we are 95% certain the risk is under this level.

IndivRiskPull = function(Age_Range='20 to 29', Pctile='95%', gender='f', outcome='death'){
  if (gender=='f'){
    if(outcome=='death'){
      individual_risk = Female_probDeath[Age_Range,Pctile]
    } else{
      if(outcome=='hosp'){
        individual_risk = Female_probHosp[Age_Range,Pctile]
      }
    }} else{
      if (gender=='m'){
        if(outcome=='death'){
          individual_risk = Male_probDeath[Age_Range,Pctile]
        } else{
          if(outcome=='hosp'){
            individual_risk = Male_probHosp[Age_Range,Pctile]
          }
        }}
    }
  return(individual_risk)
  }

#Always assumes evenly weighted group sizes across groups.
StudyRisk = function(Participants=1, Age_Range='20 to 29', Pctile='95%', gender='f', outcome='death'){
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
  groups = c()
  for(age in subgroups_a){
    for(gend in subgroups_g){
      groups[length(groups)+1] = IndivRiskPull(Age_Range=age, Pctile=Pctile, gender=gend, outcome=outcome)
    }
    }

  prob_none = 1
  for(riskamt in groups){
    prob_none = ((1-riskamt)^(Participants/length(groups)))
  }
  Risk_of_One_or_More = 1-prob_none
  return(Risk_of_One_or_More)
  }

#Assumes that per-gender and per-age percentiles are perfectly correlated, i.e. we pick a single random number for the full study for each simulation across all ages and genders. We then pick the 95th percentile of these.

Simulate_StudyRisks = function(Simulations = 1000, Participants=1, Age_Range='20 to 29', gender='f',qtile=c(0.95)){
  P_per_sim = runif(Simulations, min=0, max=100) #Pick percentiles from the overall population risk estimate to simulate
  death_probs=c()
  hosp_probs=c()
  for(i in 1:Simulations){
    P=paste0(as.character(round(P_per_sim[i])),"%")
    death_probs[i]= StudyRisk(Participants=Participants, Age_Range=Age_Range, gender=gender, outcome='death', Pctile = P)
    hosp_probs[i] = StudyRisk(Participants=Participants, Age_Range=Age_Range, gender=gender, outcome='hosp', Pctile = P)
  }
  return(c(Death_qtile=quantile(death_probs, qtile), Hosp_qtile = quantile(hosp_probs, qtile)))
}



#SUPER inefficient! (This takes a long time to run. 20 seconds per million is too slow.)
Simulate_Studies =  function(Simulations = 1000, Participants=15, Age_Range='20 to 29', gender='f', weights='Even'){
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
      d=IndivRiskPull(Age_Range=ageranges[p], Pctile=P, gender=genders[p], outcome='death')
      h=IndivRiskPull(Age_Range=ageranges[p], Pctile=P, gender=genders[p], outcome='hosp')
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