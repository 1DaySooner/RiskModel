data {
  int <lower=0> NGroups; 
  real propCasesWithOutcomeDeath[NGroups];
  real propCasesWithOutcomeICU[NGroups];
  int <lower=0> hospByGroupF[NGroups];
  int <lower=0> ICUByGroupF[NGroups];
  int <lower=0> deathsByGroupF[NGroups];
  int <lower=0> popByGroupF[NGroups];
  int <lower=0> hospByGroupM[NGroups];
  int <lower=0> ICUByGroupM[NGroups];
  int <lower=0> deathsByGroupM[NGroups];
  int <lower=0> popByGroupM[NGroups];
  real <lower=0> popByGroup_activeF[NGroups];
  real <lower=0> popByGroup_activeM[NGroups];
  real relprobInfection[NGroups];
  int TotObservedPDDeaths;
}

parameters {
  real logit_probInfec; //
  real log_relprobICUBySex; //
  real log_relprobDeathBySex; //
  real logit_probDeathBase[NGroups]; //
  real logit_probICUBase[NGroups]; //
  real logit_probHospBaseM[NGroups]; //
  real logit_probHospBaseF[NGroups]; //
}

transformed parameters {
  real probInfec;
  real probHospByAgeM[NGroups];  
  real probICUByAgeM[NGroups]; 
  real probDeathByAgeM[NGroups]; 
  real probDeathByAgeGivenInfectionM[NGroups];
  real probHospByAgeF[NGroups];  
  real probICUByAgeF[NGroups]; 
  real probDeathByAgeF[NGroups];  
  real probDeathByAgeGivenInfectionF[NGroups];
  real probHospBaseM[NGroups];
  real probDeathBase[NGroups];
  real probICUBase[NGroups];
  real probHospBaseF[NGroups];
  real relprobICUBySex;
  real relprobDeathBySex;

  probInfec = inv_logit(logit_probInfec);
  for (k in 1:NGroups) { 
    probHospBaseF[k] = inv_logit(logit_probHospBaseF[k]);
    probDeathBase[k] = inv_logit(logit_probDeathBase[k]);
    probICUBase[k] = inv_logit(logit_probICUBase[k]);
    probHospBaseM[k] = inv_logit(logit_probHospBaseM[k]); 
  }

  relprobDeathBySex = exp(log_relprobDeathBySex);
  relprobICUBySex = exp(log_relprobICUBySex);
 
  for (k in 1:NGroups) { 
    probHospByAgeM[k] = probHospBaseM[k];
    probHospByAgeF[k] = probHospBaseF[k];

    probICUByAgeM[k] = probHospByAgeM[k]*probICUBase[k];
    probICUByAgeF[k] = probHospByAgeF[k]*probICUBase[k]*relprobICUBySex;

    probDeathByAgeM[k] = probHospByAgeM[k]*probDeathBase[k];
    probDeathByAgeF[k] = probHospByAgeF[k]*probDeathBase[k]*relprobDeathBySex;

    probDeathByAgeGivenInfectionF[k] = probDeathByAgeF[k]/(relprobInfection[k]*probInfec);

    probDeathByAgeGivenInfectionM[k] = probDeathByAgeM[k]/(relprobInfection[k]*probInfec);
  } 


}


model {

  int ObservedDeaths;
  real estimatedNdeaths;

  logit_probInfec ~ cauchy(0., 1);
  
  for (k in 1:NGroups) {
    logit_probHospBaseF[k] ~ cauchy(0., 1);
    logit_probDeathBase[k] ~ cauchy(0., 1);
    logit_probHospBaseM[k] ~ cauchy(0., 1);
    logit_probICUBase[k] ~ cauchy(0., 1);
  }

  log_relprobICUBySex ~ normal(0., 0.5);
  log_relprobDeathBySex ~ normal(0., 0.5);


  for (j in 1:NGroups) {  
    hospByGroupM[j] ~ poisson(probHospByAgeM[j]*popByGroupM[j]);
    deathsByGroupM[j] ~ poisson(probDeathByAgeM[j]*popByGroupM[j]*propCasesWithOutcomeDeath[j]);
    ICUByGroupM[j] ~ poisson(probICUByAgeM[j]*popByGroupM[j]*propCasesWithOutcomeICU[j]);
    hospByGroupF[j] ~ poisson(probHospByAgeF[j]*popByGroupF[j]);
    deathsByGroupF[j] ~ poisson(probDeathByAgeF[j]*popByGroupF[j]*propCasesWithOutcomeDeath[j]);
    ICUByGroupF[j] ~ poisson(probICUByAgeF[j]*popByGroupF[j]*propCasesWithOutcomeICU[j]);
  }

  estimatedNdeaths=0;
  for (j in 1:NGroups) { 
    estimatedNdeaths=estimatedNdeaths+(probDeathByAgeGivenInfectionM[j]*popByGroup_activeM[j]+probDeathByAgeGivenInfectionF[j]*popByGroup_activeF[j]);
  }

  TotObservedPDDeaths ~ poisson(estimatedNdeaths);

}
 


generated quantities {
  real probHospM[NGroups];
  real probHospF[NGroups];
  real probICUM[NGroups];
  real probICUF[NGroups];
  real probDeathM[NGroups];
  real probDeathF[NGroups];
  real probICUByAgeGivenInfectionM[NGroups];   
  real probICUByAgeGivenInfectionF[NGroups];   

  for (k in 1:NGroups) {
      probHospF[k] = probHospByAgeF[k]/(relprobInfection[k]*probInfec);
      probHospM[k] = probHospByAgeM[k]/(relprobInfection[k]*probInfec);

      probICUF[k] = probICUBase[k]*relprobICUBySex;
      probICUM[k] = probICUBase[k];

      probDeathF[k] = probDeathBase[k]*relprobDeathBySex;
      probDeathM[k] = probDeathBase[k];

      probICUByAgeGivenInfectionF[k] = probICUByAgeF[k]/(relprobInfection[k]*probInfec);

      probICUByAgeGivenInfectionM[k] = probICUByAgeM[k]/(relprobInfection[k]*probInfec);

    }
}


