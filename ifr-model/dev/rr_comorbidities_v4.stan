data {
  int N;
  int age_group[N];
  int Nages;
  vector[N] male;
  vector[N] comorbidities;
  int Npop[N];
  int Nevents[N];
}
transformed data {
}
parameters {
  real alpha;
  real beta;
  vector[Nages] delta;
  real bsl;
}

transformed parameters {
  vector[N] theta;
  for(i in 1:N)
    theta[i] = inv_logit(bsl + alpha*comorbidities[i] + beta*male[i] + delta[age_group[i]]);
}

model {
  //Likelihood:
  Nevents ~ binomial(Npop, theta);
  alpha ~ normal(0, 10);
  beta ~ normal(0, 10);
  delta ~ normal(0, 10);
  bsl ~ normal(0, 10);
}

generated quantities {
  vector[N] Nevent_ppc;
  for(i in 1:N)
    Nevent_ppc[i] = binomial_rng(Npop[i], theta[i]);
}
