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
  vector[Nages] alpha;
  real beta;
  vector[Nages] gamma;
  real mu;
  real<lower=0> sigma;
}

transformed parameters {
  vector[N] theta;
  for(i in 1:N)
    theta[i] = inv_logit(alpha[age_group[i]]*comorbidities[i] + beta*male[i] + gamma[age_group[i]]);
}

model {
  //Likelihood:
  Nevents ~ binomial(Npop, theta);
  alpha ~ normal(mu, sigma);
  mu ~ normal(0, 10);
  sigma ~ normal(0, 2.5);
  beta ~ normal(0, 10);
  gamma ~ normal(0, 100);
}

generated quantities {
  vector[N] Nevent_ppc;
  for(i in 1:N)
    Nevent_ppc[i] = binomial_rng(Npop[i], theta[i]);
}
