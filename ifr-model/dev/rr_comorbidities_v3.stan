data {
  int N;
  vector[N] age;
  vector[N] male;
  vector[N] comorbidities;
  int Npop[N];
  int Nevents[N];
}

parameters {
  real alpha;
  real beta;
  real delta;
  real bsl;
}

transformed parameters {
  vector[N] theta;
  theta = inv_logit(bsl + alpha*comorbidities + beta*male + delta*age);
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
