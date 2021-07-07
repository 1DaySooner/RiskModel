data {
  int N;
  int Np;
  int Nloc;
  int<lower=0> obs_deaths[N];
  int loc[N];
  vector[N] mean_prevalence;
  vector[N] sd_prevalence;
  int<lower=0> population[N];
  matrix[N, Np] X;
}

parameters {
  vector<lower=0, upper=1>[N] prevalence;
  real tau;
  real<lower=0> sigma;
  vector[Nloc] theta_k;
  vector[Np] beta;
}

transformed parameters {
  vector[N] logit_ifr;
  vector[N] true_ifr;
  if(Np > 0)
    logit_ifr = theta_k[loc] + to_vector(X*beta);
  else
    logit_ifr = theta_k[loc];
  true_ifr = inv_logit(logit_ifr);
}

model {
  //Likelihood:
  prevalence ~ normal(mean_prevalence, sd_prevalence);
  obs_deaths ~ binomial(population, prevalence .* true_ifr);
  theta_k ~ normal(tau, sigma);

  //Priors (WIP):
  tau   ~ normal(logit(.0001), 5);
  sigma ~ normal(0, 10);
  beta  ~ normal(0, 10);
}

generated quantities {
  int ppc_deaths[N];
  for(i in 1:N)
    ppc_deaths[i] = binomial_rng(population[i], prevalence[i]*true_ifr[i]);
}
