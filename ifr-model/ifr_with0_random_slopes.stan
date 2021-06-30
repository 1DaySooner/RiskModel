data {
  int N;
  int Nloc;
  int<lower=0> obs_deaths[N];
  int loc[N];
  vector[N] mean_prevalence;
  vector[N] sd_prevalence;
  int<lower=0> population[N];
  vector[N] x;
}

parameters {
  vector[N] logit_prevalence;
  real tau;
  real tau_b;
  real<lower=0> sigma;
  real<lower=0> sigma_b;
  vector[Nloc] theta_k;
  vector[Nloc] beta_k;
}

transformed parameters {
  vector[N] logit_ifr;
  vector[N] ifr;
  vector[N] prevalence;
  logit_ifr = theta_k[loc] + x .* beta_k[loc];
  ifr = inv_logit(logit_ifr);
  prevalence = inv_logit(logit_prevalence);
}

model {
  //Likelihood:
  logit_prevalence ~ normal(mean_prevalence, sd_prevalence);
  obs_deaths ~ binomial(population, prevalence .* ifr);
  theta_k ~ normal(tau, sigma);
  beta_k ~ normal(tau_b, sigma_b);

  //Priors (WIP):
  tau   ~ normal(logit(.0001), 5);
  sigma ~ normal(0, 10);
  tau_b ~ normal(0, 10);
  sigma_b ~ normal(0, 10);
}

generated quantities {
  int ppc_deaths[N];
  for(i in 1:N)
    ppc_deaths[i] = binomial_rng(population[i], prevalence[i]*ifr[i]);
}
