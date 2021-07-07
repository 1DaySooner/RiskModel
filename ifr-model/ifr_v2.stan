data {
  int N1;
  int N2;
  int Nloc;
  int loc1[N1];
  int<lower=0> obs_deaths[N1];
  vector[N1] mean_prevalence;
  vector[N1] sd_prevalence;
  int<lower=0> population1[N1];
  int loc2[N2];
  vector[N2] mean_log_ifr;
  vector[N2] sd_log_ifr;
  int<lower=0> population2[N2];
  vector[N1] age1;
  vector[N2] age2;
}

parameters {
  vector[N1] logit_prevalence;
  real tau;
  real tau_b;
  real<lower=0> sigma;
  real<lower=0> sigma_b;
  vector[Nloc] theta_k;
  vector[Nloc] beta_k;
}

transformed parameters {
  vector[N1] logit_ifr;
  vector[N1] model_ifr;
  vector[N1] prevalence;
  logit_ifr = theta_k[loc1] + age1 .* beta_k[loc1];
  model_ifr = inv_logit(logit_ifr);
  prevalence = inv_logit(logit_prevalence);
}

model {
  //Likelihood -- for data with deaths and prevalences
  logit_prevalence ~ normal(mean_prevalence, sd_prevalence);
  obs_deaths ~ binomial(population1, prevalence .* model_ifr);
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
