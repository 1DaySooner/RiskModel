data {
  int N;
  int Np;
  int Nloc;
  int loc[N];
  vector[N] x;
  
  int N1;
  int<lower=0> obs_deaths[N1];
  vector[N1] mean_prevalence;
  vector[N1] sd_prevalence;
  int<lower=0> population[N1];
  
  int N2;
  vector[N2] mean_ifr;
  vector[N2] se_ifr;
}

parameters {
  vector<lower=0, upper=1>[N1] prevalence;
  real tau;
  real tau_b;
  real<lower=0> sigma;
  real<lower=0> sigma_b;
  vector[Nloc] theta_k;
  vector[Nloc] beta_k;
}

transformed parameters {
  vector[N] logit_ifr;
  vector[N] true_ifr;
  logit_ifr = theta_k[loc] + x .* beta_k[loc];
  true_ifr = inv_logit(logit_ifr);
}

model {
  //Likelihood:
  prevalence ~ normal(mean_prevalence, sd_prevalence);
  obs_deaths ~ binomial(population, prevalence .* true_ifr[1:N1]);
  mean_ifr ~ normal(true_ifr[(N1+1):N], se_ifr);
  theta_k ~ normal(tau, sigma);

  tau   ~ normal(logit(.0001), 5);
  sigma ~ normal(0, 10);
  tau_b ~ normal(0, 10);
  sigma_b ~ normal(0, 10);
}

generated quantities {
  int ppc_deaths[N1];
  for(i in 1:N1)
    ppc_deaths[i] = binomial_rng(population[i], prevalence[i]*true_ifr[i]);
}
