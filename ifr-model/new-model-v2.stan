data {
  int N;
  int Np;
  int Nloc;
  int loc[N];
  matrix[N, Np] X;
  
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
  obs_deaths ~ binomial(population, prevalence .* true_ifr[1:N1]);
  mean_ifr ~ normal(true_ifr[(N1+1):N], se_ifr);
  theta_k ~ normal(tau, sigma);

  //Priors (WIP):
  tau   ~ normal(logit(.0001), 5);
  sigma ~ normal(0, 10);
  beta  ~ normal(0, 10);
}

generated quantities {
  int ppc_deaths[N1];
  for(i in 1:N1)
    ppc_deaths[i] = binomial_rng(population[i], prevalence[i]*true_ifr[i]);
}
