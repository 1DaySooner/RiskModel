data {
  int N;
  vector[N] age;
  vector[N] male;
  vector[N] comorbidities;
  vector[N] overweight;
  vector[N] under20;
  int Npop[N];
  int Nevents[N];

}

parameters {
  real alpha;
  real beta;
  real gamma;
  real delta;
  real epsilon;
  real bsl;
}

transformed parameters {
  vector[N] theta;
  theta = inv_logit(bsl + alpha*comorbidities + beta*male + gamma*overweight + 
                    delta*age + epsilon*under20);
}

model {
  //Likelihood:
  Nevents ~ binomial(Npop, theta);
  alpha ~ normal(0, 10);
  beta ~ normal(0, 10);
  gamma ~ normal(0, 10);
  delta ~ normal(0, 10);
  epsilon ~ normal(0, 10);
  bsl ~ normal(0, 10);
}

generated quantities {

}
