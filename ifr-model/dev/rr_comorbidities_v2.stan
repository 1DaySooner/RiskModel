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
  vector[N] theta;
}

model {
  theta ~ normal(0, 100);
  //Likelihood:
  Nevents ~ binomial(Npop, inv_logit(theta));
}

generated quantities {

}
