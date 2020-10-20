library(tidyverse)
library(readxl)
library(rstan)
open_safely <- read_excel("Study Summary Data/OpenSafely_Counts.xls")
# View(open_safely)

# what are the risk ratios ?

stan_df <- open_safely %>% 
  filter(!is.na(Fatalities)) %>% 
  mutate(f = ifelse(Fatalities == "<=5", "5", Fatalities)) %>%
  mutate(f = as.numeric(f)) %>%
  mutate(p = f/Population) 

stan_df %>%
  ggplot(aes(x = Age_min, y = log(p), color = Gender)) + geom_point() + facet_wrap(~ `No Comorbidities`)

hist(rbinom(1000, prob = 0.00000408, size = 1225905))

stan_data <- list(
  N = nrow(stan_df),
  age = ifelse(stan_df$Age_min >= 20, (stan_df$Age_min - 20)/10, 0), #experimental
  male = 1*(stan_df$Gender == "M"),
  comorbidities = 1*(stan_df$`No Comorbidities` == 0),
  overweight = 1*(stan_df$`Healthy Weight` == 0),
  under20 = 1*(stan_df$Age_min < 20),
  Npop = stan_df$Population,
  Nevents = stan_df$f)

sm_como <- stan_model("rr_comorbidities.stan")
fit <- sampling(sm_como, data = stan_data, refresh = 0)

data.frame(
  stan_df,
  lci = summary(fit, "theta")$summary[,"2.5%"],
  estimate = summary(fit, "theta")$summary[,"mean"],
  uci = summary(fit, "theta")$summary[,"97.5%"]) %>%
  mutate(lci = log(lci), estimate = log(estimate), uci = log(uci)) %>%
  ggplot(aes(x = Age_min, y = log(p), color = Gender)) + 
  geom_point(pch = 21) +
  geom_point(aes(y = estimate), pch = 2) +
  geom_errorbar(aes(ymin = lci, ymax = uci)) +
  facet_grid(Healthy.Weight ~ No.Comorbidities)
