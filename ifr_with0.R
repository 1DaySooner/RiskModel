library(rstan)
library(readxl)
library(tidyverse)
rstan_options(auto_write = TRUE)
sm <- stan_model("ifr_with0.stan")

data <- list(
  N = 4, Np = 0,
  X = array(0, dim = c(4,0)),
  obs_deaths = c(10,5,20,1),
  mean_prevalence = c(.1, .1, .1, .2),
  sd_prevalence   = c(.01, .01, .01, .02),
  population = rep(1e03, 4)
)


sampling(sm, data = data)
# https://discourse.mc-stan.org/t/problem-running-rstan-with-r-4-0-2/17257/2


# Real data -----

# Several different datasets.
NBER_IFR_Benchmark_Studies <- read_excel('Study Summary Data/NBER_IFR_Meta_Dataset.xlsx',1)
#Places: Belgium. Geneva, Indiana, New York, Spain, Sweden
NBER_IFR_US_Studies <- read_excel('Study Summary Data/NBER_IFR_Meta_Dataset.xlsx',2, skip=1)
names(NBER_IFR_US_Studies)[6:7]<-c('Infect 95_lower','Infect 95_upper')
names(NBER_IFR_US_Studies)[10:11]<-c('IFR_95_lower','IFR_95_upper')

# Intervals are symmetric-ish -----

logit <- function(x) log(x/(1-x))
inv_logit <- function(x) exp(x)/(1+exp(x))

rbind(ifr_global, ifr_us) %>%
  mutate(log_sd = ifelse(ir_low != 0, 
                         (logit(ir_high/100) - logit(ir_low/100))/(2*1.96), 
                         (logit(ir_high/100) - logit(ir/100))/1.96)) %>%
  mutate(log_mean = logit(ir/100)) %>%
  mutate(midpoint_u = log_mean + log_sd*1.96, midpoint_l = log_mean - log_sd*1.96) %>%
  ggplot(aes(x=ir/100, xmax = ir_high/100, xmin=ir_low/100, y=interaction(Study, AgeGroup))) + 
  geom_point() + geom_errorbarh() +
  geom_point(aes(x = inv_logit(midpoint_l)), pch = 21) +
  geom_point(aes(x = inv_logit(midpoint_u)), pch = 21)



df <- rbind(ifr_global, ifr_us) %>%
  mutate(logit_sd = ifelse(ir_low != 0, 
                         (logit(ir_high/100) - logit(ir_low/100))/(2*1.96), 
                         (logit(ir_high/100) - logit(ir/100))/1.96)) %>%
  mutate(logit_mean = logit(ir/100))

mm <- model.matrix(ir ~ Study + Median_Age, data = df)
stan_data <- list(
  X = matrix(df$Median_Age/10 - 2.5, nrow(df), 1),
  N = nrow(df), 
  Np = 1,
  Nloc = length(unique(df$Study)),
  loc = as.numeric(as.factor(df$Study)),
  mean_prevalence = df$logit_mean,
  sd_prevalence = df$logit_sd,
  population = df$Population,
  obs_deaths = df$Deaths)
options(mc.cores = 4)
fit <- sampling(sm, data = stan_data, control = list(max_treedepth = 15))


data.frame(df, 
           broom::tidyMCMC(fit, "new_deaths", conf.int = TRUE)) %>%
  filter(Deaths < 10) %>%
  plot(Deaths ~ estimate, data = .)

print(fit, c("tau", "sigma", "beta"))
