filename <- "ifr-model/data/1DS_Meta_Dataset_25jun2021.xlsx"
library(tidyverse)
library(readxl)
# remotes::install_github("wwiecek/wwmisc")
library(wwmisc) #few shorthand functions like logit() etc. 
source("helpers.R")

df_loaded <- read_excel(filename,
                        col_types = c(rep("guess", 3), rep("date", 3), rep("guess", 18))) %>%
  mutate(StudyName = gsub(" Deaths", "", StudyName)) %>%
  # pivot_longer(cols = c("Population", "Infections", "Deaths",
  # "IFR", "ifr_ci95_low", "ifr_ci95_high",
  # "InfectionRate", "infrate_ci95_low", "infrate_ci95_high"),
  # values_to = "value",
  # names_to = "variable") %>%
  filter(Use == 1) %>% 
  # filter(!is.na(value)) %>% 
  group_by(StudyName, Gender)

df_loaded$AgeGroup %>% unique %>% sort

df_loaded$age_min <- sapply(df_loaded$AgeGroup, function(x) age_interval(x)[1]) %>% 
  unlist %>% as.numeric
df_loaded$age_max <- sapply(df_loaded$AgeGroup, function(x) age_interval(x)[2]) %>% 
  unlist %>% as.numeric
df_nested <- nest(df_loaded)

# Median size of sample in seroprevalence studies (for data without bounds)
medianps <- df_loaded %>% 
  filter(StudyType == "Seroprevalence", 
         AgeGroup != "ALL") %>% 
  pull(Population) %>% median(na.rm = T)

t1 <- df_nested %>%
  arrange(StudyName) %>%
  mutate(infection_data = map(data, prep_ir_data)) %>%
  mutate(death_data     = map(data, prep_de_data)) 

# Check potential number of data rows for Stan model:
t1 %>% mutate(id = map_dbl(infection_data, nrow),
              dd = map_dbl(death_data, nrow),
              m = map2_dbl(id, dd, function(x,y) {
                if(y == 1) return(1)
                if(y > 1) return(x)
                if(y==0) return(0)
              })) %>%
  pull(m) %>% sum()

t2 <- t1 %>%
  mutate(stan_inp = map2(infection_data, death_data, construct_inputs)) %>%
  mutate(unit = paste0(StudyName, Gender)) %>%
  ungroup() %>%
  select(unit, stan_inp) %>% 
  unnest(stan_inp) %>%
  mutate(ir = infected/pop_i) %>% 
  mutate(se = se_prop(ir, pop_i)) %>% View
  mutate(ir_high = ir + 1.96*se) %>%
  mutate(ir_low  = ir - 1.96*se) %>%
  select(-se, -population)

mutate(loc = as.numeric(factor(unit)),
       # !!!THIS NEEDS THINKING ABOUT!!!
       age_max = ifelse(is.infinite(age_max), 100, age_max),
       age_median = age_min + (age_max - age_min)/2 + .5,
       # FIX LOW VALUES:
       ir = ifelse(ir <= 0, .000005, ir),
       mean_prevalence = logit(ir),
       ir_high = ifelse(ir_high <= 0, .00001, ir_high),
       ir_low =  ifelse(ir_low <= 0, .000001, ir_low),
       sd_prevalence = (logit(ir_high) - logit(ir_low))/(2*1.96),
       obs_deaths = round(deaths),
       population = round(population))

summary(t2)  

View(t2)
stantest <- list_modify(
  as.list(t2),
  N = nrow(t2),
  Np = 1,
  X = array(t2$age_median, dim = c(nrow(t2), 1)),
  Nloc = length(unique(t2$unit))
)

library(rstan)
sm <- stan_model("ifr-model/ifr_with0.stan")
options(mc.cores = 4, digits = 2)

fit <- sampling(sm, data = stantest, 
                control = list(max_treedepth = 15), 
                iter = 500, seed = 1990,
                pars = c("logit_ifr", "logit_prevalence"), include = FALSE)


mutate(t2, ratio = deaths/population) %>% 
  select(unit, age_min, age_max, deaths, population, ratio) %>% 
  filter(ratio > .05) %>% 
  write.csv("pop_too_low.csv")

construct_inputs(tt$infection_data[[99]], tt$death_data[[99]])

tt$death_data[[1]]

prep_ir_data(df_nested$data[[2]])
