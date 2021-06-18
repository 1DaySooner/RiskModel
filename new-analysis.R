filename <- "ifr-model/data/1DS_Meta_Dataset_18jun2021.xlsx"
library(tidyverse)
library(readxl)
# remotes::install_github("wwiecek/wwmisc")
library(wwmisc) #few shorthand functions like logit() etc. 
source("helpers.R")

df_loaded <- read_excel(filename,
                        col_types = c(rep("guess", 3), rep("date", 3), rep("guess", 17))) %>%
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

df_loaded$age_min <- sapply(df_loaded$AgeGroup, \(x) age_interval(x)[1]) %>% unlist %>% as.numeric
df_loaded$age_max <- sapply(df_loaded$AgeGroup, \(x) age_interval(x)[2]) %>% unlist %>% as.numeric
df_nested <- nest(df_loaded)

# Mean size of sample in seroprevalence studies (for data without bounds)
mean_pop_size <- df_loaded %>% 
  filter(StudyType == "Seroprevalence", 
         AgeGroup != "ALL") %>% 
  pull(Population) %>% median(na.rm = T)

data <- df_nested$data[[50]]
# data <- select(data, StudyType, variable, value, AgeGroup, age_min, age_max)


df_loaded %>% 
  filter(StudyType == "Seroprevalence", AgeGroup != "ALL", variable == "Population") %>% 
  select(StudyName, StudyID, AgeGroup, variable, value)

construct_inputs <- function(id, dd) {
  if(nrow(dd) == 0)
    return(NULL)
  id$deaths     <- match_ages(dd$age_min, dd$age_max, id$age_min, id$age_max, dd$deaths)
  id$population <- match_ages(dd$age_min, dd$age_max, id$age_min, id$age_max, dd$population)
  id
}

# check_age_groups(dt)
tt <- df_nested %>%
  arrange(StudyName) %>%
  mutate(infection_data = map(data, prep_ir_data)) %>%
  mutate(death_data     = map(data, prep_de_data)) 

t2 <- tt %>%
  mutate(stan_inp = map2(infection_data, death_data, construct_inputs)) %>%
  mutate(unit = paste0(StudyName, Gender)) %>%
  ungroup() %>%
  select(unit, stan_inp) %>% unnest(stan_inp) %>%
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

stantest <- list_modify(
  as.list(t2),
  N = nrow(t2),
  Np = 1,
  X = array(t2$age_median, dim = c(nrow(t2), 1)),
  Nloc = length(unique(t2$unit))
)

library(rstan)
sm <- stan_model("ifr-model/ifr_with0.stan")
options(mc.cores = 1, digits = 2)

fit <- sampling(sm, data = stantest, 
                control = list(max_treedepth = 15), 
                iter = 5000, seed = 1990,
                pars = c("logit_ifr", "logit_prevalence"), include = FALSE)


mutate(t2, ratio = deaths/population) %>% 
  select(unit, age_min, age_max, deaths, population, ratio) %>% 
  filter(ratio > .05) %>% 
  write.csv("pop_too_low.csv")

construct_inputs(tt$infection_data[[99]], tt$death_data[[99]])

tt$death_data[[1]]

prep_ir_data(df_nested$data[[2]])
