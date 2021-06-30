filename <- "ifr-model/data/1DS_Meta_Dataset_29jun2021.xlsx"
library(tidyverse)
library(readxl)
# remotes::install_github("wwiecek/wwmisc")
library(wwmisc) #few shorthand functions like logit() etc. 
source("helpers.R")
source("helpers-grab.R")

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
  mutate(death_data     = map(data, prep_de_data)) %>%
  mutate(ifr_data       = map(data, prep_ifr_data)) 

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
  mutate(new_ir = infected/pop_i) %>% 
  mutate(se = se_prop(new_ir, pop_i)) %>% 
  mutate(new_ir_lci  = new_ir - 1.96*se) %>%
  mutate(new_ir_uci = new_ir + 1.96*se) %>%
  select(-se, -pop_i)

t3 <- t1 %>%
  mutate(unit = paste0(StudyName, Gender)) %>%
  ungroup() %>%
  select(-data, -infection_data, -death_data, -StudyName, -Gender) %>%
  unnest(ifr_data)

t4 <- full_join(t2, t3, by = c("unit", "age_min", "age_max")) %>% 
  group_by(unit) %>% 
  # Studies with 1 IFR row in addition to IRs,
  mutate(ifr_remove = (sum(is.na(ifr)) == n() - 1) & (n() > 1)) %>% 
  # in the cases so far this IFR row should be removed
  ungroup() %>%
  filter(!((ifr_remove == 1) & (!is.na(ifr)))) %>%
  select(-ifr_remove) %>%
  mutate(# Construct age median covariate (THIS WILL BE FIXED)
         age_max = ifelse(is.infinite(age_max), 100, age_max),
         age_median = age_min + (age_max - age_min)/2 + .5
  )

# Grab infection rates inputs:
t5 <- t4 %>% 
  group_by(unit) %>%
  mutate(ir1_a = (!is.na(ir)) & (!is.na(ir_lci)) & (!is.na(ir_uci))) %>%
  mutate(ir2_a = (!is.na(new_ir)) & (!is.na(new_ir_lci)) & (!is.na(new_ir_uci))) %>%
  ungroup() %>%
  filter(ir1_a | ir2_a) %>%
  mutate(ir = ifelse(ir1_a, ir, new_ir),
         ir_lci = ifelse(ir1_a, ir_lci, new_ir_lci),
         ir_uci = ifelse(ir1_a, ir_uci, new_ir_uci)) %>%
  select(unit, age_median, ir, ir_lci, ir_uci, pop_d, deaths)

t6 <- t4 %>%
  mutate(ir_exists = sapply(unit, function(x) x %in% t5$unit)) %>%
  filter(!ir_exists & (!is.na(ifr)) & (!is.na(ifr_lci)) & (!is.na(ifr_uci))) %>%
  select(unit, age_median, ifr, ifr_lci, ifr_uci) 

# Comparison of intervals: source data vs calulated from proportions
# filter(t2, !is.na(ir), !is.na(ir_lci)) %>% 
t2 %>%
  filter(!(is.na(ir) & is.na(new_ir))) %>%
  # pull(ir) %>% summary()
  mutate(gr = cut(ir, c(-Inf, 8e-03, 3e-02, 6e-02, 2e-01, Inf))) %>%
  # pull(gr) %>% table()
  mutate(ylab = paste0(substr(unit, 1, 12), age_min, "-", age_max)) %>%
  ggplot() + 
  geom_point(aes(x=ir, y=ylab)) + 
  geom_errorbarh(aes(x=ir, y=ylab, xmax = ir_uci, xmin=ir_lci)) +
  geom_point(aes(x=new_ir, y=ylab), color = "red", pch = 21) + 
  geom_errorbarh(aes(x=new_ir, y=ylab, xmax = new_ir_uci, xmin=new_ir_lci), color = "red") +
  xlab("Infection rate (95% interval)") + ylab("") +
  facet_wrap(~ gr, ncol = 3, scales = "free") +
  scale_x_log10() +
  theme_gray(base_size = 9)

t7 <- t5 %>% 
  mutate(
    # !!!THIS NEEDS THINKING ABOUT!!!
    # FIX LOW VALUES:
    ir = ifelse(ir <= 0, .000005, ir),
    ir_uci = ifelse(ir_uci <= 0, .00001, ir_uci),
    ir_lci =  ifelse(ir_lci <= 0, .000001, ir_lci)) %>%
  transmute(
    mean_prevalence = logit(ir),
    sd_prevalence = (logit(ir_uci) - logit(ir_lci))/(2*1.96),
    obs_deaths = round(deaths),
    population = round(pop_d))

t8 <- t6 %>% 
  mutate(
    # !!!THIS NEEDS THINKING ABOUT!!!
    # FIX LOW VALUES:
    ifr = ifelse(ifr <= 0, .0000005, ifr),
    ifr_uci = ifelse(ifr_uci <= 0, .000001, ifr_uci+1e-05),
    ifr_lci =  ifelse(ifr_lci <= 1e-05, .0000001, ifr_lci-1e-05)) %>%
  transmute(
    mean_ifr = logit(ifr),
    se_ifr = (logit(ifr_uci) - logit(ifr_lci))/(2*1.96)
  )

stan_data <- list_modify(
  append(
    as.list(t7),
    as.list(t8)),
  N = nrow(t7) + nrow(t8),
  N1 = nrow(t7),
  N2 = nrow(t8),
  Np = 1,
  X = array(c(
    t5$age_median, 
    t6$age_median),
    dim = c(nrow(t5)+nrow(t6), 1)),
  loc = as.numeric(factor(c(t5$unit, t6$unit))),
  Nloc = length(unique(c(t5$unit, t6$unit)))
)




# Generate and check input data for Stan -----

data.frame(t7, age = t5$age_median) %>% 
  ggplot(aes(x = age, y=mean_prevalence, 
             ymin = mean_prevalence - 2*sd_prevalence, 
             ymax = mean_prevalence + 2*sd_prevalence)) + 
  geom_point() + geom_errorbar()

data.frame(t7, age = t5$age_median) %>% 
  ggplot(aes(x = age, y = obs_deaths/population)) + geom_point() +
  scale_y_log10()

stan_data <- list_modify(
  as.list(t7),
  mean_ifr = array(NA, c(0)),
  se_ifr = array(NA, c(0)),
  N = nrow(t7),
  N1 = nrow(t7),
  N2 = 0,
  Np = 1,
  X = array(c(
    t5$age_median),
    dim = c(nrow(t5), 1)),
  loc = as.numeric(factor(c(t5$unit))),
  Nloc = length(unique(c(t5$unit)))
)

stan_data_old <- list_modify(
  as.list(t7),
  N = nrow(t7),
  Np = 1,
  X = array(c(
    t5$age_median),
    dim = c(nrow(t5), 1)),
  loc = as.numeric(factor(c(t5$unit))),
  Nloc = length(unique(c(t5$unit)))
)


library(rstan)
rstan_options(auto_write = TRUE)
sm <- stan_model("ifr-model/new-model-v2.stan")
sm_old <- stan_model("ifr-model/ifr_with0.stan")
options(mc.cores = 4, digits = 3)

fit <- sampling(sm, data = stan_data, 
                control = list(max_treedepth = 20), 
                iter = 500, 
                pars = c("logit_ifr", "prevalence"), include = FALSE)
fit <- sampling(sm_old, data = stan_data_old, 
                control = list(max_treedepth = 15), 
                iter = 500, 
                pars = c("logit_ifr", "prevalence"), include = FALSE)


mutate(t2, ratio = deaths/population) %>% 
  select(unit, age_min, age_max, deaths, population, ratio) %>% 
  filter(ratio > .05) %>% 
  write.csv("pop_too_low.csv")

construct_inputs(tt$infection_data[[99]], tt$death_data[[99]])

tt$death_data[[1]]

prep_ir_data(df_nested$data[[2]])
