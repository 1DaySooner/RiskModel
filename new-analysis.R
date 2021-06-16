filename <- "ifr-model/data/1DS_Meta_Dataset_16jun2021.xlsx"
library(tidyverse)
library(readxl)
# remotes::install_github("wwiecek/wwmisc")
library(wwmisc) #few shorthand functions like logit() etc. 
source("helpers.R")

df_loaded <- read_excel(filename,
                        col_types = c(rep("guess", 3), rep("date", 3), rep("guess", 17))) %>%
  mutate(StudyName = gsub(" Deaths", "", StudyName)) %>%
  pivot_longer(cols = c("Population", "Infections", "Deaths",
                        "IFR", "ifr_ci95_low", "ifr_ci95_high",
                        "InfectionRate", "infrate_ci95_low", "infrate_ci95_high"),
               values_to = "value",
               names_to = "variable") %>%
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
         AgeGroup != "ALL", 
         variable == "Population") %>% 
  pull(value) %>% median

data <- df_nested$data[[50]]
# data <- select(data, StudyType, variable, value, AgeGroup, age_min, age_max)

check_age_groups(data)


df_loaded %>% 
  filter(StudyType == "Seroprevalence", AgeGroup != "ALL", variable == "Population") %>% 
  select(StudyName, StudyID, AgeGroup, variable, value)

# check_age_groups(dt)
df_nested %>%
  mutate(status = map(data, prep_ir_data))
