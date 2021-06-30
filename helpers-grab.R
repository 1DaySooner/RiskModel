# Functions used to grab IR, IFR and death data from our extraction datasheet

prep_ifr_data <- function(data) {
  data %>%
    filter(!is.na(IFR)) %>%
    mutate(ifr = IFR/100,
           ifr_lci = ifr_ci95_low/100,
           ifr_uci = ifr_ci95_high/100) %>%
    select(age_min, age_max, ifr, ifr_lci, ifr_uci)
}

prep_de_data <- function(data) {
  data %>%
    filter(!is.na(Population), !is.na(Deaths)) %>%
    select(age_min, age_max, Population, Deaths) %>%
    rename(population = Population,
           deaths = Deaths)
}

prep_ir_data <- function(data) {
  if(is.null(data$InfectionRate))
    browser()
  
  data %>% 
    mutate(population = ifelse(is.na(Population), 
                               ifelse(is.na(InfectionRate) | is.na(Infections),
                                      NA,
                                      Infections/(InfectionRate/100)),
                               Population)) %>%
    mutate(infected = ifelse(is.na(Infections), 
                             (InfectionRate/100)*Population, 
                             Infections)) %>%
    mutate(ir = InfectionRate/100,
           ir_lci = infrate_ci95_low/100,
           ir_uci = infrate_ci95_high/100) %>%
    select(age_min, age_max, 
           ir, ir_lci, ir_uci,
           population, infected) %>%
    filter(!(is.na(infected) & is.na(ir)))
}