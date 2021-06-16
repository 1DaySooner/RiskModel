#' Adjust distribution of counts over age intervals to match intervals in
#' another distribtuion
#'
#' @param age1l lower interval for the dist to adjust (vector)
#' @param age1u upper interval
#' @param age2l lower end of the target interval
#' @param age2u upper end of the target interval
#' @param count number of events in each interval
#'
#' @return vector of length `age2l`
#' @export
#'
#' @examples
#' match_ages(
#'   c(0, 10, 20, 30),
#'   c(9, 19, 29, 59),
#'   c(0, 30),
#'   c(29, 59),
#'   c(100,100,100,300)
#' )
match_ages <- function(age1l, age1u, age2l, age2u, n) {
  
  
  age1_all <- which((age1l == 0) & (age1u == Inf))
  age2_all <- which((age2l == 0) & (age2u == Inf))
  if(length(age1_all) > 1)
    stop("Multiple entries for all age groups")
  if(length(age2_all) > 1)
    stop("Multiple entries for all age groups")
  
  
  if(length(age1l) > 1 && length(age1_all) == 1){
    age1l <- age1l[-age1_all]
    age1u <- age1u[-age1_all]
    n <- n[-age1_all]
  }
  
  if(!monotonic(age1l) || !monotonic(age2l) || 
     !monotonic(age1u) || !monotonic(age2u)){
    browser()
    stop("Non-monotonic age vectors")
  }
  
  N1 <- length(age1u)
  N2 <- length(age2u)
  
  
  
  if(is.infinite(age1u[N1]))
    age1u[N1] <- 100
  if(is.infinite(age2u[N2]))
    age2u[N2] <- 100
  age1l[age1l==0] <- 1
  age2l[age2l==0] <- 1
  
  nast <- rep(NA, max(age1u))
  n2 <- 0*age2l
  
  if(length(age2u) > 1){
    for(i in 2:(length(age2u))) 
      if(age2u[i-1] != (age2l[i] - 1))
        stop("Target intervals do not touch: ", age2u[i-1],", ", age2l[i])
    for(i in 2:(length(age1u))) 
      if(age1u[i-1] != (age1l[i] - 1))
        stop("Input intervals do not touch: ", age1u[i-1], ", ", age1l[i])
    
  }
  
  
  for(i in seq_along(n)){
    nast[age1l[i]:age1u[i]] <- n[i]/(age1u[i] - age1l[i] + 1)
  }
  
  
  for(i in seq_along(age2l)){
    n2[i] <- sum(nast[age2l[i]:age2u[i]], na.rm = TRUE)
  }
  
  
  n2
}

monotonic <- function(x) {
  if(length(x) == 1)
    return(TRUE)
  for(i in 2:length(x))
    if(x[i] <= x[i-1])
      return(FALSE)
  return(TRUE)
  
}

age_interval <- function(x) {
  if(x == "ALL")
    return(c(0, Inf))
  if(grepl("<", x))
    return(c(0, as.numeric(gsub("<", "", x))))
  if(grepl("-", x))
    return(as.numeric(strsplit(x, "-")[[1]]))
  if(grepl("\\+", x))
    return(c(as.numeric(gsub("\\+", "", x)), Inf))
  return(c(as.numeric(gsub("\\+", "", x)), Inf))
}

se_prop <- function(p, n) sqrt(p*(1-p)/n)


prep_ir_data <- function(data, mps = mean_pop_size) {
  dt_wide <- data %>% 
    # spread(variable, value) %>% 
    filter(StudyType == "Seroprevalence") %>%
    mutate(InfectionRate = ifelse(is.na(InfectionRate), Infections/Population, InfectionRate)) %>%
    mutate(population = Population,
           ir = InfectionRate/100, 
           ir_low = infrate_ci95_low/100, 
           ir_high = infrate_ci95_high/100) %>%
    select(age_min, age_max, 
           population,
           ir, ir_low, ir_high)
  browser()
  if(any(is.na(dt_wide$InfectionRate)))
    stop("Can't calculate IR based on these data")
  
  dt_wide %>%
    mutate(population = ifelse(is.na(population), mps, population)) %>%
    mutate(se = se_prop(ir/100, population)) %>%
    mutate(ir_high = ifelse(is.na(ir_high), ir + 1.96*se, ir_high)) %>% 
    mutate(ir_low  = ifelse(is.na(ir_low),  ir - 1.96*se, ir_low)) %>%
    select(-se, -population)
  
}


check_age_groups <- function(data) {
  p_dt <- filter(data, variable == "Population") %>% arrange(age_min)
  d_dt <- filter(data, variable == "Deaths")
  i_dt <- filter(data, variable != "Deaths" & variable != "Population") %>%
    prep_ir_data()
  
  p_gr <- sort(unique(p_rows$AgeGroup))
  d_gr <- sort(unique(d_rows$AgeGroup))
  i_gr <- sort(unique(i_rows$AgeGroup))
  
  if(identical(p_gr, d_gr))
    return(1)
  v <- round(match_ages(p_dt$age_min, p_dt$age_max, d_dt$age_min, d_dt$age_max, p_dt$value))
  v
}


