#' Adjust distribution of counts over age intervals to match intervals in
#' another distribtuion
#'
#' @param age1l lower interval for the dist to adjust (vector)
#' @param age1u upper interval
#' @param age2l lower end of the target interval
#' @param age2u upper end of the target interval
#' @param count number of events in each interval (from `age1l[i]` to `age1u[i]`)
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
  
  if(identical(age1l, age2l) && identical(age2l, age2u))
    return(n)
  
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
  
  # if(length(age2l) > 1 && length(age2_all) == 1){
  #   age2l <- age2l[-age2_all]
  #   age2u <- age2u[-age2_all]
  # }
  
  if(!is.monotonic(age1l) || !is.monotonic(age1u)){
    stop("Non-monotonic age vectors")
  }
  
  N1 <- length(age1u)
  N2 <- length(age2u)
  
  age1u[is.infinite(age1u)] <- 100
  age2u[is.infinite(age2u)] <- 100
  age1l[age1l==0] <- 1
  age2l[age2l==0] <- 1
  
  nast <- rep(NA, max(age1u))
  n2 <- 0*age2l
  
  # if(length(age2u) > 1)
  #   for(i in 2:(length(age2u))) 
  #     if(age2u[i-1] != (age2l[i] - 1))
  #       stop("Target intervals do not touch: ", age2u[i-1],", ", age2l[i])
  
  if(length(age1u) > 1)
    for(i in 2:(length(age1u))) 
      if(age1u[i-1] != (age1l[i] - 1))
        stop("Input intervals do not touch: ", age1u[i-1], ", ", age1l[i])
  
  
  for(i in seq_along(n)){
    nast[age1l[i]:age1u[i]] <- n[i]/(age1u[i] - age1l[i] + 1)
  }
  
  for(i in seq_along(age2l)){
    n2[i] <- sum(nast[age2l[i]:age2u[i]], na.rm = TRUE)
  }
  
  
  n2
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

find_all_row <- function(df){
  wh <- which(df$age_min == 0 & df$age_max == Inf)
  if(length(wh) > 1){
    browser()
    stop("Multiple rows with ALL ages???")
  }
  if(length(wh) == 0)
    wh <- 0
  
  wh
}

remove_all_row <- function(df) {
  wh <- find_all_row(df)
  if(nrow(df) == 1) 
    return(df)
  if(wh == 0)
    return(df)
  else 
    return(df[-wh,])
}

construct_inputs <- function(id, dd) {
  if(nrow(dd) == 0)
    return(NULL)
  if(nrow(id) == 0)
    return(NULL)
  if(nrow(dd) == 1){
    wh <- find_all_row(id)
    if(wh > 0){
      # cat("A")
      return(
        full_join(rename(id[wh,], pop_i = population), dd,
                  by = c("age_min", "age_max")) %>%
        rename(pop_d = population))
    } else {
      id <- remove_all_row(id)
      return(mutate(dd, 
                    infected = sum(id$infected),
                    pop_i = sum(id$population)) %>%
               rename(pop_d = population))
    }
  } else {
    id$deaths <- match_ages(dd$age_min, dd$age_max, 
                            id$age_min, id$age_max, dd$deaths)
    id$pop_d  <- match_ages(dd$age_min, dd$age_max, 
                            id$age_min, id$age_max, dd$population)
    return(rename(id, pop_i = population))
  }
}

