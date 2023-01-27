
#'read UIS data
#'
#'\code{read_uis_data} reads a supplied csv data submission  to a dataframe.
#'The function checks that the csv is in the required and that the minimum
#'necessary columns needed to validate the data are present:
#' namely CO_CODE, IND_ID, IND_YEAR, FIG, QAL and MAGN,
#'
#'@param path path to file
#'
#'@import dplyr
#'@export


read_uis_data <- function(path) {

  data <- read.csv(path)

  #check if any variables missing from dataframe
  required_vars <- c("CO_CODE", "IND_ID", "IND_YEAR", "FIG", "QUAL", "MAGN")
  missing_vars <- find_missing_vars(data, required_vars)

  if(!is.null(missing_vars)) {
    stop(sprintf("The following required variables are missing:\n %s"), missing_vars)
  }

  #check if any duplicate observations

  dups <- find_dups(data, c("CO_CODE", "IND_ID", "IND_YEAR"))

  if(!is.null(dups)) {
    stop(sprintf("There are suplicate observations for %s indicators"), nrow(dups))
  }
  return(data)
}

#'parse validation rules
#'
#'\code{parse_rules} reads a yaml file of rules, checks that it is formatted correctly
#'and returns an object of class validate::validator if checks pass.
#'
#'@param path path to file
#'@export
#'

parse_rules <- function(path) {

  #read file to list for checks
  rl <- yaml::read_yaml(path) |>
    purrr::pluck(1)

  #perform checks
  for(i in seq_along(rl)) {
    missing_fields <- setdiff(c("expr", "name", "label", "description"), names(rl[[i]]))
    if(length(missing_fields)> 0) {
      stop("rule  %s is missing the following fields:\n %s", i, missing_fields)
    }
    for(field in rl[[i]]) {
      if(is.null(field)) {
        stop("A field in rule %s has no entry", i)
      }
    }
  }
  rules <- validate::validator(.file = path)
}
