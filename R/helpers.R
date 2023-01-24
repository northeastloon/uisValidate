#'check variables are present
#'
#'\code{find_missing_vars} checks that the specified variables are present in a dataframe
#'
#'@param df a data frame
#'@param vars character vector of variable names

find_missing_vars <- function(df, vars) {
  required_vars <- vars
  missing_vars <- setdiff(required_vars, names(df))

  if(length(missing_vars) == 0) {
    return(NULL)
    }
  return(missing_vars)
}

#'check no duplicate observations
#'
#'\code{find_dups} checks that there are no duplicate observations for
#'each country-indicator-year combination
#'
#'@param df a dataframe
#'@param vars character vector of variable names to group by
#'
#'@export

find_dups <- function(df, vars) {

  cols <- rlang::syms(vars)

  dups <- df |>
    group_by(!!!cols) |>
    count() |>
    filter(n > 1)

  if( nrow(dups) == 0) {
    return(NULL)
  }
  return(dups)
}

#'convert UIS submission to wide format
#'
#'\code{widen} converts a UIS data submission to wide format
#'
#'@param df a dataframe
#'@export

widen <- function(df) {

  df |>
    select(CO_CODE, IND_ID, IND_YEAR, FIG) |>
    tidyr::pivot_wider(names_from = IND_ID, values_from = FIG)
}


#'convert UIS submission to long format
#'
#'\code{lengthen} converts a UIS data submission to long format
#'
#'@param df a dataframe
#'@export

lengthen <- function(df) {

  df |>
    tidyr::pivot_longer(cols = -c("CO_CODE", "IND_YEAR"), names_to = "IND_ID", values_to = "FIG")
}

