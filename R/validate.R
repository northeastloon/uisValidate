
#'Summarise validation test
#'
#'\code{validate_summary} is a wrapper around \code{validate::confront}.
#'Provided a data frame and rule set, it returns a validation
#' summary of passed and failed rules.
#'
#'@param data data frame to validate
#'@param rules an object of class validator (returned from \code{parse_rules})
#'
#'@import dplyr
#'@export


validate_summary <- function(data, rules) {

  data <- as.data.frame(data)

  #confront crosswalk with rules
  cf <- validate::confront(dat = data, x= rules)

  summary <- cf |>
    validate::summary()

    rules <- rules |>
      validate::meta() |>
      as.data.frame() |>
      unique()

    val_summary <- dplyr::inner_join(summary, rules, by = "name")

    errors <- validate::errors(cf) |>
      dplyr::tibble()
    if(nrow(errors) > 0) {
     errors <- errors |>
       tidyr::pivot_longer(everything(), values_to = "error")
    }

   return(list(summary = val_summary, errors = errors))

}


#'show violating observations
#'
#'\code{violating_obs} returns a dataframe fo violating observations
#'
#'@param data data frame to validate
#'@param rules an object of class validator (returned from \code{parse_rules})
#'@param names names of the rules to evaluate
#'
#'@export

violating_obs <-function(data, rules, names = NULL) {

  if(!is.null(names)) {
     rules <- rules[names]
  }

  vars <- validate::variables(rules)
  id_vars <- c("CO_CODE", "IND_YEAR", "CO_SHORT_NAME", "CO_LONG_NAME", "COUNTRY")

  cf <- validate::confront(dat = data, x= rules)

  violating_obs <- validate::violating(data, cf, include_missing = FALSE) |>
    dplyr::select(dplyr::any_of(c(id_vars, vars)))

}
