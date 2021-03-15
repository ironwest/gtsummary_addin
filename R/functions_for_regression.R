#' Function for capture terms in model formula.
#'
#' @description This function takes a regression model object and return terms of model formula. This function intended to use in the interactive tbl_regression add-in for include argument.

get_terms <- function(mod){
  attributes(mod$terms)$term.labels
}
