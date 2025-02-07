#' Mol-to-mass conversion
#'
#' Function to calculate the molar mass of a compound from its chemical sum formula.
#'
#' @param sum_formula description
#' @param m
#' @param n
#'
#' @returns a numeric
#'
#' @importFrom PeriodicTable mass
#'
#' @export

M <- function(sum_formula) {

  # Calculate the molar mass
  M = sum(PeriodicTable::mass(unlist(convert_chemSum(sum_formula))))

  return(M)
}

