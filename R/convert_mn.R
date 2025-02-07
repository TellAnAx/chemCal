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

convert_mn <- function(sum_formula, m = NULL, n = NULL) {

  if(!is.null(m) & is.null(n)) {
      result = m / M(sum_formula)
  } else if (is.null(m) & !is.null(n)) {
      result = n * M(sum_formula)
  } else {
      stop()
  }

  return(result)
}

