#' Atomize a chemical sum formula
#'
#' Converts a chemical formula to a vector holding the individual atoms.
#'
#' @param sum_formula a character string or a character vector with the sum
#' formula(s) of the molecules to be converted.
#'
#' @returns a list with each list element being a vector with the individual
#' atoms of a molecule.
#'
#' @author Anil Axel Tellbüscher
#'
#' @importFrom base gregexpr
#' @importFrom base regmatches
#' @importFrom base gsub
#' @importFrom base as.numeric
#' @importFrom base is.na
#' @importFrom base c
#'
#' @export
convert_chemSum <- function(sum_formula) {

  # Use regular expression to match elements and their counts
  matches <- gregexpr("[A-Z][a-z]?\\d*", sum_formula)

  # Extract matched substrings
  strings <- regmatches(sum_formula, matches)

  # Initialize an empty list to store the elements
  elements <- list()

  for(string_no in 1:length(strings)){
    single_string <- strings[[string_no]]

    atoms <- c()

    for (substring in single_string) {
      element <- gsub("\\d", "", substring)  # Extract element
      count <- as.numeric(gsub("[A-Za-z]", "", substring))  # Extract count, if any

      # If no count is specified, default to 1
      count[is.na(count)] <- 1

      atoms <- c(atoms, rep(element, count))
    }

    # Repeat the element according to its count and append to the vector
    elements[[string_no]] <- atoms
  }

  return(elements)
}
