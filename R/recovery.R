#' Calculate the recovery
#' 
#' Function to calculate the recovery of an analysis based on the ratio between 
#' the measurement result of a sample that was spiked with a known amount of the
#' target analyte or a chemically similar substance.
#' 
#' @param measured numeric value representing the measured concentration of the
#' analyte in the spiked test sample.
#' @param expected numeric value representing the expected concentration of the
#' analyte in the spiked test sample.
#' @return a numeric value, being the relative recovery of the analyte.
#' @export
recovery <- function(measured,
                     expected) {
  result <- measured / expected
  
  return(result)
}