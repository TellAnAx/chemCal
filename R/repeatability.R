#' Calculate the repeatability
#' 
#' Function to calculate the repeatability
#' 
#' @param signal numeric vector containing the signal values
#' @details
#' Additional details...
#' 
#' @export 
repeatability <- function(signal) {
  
  mean_signal <- mean(signal, na.rm = TRUE)
  sd_signal <- sd(signal, na.rm = TRUE)
  RSD <- sd_signal / mean_signal
  
  return(RSD)
}