#' Calculate quantitative results
#' 
#' Calculate the concentration or other things based on the signal of an 
#' analytical device and a calibration.
#' 
#' @param calibration object
#' @param signal numeric value
#' @param df numeric value
#' @param wf numeric value
#' @param vf numeric value
#' @return numeric value
#' @export
calculate_result <- function(calibration,
                             signal,
                             df = 1,
                             wf = 1,
                             vf = 1) {
  # 1. Do things depending on the input
  if (length(df > 1)) {
    df <- prod(df)
  }
  
  result <- vector(mode = "numeric", length = length(signal))
  
  
  for (i in 1:length(signal)) {
    # 2. use inverse.predict to predict the concentration based on
    #    the calibration object and signal
    prediction_results <- chemCal::inverse.predict(calibration, signal[i])
    concentration <- prediction_results$Prediction
    
    
    # 3. recalculate the results based on the additional factors
    result[i] <- concentration * 1 / df * 1 / wf * vf
  }
  
  
  # 4. return the result
  return(result)
}