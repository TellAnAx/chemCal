#' Calculate quantitative results
#'
#' Calculate the concentration or other things based on the signal of an
#' analytical device and a calibration.
#'
#' @param signal numeric
#' @param cal object
#' @param df numeric; dimensionless dilution factor. Calculated by dividing
#' the total volume after fill-up with solvent by the volume of the sample aliquot.
#' @param vf numeric; total volume of the sample
#' @param wf numeric; weight
#'
#' @return numeric; the concentration calculated by plugging in the signal value
#' into the provided calibration function
#'
#' @examples
#' # fit model
#' data(massart97ex3)
#' m <- lm(y ~ x, data = massart97ex3)
#'
#' # create example data
#' measured <- tibble::tribble(
#' ~y,
#' 1,
#' 2,
#' 3
#' )
#'
#' calculate_result(measured$y, m)
#'
#' @importFrom tibble tribble
#'
#' @export
calculate_result <- function(sig,
                             cal,
                             df = 1,
                             vf = 1,
                             wf = 1) {

  # rename inputs for clarity----
  signal <- sig
  calibration <- cal
  dilution_factor <- df
  volume_factor <- vf
  weight_factor <- wf


  # Do things depending on the input----

  ## multiply dilution factors if multiple dilution steps were done
  if (length(dilution_factor > 1)) {
    dilution_factor <- prod(dilution_factor)
  }

  result <- vector(mode = "numeric", length = length(signal))


  for (i in 1:length(signal)) {
    # 2. use inverse.predict to predict the concentration based on
    #    the calibration object and signal----
    prediction_results <- chemCal::inverse.predict(calibration, signal[i])
    concentration <- prediction_results$Prediction


    # 3. recalculate the results based on the additional factors----
    result[i] <- concentration * dilution_factor * volume_factor * 1/weight_factor
  }


  # 4. return the result----
  return(result)
}
