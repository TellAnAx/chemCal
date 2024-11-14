#' Assess the linearity of a calibration curve
#' 
#' A function to create diagnostic plots for the assessment of the linearity of 
#' calibration data. The underlying methods can be found in DIN 32 402 (German Industrial Norm).
#' 
#' @param x numeric vector of independent values (usually concentrations)
#' @param y numeric vector of dependent values (usually the signal of the analytical device)
#' @param method character string. Supported methods are "point-to-point" and "curvature".
#' @param range numeric value describing the acceptable deviation from the median of the slopes or 
#' the signal-to-concentration ratio
#' 
#' @importFrom graphics abline
#' @importFrom stats median
#' 
#' @examples
#' # linearity(din32645$x, din32645$y, method = "point-to-point")
#' # linearity(din32645$x, din32645$y, method = "curvature", range = 0.2)
#' 
#' @export
linearity <- function(x,
                      y,
                      method = NULL,
                      range = 0.1) {
  
  # Check data integrity
  stopifnot("x and y must have the same length!"=length(x) == length(y))
  stopifnot("method must be defined! \n
  Select either 'point-to point' for the point-to-point slope or 'curvature' 
            for the empirical curvature test" = !is.null(method))
  
  # Calculate the necessary result based on the chosen method
  if(method == "point-to-point") {
    x_diff = diff(x)
    y_diff = diff(y)
    result = y_diff / x_diff # quotient of differences
  } else if (method == "curvature") {
    result = y/x # signal-to-concentration ratio
  } else {
    warning("Irregular input. No result output has been generated. Check your input data.")
  }
  
  result_median <- median(result)
  
  # Create diagnostic plot
  plot(result, main = method)
  abline(h = result_median, col = "red")
  abline(h = result_median - range*result_median, col = "red", lty = 3)
  abline(h = result_median + range*result_median, col = "red", lty = 3)
}
