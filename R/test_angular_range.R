#' Angular range test
#' 
#' This function tests if angles are within a specified range. The angles `x`
#' and the test range `angular_range` need to be in the same units but can be
#' any circular revolution of the angles ( `a = a + 2*pi` ).
#' @param x Numeric vector with angles to be tested.
#' @param angular_range Numeric vector with only the limits of
#' the angular test range. The full circle range in radians, `anglular_range=c(0, 2*pi)`,
#' is chosen by default.
#' @return A logical vector of same length as x with values `TRUE` if the 
#' individual angle is within the test range or `FALSE` if not.
#' @export
angular_range_test <- function(x, angular_range=c(0, 2*pi)) {
  # Test if an angle x falls within angular_range
  
  # Check input types
  if (!all(is.numeric(x), is.numeric(angular_range))){
    stop("Invalid values! Both `x` and `angular_range` must be numeric.")
  }

  angle <- nice_angles(x)
  start <- nice_angles(angular_range[1])
  end <- nice_angles(angular_range[2])
  
  if(start <= end) {
    test <- (angle >= start) & (end > angle)
  } else {
    test1 <- (angle >= 0) & (end > angle)
    test2 <- (angle >= start) & ((2*pi) > angle)
    test <- test1 | test2
  }
  
  return(test)
}