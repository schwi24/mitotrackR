#' Nice angles
#' 
#' Circular angles repeat every 2*pi radians and can make angular calculations
#' challenging. The function `nice_angles` converts angles to 
#' `[0, 2*pi[` or `[0°, 360°[` to simplify calculations.
#' @param x Numeric vector
#' @param units A character string declaring the units of the angles. The
#' default is `'radians'` but can also be `'degree'`.
#' @return A numeric vector with angles converted to the range `[0, 2*pi[`. If
#' the units are in `'degree'`, the output is converted to the range `[0, 360[`
#' instead.
#' @export
nice_angles <- function(x, units="radians"){
  # Transform angles to radians from 0 to 2*pi
  
  # Check input type
  if(!is.numeric(x)){
    stop("Invalid type! x must be numeric.")
  }
  # Check units
  if (!any(c("radians", "degree") %in% units)) {
    stop("Invalid units! It must be any units = c('radians', 'degree').")
  }
  
  if(units == "degree"){
    x <- x/180*pi
  }
  angle <- x %% (2*pi)
  test <- which(angle < 0)
  angle[test] <- angle[test] + 2*pi
  if(units == "degree"){
    angle <- 180*angle/pi
  }
  return(angle)
}
