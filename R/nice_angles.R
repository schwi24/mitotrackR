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

#' Mean angle
#' 
#' Circular angles repeat every 2*pi radians which has to be taken into account
#' for calculating the averages over angles.
#' @param x Numeric vector
#' @param units A character string declaring the units of the angles. The
#' default is `'radians'` but can also be `'degree'`.
#' @return The average angle in radians or degrees.
#' @export
angular_mean <- function(x, units="radians"){
  
  # Check input type
  if(!is.numeric(x)){
    stop("Invalid type! x must be numeric.")
  }
  
  # Check units
  if (!any(c("radians", "degree") %in% units)) {
    stop("Invalid units! It must be any units = c('radians', 'degree').")
  }
  
  # Calculations in radians
  if(units == "degree"){
    x <- x/180*pi
  }
  
  # Calculate mean angle
  sx <- mean(sin(x))
  cx <- mean(cos(x))
  mean_angle <- atan2(sx, cx)
  
  # Convert back to degree and return mean angle
  if(units == "degree"){
    mean_angle <- 180*mean_angle/pi
  }
  return(mean_angle)
  
}

#' Standard deviation of angle
#' 
#' Circular angles repeat every 2*pi radians which has to be taken into account
#' for calculating the standard deviation over angles.
#' @param x Numeric vector
#' @param units A character string declaring the units of the angles. The
#' default is `'radians'` but can also be `'degree'`.
#' @return The average angle in radians or degrees.
#' @export
angular_sd <- function(x, units="radians"){
  
  # Check input type
  if(!is.numeric(x)){
    stop("Invalid type! x must be numeric.")
  }
  
  # Check units
  if (!any(c("radians", "degree") %in% units)) {
    stop("Invalid units! It must be any units = c('radians', 'degree').")
  }
  
  # Calculations in radians
  if(units == "degree"){
    x <- x/180*pi
  }
  
  # Calculate mean angle
  sx <- mean(sin(x))
  cx <- mean(cos(x))
  r <- sqrt(sx*sx + cx*cx)
  sd_angle <- sqrt(-2*log(r))
  
  # Convert back to degree and return mean angle
  if(units == "degree"){
    sd_angle <- 180*sd_angle/pi
  }
  return(sd_angle)
  
}

#' Speed-weighted angular mean (Yamartino 1984)
#' 
#' Circular angles repeat every 2*pi radians which has to be taken into account
#' for calculating the angular means.
#' @param angle Numeric vector with angles
#' @param velocity Numeric vector with velocities used to weigh the mean
#' @param units A character string declaring the units of the angles. The
#' default is `'radians'` but can also be `'degree'`.
#' @return The average angle in radians or degrees.
#' @export
weighted_angular_mean <- function(angle, speed, units="radians"){
  
  # Check input type
  if(!is.numeric(angle)){
    stop("Invalid type! x must be numeric.")
  }
  if(!is.numeric(speed)){
    stop("Invalid type! x must be numeric.")
  }
  
  # Check units
  if (!any(c("radians", "degree") %in% units)) {
    stop("Invalid units! It must be any units = c('radians', 'degree').")
  }
  
  # Calculations in radians
  if(units == "degree"){
    angle <- angle/180*pi
  }
  
  # Calculate weighted mean angle
  sx <- mean(speed * sin(angle))
  cx <- mean(speed * cos(angle))
  mean_angle <- atan2(sx, cx)
  
  # Convert back to degree and return mean angle
  if(units == "degree"){
    mean_angle <- 180*mean_angle/pi
  }
  return(mean_angle)
  
}

#' Standard deviation of angle (Yamartino 1984)
#' 
#' Circular angles repeat every 2*pi radians which has to be taken into account
#' for calculating the standard deviation over angles.
#' @param x Numeric vector
#' @param units A character string declaring the units of the angles. The
#' default is `'radians'` but can also be `'degree'`.
#' @return The average angle in radians or degrees.
#' @export
angular_sd_yamartino <- function(x, units="radians"){
  
  # Check input type
  if(!is.numeric(x)){
    stop("Invalid type! x must be numeric.")
  }
  
  # Check units
  if (!any(c("radians", "degree") %in% units)) {
    stop("Invalid units! It must be any units = c('radians', 'degree').")
  }
  
  # Calculations in radians
  if(units == "degree"){
    x <- x/180*pi
  }
  
  # Calculate mean angle
  sx <- mean(sin(x))
  cx <- mean(cos(x))
  eps <- sqrt(1-(sx*sx + cx*cx))
  sd_angle <- asin(eps) * (1 + eps*eps*eps*(2/sqrt(3)-1))
  
  # Convert back to degree and return mean angle
  if(units == "degree"){
    sd_angle <- 180*sd_angle/pi
  }
  return(sd_angle)
  
}

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
