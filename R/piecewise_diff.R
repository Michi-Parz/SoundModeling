# left:         Univariate Function left of change
# right:        Univariate Function right of change
# change:       Change Point
# transition:   Transition area before and after the change point

##############################################################
# Help functions #############################################

cubic_poly <- function(x, l, r, dl, dr, change, transition, direction){
  if (direction == "left") {
    return(
      cubic_poly_left(x, l, r, dl, dr, change, transition)
    )
  }
  
  cubic_poly_right(x, l, r, dl, dr, change, transition)
}


# Polynomial for type = left
cubic_poly_left <- function(x, l, r, dl, dr, change, transition){
  a <- (2*l - 2*r + dr*transition + dl*transition) / (transition^3)
  b <- (dr - dl) / (2*transition) + (a * (-6 * change  + 3 * transition)) / 2
  c <- dr - 3 * a * change^2 - 2 * b * change
  d <- r - c * change - b * change^2 - a * change^3
  
  function(x){
    a * x^3 + b * x^2 + c * x + d
  }
}

# Polynomial for type = right
cubic_poly_right <- function(x, l, r, dl, dr, change, transition){
  a <- (2*l - 2*r + dr*transition + dl*transition) / (transition^3)
  b <- (dr - dl) / (2*transition) - (a * (6 * change + 3 * transition)) / 2
  c <- dl - 3 * a * change^2 - 2 * b * change
  d <- l - c * change - b * change^2 - a * change^3
  
  function(x){
    a * x^3 + b * x^2 + c * x + d
  }
}

# Numerical derivation of a function f at a point x.
num_diff <- function(f, x, eps = 1e-6){
  (f(x + eps) - f(x - eps)) / (2 * eps)
}

##############################################################
# Main function ##############################################
piecewise_diff <- function(x, left, right, change,
                           transition, direction = c("left","right")){
  
  # Special cases transition < 0
  if (transition < 0) {
    stop("Transition must be at least 0.")
  }
  
  # Special cases transition = 0
  if (transition == 0) {
    return(
      function(x){ifelse(x < change, left(x), right(x))}
    )
  }
  
  # Determine transition type
  direction <- match.arg(direction)
  switch (direction,
          "left" = {
            left_change <- change - transition
            right_change <- change
          },
          "right" = {
            left_change <- change
            right_change <- change + transition
          }
          
  )
  
  # Derivation left and right functions at the appropriate points
  left_diff <- num_diff(left, left_change)
  right_diff <- num_diff(right, right_change)
  
  
  # Trasition Polynom
  
  transition_polynom <- cubic_poly(x,
                                   l = left(left_change),
                                   r = right(right_change),
                                   dl = left_diff,
                                   dr = right_diff,
                                   change, transition, direction)
  
  
  
  # Combine Functions
  new_right <- function(x){
    ifelse(x > right_change, right(x), transition_polynom(x))
  }
  function(x){
    ifelse(x < left_change, left(x), new_right(x))
  }
  
}

