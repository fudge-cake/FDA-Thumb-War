# This object will function as a single cubic Bezier curve which will be used to
# more complicated structures like Bezier-splines and B-splines.

# TODO : Implement more general curves of different order.



# checks that the parameter controlePoints is a matrix with 4 columns.
check_controle_points <- function(object) {
  errors <- character()
  cp <- object@controlePoints
  if (!(is.matrix(cp))) {
    msg <- paste("controlePoints must be a matrix with 4 columns.",
                 sep = "")
    errors <- c(errors, msg)
  } else if (ncol(cp) != 4) {
    msg <- paste("controlePoints has ", ncol(cp), " columns.  Should be 4",
                 sep = "")
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors
}

# Cubic Bezier curve
#
# @slots controlePoints : a matrix of 4 vectors that are the points
# the curve interpolates between.
setClass("Bezier", slots = list(controlePoints = "numeric"))
setValidity("Bezier", check_controle_points)

# Function that interpolates between the controle points
point_at_t <- function(curve,t) {
  coeff_mat <- matrix(c(1,   0,  0,  0,
                        -3,  3,  0,  0,
                        3,  -6,  3,  0,
                        -1,  3, -3,  1),
                      ncol = 4, byrow = TRUE)
  t_vector <- matrix(c(1, t, t * t, t * t * t), ncol = 4)
  factor <- t_vector %*% coeff_mat

  x_vector <- matrix(c(curve@controlePoints[, 1],
                       curve@controlePoints[, 2],
                       curve@controlePoints[, 3],
                       curve@controlePoints[, 4]),
                     nrow = 4, byrow = TRUE)
  factor[1, 1] * x_vector[1, ] + factor[1, 2] * x_vector[2, ] +
    factor[1, 3] * x_vector[3, ] + factor[1, 4] * x_vector[4, ]
}

# Function that finds the derivative of the curve
derivative_at_t <- function(curve,t) {
  coeff_mat <- matrix(c(1,   0,  0,  0,
                        -3,  3,  0,  0,
                        3,  -6,  3,  0,
                        -1,  3, -3,  1),
                      ncol = 4, byrow = TRUE)
  t_vector <- matrix(c(0, 1, 2 * t, 3 * t * t), ncol = 4)
  factor <- t_vector %*% coeff_mat

  x_vector <- matrix(c(curve@controlePoints[, 1],
                       curve@controlePoints[, 2],
                       curve@controlePoints[, 3],
                       curve@controlePoints[, 4]),
                     nrow = 4, byrow = TRUE)
  factor[1, 1] * x_vector[1, ] + factor[1, 2] * x_vector[2, ] +
    factor[1, 3] * x_vector[3, ] + factor[1, 4] * x_vector[4, ]
}


# Constructor for the Bezier object
bezier <- function(point) new("Bezier", controlePoints = point)

# Plotting function
plot_bezier <- function(curve, start = 0, end = 1, step = .01) {
  tvec <- seq(from = start, to = end, by = step)
  point <- point_at_t(curve = curve, tvec = tvec)
  x <- point[,1]
  y <- point[,2]
  
  plot.new()
  plot.default(x=x,y=y, type='n')
  lines(x,y)
}
