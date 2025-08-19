# This object will function as a single cubic Bezier curve which will be used to
# more complicated structures like Bezier-splines and B-splines.


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
setClass("Bezier", slots = list(controlePoints = "numeric"),
         validity = check_controle_points())

# TODO: add the function that interpolates between the controle points