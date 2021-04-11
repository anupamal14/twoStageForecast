#' @title  Classical decomposition method for seasonality estimation
#'
#' @description Estimates the seasonality at different levels by
#'     subtracting the mean for additive seasonality
#'     (dividing for multiplicative) and then averaging across
#'     the entire data.
#'
#' @param d the actual data.
#'
#' @param period seasonality levels.
#'
#'
#' @examples
#'

twoStage.classical <- function(d, period) {
  n <- length(d)
  agg_vec <- rep(1:period,ceiling(n/period))
  agg <- aggregate(d,list(vec = agg_vec[1:n]),mean)
  # Replicate to original length so that it can be applied directly
  #predSeas <- as.vector(t(matrix(rep(agg$x,ceiling(n/length(agg$x))), nrow = period)))
  predSeas <- rep(agg$x,ceiling(n/length(agg$x)))

}
