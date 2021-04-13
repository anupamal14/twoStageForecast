#' @title  splines for seasonality estimation
#'
#' @description Estimates the seasonality for an input function
#'     by using splines
#'
#' @param x the actual data.
#'
#'
#'
#' @examples
#'
#' @importFrom stats smooth.spline
#'


twoStage.spline <- function(x) {
  seas <- smooth.spline(1:length(x), x)
  seas$y
}
