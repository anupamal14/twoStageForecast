#' @title  Find the accuracy of a forecast
#'
#' @description Calculates the Mean Square Error (MSE), Mean Absolute
#'     Deviation (MAD) and Mean Absolute Percentage Error (MAPE) of
#'     a forecast.
#'
#' @param y the actual data.
#'
#' @param yhat the forecasted vector.
#'
#'
#' @examples
#'
#' @export
#'


twoStage.accuracy <- function(yhat, y) {
  err <- y - yhat
  # MSE
  MSE <- round(mean(err^2), 2)
  # MAD
  MAD <- round(mean(abs(err)), 2)
  # MAPE
  MAPE <- round(mean(abs(err/y)) * 100, 2)

  ErrVec <- cbind(MSE, MAD, paste(toString(MAPE),"%"))
  colnames(ErrVec) <- c("MSE","MAD","MAPE")
  return(ErrVec)
}
