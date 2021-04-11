#' @title  Seasonality estimation for high frequency time series
#'
#' @description Estimates the seasonality using different methods like
#'     classical decomposition method based, polynomial fitting,
#'     trigonometric methods and spline.
#'
#' @param d the actual data.
#'
#' @param seas_periods periods associated with the levels of seasonality
#'
#'
#' @examples
#'
#' @importFrom data.table data.table
#'


secondStage <- function(d, seas_periods) {
  sortedSeas <- sort(seas_periods)
  nrSeas <- length(seas_periods)

  seas <- list()
  for (k in 1:nrSeas)
    seas$level[k] <- data.table()

  for (k in 1:nrSeas) {
    #df <- data.table()

    # lowest level
    d_1 <- colMeans(matrix(d, nrow = sortedSeas[k]))
    # Replicate this 12 times and reshape into a vector so that
    # it is the same length as d
    d_1_rep <- as.vector(t(matrix(t(rep(t(d_1), sortedSeas[k])), ncol = sortedSeas[k])))


    # Get de-mean signal
    multInput <- d/d_1_rep
    addInput <- d - d_1_rep

    df <- data.table(trigMult = twoStage.trig(multInput),
                     trigAdd = twoStage.trig(addInput),
                     splineMult = twoStage.spline(multInput),
                     splineAdd = twoStage.spline(addInput),
                     classicalMult = twoStage.classical(multInput, sortedSeas[k]),
                     classicalAdd = twoStage.classical(addInput, sortedSeas[k]))
    polySeas <- twoStage.poly(d, sortedSeas[1],sum((sortedSeas%%5 == 0)*5 + (sortedSeas%%7 == 0)*7))
    df$polyMult <- polySeas$mult
    df$polyAdd <- polySeas$add


    seas$level[[k]] <- df
  }

  seas
}
