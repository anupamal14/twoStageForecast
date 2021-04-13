#' @title  Two-stage methods for forecasting time series with mulitple
#'    levels of seasonality
#'
#' @description Estimates the seasonality at different levels by
#'     subtracting the mean for additive seasonality
#'     (dividing for multiplicative) and then averaging across
#'     the entire data.
#'
#' @param d the actual data.
#'
#' @param n Number of samples in the model period.
#'
#' @param p Number of samples in the holdout period.
#'
#' @param seas_periods periods corresponding to levels of
#'     seasonality in the data
#'
#' @param regMat regression matrix with explanatory variables as columns
#'
#' @param plotFlag whether plot is required at end of stage 1. Default is FALSE.
#'
#' @examples
#'
#' @importFrom stats aggregate fft lm
#'
#' @export
#'

auto.twoStage <- function(d, n = NULL, p = NULL, seas_periods = NULL, regMat = NULL,
                          plotFlag = FALSE) {
  # Store length of input data in len
  len <- length(d)

  # number of levels
  nrlevels <- length(seas_periods)
  if (is.null(seas_periods)) {
    seas_periods <- 2
    nrlevels <- 1
  }
  if (is.null(n) & is.null(p)) {
    p <- seas_periods[nrlevels]
    n <- len - p
  }

  meanBy <- rep(c(1:ceiling(len/seas_periods[1])), each = seas_periods[1])
  d2 <- aggregate(d,by = list(meanBy[1:len]),mean)
  ylow <- d2$x

  # First Stage
  stage1 <- firstStage(ylow, n/seas_periods[1], p/seas_periods[1],
                       seas_periods[2:length(seas_periods)]/seas_periods[1], regMat, plotFlag)
  # Second Stage
  stage2 <- secondStage(d[1:n], seas_periods)

  # Combine the output of the two stages
  #--------------------------------------
  # Additive model predictions
  stage1TBATSout <- rep(stage1$tbats$model$fitted.values, each = seas_periods[1])
  stage1ARIMAout <- rep(stage1$arima$model$fitted, each = seas_periods[1])

  # Find psi for each of the predictions - additive and multiplicative seasonality with
  # each of the stage 1 fitted values
  fit1 <- stage1ARIMAout + stage2$level[[1]]$trigAdd
  fit2 <- stage1ARIMAout * stage2$level[[1]]$trigMult
  fit3 <- stage1ARIMAout + stage2$level[[1]]$classicalAdd
  fit4 <- stage1ARIMAout * stage2$level[[1]]$classicalMult
  fit5 <- stage1ARIMAout + stage2$level[[1]]$splineAdd
  fit6 <- stage1ARIMAout * stage2$level[[1]]$splineMult
  fit7 <- stage1ARIMAout + stage2$level[[1]]$polyAdd
  fit8 <- stage1ARIMAout * stage2$level[[1]]$polyMult
  fit9 <- stage1TBATSout + stage2$level[[1]]$trigAdd
  fit10 <- stage1TBATSout * stage2$level[[1]]$trigMult
  fit11 <- stage1TBATSout + stage2$level[[1]]$classicalAdd
  fit12 <- stage1TBATSout * stage2$level[[1]]$classicalMult
  fit13 <- stage1TBATSout + stage2$level[[1]]$splineAdd
  fit14 <- stage1TBATSout * stage2$level[[1]]$splineMult
  fit15 <- stage1TBATSout + stage2$level[[1]]$polyAdd
  fit16 <- stage1TBATSout * stage2$level[[1]]$polyMult

  MSE <- function(Act, Obs){mean((Act - Obs)^2)}

  nrParClassSeas <- 7*seas_periods[1] + 7*24
  nrParTrigSeas <- 9
  nrParSplSeas <- smooth.spline(d[1:n])$fit$nk
  nrParPoly <- 4
  nrParTBATS <- length(stage1$tbats$model$parameters$vect) +
    stage1$tbats$model$parameters$control$length.gamma +
    stage1$tbats$model$parameters$control$use.beta*1 +
    stage1$tbats$model$parameters$control$use.box.cox*1 +
    stage1$tbats$model$parameters$control$use.damping*1 +
    stage1$tbats$model$parameters$control$p +
    stage1$tbats$model$parameters$control$q + 1
  nrParARIMA <- length(stage1$arima$model$coef) + 1


  psiVals <- c(twoStage.penalizedLikelihood(MSE(fit1, d[1:n]),n,nrParTrigSeas+nrParARIMA),
               twoStage.penalizedLikelihood(MSE(fit2, d[1:n]),n,nrParTrigSeas+nrParARIMA),
               twoStage.penalizedLikelihood(MSE(fit3, d[1:n]),n,nrParClassSeas+nrParARIMA),
               twoStage.penalizedLikelihood(MSE(fit4, d[1:n]),n,nrParClassSeas+nrParARIMA),
               twoStage.penalizedLikelihood(MSE(fit5, d[1:n]),n,nrParSplSeas+nrParARIMA),
               twoStage.penalizedLikelihood(MSE(fit6, d[1:n]),n,nrParSplSeas+nrParARIMA),
               twoStage.penalizedLikelihood(MSE(fit7, d[1:n]),n,nrParPoly+nrParARIMA),
               twoStage.penalizedLikelihood(MSE(fit8, d[1:n]),n,nrParPoly+nrParARIMA),
               twoStage.penalizedLikelihood(MSE(fit9, d[1:n]),n,nrParTrigSeas+nrParTBATS),
               twoStage.penalizedLikelihood(MSE(fit10, d[1:n]),n,nrParTrigSeas+nrParTBATS),
               twoStage.penalizedLikelihood(MSE(fit11, d[1:n]),n,nrParClassSeas+nrParTBATS),
               twoStage.penalizedLikelihood(MSE(fit12, d[1:n]),n,nrParClassSeas+nrParTBATS),
               twoStage.penalizedLikelihood(MSE(fit13, d[1:n]),n,nrParSplSeas+nrParTBATS),
               twoStage.penalizedLikelihood(MSE(fit14, d[1:n]),n,nrParSplSeas+nrParTBATS),
               twoStage.penalizedLikelihood(MSE(fit15, d[1:n]),n,nrParPoly+nrParTBATS),
               twoStage.penalizedLikelihood(MSE(fit16, d[1:n]),n,nrParPoly+nrParTBATS))

  # Prepare output
  modNames <- c('ARIMA','ARIMA','ARIMA','ARIMA','ARIMA','ARIMA','ARIMA','ARIMA',
                'TBATS', 'TBATS','TBATS','TBATS','TBATS','TBATS','TBATS','TBATS')

  stage2names <- c("Trigonometric Additive", "Trigonometric Multiplicative",
                   "Classical Additive", "Classical Multiplicative",
                   "Spline Additive", "Spline Multiplicative",
                   "Polynomial Additive", "Polynomial Multiplicative",
                   "Trigonometric Additive", "Trigonometric Multiplicative",
                   "Classical Additive", "Classical Multiplicative",
                   "Spline Additive", "Spline Multiplicative",
                   "Polynomial Additive", "Polynomial Multiplicative")

  bestModelIdx <- which.min(psiVals)

  errOut <- twoStage.accuracy(eval(parse(text=paste0("fit", bestModelIdx))), d[1:n])

  myCall <- deparse(sys.calls()[[sys.nframe()]])
  cat("Call: ", myCall)
  cat(paste(paste0("Seasonality level selected: ", 1, ",  period = ", seas_periods[1]),
            paste0("First Stage method: ", modNames[bestModelIdx]),
            paste0("Second Stage method: ", stage2names[bestModelIdx]),
            " ",
            paste0("psi: ", round(psiVals[bestModelIdx], 2)),
            " ",
            "Error measures:",
            "MSE             MAD          MAPE",
            paste0(errOut[1], "       ", errOut[2], "       ", errOut[3]),
            sep="\n"))

  # Prediction - holdout
  predOut <- predict(eval(parse(text=paste0("stage1$",tolower(modNames[bestModelIdx]),"$model"))), p)
  # For picking 2nd stage method
  stage2NameMatch <- c("trigAdd", "trigMult", "classicalAdd", "classicalMult",
                       "splineAdd", "splineMult", "polyAdd", "polyMult",
                       "trigAdd", "trigMult", "classicalAdd", "classicalMult",
                       "splineAdd", "splineMult", "polyAdd", "polyMult")
  addOrMult <- c('*','+')
  predFinal <- eval(parse(text=paste0("predOut$pred", addOrMult[bestModelIdx%%2 + 1],
                                      "stage2$level[[1]]$", stage2NameMatch[bestModelIdx],
                                      "[n - p - seas_periods[1] + 1:p]")))
  predErr <- twoStage.accuracy(predFinal, d[n+(1:p)])

  # Return list
  output <- list(pred = predFinal, Errors = predErr)

}
