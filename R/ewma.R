#' Exponentially Weighted Moving Average (EWMA) Control Chart
#'
#' This function calculates the exponentially weighted moving average (EWMA) control chart.
#'
#' @param x `vector<numeric>`: A numeric vector of data with mean zero and unit variance.
#' @param lambda `numeric`: The smoothing parameter, lower values indicate stronger smoothing and is associated with earlier detection of smaller shifts.
#' Larger shifts are typically detected earlier with higher values of lambda. Must lie in the interval `(0,1]`. `lambda=1` results in a shewart control chart.
#' @param h `numeric`: A positive numeric used for constructing the control limit. Must be typically chosen together with `lambda`.
#' @param fir `logical`: should Fast Initial Response (FIR) be used? If `TRUE`, the initial value of the EWMA is set to the first value of the data.
#' @param side `character`: The side of the control chart, one of "both", "upper", "lower".
#' @param stop_on_exceedance `logical`: If `TRUE`, the function stops when the control limit is exceeded. Otherwise processes x completely.
#'
#' @examples res <- ewma_cc(x = rnorm(2e3), lambda = 0.05, h = 2.615)
#'
#'
#' @return `list`: A list with the following components:
#' * `z vector<numeric>` - The EWMA statistic.
#' * `upper_limit/lower_limit vector<numeric>` - Vector, containing the control limits.
#' * `exceedance vector<logical>` - TRUE at times where exceedances are detected FALSE otherwise.
#' * `side `character`` - The side of the control chart.
#'
#'
#' @export
ewma_cc <- function(x, lambda = 0.05, h, side = c("both", "upper", "lower"), fir = TRUE, stop_on_exceedance = FALSE) {
  side <- match.arg(side)
  T <- length(x)

  # construction of control limits
  cl_template <- cl(lambda = lambda, h = h, T = T, fir = fir)

  # condition that checks if process is in control
  no_exceedance <- switch(side,
                          "both" = function(z, cl) return(z < cl && z > -cl),
                          "upper" = function(z, cl) return(z < cl),
                          "lower" = function(z, cl) return(z > -cl)
  )
  # run length
  rl <- 0L
  # control statistic, initial value 0
  z <- rep(0.0, T + 1)
  x <- c(0.0, x)
  cl <- c(1.0, cl_template)
  exceedance <- rep(FALSE, T + 1)
  for (t in (seq_len(T) + 1)) {
    if (no_exceedance(z = z[t - 1], cl = cl[t - 1])) {
      z[t] <- lambda * x[t] + (1 - lambda) * z[t - 1]
    } else {
      exceedance[t - 1] <- TRUE
      if (stop_on_exceedance) {
        break
      } else {
        z[t] <- lambda * x[t]
        cl[t:(T + 1)] <- cl_template[1:(T + 1 - t + 1)]
      }
    }
  }

  # remove initial values
  z <- z[-1]
  x <- x[-1]
  cl <- cl[-1]
  exceedance <- exceedance[-1]

  # output preparation
  cl <- switch(side,
               "both" = list(upper = cl, lower = -cl),
               "upper" = list(upper = cl, lower = rep(NA_real_, length(cl))),
               "lower" = list(upper = rep(NA_real_, length(cl)), lower = -cl)
  )
  return(list(z = z, upper_limit = cl$upper, lower_limit = cl$lower, exceedance = exceedance))
}
