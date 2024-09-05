#' Calibrating Control Limits for Shewart Control Charts
#'
#' @description Calculates the critical value `h` governing the control limits for Shewart Control Charts. `h` is chosen such that the observed
#' Average Run Length (ARL) of the process is as close as possible to the user-specified ARL. The value is determined through numerical optimization
#' over a Monte-Carlo simulation.
#'
#' @inheritParams calculate_control_limit_ewma
#' @export
#'
#' @return `numeric`: The control limit h.
calculate_control_limit_shewart <- function(ARL = 500L, side = c("both", "upper", "lower"), replications = 5000L, interval = c(1e-4, 3.5),
                                            distribution_fun = stats::rnorm, ...) {
  return(
    calculate_control_limit_ewma(
      lambda = 1, ARL = 500L, fir = FALSE, side = c("both", "upper", "lower"), replications = 5000L, interval = c(1e-4, 3.5),
      distribution_fun = stats::rnorm, ...)
  )
}