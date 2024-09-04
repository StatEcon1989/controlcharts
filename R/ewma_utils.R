#' Calibrating Control Limits for EWMA Control Charts
#'
#' @description Calculates the critical value `h` governing the control limits for EWMA Control Charts. `h` is chosen such that the observed
#' Average Run Length (ARL) of the process is as close as possible to the user-specified ARL. The value is determined through numerical optimization
#' over a Monte-Carlo simulation.
#'
#' @details Usually, the time-series is assumed to be iid normally distributed. However, different distribution functions can be used.
#' To this end, an arbitrary function can be provided for `distribution_fun` and the control limits will be chosen accordingly. Additional parameters can be supplied by `...`.
#' \strong{Note:} The function must fulfill two properties:
#' * It must have a parameter `n` that governs the sample size.
#' * The resulting process has mean zero
#'
#' @inheritParams ewma_cc
#' @param ARL `integer`: The desired ARL, defaults to 500.
#' @param interval `vector<numeric>`: The interval over which the control limit is searched.
#' @param distribution_fun `function`: Distribution function that is used in the simulation. Defaults to [stats::rnorm()], but user-specified
#' functions can be provided (see details).
#' @param ... `ANY`: Additional arguments that will be passed on to `distribution_fun`.
#' @param replications `integer`:Number of replications used in the, Monte-Carlo simulation.
#' @examples
#' # normally distributed observations
#' calculate_control_limit_ewma(
#'   lambda = 0.05, ARL = 250L, fir = TRUE, side = "both",
#'   replications = 500L, distribution_fun = stats::rnorm
#' )
#' # t-distributed observations with additional parameter
#' calculate_control_limit_ewma(
#'   lambda = 0.05, ARL = 250L, fir = TRUE, side = "both",
#'   replications = 500L, distribution_fun = stats::rt, df = 3
#' )
#'
#' @return `numeric`: The control limit h.
#' @export
calculate_control_limit_ewma <- function(lambda, ARL = 500L, fir = TRUE, side = c("both", "upper", "lower"),
                                         replications = 5000L, interval = c(1e-4, 3.5),
                                         distribution_fun = stats::rnorm, ...) {
  side <- match.arg(side)
  exceedance <- switch(side,
                       "both" = function(z, cl) return(z > cl | z < -cl),
                       "upper" = function(z, cl) return(z > cl),
                       "lower" = function(z, cl) return(z < -cl)
  )

  T <- ARL * 3L
  x_mat <- matrix(distribution_fun(n = replications * T, ...), ncol = replications, nrow = T, byrow = FALSE)
  # smooth all series
  z_mat <- matrix(data = NA_real_, ncol = replications, nrow = T)
  for (i in seq_len(replications)) {
    z_mat[, i] <- ewma_smoothing(x_mat[, i], lambda = lambda)
  }
  # preliminary cl (needs to be scaled by h)
  cl_prelim <- cl(lambda = lambda, h = 1, T = T, fir = fir)

  optim_results <- stats::optimize(f = optim_func_ewma, ARL = ARL, z_mat = z_mat, cl_prelim = cl_prelim, exceedance = exceedance, T = T,
                            fallback_arl = T * 2L, interval = interval)
  return(optim_results$minimum)
}

#' Construct Control Limits for EWMA Control Charts (internal)
#'
#' @description Constructs control limits for EWMA control charts and a given critical value `h`. They are always one-sided and positive,
#' so they must be transformed, if they should be applied to lower or two-sided control charts.
#'
#' @inheritParams ewma_cc
#' @param T `integer`: Desired length of control limit vector
#'
#' @return `vector<numeric>`: control limits
cl <- function(lambda, h, T, fir = TRUE) {
  # fir = FALSE
  cl <- rep(h * sqrt(lambda / (2 - lambda)), T)
  # fir = TRUE
  if (fir) {
    cl <- cl * sqrt(1 - (1 - lambda)^(2 * seq_len(T)))
  }
  return(cl)
}

#' Sub-Function for optimization over the control limit critical values (internal)
#'
#' @description Is called by [calculate_control_limit_ewma()].
#'
#' @param h `numeric`: Critical value for the control limit
#' @param ARL `integer`: Desired ARL
#' @param T `integer`: Sample length
#' @param z_mat `matrix<numeric>`: EWMA smoothed data. Dimensions: `T` x replications
#' @param exceedance `function`: Function, that checks for breaches of the control limit
#' @param cl_prelim `numeric`: preliminary control limit, i.e. needs to be scaled by `h`
#' @param fallback_arl `integer`: If no breach is detected, which (artificial) ARL should be returned?
#'
#' @seealso calculate_control_limit_ewma
#'
#' @return `numeric`: Squared difference between desired and actual ARL
optim_func_ewma <- function(h, ARL, z_mat, T, exceedance, cl_prelim, fallback_arl) {
  cl <- cl_prelim * h
  exceedances <- exceedance(z_mat, cl)
  RL <- integer(length = ncol(z_mat))
  for (i in seq_len(ncol(exceedances))) {
    RL_temp <- which(exceedances[, i])
    if (length(RL_temp) > 0L) {
      RL_temp <- RL_temp[1]
    }else {
      RL_temp <- fallback_arl
    }
    RL[i] <- RL_temp
  }
  return((ARL - mean(RL))^2)
}

