rm(list = ls()); devtools::load_all()
T <- 1e3
side <- "both"
lambda <- 0.5
ARL <- 500L

h <- calculate_control_limit_ewma(lambda = lambda, ARL = ARL, fir = TRUE, side = side,
                                  replications = 5000L)
table <- data.table::data.table(X = rnorm(n = T, mean = 0.0), TIME = Sys.Date() + 1:T)
table[, c("z", "upper_limit", "lower_limit", "exceedance") := ewma_cc(x = X, lambda = lambda, h = h, fir = TRUE, side = side)]
table[, plot_ewma(.SD, dates = TIME)]