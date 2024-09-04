rm(list = ls()); devtools::load_all()
# create the example data
T <- 500L # length of the time series
table <- data.table::data.table(X = rnorm(n = T, mean = 0.5), TIME = Sys.Date() + 1:T) # simulating the (out-of-control) data as table

# parametrization of the EWMA control chart
side <- "both"
lambda <- 0.5
ARL <- 500L

# calculate the critical value for the control limits of the EWMA chart
h <- calculate_control_limit_ewma(lambda = lambda, ARL = ARL, fir = TRUE, side = side,
                                  replications = 5000L)
# apply the EWMA and append the corresponding columns
table[, c("z", "upper_limit", "lower_limit", "exceedance") := ewma_cc(x = X, lambda = lambda, h = h, fir = TRUE, side = side)]
head(table)

# plot the control chart
table[, plot_ewma(.SD, dates = TIME)]