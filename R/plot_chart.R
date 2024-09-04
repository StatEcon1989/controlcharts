#' Erstellt die Grafik fuer einen EWMA Control Chart
#'
#' @description Creates a plot for an EWMA control chart. If available, [ggplot2::ggplot()] will be used, otherwise, the plot will be created with base R.
#'
#' @param ewma_cc `list`: The value of the function [ewma_cc]
#' @param dates `vector<dates>`: Optional. The dates corresponding to each observation. Must be of the same length as the control chart.
#' @param plot_name `character`: The title to be displayed above the plot. Defaults to 'EWMA Control Chart'
#'
#' @return If ggplot2 is installed, a plot. Otherwise NULL
#' @export
plot_ewma <- function(ewma_cc, dates = NULL, plot_name = "EWMA Control Chart") {
  # if no dates given, construct index
  if (is.null(dates)) {
    dates <- seq_len(length(ewma_cc$z))
  }
  df <- data.frame(Date = dates, EWMA = ewma_cc$z, Upper = ewma_cc$upper_limit, Lower = ewma_cc$lower_limit, Exceedance = ewma_cc$exceedance)
  # create plot
  if (requireNamespace(package = "ggplot2")) {
    p <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = "Date", y = "EWMA")) +
      ggplot2::geom_line(ggplot2::aes_string(y = "EWMA")) +
      ggplot2::geom_area(ggplot2::aes_string(y = "Upper"), fill = "yellow", alpha = 0.5) +
      ggplot2::geom_area(ggplot2::aes_string(y = "Lower"), fill = "yellow", alpha = 0.5) +
      ggplot2::geom_vline(xintercept = df[df$Exceedance, "Date"], linetype = "dashed", color = "red") +
      ggplot2::labs(title = plot_name, x = "Date", y = "EWMA") +
      ggplot2::theme_minimal()
    return(p)
  }else {
    graphics::matplot(x = df[, "Date"], y = df[, c("EWMA", "Upper", "Lower")], type = "l", lty = 1, col = c("black", "yellow", "yellow"), xlab = "Date", ylab = "EWMA")
    graphics::abline(v = df[df$Exceedance, "Date"], lty = 2, col = "red")
    return(NULL)
  }
}


