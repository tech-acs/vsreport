#' Line chart
#'
#' @description
#' Produces a simple line chart in ggplot2 which can be customised for publication
#'
#' @param data data frame
#' @param x numeric column of data
#' @param y numeric column of data
#' @param group character or factor column of data defining individual lines
#' @param line_colour line colour
#' @param line_width line width
#' @param xlab x-axis title
#' @param ylab y-axis title
#' @param title plot title
#' @param subtitle plot subtitle
#' @param caption plot caption
#'
#' @details
#' The line_chart() function can produce multiple lines if a grouping variable is supplied.
#' A specific colour can be specified for single lines.
#' Data should be in long format.
#'
#' @return An object of class ggplot
#' @export
#'
#' @import ggplot2
#' @import rlang
#'
#' @examples vs_line_chart(data, x, y)
vs_line_chart <- function(data, x, y, group, line_colour = "ByGroup", line_width = 1, xlab = NULL, ylab = NULL, title = NULL, subtitle = NULL, caption = NULL) {

  x_var <- deparse(substitute(x))
  y_var <- deparse(substitute(y))

  # Error handling
  if (!is.data.frame(data)) stop("Data needs to be a data frame")
  if (!is.numeric(pull(data[x_var]))) stop("Column x needs to be of type numeric")
  if (!is.numeric(pull(data[y_var]))) stop("Column y needs to be of type numeric")

  if (line_colour == "ByGroup") {
    line <- list(geom_line(aes(colour = {{group}}), linewidth = line_width),
                 scale_color_brewer(palette = "Set2", name = NULL))
  }
  else {
    line <- list(geom_line(colour = line_colour, linewidth = line_width))
  }

  plot <- ggplot(data, aes({{x}}, {{y}}, group = {{group}})) +
    line +
    labs(x = xlab,
         y = ylab,
         title = title,
         subtitle = subtitle,
         caption = caption) +
    scale_x_continuous(expand = expansion()) +
    scale_y_continuous(expand = expansion()) +
    expand_limits(y = 0) +
    theme_minimal()

  return(plot)
}
