#' Normal Q-Q Plot Using Lattice
#'
#' Creates a normal quantile-quantile (Q-Q) plot to assess the normality of a variable.
#' The function utilizes the \code{qqmath()} function from the \code{lattice} package. If the
#' \code{formula} contains two variables, a conditional Q-Q plot is produced with the second
#' variable serving as the grouping factor.
#'
#' @param formula A formula specifying the variable(s) to plot. If a single variable is provided
#'   (e.g., \code{~ x}), a single Q-Q plot is produced. If two variables are provided (e.g.,
#'   \code{x ~ g}), the first variable (\code{x}) is the response variable and the second (\code{g})
#'   is a grouping variable used for conditioning the plot.
#' @param data An optional data frame containing the variables referenced in \code{formula}. If not
#'   provided, the variables are expected to exist in the global environment.
#' @param ylab A character string specifying the label for the y-axis. Default is \code{"Sample quantiles"}.
#' @param main A character string specifying the main title for the plot. Default is \code{NULL}.
#'
#' @details
#' The function first determines whether the \code{formula} contains one or two variables.
#' If two variables are provided, it evaluates both the response and grouping variables from the
#' supplied data frame (or the global environment if \code{data} is \code{NULL}) and creates a
#' conditional Q-Q plot using \code{qqmath(~ resp | gvar, ...)}. If only one variable is provided,
#' a standard Q-Q plot is created using \code{qqmath(~ resp, ...)}. In both cases, the plot includes
#' a reference line (via \code{panel.qqmathline()}) to help assess normality.
#'
#' @return A \code{trellis} object (from the \code{lattice} package) representing the Q-Q plot.
#'
#' @importFrom lattice qqmath panel.qqmathline panel.qqmath
#' @importFrom stats qnorm
#'
#' @examples
#' \dontrun{
#' # Single Q-Q plot for a numeric vector 'x'
#' normqqplot(~ x, data = data.frame(x = rnorm(100)))
#'
#' # Conditional Q-Q plot for a numeric vector 'x' grouped by factor 'g'
#' normqqplot(x ~ g, data = data.frame(x = rnorm(100), g = rep(letters[1:2], each = 50)))
#' }
#'
#' @export
normqqplot <- function(formula, data = NULL, ylab = "Sample quantiles", main = NULL) {

  if (length(all.vars(formula)) == 2) {
    varName <- all.vars(formula)[1]
    gvarName <- all.vars(formula)[2]

    if (!is.null(data)) {
      resp <- eval(parse(text = paste0("data$", varName)))
      gvar <- eval(parse(text = paste0("data$", gvarName)))
    } else {
      resp <- eval(parse(text = paste0(varName)))
      gvar <- eval(parse(text = paste0(gvarName)))
    }

    qqmath(~ resp | gvar, ylab = ylab, main = main, distribution = qnorm,
           xlab = "Normal quantiles", data,
           panel = function(...) {
             panel.qqmathline(...)
             panel.qqmath(...)
           })
  } else {
    gvar <- NULL
    varName <- all.vars(formula)

    if (!is.null(data)) {
      resp <- eval(parse(text = paste0("data$", varName)))
    } else {
      resp <- eval(parse(text = paste0(varName)))
    }

    qqmath(~ resp, ylab = ylab, main = main, distribution = qnorm,
           xlab = "Normal quantiles", data,
           panel = function(...) {
             panel.qqmathline(...)
             panel.qqmath(...)
           })
  }
}
