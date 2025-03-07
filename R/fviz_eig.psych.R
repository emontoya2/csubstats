#' Scree Plot Visualization for Psych Analysis
#'
#' Creates a scree plot from an object containing eigenanalysis results (typically from a
#' factor analysis or principal component analysis). The function plots either the eigenvalues
#' or the percentage of explained variance using \code{ggplot2} and allows customization via
#' bar and/or line geometries.
#'
#' @param X An object containing eigenanalysis results with a \code{Vaccounted} component.
#'   \code{X$Vaccounted} should be a matrix with three rows, where the first row contains
#'   eigenvalues and the second row contains the proportion of variance explained (which will
#'   be multiplied by 100 to obtain percentages).
#' @param choice A character string specifying the measure to plot. Either \code{"variance"}
#'   (to plot the percentage of explained variance) or \code{"eigenvalue"} (to plot the raw
#'   eigenvalues). Default is \code{c("variance", "eigenvalue")} (the first element is used).
#' @param geom A character vector indicating the type(s) of geometric objects to use in the plot.
#'   Allowed values are \code{"bar"} and/or \code{"line"}. Default is \code{c("bar", "line")}.
#' @param barfill A color specification for the fill of bars when \code{"bar"} is used.
#'   Default is \code{"steelblue"}.
#' @param barcolor A color specification for the border of bars. Default is \code{"steelblue"}.
#' @param linecolor A color specification for the line and points when \code{"line"} is used.
#'   Default is \code{"black"}.
#' @param ncp An integer specifying the maximum number of components (dimensions) to plot.
#'   Default is \code{10}.
#' @param addlabels Logical indicating whether to add numeric labels (eigenvalues or percentages)
#'   to the plot. Default is \code{FALSE}.
#' @param hjust A numeric value for horizontal justification of the text labels. Default is \code{0}.
#' @param main A character string specifying the main title of the plot. If \code{NULL}, defaults to
#'   \code{"Scree plot"}.
#' @param xlab A character string for the x-axis label. If \code{NULL}, defaults to \code{"Dimensions"}.
#' @param ylab A character string for the y-axis label. If \code{NULL}, it defaults to
#'   \code{"Eigenvalue"} when \code{choice = "eigenvalue"} or \code{"Percentage of explained variances"}
#'   when \code{choice = "variance"}.
#' @param ggtheme A \code{ggplot2} theme object to apply to the plot. Default is \code{theme_minimal()}.
#' @param ... Additional arguments passed to \code{ggpubr::ggpar} for further customization.
#'
#' @details
#' The function first scales the second and third rows of \code{X$Vaccounted} by 100 to convert
#' proportions into percentages. It then transposes the first three rows of \code{X$Vaccounted} and
#' limits the number of components plotted to the minimum of \code{ncp} and the total available. Based
#' on the \code{choice} parameter, either the eigenvalues or the percentage of explained variance is
#' extracted and corresponding text labels are created. The function constructs a data frame of the eigen
#' information and uses \code{ggplot2} to generate the plot. Depending on the values in \code{geom}, a
#' bar plot and/or line plot is added. Optional text labels can be added, and axis labels as well as the
#' plot title are set with default values if not provided. Finally, the \code{ggpubr::ggpar} function is
#' called to apply the specified theme and additional graphical parameters.
#'
#' @return A \code{ggplot} object representing the scree plot.
#'
#' @importFrom ggplot2 ggplot theme_minimal aes geom_bar geom_line geom_point geom_text labs
#' @importFrom ggpubr ggpar
#'
#' @examples
#' \dontrun{
#' # Assume fa_result is an object from a factor analysis (e.g., from the psych package)
#' # with a Vaccounted component.
#'
#' # Plot the percentage of explained variance using both bar and line geometries.
#' fviz_eig.psych(fa_result, choice = "variance", geom = c("bar", "line"))
#'
#' # Plot raw eigenvalues with only a line plot, and customize the line color.
#' fviz_eig.psych(fa_result, choice = "eigenvalue", geom = "line", linecolor = "red")
#' }
#'
#' @export
fviz_eig.psych <- function (X, choice = c("variance", "eigenvalue"), geom = c("bar", "line"),
                            barfill = "steelblue", barcolor = "steelblue", linecolor = "black", ncp = 10,
                            addlabels = FALSE, hjust = 0, main = NULL, xlab = NULL, ylab = NULL,
                            ggtheme = theme_minimal(), ...)
{

  #require(ggplot2)
  X$Vaccounted[2:3,] <- X$Vaccounted[2:3,] * 100
  eig <- t(X$Vaccounted[1:3,])
  eig <- eig[1:min(ncp, nrow(eig)), , drop = FALSE]
  choice <- choice[1]
  if (choice == "eigenvalue") {
    eig <- eig[, 1]
    text_labels <- round(eig, 1)
    if (is.null(ylab))
      ylab <- "Eigenvalue"
  }
  else if (choice == "variance") {
    eig <- eig[, 2]
    text_labels <- paste0(round(eig, 1), "%")
  }
  else stop("Allowed values for the argument choice are : 'variance' or 'eigenvalue'")
  if (length(intersect(geom, c("bar", "line"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  df.eig <- data.frame(dim = factor(1:length(eig)), eig = eig)
  extra_args <- list(...)
  bar_width <- extra_args$bar_width
  linetype <- extra_args$linetype
  if (is.null(linetype))
    linetype <- "solid"
  p <- ggplot(df.eig, aes(dim, eig, group = 1))
  if ("bar" %in% geom)
    p <- p + geom_bar(stat = "identity", fill = barfill,
                      color = barcolor, width = bar_width)
  if ("line" %in% geom)
    p <- p + geom_line(color = linecolor, linetype = linetype) +
    geom_point(shape = 19, color = linecolor)
  if (addlabels)
    p <- p + geom_text(label = text_labels, vjust = -0.4,
                       hjust = hjust)
  if (is.null(main))
    main <- "Scree plot"
  if (is.null(xlab))
    xlab <- "Dimensions"
  if (is.null(ylab))
    ylab <- "Percentage of explained variances"
  p <- p + labs(title = main, x = xlab, y = ylab)
  ggpar(p, ggtheme = ggtheme, ...)
}
