#' Two-Sample t-Test with Optional Randomization Test
#'
#' This function computes a two-sample t-test based on a formula of the form
#' \code{response ~ explanatory} using data in tidy format. It calculates the test
#' statistic, p-value, and confidence interval using either Welch's t-test (default)
#' or Student's t-test. In addition, it can perform a randomization test by shuffling
#' the response values and recalculating the t-test statistic.
#'
#' @param formula An object of class \code{formula} specifying the model as
#'   \code{response ~ explanatory}. The response should be the measured variable
#'   and the explanatory should be a factor indicating group membership.
#' @param data A data frame in tidy format containing the variables referenced in
#'   \code{formula}.
#' @param first.level A character string specifying the level from the grouping
#'   variable to be used as the reference. The difference in sample means is computed
#'   accordingly.
#' @param welch Logical indicating whether to use Welch's t-test (default is
#'   \code{TRUE}). If \code{FALSE}, the function uses Student's t-test assuming equal
#'   variances.
#' @param direction A character string specifying the alternative hypothesis.
#'   Must be one of \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.
#' @param conf.level A numeric value specifying the confidence level for the
#'   confidence interval (default is 0.95).
#' @param randtest Logical; if \code{TRUE}, a randomization test is performed using
#'   the Welch t-test statistic.
#' @param nshuffles An integer specifying the number of randomizations (shuffles)
#'   to perform when \code{randtest} is \code{TRUE}.
#' @param returnRandStats Logical; if \code{TRUE}, the function returns the vector of
#'   test statistics obtained from the randomization test.
#' @param printout Logical; if \code{TRUE} (default), the function prints detailed
#'   output including group means, standard deviations, test statistics, degrees of
#'   freedom, p-value, and confidence interval.
#' @param printoutND Logical; if \code{TRUE}, the function returns a histogram plot
#'   object of the randomization test statistics.
#'
#' @details
#' The function first processes the input formula and data, ensuring that the
#' grouping variable is re-leveled so that the specified \code{first.level} is used as
#' the reference. It then uses \code{t.test()} to perform the t-test with the chosen
#' settings. If \code{randtest} is set to \code{TRUE}, a randomization test is conducted
#' by shuffling the response variable and recalculating the test statistic over the
#' specified number of shuffles (\code{nshuffles}). The mosaic package is required for
#' some of the internal functions (e.g., \code{plotDist} and \code{histogram}).
#'
#' @return This function is primarily called for its side effects (printing output
#'   to the console). Optionally, it can return:
#'   \itemize{
#'     \item A vector of randomization test statistics if \code{returnRandStats} is
#'           \code{TRUE}.
#'     \item A histogram plot object (from \code{plotDist}) if \code{printoutND} is
#'           \code{TRUE}.
#'   }
#'   Otherwise, no value is explicitly returned.
#'
#' @importFrom mosaic plotDist
#' @importFrom lattice histogram do.breaks panel.xyplot panel.polygon
#' @importFrom stats ptukey qtukey qnorm kruskal.test relevel sd t.test wilcox.test median
#' @importFrom graphics hist
#'
#'
#' @examples
#' \dontrun{
#' # Create example data
#' set.seed(123)
#' group <- factor(rep(c("Control", "Treatment"), each = 30))
#' response <- c(rnorm(30, mean = 5, sd = 1), rnorm(30, mean = 6, sd = 1.2))
#' data_example <- data.frame(response = response, group = group)
#'
#' # Perform Welch's t-test (default) with a two-sided alternative
#' two.mean.test(response ~ group, data = data_example, first.level = "Control",
#'               welch = TRUE, direction = "two.sided", conf.level = 0.95)
#'
#' # Perform a randomization test with 1000 shuffles and return the random statistics
#' rand_stats <- two.mean.test(response ~ group, data = data_example, first.level = "Control",
#'                             welch = TRUE, direction = "two.sided", conf.level = 0.95,
#'                             randtest = TRUE, nshuffles = 1000, returnRandStats = TRUE)
#'
#' # Obtain the histogram plot of the randomization test statistics
#' hist_plot <- two.mean.test(response ~ group, data = data_example, first.level = "Control",
#'                            welch = TRUE, direction = "two.sided", conf.level = 0.95,
#'                            randtest = TRUE, nshuffles = 1000, printoutND = TRUE)
#' }
#'
#' @export
two.mean.test <- function(formula, data, first.level, welch = TRUE,
                          direction = c("two.sided", "greater", "less"), conf.level = .95,
                          randtest = FALSE, nshuffles = NULL, returnRandStats = FALSE, printout = TRUE, printoutND = FALSE) {
  # Description: Function to compute the test statistic, null distribution, p-value,
  # and confidence interval for a two-sample t-based method. It also allows for a randomization
  # test using Welch's t-test statistic.

  #require(mosaic)

  var.equal = TRUE
  if (welch)
    var.equal = FALSE

  first.level <- trimws(first.level, which = c("both"), whitespace = "[ \t\r\n]")
  direction <- trimws(direction, which = c("both"), whitespace = "[ \t\r\n]")

  if (!(direction %in% c("greater", "less", "two.sided")))
    stop("Error: Direction must be either 'greater', 'less', or 'two.sided'! Try again :)")

  resp.varName <- all.vars(formula)[1]
  pred.varName <- all.vars(formula)[2]
  if (is.na(pred.varName) | is.na(resp.varName))
    stop("Wrong formula, should be of the form y ~ x or your data is not in tidy or data matrix format.")

  resp <- eval(parse(text = paste0("data$", resp.varName)))
  pred <- eval(parse(text = paste0("data$", pred.varName)))

  n <- dim(data)[1]  # sample size

  tmpidx2 = which(levels(pred) == first.level)  # determine primary level
  pred <- relevel(pred, levels(pred)[tmpidx2])  # relevel with first.level as reference
  dataOG <- data.frame(resp = resp, pred = pred)

  respord <- dataOG$resp
  preord <- dataOG$pred

  # Convert group to numeric coding (0 and 1)
  datax <- as.numeric(as.factor(preord)) - 1
  n1 <- sum(datax == 0)
  n2 <- sum(datax == 1)

  tmpa <- mean(respord[datax == 0])
  tmpb <- mean(respord[datax == 1])
  tmpasd <- sd(respord[datax == 0])
  tmpbsd <- sd(respord[datax == 1])

  alternative <- direction

  tresult <- t.test(resp ~ pred, data = dataOG, alternative = direction,
                    conf.level = conf.level, var.equal = var.equal)
  obsTS <- tresult$statistic
  obs.teststat <- obsTS

  first.level <- levels(pred)

  df <- c(unlist(tresult$parameter))
  if (randtest)
    df <- "N/A"

  dataOGsim <- dataOG

  if (randtest) {
    tmpfun <- function(X, datasim, data, alternative) {
      dataOGsim$resp <- sample(data$resp, size = dim(data)[1], replace = FALSE)
      tresultrand <- t.test(resp ~ pred, data = dataOGsim, alternative = alternative,
                            var.equal = var.equal)
      obsTSrand <- tresultrand$statistic
      return(obsTSrand)
    }

    randstats <- lapply(X = 1:nshuffles, FUN = tmpfun, datasim = dataOGsim, data = dataOG, alternative = direction)
    randstats <- sort(unlist(randstats))

    aboveidx <- randstats >= obs.teststat
    howmanyAbove <- sum(aboveidx)
    greater.pval <- howmanyAbove / nshuffles  # one-sided p-value

    belowidx <- randstats <= obs.teststat
    howmanyBelow <- sum(belowidx)
    less.pval <- howmanyBelow / nshuffles  # one-sided p-value

    twsd.aboveidx <- randstats >= abs(obs.teststat)
    twsd.belowidx <- randstats <= -abs(obs.teststat)
    howmanyAboveBelow <- sum(twsd.aboveidx) + sum(twsd.belowidx)
    two.sided.pval <- howmanyAboveBelow / nshuffles  # two-sided p-value

    htmp <- hist(randstats, plot = FALSE)
    brks <- htmp$breaks
    br1 <- do.breaks(c(abs(obs.teststat), max(brks)), length(brks)/3)
    br2 <- do.breaks(c(min(brks), -abs(obs.teststat)), length(brks)/3)
    br3 <- do.breaks(c(-abs(obs.teststat), abs(obs.teststat)), length(brks)/3)
    brksall <- sort(unique(c(br1, br2, br3)))

    if (direction == "greater") {
      pvalue <- greater.pval
      cat1 <- rep(NA, length(randstats))
      cat1[aboveidx] <- "yes"
      cat1[!aboveidx] <- "no"
      cat2 <- as.factor(cat1)
      cprtmpdf <- data.frame(randstats, cat2)
    }

    if (direction == "less") {
      pvalue <- less.pval
      cat1 <- rep(NA, length(randstats))
      cat1[belowidx] <- "yes"
      cat1[!belowidx] <- "no"
      cat2 <- as.factor(cat1)
      cprtmpdf <- data.frame(randstats, cat2)
    }

    if (direction == "two.sided") {
      pvalue <- two.sided.pval
      cat1 <- rep(NA, length(randstats))
      cat1[twsd.aboveidx] <- "yes"
      cat1[twsd.belowidx] <- "yes"
      cat1[is.na(cat1)] <- "no"
      cat2 <- as.factor(cat1)
      cprtmpdf <- data.frame(randstats, cat2)
    }

    hg <- histogram(~ randstats, type = "count", ylab = "Number of simulations",
                    xlab = "Difference in means", groups = cat2, data = cprtmpdf,
                    breakds = brksall, nint = length(brksall))

    cat("     Simulation based two-sample test for independent samples", "\n")
    cat("\n")

  } else {
    tresultCI <- t.test(resp ~ pred, data = dataOG, alternative = "two.sided",
                        conf.level = conf.level, var.equal = var.equal)

    if (direction == "greater")
      pvalue <- tresult$p.value
    if (direction == "less")
      pvalue <- tresult$p.value
    if (direction == "two.sided")
      pvalue <- tresult$p.value

    lb <- tresultCI$conf.int[1]
    ub <- tresultCI$conf.int[2]

    eval(parse(text = paste0("data$", pred.varName, " = factor(data$", pred.varName, ")")))
    pred <- eval(parse(text = paste0("data$", pred.varName)))

    n <- dim(data)[1]
    teststat <- tresult$statistic
    if (direction == "greater") {
      hg <- plotDist('t', df = df, kind = 'density', xlim = c(-4.5, 4.5),
                     panel = function(x, y, ...) {
                       panel.xyplot(x, y, ...)
                       xx <- c(teststat, x[x >= teststat & x <= 5], 5)
                       yy <- c(0, y[x >= teststat & x <= 5], 0)
                       panel.polygon(xx, yy, ..., col = 'blue')
                     })
    }
    if (direction == "less") {
      hg <- plotDist('t', df = df, kind = 'density', xlim = c(-4.5, 4.5),
                     panel = function(x, y, ...) {
                       panel.xyplot(x, y, ...)
                       xx <- c(-5, x[x >= -5 & x <= teststat], teststat)
                       yy <- c(0, y[x >= -5 & x <= teststat], 0)
                       panel.polygon(xx, yy, ..., col = 'blue')
                     })
    }
    if (direction == "two.sided") {
      hg <- plotDist('t', df = df, kind = 'density', xlim = c(-4.5, 4.5),
                     panel = function(x, y, ...) {
                       panel.xyplot(x, y, ...)
                       xx <- c(abs(teststat), x[x >= abs(teststat) & x <= 5], 5)
                       yy <- c(0, y[x >= abs(teststat) & x <= 5], 0)
                       panel.polygon(xx, yy, ..., col = 'blue')
                       xx <- c(-5, x[x >= -5 & x <= -abs(teststat)], -abs(teststat))
                       yy <- c(0, y[x >= -5 & x <= -abs(teststat)], 0)
                       panel.polygon(xx, yy, ..., col = 'blue')
                     })
    }

    cat("     Theoretical-based two-sample test for independent samples", "\n")
    cat("\n")
  }

  pvalue <- round(pvalue, 3)

  if (printout) {
    cat("formula: ", resp.varName, "~", pred.varName, "\n")
    cat("sample mean of ", first.level[1], " group:", tresult[[5]][1], "\n")
    cat("sample mean of ", first.level[2], " group:", tresult[[5]][2], "\n")
    cat("sample sd of ", first.level[1], " group:", tmpasd, "\n")
    cat("sample sd of ", first.level[2], " group:", tmpbsd, "\n")
    cat("\n")
    cat("difference between groups: (", first.level[1], " group ) - ( ", first.level[2], " group )", "\n")
    cat("obs t-test statistic:", obsTS, "            ", "p-value =", pvalue, "\n")
    cat("df= ", df, "\n")
    cat("direction:", direction, "\n")
    cat("\n")

    if (!randtest) {
      cat("Confidence level:", conf.level, "\n")
      cat("CI: (", lb, ",", ub, ")\n")
    }
  }

  if (returnRandStats)
    return(randstats)

  if (printoutND) {
    return(hg)
  }
}

