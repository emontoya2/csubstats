#' Two-Sample Wilcoxon Test with Optional Randomization Test
#'
#' Performs a two-sample Wilcoxon rank-sum test (Mann-Whitney U test) using a formula of the
#' form \code{response ~ explanatory} on data in tidy format. The function computes the test
#' statistic, p-value, and confidence interval for the difference in location (using medians and
#' interquartile ranges). It also optionally computes a standardized test statistic (z-value) and
#' can perform a randomization test by shuffling the response values.
#'
#' @param formula An object of class \code{formula} specifying the model as \code{response ~ explanatory}.
#'   The response variable is the measurement of interest and the explanatory variable is a factor
#'   indicating group membership.
#' @param data A data frame in tidy format containing the variables referenced in \code{formula}.
#' @param first.level A character string specifying which level of the grouping variable should be
#'   treated as the reference. This level determines how the difference in sample medians is computed.
#' @param direction A character string specifying the alternative hypothesis. Must be one of
#'   \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.
#' @param conf.int Logical indicating whether to compute a confidence interval for the difference in
#'   location (default is \code{TRUE}).
#' @param conf.level A numeric value specifying the confidence level for the confidence interval (default is 0.95).
#' @param randtest Logical; if \code{TRUE}, a randomization test is performed by shuffling the data
#'   and recalculating the Wilcoxon test statistic.
#' @param nshuffles An integer specifying the number of randomizations (shuffles) to perform when
#'   \code{randtest} is \code{TRUE}.
#' @param returnRandStats Logical; if \code{TRUE}, the function returns the vector of test statistics
#'   obtained from the randomization test.
#' @param printout Logical; if \code{TRUE} (default), the function prints detailed output including
#'   sample medians, interquartile ranges, test statistics (raw and standardized), p-value, and confidence interval.
#' @param printoutND Logical; if \code{TRUE}, the function returns a histogram plot object of the
#'   randomization test statistics.
#'
#' @details
#' The function begins by trimming whitespace from the \code{first.level} and \code{direction} arguments
#' and validating that \code{direction} is one of \code{"two.sided"}, \code{"greater"}, or \code{"less"}.
#' It then extracts the response and grouping variables from the formula and relevels the grouping factor so
#' that \code{first.level} is the reference. Sample medians and interquartile ranges (IQR) are computed for each
#' group. A Wilcoxon rank-sum test is performed via \code{wilcox.test()} with warnings suppressed, and the
#' test statistic and confidence interval are extracted. When applicable, a standardized z-value is calculated
#' based on the ranking of the observations. If \code{randtest} is \code{TRUE}, the function shuffles the
#' response values repeatedly (as specified by \code{nshuffles}), recomputes the Wilcoxon test statistic, and
#' constructs an empirical null distribution from which an empirical p-value is computed. A histogram of the
#' randomized test statistics is generated for visual inspection.
#'
#' @return This function is primarily called for its side effects (printing output). Optionally, it returns:
#'   \itemize{
#'     \item A vector of randomization test statistics if \code{returnRandStats} is \code{TRUE}.
#'     \item A histogram plot object if \code{printoutND} is \code{TRUE}.
#'   }
#'   Otherwise, no value is explicitly returned.
#'
#' @importFrom mosaic plotDist iqr
#' @importFrom lattice histogram do.breaks panel.xyplot panel.polygon
#' @importFrom graphics hist
#' @importFrom stats wilcox.test
#'
#' @examples
#' \dontrun{
#' # Example data
#' set.seed(123)
#' data_example <- data.frame(
#'   score = c(rnorm(30, mean = 50, sd = 10), rnorm(30, mean = 55, sd = 12)),
#'   group = factor(rep(c("Control", "Treatment"), each = 30))
#' )
#'
#' # Perform the Wilcoxon test (with continuity correction) for a two-sided alternative
#' two.wilcox.test(score ~ group, data = data_example, first.level = "Control",
#'                 direction = "two.sided", conf.int = TRUE, conf.level = 0.95)
#'
#' # Perform a randomization test with 1000 shuffles
#' two.wilcox.test(score ~ group, data = data_example, first.level = "Control",
#'                 direction = "two.sided", conf.int = TRUE, conf.level = 0.95,
#'                 randtest = TRUE, nshuffles = 1000)
#' }
#'
#' @export
two.wilcox.test <- function(formula, data, first.level,
                            direction = c("two.sided", "greater", "less"), conf.int = TRUE,
                            conf.level = .95,
                            randtest = FALSE, nshuffles = NULL, returnRandStats = FALSE, printout = TRUE, printoutND = FALSE) {

  #require(mosaic)

  first.level <- trimws(first.level, which = c("both"), whitespace = "[ \t\r\n]")
  direction <- trimws(direction, which = c("both"), whitespace = "[ \t\r\n]")

  if(!(direction %in% c("greater", "less", "two.sided")))
    stop("Error: Direction must be either 'greater', 'less', or 'two.sided'! Try again :)")

  direction <- trimws(direction, which = c("both"), whitespace = "[ \t\r\n]")

  resp.varName <- all.vars(formula)[1]
  pred.varName <- all.vars(formula)[2]
  if(is.na(pred.varName) | is.na(resp.varName))
    stop("Wrong formula, should be of the form y ~ x or your data is not in tidy or data matrix format.")

  resp <- eval(parse(text = paste0("data$", resp.varName)))
  pred <- eval(parse(text = paste0("data$", pred.varName)))

  n <- dim(data)[1]  # sample size

  tmpidx2 <- which(levels(pred) == first.level)  # determine primary level
  pred <- relevel(pred, levels(pred)[tmpidx2])  # relevel to set first.level as reference
  dataOG <- data.frame(resp = resp, pred = pred)

  respord <- dataOG$resp
  preord <- dataOG$pred

  # Convert group to numeric coding (0 and 1)
  datax <- as.numeric(as.factor(preord)) - 1
  n1 <- sum(datax == 0)
  n2 <- sum(datax == 1)

  # Compute sample medians and IQRs for each group
  tmpa <- median(respord[datax == 0])
  tmpb <- median(respord[datax == 1])
  tmpasd <- iqr(respord[datax == 0])
  tmpbsd <- iqr(respord[datax == 1])

  alternative <- direction

  tresult <- suppressWarnings(wilcox.test(resp ~ pred, data = dataOG, alternative = direction, conf.int = TRUE,
                                          conf.level = conf.level))
  obsTS <- tresult$statistic
  obs.teststat <- obsTS

  first.level <- levels(pred)

  dataOGsim <- dataOG

  # Compute the number of pairwise comparisons (used in later calculations)
  opts <- sum(outer(respord[which(datax == 0)], respord[which(datax == 1)], "<"))

  # Compute a standardized test statistic (z-value) if using the theoretical distribution
  r <- rank(c(dataOG$resp))
  STATISTIC <- obs.teststat
  TIES <- (length(r) != length(unique(r)))
  NTIES <- table(r)
  theta <- 0
  z <- STATISTIC - n1 * n2 * (1/2 + theta)
  SIGMA <- sqrt((n1 * n2 / 12) * ((n1 + n2 + 1) - sum(NTIES^3 - NTIES) / ((n1 + n2) * (n1 + n2 - 1))))
  zteststat <- round(z / SIGMA, 3)

  pvalue <- tresult$p.value
  lb <- tresult$conf.int[1]
  ub <- tresult$conf.int[2]

  n <- dim(data)[1]
  teststat <- tresult$statistic
  minx <- n1 - 5
  maxx <- n1 * n2 + 5

  if(!randtest) {
    if(direction == "greater") {
      if(tresult$method == "Wilcoxon rank sum test with continuity correction") {
        hg <- plotDist('norm', kind = 'density', xlim = c(-3.5, 3.5),
                       panel = function(x, y, ...) {
                         panel.xyplot(x, y, ...)
                         xx <- c(zteststat, x[x >= zteststat & x <= 5], 5)
                         yy <- c(0, y[x >= zteststat & x <= 5], 0)
                         panel.polygon(xx, yy, ..., col = 'blue')
                       })
      } else {
        tmpts <- tresult$statistic
        hg <- plotDist('wilcox', kind = 'density', m = n1, n = n2,
                       panel = function(x, y, ...) {
                         panel.xyplot(x, y, ...)
                         xx <- c(tmpts, x[x >= tmpts & x <= maxx], maxx)
                         yy <- c(0, y[x >= tmpts & x <= maxx], 0)
                         panel.polygon(xx, yy, ..., col = 'aliceblue')
                       })
      }
    }

    if(direction == "less") {
      if(tresult$method == "Wilcoxon rank sum test with continuity correction") {
        hg <- plotDist('norm', kind = 'density', xlim = c(-3.5, 3.5),
                       panel = function(x, y, ...) {
                         panel.xyplot(x, y, ...)
                         xx <- c(-5, x[x >= -5 & x <= zteststat], zteststat)
                         yy <- c(0, y[x >= -5 & x <= zteststat], 0)
                         panel.polygon(xx, yy, ..., col = 'blue')
                       })
      } else {
        tmpts <- tresult$statistic
        hg <- plotDist('wilcox', kind = 'density', m = n1, n = n2,
                       panel = function(x, y, ...) {
                         panel.xyplot(x, y, ...)
                         xx <- c(minx, x[x >= minx & x <= tmpts], tmpts)
                         yy <- c(0, y[x >= minx & x <= tmpts], 0)
                         panel.polygon(xx, yy, ..., col = 'aliceblue')
                       })
      }
    }

    if(direction == "two.sided") {
      if(tresult$method == "Wilcoxon rank sum test with continuity correction") {
        hg <- plotDist('norm', kind = 'density', xlim = c(-3.5, 3.5),
                       panel = function(x, y, ...) {
                         panel.xyplot(x, y, ...)
                         xx <- c(abs(zteststat), x[x >= abs(zteststat) & x <= 5], 5)
                         yy <- c(0, y[x >= abs(zteststat) & x <= 5], 0)
                         panel.polygon(xx, yy, ..., col = 'blue')
                         xx <- c(-5, x[x >= -5 & x <= -abs(zteststat)], -abs(zteststat))
                         yy <- c(0, y[x >= -5 & x <= -abs(zteststat)], 0)
                         panel.polygon(xx, yy, ..., col = 'blue')
                       })
      } else {
        tmptsL <- opts
        tmptsU <- tresult$statistic
        if(tmptsU < tmptsL) {
          tmptsU <- opts
          tmptsL <- tresult$statistic
        }
        hg <- plotDist('wilcox', kind = 'density', m = n1, n = n2,
                       panel = function(x, y, ...) {
                         panel.xyplot(x, y, ...)
                         xx <- c(tmptsU, x[x >= tmptsU & x <= maxx], maxx)
                         yy <- c(0, y[x >= tmptsU & x <= maxx], 0)
                         panel.polygon(xx, yy, ..., col = 'aliceblue')
                         xx <- c(minx, x[x >= minx & x <= tmptsL], tmptsL)
                         yy <- c(0, y[x >= minx & x <= tmptsL], 0)
                         panel.polygon(xx, yy, ..., col = 'aliceblue')
                       })
      }
    }

    cat("     Theoretical-based two-sample test for independent samples", "\n")
    cat("                             ", "\n")

  } else {
    tmpfun <- function(X, datasim, data, alternative) {
      dataOGsim$resp <- sample(data$resp, size = dim(data)[1], replace = FALSE)
      tresultrand <- suppressWarnings(wilcox.test(resp ~ pred, data = dataOGsim, alternative = alternative))
      obsTSrand <- tresultrand$statistic
      return(obsTSrand)
    }

    randstats <- lapply(X = 1:nshuffles, FUN = tmpfun, datasim = dataOGsim, data = dataOG, alternative = direction)
    randstats <- sort(unlist(randstats))

    aboveidx <- randstats >= obs.teststat
    howmanyAbove <- sum(aboveidx)
    greater.pval <- howmanyAbove / nshuffles

    belowidx <- randstats <= obs.teststat
    howmanyBelow <- sum(belowidx)
    less.pval <- howmanyBelow / nshuffles

    tmptsL <- opts
    tmptsU <- tresult$statistic
    if(tmptsU < tmptsL) {
      tmptsU <- opts
      tmptsL <- tresult$statistic
    }

    twsd.aboveidx <- randstats >= tmptsU
    twsd.belowidx <- randstats <= tmptsL
    howmanyAboveBelow <- sum(twsd.aboveidx) + sum(twsd.belowidx)
    two.sided.pval <- howmanyAboveBelow / nshuffles

    htmp <- hist(randstats, plot = FALSE)
    brks <- htmp$breaks
    br1 <- do.breaks(c(abs(obs.teststat), max(brks)), length(brks) / 3)
    br2 <- do.breaks(c(min(brks), -abs(obs.teststat)), length(brks) / 3)
    br3 <- do.breaks(c(-abs(obs.teststat), abs(obs.teststat)), length(brks) / 3)
    brksall <- sort(unique(c(br1, br2, br3)))

    if(direction == "greater") {
      pvalue <- greater.pval
      cat1 <- rep(NA, length(randstats))
      cat1[aboveidx] <- "yes"
      cat1[!aboveidx] <- "no"
      cat2 <- as.factor(cat1)
      cprtmpdf <- data.frame(randstats, cat2)
    }

    if(direction == "less") {
      pvalue <- less.pval
      cat1 <- rep(NA, length(randstats))
      cat1[belowidx] <- "yes"
      cat1[!belowidx] <- "no"
      cat2 <- as.factor(cat1)
      cprtmpdf <- data.frame(randstats, cat2)
    }

    if(direction == "two.sided") {
      pvalue <- two.sided.pval
      cat1 <- rep(NA, length(randstats))
      cat1[twsd.aboveidx] <- "yes"
      cat1[twsd.belowidx] <- "yes"
      cat1[is.na(cat1)] <- "no"
      cat2 <- as.factor(cat1)
      cprtmpdf <- data.frame(randstats, cat2)
    }

    hg <- histogram(~ randstats, type = "count", ylab = "Number of simulations", xlab = "Difference in means",
                    groups = cat2, data = cprtmpdf, breakds = brksall, nint = length(brksall))

    cat("     Simulation based two-sample test for independent samples", "\n")
    cat("                             ", "\n")
  }

  pvalue <- round(pvalue, 3)

  if(printout) {
    cat("formula: ", resp.varName, "~", pred.varName, "\n")
    cat("sample median of ", first.level[1], " group:", tmpa, "\n")
    cat("sample median of ", first.level[2], " group:", tmpb, "\n")
    cat("sample IQR of ", first.level[1], " group:", tmpasd, "\n")
    cat("sample IQR of ", first.level[2], " group:", tmpbsd, "\n")
    cat("\n")
    if(!randtest) {
      cat("method: ", tresult$method, "\n")
    } else {
      cat("method: randomization test", "\n")
    }
    cat("difference between groups: (", first.level[1], " group ) - ( ", first.level[2], " group )", "\n")
    cat("obs test statistic: U= ", obsTS, "            ", "p-value =", pvalue, "\n")
    if(!randtest) {
      cat("obs standardized test statistic: Z= ", zteststat, "\n")
    }
    cat("direction:", direction, "\n")
    cat("\n")

    if(!randtest) {
      cat("difference in location (pseudomedian): ", tresult$estimate, "\n")
      cat("confidence level: ", conf.level, "\n")
      cat("CI: (", lb, ",", ub, ")\n")
    }
  }

  if(returnRandStats)
    return(randstats)

  if(printoutND) {
    return(hg)
  }
}
