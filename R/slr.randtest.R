#' Randomization Test for Simple Linear Regression
#'
#' This function performs a randomization (permutation) test for a linear association
#' in a simple linear regression model. It fits a linear model using the formula
#' \code{response ~ explanatory} and calculates the observed t-statistic for the slope.
#' Then, it randomly shuffles the response variable a specified number of times to generate
#' a null distribution of t-statistics. The p-value is computed based on the proportion of
#' permuted t-statistics that meet the criteria specified by the alternative hypothesis.
#'
#' @param formula An object of class \code{formula} specifying the model in the form
#'   \code{response ~ explanatory}. The response variable is the outcome and the explanatory
#'   variable is the predictor.
#' @param data A data frame containing the variables referenced in \code{formula}.
#'   The data must be in tidy or matrix format.
#' @param nshuffles An integer specifying the number of random permutations to perform.
#'   If not provided or set to 0, it defaults to 10. Must be a positive whole number.
#' @param direction A character string specifying the alternative hypothesis.
#'   Must be one of \code{"greater"}, \code{"less"}, or \code{"two.sided"}.
#' @param plt Logical; if \code{TRUE}, a histogram of the permuted t-statistics is generated
#'   using the \code{lattice} package and returned.
#'
#' @details
#' The function first fits a linear model using \code{lm()} to compute the observed t-statistic
#' for the slope coefficient, along with the slope estimate and R-squared value. A permutation test
#' is then conducted by shuffling the response variable and recalculating the t-statistic for the slope
#' coefficient in each permutation. Depending on the specified \code{direction}, the p-value is computed
#' as the proportion of permuted t-statistics that are greater than, less than, or as extreme as the
#' observed t-statistic. If \code{plt = TRUE}, the function produces a histogram plot of the randomization
#' distribution.
#'
#' @return If \code{plt} is \code{TRUE}, a histogram plot (a \code{trellis} object from the \code{lattice} package)
#'   is printed and returned. Otherwise, the function prints the observed test statistic, slope estimate,
#'   R-squared, and p-value to the console, and does not return a value.
#'
#' @importFrom lattice histogram do.breaks
#' @importFrom graphics hist
#' @importFrom stats lm coef
#'
#' @examples
#' \dontrun{
#' # Example data
#' set.seed(123)
#' data_example <- data.frame(
#'   y = rnorm(50, mean = 10, sd = 2),
#'   x = rnorm(50, mean = 5, sd = 1)
#' )
#'
#' # Perform the randomization test with 1000 shuffles and no plot
#' slr.randtest(y ~ x, data = data_example, nshuffles = 1000, direction = "two.sided", plt = FALSE)
#'
#' # Perform the randomization test with a histogram plot of the permutation distribution
#' slr.randtest(y ~ x, data = data_example, nshuffles = 1000, direction = "greater", plt = TRUE)
#' }
#'
#' @export
slr.randtest <- function(formula, data = NULL, nshuffles = 0, direction = c("greater", "less", "two.sided"),
                         plt = FALSE) {

  if(length(nshuffles) == 0)
    nshuffles <- 10

  if(nshuffles < 1 || nshuffles %% 1 != 0)
    stop("Error: number of reps must be a positive whole number! --- Try again :)")

  if(!(direction %in% c("greater", "less", "two.sided")))
    stop("Error: Direction must be either 'greater', 'less', or 'two.sided'! --- Try again :)")

  direction <- trimws(direction, which = "both", whitespace = "[ \t\r\n]")

  resp.varName <- all.vars(formula)[1]
  pred.varName <- all.vars(formula)[2]

  if(is.na(pred.varName) || is.na(resp.varName))
    stop("Wrong formula, should be of the form y ~ x, or your data is not in tidy or data matrix format.")

  resp <- eval(parse(text = paste0("data$", resp.varName)))
  pred <- eval(parse(text = paste0("data$", pred.varName)))

  lmfit <- lm(resp ~ pred)
  tmpsum <- summary(lmfit)
  obs.teststat <- tmpsum$coefficients[2, 3]
  l2.coefs <- coef(lmfit)
  r.squared <- tmpsum$r.squared

  tmpfun <- function(idx, resp, pred) {
    resp2 <- sample(resp)
    lmfitsim <- lm(resp2 ~ pred)
    lmsumfitlmfitsim <- summary(lmfitsim)
    TSobssim <- lmsumfitlmfitsim$coefficients[2, 3]
    return(TSobssim)
  }

  randstats <- lapply(1:nshuffles, tmpfun, resp = resp, pred = pred)
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
  two.sided.pval <- howmanyAboveBelow / nshuffles

  if(direction == "greater"){
    pvalue <- greater.pval
  }

  if(direction == "less"){
    pvalue <- less.pval
  }

  if(direction == "two.sided"){
    pvalue <- two.sided.pval
  }

  if(plt) {
    #require(lattice)

    htmp <- hist(randstats, plot = FALSE)
    brks <- htmp$breaks

    br1 <- do.breaks(c(abs(obs.teststat), max(brks)), length(brks) / 3)
    br2 <- do.breaks(c(min(brks), -abs(obs.teststat)), length(brks) / 3)
    br3 <- do.breaks(c(-abs(obs.teststat), abs(obs.teststat)), length(brks) / 3)
    brksall <- sort(unique(c(br1, br2, br3)))

    if(direction == "greater") {
      cat1 <- rep(NA, length(randstats))
      cat1[aboveidx] <- "yes"
      cat1[!aboveidx] <- "no"
      cat2 <- as.factor(cat1)
      cprtmpdf <- data.frame(randstats, cat2)
    }

    if(direction == "less") {
      cat1 <- rep(NA, length(randstats))
      cat1[belowidx] <- "yes"
      cat1[!belowidx] <- "no"
      cat2 <- as.factor(cat1)
      cprtmpdf <- data.frame(randstats, cat2)
    }

    if(direction == "two.sided") {
      cat1 <- rep(NA, length(randstats))
      cat1[twsd.aboveidx] <- "yes"
      cat1[twsd.belowidx] <- "yes"
      cat1[is.na(cat1)] <- "no"
      cat2 <- as.factor(cat1)
      cprtmpdf <- data.frame(randstats, cat2)
    }

    hg <- histogram(~ randstats, type = "count", ylab = "Number of simulations",
                    xlab = "Slope estimate", groups = cat2, data = cprtmpdf,
                    breakds = brksall, nint = length(brksall))
    print(hg)
    return(hg)
  }

  cat("Randomization test for a linear association in SLR", "\n")
  cat("data: ", resp.varName, "~", pred.varName, "\n")
  cat("Obs. test statistic = ", obs.teststat, "\n")
  cat("Obs. slope estimate = ", l2.coefs[2], "\n")
  cat("R-squared = ", r.squared, "\n")
  cat("p-value = ", pvalue, "\n\n")
}
