#' Games-Howell Post-Hoc Multiple Comparisons Test
#'
#' Performs pairwise comparisons between group means using the Games-Howell procedure,
#' which does not assume equal variances or equal sample sizes. This test is useful
#' for comparing all possible pairs of groups following an ANOVA.
#'
#' @param grp A factor or vector indicating group membership for each observation.
#' @param obs A numeric vector of observations corresponding to the groups in \code{grp}.
#'
#' @details
#' The function calculates the mean difference, standard error, t-statistic, degrees
#' of freedom, and p-value for each pairwise comparison between groups. It also computes
#' the upper and lower confidence limits for the mean differences using the Tukey distribution
#' (\code{ptukey} and \code{qtukey}). These statistics are then compiled into a data frame.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{groups}{A character string indicating the pair of groups compared (formatted as "group1 : group2").}
#'   \item{Mean Difference}{The difference in means between the two groups.}
#'   \item{Standard Error}{The standard error of the mean difference.}
#'   \item{t}{The t-statistic for the comparison.}
#'   \item{df}{The degrees of freedom used in the test.}
#'   \item{p}{The p-value associated with the comparison.}
#'   \item{upper limit}{The upper limit of the confidence interval.}
#'   \item{lower limit}{The lower limit of the confidence interval.}
#' }
#'
#' @source \url{https://gist.github.com/aschleg/ea7942efc6108aedfa9ec98aeb6c2096}
#'
#' @importFrom utils combn
#' @importFrom stats ptukey qtukey
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' set.seed(123)
#' grp <- factor(rep(c("A", "B", "C"), each = 20))
#' obs <- c(rnorm(20, mean = 10, sd = 2),
#'          rnorm(20, mean = 12, sd = 2.5),
#'          rnorm(20, mean = 14, sd = 3))
#' result <- games.howell(grp, obs)
#' print(result)
#' }
#'
#' @export
games.howell <- function(grp, obs) {
# source: https://gist.github.com/aschleg/ea7942efc6108aedfa9ec98aeb6c2096


  #Create combinations
  combs <- combn(unique(grp), 2)

  # Statistics that will be used throughout the calculations:
  # n = sample size of each group
  # groups = number of groups in data
  # Mean = means of each group sample
  # std = variance of each group sample
  n <- tapply(obs, grp, length)
  groups <- length(tapply(obs, grp, length))
  Mean <- tapply(obs, grp, mean)
  std <- tapply(obs, grp, var)

  statistics <- lapply(1:ncol(combs), function(x) {

    mean.diff <- Mean[combs[1,x]] - Mean[combs[2,x]]

    #t-values
    t <- abs(Mean[combs[2,x]] - Mean[combs[1,x]]) / sqrt((std[combs[1,x]] / n[combs[1,x]]) + (std[combs[2,x]] / n[combs[2,x]]))

    # Degrees of Freedom
    df <- (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]])^2 / # Numerator Degrees of Freedom
      ((std[combs[1,x]] / n[combs[1,x]])^2 / (n[combs[1,x]] - 1) + # Part 1 of Denominator Degrees of Freedom
         (std[combs[2,x]] / n[combs[2,x]])^2 / (n[combs[2,x]] - 1)) # Part 2 of Denominator Degrees of Freedom

    #p-values
    p <- ptukey(t * sqrt(2), groups, df, lower.tail = FALSE)

    # Sigma standard error
    se <- sqrt(0.5 * (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]]))

    # Upper Confidence Limit
    upper.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff + qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]

    # Lower Confidence Limit
    lower.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff - qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]

    # Group Combinations
    grp.comb <- paste(combs[1,x], ':', combs[2,x])

    # Collect all statistics into list
    stats <- list(grp.comb, mean.diff, se, t, df, p, upper.conf, lower.conf)
  })

  # Unlist statistics collected earlier
  stats.unlisted <- lapply(statistics, function(x) {
    unlist(x)
  })

  # Create dataframe from flattened list
  results <- data.frame(matrix(unlist(stats.unlisted), nrow = length(stats.unlisted), byrow=TRUE))

  # Select columns set as factors that should be numeric and change with as.numeric
  results[c(2, 3:ncol(results))] <- round(as.numeric(as.matrix(results[c(2, 3:ncol(results))])), digits = 3)

  # Rename data frame columns
  colnames(results) <- c('groups', 'Mean Difference', 'Standard Error', 't', 'df', 'p', 'upper limit', 'lower limit')

  return(results)
}
