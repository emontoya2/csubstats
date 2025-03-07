#' One-Way Analysis of Means with Optional Multiple Comparisons
#'
#' This function performs a one-way analysis of means using a specified formula and dataset.
#' It computes an ANOVA table assuming equal variances by default, but can also perform Welch's ANOVA
#' when variances are not assumed equal. Additionally, if requested via the \code{PWC} parameter, the function
#' conducts post-hoc pairwise comparisons using Tukey's HSD test (for equal variances) or the Games-Howell test
#' (for unequal variances).
#'
#' @param formula A formula object of the form \code{response ~ predictor} that specifies the dependent and independent variables.
#' @param data A data frame containing the variables specified in the formula.
#' @param conf.level A numeric value between 0 and 1 indicating the confidence level for the multiple comparisons (default is 0.95).
#' @param var.equal Logical value indicating whether equal variances across groups are assumed (default is \code{TRUE}).
#'                  If set to \code{FALSE}, Welch's ANOVA is performed.
#' @param PWC Logical value indicating whether to perform post-hoc pairwise comparisons (default is \code{FALSE}).
#'            When \code{TRUE}, Tukey's HSD test is applied if variances are assumed equal, and the Games-Howell test is used otherwise.
#'
#' @return The function prints the ANOVA table, R-squared value, and (if requested) the results of the multiple comparisons to the console.
#'         It does not return a value.
#'
#' @details The function first checks that the provided formula contains exactly two variables.
#'          It then extracts and evaluates the corresponding variables from the provided data frame.
#'          A standard ANOVA is performed using \code{aov()}. If \code{var.equal = FALSE}, Welch's ANOVA is computed using \code{oneway.test()}.
#'          The R-squared value is calculated as the ratio of the treatment sum of squares to the error sum of squares.
#'          When \code{PWC} is \code{TRUE}, the function computes multiple comparisons:
#'          \itemize{
#'            \item If \code{var.equal = TRUE}, Tukey's HSD test is performed.
#'            \item If \code{var.equal = FALSE}, the Games-Howell test is applied.
#'          }
#'
#' @importFrom stats aov oneway.test TukeyHSD model.tables
#'
#' @examples
#' # Example using the built-in iris dataset with equal variances assumption
#' sfaov(Sepal.Length ~ Species, data = iris)
#'
#' # Example using Welch's ANOVA and performing post-hoc comparisons with the Games-Howell test
#' sfaov(Sepal.Length ~ Species, data = iris, var.equal = FALSE, PWC = TRUE)
#'
#' @export
sfaov <- function(formula, data, conf.level = .95, var.equal = TRUE, PWC = FALSE) {

  # Check that formula contains exactly two variables
  if (length(all.vars(formula)) != 2) {
    stop("Wrong formula: the formula should be of the form 'response ~ predictor'.
         If you are trying to include more covariates, then use aov() or lm().")
  }

  # Extract variable names from formula
  resp.varName <- all.vars(formula)[1]
  pred.varName <- all.vars(formula)[2]

  # Evaluate the variables within the provided data
  resp <- eval(parse(text = paste0("data$", resp.varName)))
  pred <- eval(parse(text = paste0("data$", pred.varName)))

  # Perform standard ANOVA using aov()
  aovfitobj <- aov(resp ~ pred)
  aovfit <- summary(aovfitobj)

  pvalue <- round(aovfit[[1]]$`Pr(>F)`[1], 3)
  obs.teststat <- round(aovfit[[1]]$`F value`[1], 3)
  dfMST <- aovfit[[1]]$Df[2]

  # If equal variances are not assumed, perform Welch's ANOVA
  if (!var.equal) {
    welchaovfit <- oneway.test(resp ~ pred, var.equal = var.equal)
    obs.teststat <- round(welchaovfit$statistic, 3)
    dfMST <- round(welchaovfit$parameter[2], 3)
    pvalue <- round(welchaovfit$p.value, 3)
  }

  n <- nrow(data)  # sample size

  if (var.equal) {
    cat("One-way analysis of means (assuming equal variances)", "\n")
  } else {
    cat("One-way analysis of means (not assuming equal variances)", "\n")
  }
  cat("\n")

  cat("data: ", resp.varName, "~", pred.varName, "\n")
  cat("============== \n")
  cat("ANOVA Table \n")

  # Format the ANOVA table for printing
  aovfit2 <- aovfit
  aovfit2[[1]]$`Pr(>F)`[1] <- round(pvalue, 3)
  names(aovfit2[[1]]) <- c(names(aovfit2[[1]])[1:4], "p-value")
  aovfit2[[1]]$Df[2] <- dfMST
  aovfit2[[1]]$`F value`[1] <- obs.teststat

  rownames(aovfit2[[1]])[1] <- "Treatment"
  rownames(aovfit2[[1]])[2] <- "Error"

  print(aovfit2[[1]])

  cat("============== \n\n")

  SumSqs <- aovfit[[1]]$`Sum Sq`
  rsqrd <- round(SumSqs[1] / SumSqs[2], 3)

  cat("R-squared= ", rsqrd, "\n\n")

  if (!var.equal)
    cat("Note: The Df for the Error component, F value and p-value provided are for Welch's F-test", "\n")

  # Multiple comparisons if PWC is TRUE
  if (PWC) {
    samplemeansbylevel <- model.tables(aovfitobj, "means")
    samplemeansbylevel <- c(round(samplemeansbylevel$tables$pred, 2))

    if (var.equal) {
      tukeyresult <- TukeyHSD(x = aovfitobj, conf.level = conf.level)

      cat("\n")
      cat("Sample group means:", paste(names(samplemeansbylevel), "-", samplemeansbylevel, " ", sep = ""), "\n")
      cat("\n")

      cat("Tukey multiple comparisons of means", "\n")
      print(tukeyresult$pred)

    } else {
      cat("\n")
      cat("Sample group means:", paste(names(samplemeansbylevel), "-", samplemeansbylevel, " ", sep = ""), "\n")
      cat("\n")

      cat("Games-Howell multiple comparisons of means", "\n")

      ghresult <- games.howell(grp = pred, obs = resp)
      ghresult2 <- ghresult[, c(1, 2, 7, 8, 6)]
      names(ghresult2)[2:5] <- c("diff", "lwr", "upr", "p adj")
      ghresult2 <- as.data.frame(ghresult2)
      print(ghresult2)
    }
  }
}
