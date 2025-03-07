#' Kruskal-Wallis Test with Effect Size and Optional Post-hoc Comparisons
#'
#' This function performs a non-parametric Kruskal-Wallis test to compare groups defined by a formula.
#' It calculates the test statistic, p-value, degrees of freedom, and effect size (eta-squared) using the
#' rank-based method. Optionally, if requested via the \code{PWC} parameter, it conducts post-hoc pairwise
#' comparisons using Dunn's test with Holm's correction.
#'
#' @param formula A formula object of the form \code{response ~ predictor} that specifies the dependent and independent variables.
#' @param data A data frame containing the variables referenced in the formula.
#' @param PWC Logical value indicating whether to perform post-hoc pairwise comparisons (default is \code{FALSE}).
#'            If \code{TRUE}, the function prints the sample group medians and the results of Dunn's test.
#'
#' @return The function prints the results of the Kruskal-Wallis test, including the test statistic, p-value, degrees of freedom,
#'         and effect size (eta-squared). When \code{PWC = TRUE}, it also prints the sample group medians and Dunn's test results.
#'         No value is returned.
#'
#' @details The function first extracts the response and predictor variable names from the provided formula and evaluates them within the supplied data frame.
#'          It then performs the Kruskal-Wallis test and calculates the effect size using the \code{rank_eta_squared} function from the \code{effectsize} package.
#'          When \code{PWC} is set to \code{TRUE}, the function computes sample group medians and applies Dunn's test with Holm's correction
#'          (using the \code{dunnTest} function from the \code{FSA} package) to assess pairwise differences.
#'
#' @importFrom stats kruskal.test oneway.test TukeyHSD model.tables pairwise.t.test
#' @importFrom effectsize rank_eta_squared
#' @importFrom FSA dunnTest
#'
#' @examples
#' \dontrun{
#' # Example data
#' data <- data.frame(
#'   outcome = c(1, 1, 0, 1, 1, 0),
#'   group = factor(c("A", "A", "A", "C", "C", "C"))
#' )
#'
#' # Run the one-way ANOVA
#' sfaov(outcome ~ group, data = data)
#' }
#'
#' @export
sfkw <- function(formula,   data, PWC = FALSE ){
  # Description: Function to compute the null distribution when comparing two props

  #require(effectsize)


  #stop("Wrong formula, should be of the form  y ~  x   ... or your data is not in tidy or data matrix format... of if you are trying to include more covariates this method is not applicable")

  resp.varName <- all.vars(formula)[1]
  pred.varName <- all.vars(formula)[2]


  resp  <- eval(parse(text = paste0("data$", resp.varName)))
  # respOG  <- eval(parse(text = paste0("data$", resp.varName)))

  pred  <- eval(parse(text = paste0("data$", pred.varName)))
  #predOG  <- eval(parse(text = paste0("data$", pred.varName)))

  kwtobj<- kruskal.test(  resp  ~  pred )
  esqd <-rank_eta_squared( resp  ~  pred)$rank_eta_squared


  obs.teststat<- round( kwtobj$statistic, 3)
  pvalue<- round( kwtobj$p.value, 3)
  dfkw<- round( kwtobj$parameter, 3)



  cat("Kruskal-Wallis test (single-factor ANOVA on ranks)", "\n")

  cat(" \n")

  cat("data: ", resp.varName, "~", pred.varName, "\n")
  cat("============== \n")

  cat("Kruskal-Wallis test statistic= ", obs.teststat, "\n")
  cat("p-value= ", pvalue, "\n")
  cat("Null distribition is chi-squared with df= ", dfkw, "\n")
  cat("============== \n")

  cat(" \n")

  cat("Eta-squared= ", esqd, "\n")



  if(PWC){
    # require(broom)
    #require(FSA)

    samplemeds <- tapply(resp, pred, median)

    cat("\n")
    cat("Sample group medians:" , paste(names(samplemeds), "-", samplemeds, " " , sep=""), "\n")
    cat("\n")

    # cat("Dunn's test with Holm's correction", "\n")


    dtresult <- dunnTest(x=resp,g=pred,
                         method="holm")
    print(dtresult)
    cat("\n")


    #tidy(pairwise.t.test(x=resp, g=pred, p.adj = "bonf"))
  }




  # returns the p-value, the test stat for each randomization, and the
  # observed test stat
}

