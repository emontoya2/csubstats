#' Motivation and Creativity Scores Dataset
#'
#' This dataset contains experimental data from a case study examining whether
#' creativity scores differ based on the type of motivation. Undergraduate students
#' were randomly assigned to receive either an intrinsic or extrinsic motivation questionnaire.
#' After completing the questionnaire, each subject wrote a Haiku that was later evaluated
#' for creativity. The study was designed to investigate if thinking about intrinsic motivations
#' (i.e., engaging in an activity for its own sake) versus extrinsic motivations (i.e., performing
#' an activity to earn a reward or avoid punishment) affects creative performance.
#'
#' @docType data
#' @usage data(motidf)
#'
#' @format A data frame with 58 observations on 2 variables:
#' \describe{
#'   \item{Score}{Numeric. The creativity score assigned to the subject's Haiku.}
#'   \item{Treatment}{Factor. The type of motivation questionnaire administered, with two levels:
#'   \code{"Extrinsic"} and \code{"Intrinsic"}.}
#' }
#'
#' @source The "case0101" dataframe from the R package "Sleuth3".
#'
#' @examples
#' \dontrun{
#'   # Load the dataset
#'   data(motidf)
#'   # Display the first few rows of the dataset
#'   head(motidf)
#' }
"motidf"
