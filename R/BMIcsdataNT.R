#' BMI Case Study Data (Non-tidy)
#'
#' A dataset containing average Body Mass Index (BMI) values for different groups,
#' compiled as part of a case study on global patterns of obesity across rural and urban regions.
#'
#'
#' @docType data
#' @usage data(BMIcsdataNT)
#'
#' @format A data frame with *n* rows and 5 variables:
#' \describe{
#'   \item{Country}{A factor indicating the country of observation (e.g., "Afghanistan").}
#'   \item{Sex}{A factor indicating the sex group, with levels "Men" and "Women".}
#'   \item{Region}{A factor indicating the type of region, with levels "Rural" and "Urban".}
#'   \item{yr.1985}{A numeric variable representing the average Body Mass Index for the group in 1985}
#'   \item{yr.2017}{A numeric variable representing the average Body Mass Index for the group in 2017.}
#' }
#'
#' @details The dataset is used to illustrate non-tidy data.
#'
#' \preformatted{
#' Country   Sex Region yr.1985 yr.2017
#' 1 Afghanistan   Men  Rural    19.7    22.5
#' 2 Afghanistan   Men  Urban    22.4    23.6
#' 3 Afghanistan Women  Rural    20.1    23.6
#' 4 Afghanistan Women  Urban    23.2    26.3
#' 5     Albania   Men  Rural    25.0    26.9
#' 6     Albania   Men  Urban    25.4    27.0
#' }
#'
#' @source “Rising Rural Body-Mass Index Is the Main Driver of the Global Obesity Epidemic in Adults.” 2019. Nature 569 (7755): 260–64
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(BMIcsdataNT)
#'
#' # View the first few rows
#' head(BMIcsdataNT)
#' }
#'
#' @keywords datasets case-study BMI obesity
"BMIcsdataNT"
