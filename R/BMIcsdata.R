#' BMI Case Study Data
#'
#' A dataset containing average Body Mass Index (BMI) values for different groups,
#' compiled as part of a case study on global patterns of obesity across rural and urban regions.
#'
#' The data record BMI rates for various countries, split by sex (Men or Women), region
#' (Rural or Urban), and the year of measurement (1985 or 2017). These aggregated values
#' are used to assess whether BMI rates differ between urban and rural settings.
#'
#' @docType data
#' @usage data(BMIcsdata)
#'
#' @format A data frame with *n* rows and 5 variables:
#' \describe{
#'   \item{Country}{A factor indicating the country of observation (e.g., "Afghanistan").}
#'   \item{Sex}{A factor indicating the sex group, with levels "Men" and "Women".}
#'   \item{Region}{A factor indicating the type of region, with levels "Rural" and "Urban".}
#'   \item{Year}{A numeric variable indicating the year of the observation (1985 or 2017).}
#'   \item{BMI}{A numeric variable representing the average Body Mass Index for the group.}
#' }
#'
#' @details The dataset is used to explore the association between BMI rates and region
#' (rural vs urban), as well as to investigate differences by country and sex. It was
#' motivated by research such as that by @ncd2019rising and @Wright2020, which examined
#' global obesity patterns. The following are a few of the first observations:
#'
#' \preformatted{
#'   Country      Sex   Region Year  BMI
#' 1 Afghanistan  Men   Rural 1985 19.7
#' 2 Afghanistan  Men   Urban 1985 22.4
#' 3 Afghanistan  Men   Rural 2017 22.5
#' 4 Afghanistan  Men   Urban 2017 23.6
#' 5 Afghanistan  Women Rural 1985 20.1
#' 6 Afghanistan  Women Urban 1985 23.2
#' }
#'
#' @source “Rising Rural Body-Mass Index Is the Main Driver of the Global Obesity Epidemic in Adults.” 2019. Nature 569 (7755): 260–64
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(BMIcsdata)
#'
#' # View the first few rows
#' head(BMIcsdata)
#'
#' # Summary statistics by region
#' aggregate(BMI ~ Region, data = BMIcsdata, FUN = function(x) c(mean = mean(x), sd = sd(x)))
#' }
#'
#' @keywords datasets case-study BMI obesity
"BMIcsdata"
