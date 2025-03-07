#' Delta Smelt Survival Dataset
#'
#' This dataset contains information on the survival percentages of early-stage Delta smelt larvae (0–40 days post hatch) reared under different environmental conditions. The study investigated the influence of light intensity and water turbidity on larval survival, providing insights for conservation efforts of this endangered species.
#'
#' @docType data
#' @usage data(dssurv)
#'
#' @format A data frame with 20 observations and 3 variables:
#' \describe{
#'   \item{Light}{A factor indicating the light intensity conditions under which the larvae were reared. The levels include "low", "med", and "high".}
#'   \item{Turbidity}{A factor representing the water turbidity levels during rearing. The levels include "low", "med", and "high".}
#'   \item{Survival}{A numeric variable indicating the survival percentage of the larvae under the given conditions.}
#' }
#'
#' @details The Delta smelt is a small, endangered fish native to the Sacramento-San Joaquin River Delta. This dataset, referenced from Tigan et al. (2020), was used to explore whether variations in light intensity affect the survival rates of Delta smelt larvae. Light intensity is measured in \eqn{\mu mol/m^2/s} and turbidity is measured in nephelometric turbidity units (NTUs). The data provides a basis for understanding how environmental factors may impact the viability of early-stage larvae.
#'
#' @source \url{https://calfish.ucdavis.edu/species/} and Tigan et al. (2020).  Data provided by Dr. Tien-Chieh Hung.
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(dssurv)
#'
#' # View the first few rows of the dataset
#' head(dssurv)
#'
#' # Summarize survival percentages by light intensity
#' library(dplyr)
#' dssurv %>%
#'   group_by(Light) %>%
#'   summarise(
#'     Mean = mean(Survival, na.rm = TRUE),
#'     SD = sd(Survival, na.rm = TRUE)
#'   )
#'
#' # Create a boxplot of survival percentages by light intensity
#' boxplot(Survival ~ Light, data = dssurv,
#'         main = "Survival of Delta Smelt Larvae by Light Intensity",
#'         xlab = "Light Intensity", ylab = "Survival (%)")
#' }
"dssurv"
