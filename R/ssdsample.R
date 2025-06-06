#' K-12 School Shooting Database Sample
#'
#' A sample dataset from the K-12 School Shooting Database, which documents instances
#' where a gun is brandished, fired, or a bullet hits K-12 school property. The data,
#' covering incidents from January 1970 to November 2022, includes details on the timing,
#' location, and characteristics of each incident, such as the number of victims, media
#' attention level, school type, and information on the shooter and weapon used.
#'
#' @docType data
#' @usage data(ssdsample)
#'
#' @format A data frame with 16 variables. The key variables are:
#' \describe{
#'   \item{Month}{Integer. Month of the incident (numeric representation).}
#'   \item{Day}{Integer. Day of the incident.}
#'   \item{Year}{Integer. Year when the incident occurred.}
#'   \item{Date}{Character. Date of the incident in MM/DD/YYYY format.}
#'   \item{Victims_Killed}{Integer. Number of victims killed in the incident.}
#'   \item{Victims_Wounded}{Integer. Number of victims wounded in the incident.}
#'   \item{Number_Victims}{Integer. Total number of victims (sum of killed and wounded).}
#'   \item{Media_Attention}{Factor. Highest level of media coverage (e.g., Local, Regional, National).}
#'   \item{Quarter}{Factor. Quarter of the year when the incident occurred.}
#'   \item{State}{Character. U.S. state where the incident occurred.}
#'   \item{School_Level}{Factor. Educational level of the school targeted (e.g., High School, Elementary, Junior High, etc.).}
#'   \item{During_School}{Character. Indicates whether the incident occurred during school hours (Yes/No).}
#'   \item{Time_Period}{Factor. Specific time period during which the incident occurred (e.g., Morning Classes, Afternoon Classes, Sport Event, etc.).}
#'   \item{Age}{Character. Age of the shooter when known (may be missing for some incidents).}
#'   \item{Weapon_Type}{Factor. Type of weapon used (e.g., Handgun, Multiple Handguns, etc.).}
#'   \item{AgeGroup}{Factor. Age group of the shooter (e.g., Child, Teen, Adult; may be missing in some cases).}
#' }
#'
#' @details
#' This dataset is used to explore relationships between variables such as media coverage,
#' the number of victims, and the type of weapon used. It also facilitates investigation into
#' whether the level of media coverage differs by the age group of the shooter. For more detailed
#' information on variable definitions and methodology, please refer to the
#' [K-12 School Shooting Database data methodology page](https://k12ssdb.org/methodology-1).
#'
#' @source Data provided by Dr. David Riedman, founder and maintainer of the K-12 School Shooting Database.
#'
#' @examples
#' # Load the dataset (assumed to be available in your package)
#' data(ssdsample)
#'
#' # Display the first few observations
#' head(ssdsample)
"ssdsample"
