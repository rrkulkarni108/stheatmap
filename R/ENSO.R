#' Functions that create a ENSO Time Series Plot of ONI values
#'
#' @param dataframe n by p matrix containing ONI values, from National Weather Service, Climate Prediction Center: https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php
#' @param StartDate string value for season start date "year1-year2", the starting date of your subset, i.e. "2000-2001", comes from season column of the dataframe
#' @param EndDate string value for season end date "year19-year20", the starting date of your subset, i.e. "2020-2021", comes from season column of the dataframe
#'
#' @return Explain return
#' @export
#'
#' @examples
#' # Give example


library(dplyr)
library(tidyr)
library(readxl)

# Load the data
enso_data <- read_excel("Data/ENSO.xlsx")

findDateSubset <- function(dataframe, StartDate, EndDate) {
  dataframe <- dataframe %>%
    filter(Season >= StartDate & Season <= EndDate)
  return(dataframe)
}
