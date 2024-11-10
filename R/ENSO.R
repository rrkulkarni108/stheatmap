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

## Function takes in dataframe from any timepoint of this website (it is updated frequently) https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php
## StartDate string value for season start date "year1-year2"
## EndDate string value for season end date "year19-year20"
findDateSubset <- function(dataframe, StartDate, EndDate) {
  dataframe <- dataframe %>%
    filter(Season >= StartDate & Season <= EndDate)
  return(dataframe)
}
#findDateSubset(enso_data, "2001-2002", "2020-2021") test that subset function works

# Reshape the data into a long format

makeLongForm <- function(dataframe) {

enso_long <- enso_data %>%
  pivot_longer(
    cols = -c("ENSO Type", "Season"),
    names_to = "Month",
    values_to = "Values"
  ) %>%
  mutate(
    YrMon = paste(Season, Month, sep = "-"), # Create a Year-Month column
    YrMon = factor(YrMon, levels = unique(YrMon)) # Ensure order is preserved
  )

# Display a few rows for verification
head(enso_long)

return (enso_long)

}

#makeLongForm(enso_data)


library(ggplot2)

# Assign ENSO phase colors based on value
assignENSOColors <- function(data) {
  data <- data %>%
    mutate(Color = case_when(
      Values >= 0.5 & Values < 1.0 ~ '#F1959B',   # Weak El Niño
      Values >= 1.0 & Values < 1.5 ~ '#F07470',   # Medium El Niño
      Values >= 1.5 & Values < 2.0 ~ '#EA4C46',   # Strong El Niño
      Values >= 2.0 ~ '#DC1C13',                  # Very Strong El Niño
      Values <= -0.5 & Values > -1.0 ~ '#2A9DF4', # Weak La Niña
      Values <= -1.0 & Values > -1.5 ~ '#1167B1', # Medium La Niña
      Values <= -1.5 ~ '#003D80',                 # Strong La Niña
      TRUE ~ '#d3d3d3'                            # ENSO Neutral
    ))
  return(data)
}


