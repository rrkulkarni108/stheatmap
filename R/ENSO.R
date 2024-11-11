#' Functions that create a ENSO Time Series Plot of ONI values
#'
#' @name enso_main
#' @param dataframe n by p matrix containing ONI values, from National Weather Service, Climate Prediction Center: https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php
#' @param StartDate string value for season start date "year1-year2", the starting date of your subset, i.e. "2000-2001", comes from season column of the dataframe
#' @param EndDate string value for season end date "year19-year20", the starting date of your subset, i.e. "2020-2021", comes from season column of the dataframe
#'
#' @return p Main package plot which is a barplot visualization of the ENSO intensities by month
#' @export
#'
#' @examples
#' # Give example
#' # Load the data
#' enso_data <- read_excel("Data/ENSO.xlsx")
#' enso_main(enso_data, "2000-2001",  "2020-2021")


library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)

# Load the data
enso_data <- readxl::read_excel("Data/ENSO.xlsx")

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

#get the 3 month rolling averages of ONI to get avg ONI for individual month
rollingAvg <- function(enso_arr) {

  # Calculate 3-month rolling averages
  val_arr <- c()
  # Since my desired data range starts from January of 2001, I ignore the first five entries of 2000
  # and start with the the 6th entry- which is the 3month average ONI for december 2000, january 2001 and february 2001.
  # I take the rolling average of the three entries which contain the month January: NDJ, DJF, JFM to get an ONI value for January 2001
  for (i in 6:length(enso_arr)) {
    if (!is.na(enso_arr[i]) && !is.na(enso_arr[i + 1]) && !is.na(enso_arr[i + 2])) {
      avg_val <- mean(enso_arr[i:(i + 2)])
      val_arr <- c(val_arr, avg_val)
    } else {
      break
    }
  }
  val_arr <- round(val_arr, 2)
  print(val_arr)

  return (val_arr)

}





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

## PLOT 1: Plot time series curve (overall pattern)

timeSeriesPlot <- function(data) {
  # Plot time series
  options(repr.plot.width = 20, repr.plot.height = 8)
  ggplot(data, aes(x = YrMon, y = Values, group = 1)) +
    geom_line(color = "blue") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(size = 10, hjust = 0.5)
    ) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 12)]) +
    labs(x = "Time", y = "Average ONI Value", title = "Time Series of ONI Averages in NINO 3.4 Region")
}


## PLOT 2: MAIN PLOT
# Plotting function
plotENSOSeries <- function(data) {
  # Plot using ggplot
  # Color-coded time series barplot with legend
  options(repr.plot.width = 30, repr.plot.height = 200)
  ggplot(data, aes(y = reorder(YrMon, DATE), x = abs(Values), fill = Color)) +

    geom_bar(aes(x=abs(Values), y=reorder(YrMon, DATE), fill=Color), stat = "identity", show.legend = TRUE) +
    scale_fill_identity(name = "ENSO Phase", guide = "legend",
                        labels = c("Strong La Niña", "Medium La Niña", "Weak La Niña", "ENSO Neutral",
                                   "Very Strong El Niño", "Strong El Niño", "Medium El Niño", "Weak El Niño")) +
    theme_minimal() +
    labs(
      x = "Monthly Nino 3.4 Region Average",
      y = "Time",
      title = "Oceanic Niño Index, 2001-2021\nENSO Intensities by Month"
    ) +
    theme(
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(size = 20, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )+  scale_y_discrete(limits = rev(unique(data$YrMon)),breaks = function(y) y[seq(1, length(y), by = 6)])#scale_y_discrete(limits = rev(unique(data$YrMon)), breaks = function(y) y[seq(1, length(y), by = 12)])# scale_y_discrete(breaks = function(y) y[seq(1, length(y), by = 12)])
}


# Main function which calls the subfunctions
enso_main <- function(data, StartDate, EndDate) {
  # Reshape data to long format
  enso <- findDateSubset(data, StartDate, EndDate) %>%
    rename(EnsoType = `ENSO Type`)

  # Extract only the relevant columns (exclude first two)
  enso_subset <- enso %>% select(-c(EnsoType, Season))
  enso_arr <- as.vector(t(enso_subset))
  val_arr <- rollingAvg(enso_arr)

  # Extract years labels from Season column
  yrs <- c()
  for (season in enso$Season) {
    year2 <- unlist(strsplit(season, "-"))[2]
    yrs <- c(yrs, rep(year2, 12))
  }
  yrs <- yrs[1:length(val_arr)]

  # create dataframe to be used for plotting
  # Create data frame
  months <- rep(1:12, length.out = length(val_arr))
  plot_data <- data.frame(Year = yrs, Month = months, Values = val_arr)
  plot_data <- plot_data %>% mutate(DATE = make_date(Year, Month, 1), YrMon = format(DATE, "%Y-%m"))


  #Plot 1 - overall time series to get general picture
  timeSeriesPlot(plot_data)

  # Assign colors for plotting
  colored_data <- assignENSOColors(plot_data)

  # Plot 2- plot the data- main plot
  p <-  plotENSOSeries(colored_data)
  return(p)
}

# tester code
enso_main(enso_data, "2000-2001",  "2020-2021")















##############################################################################
# # Load the data
# enso_data <- read_excel("Data/ENSO.xlsx")
#
# ## Function takes in dataframe from any timepoint of this website (it is updated frequently) https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php
# ## StartDate string value for season start date "year1-year2"
# ## EndDate string value for season end date "year19-year20"
# findDateSubset <- function(dataframe, StartDate, EndDate) {
#   dataframe <- dataframe %>%
#     filter(Season >= StartDate & Season <= EndDate)
#   return(dataframe)
# }
# #findDateSubset(enso_data, "2001-2002", "2020-2021") test that subset function works
#
# # Reshape the data into a long format
#
# makeLongForm <- function(dataframe) {
#
#   enso_long <- enso_data %>%
#     pivot_longer(
#       cols = -c("ENSO Type", "Season"),
#       names_to = "Month",
#       values_to = "Values"
#     ) %>%
#     mutate(
#       YrMon = paste(Season, Month, sep = "-"), # Create a Year-Month column
#       YrMon = factor(YrMon, levels = unique(YrMon)) # Ensure order is preserved
#     )
#
#   # Display a few rows for verification
#   head(enso_long)
#
#   return (enso_long)
#
# }
#
# #makeLongForm(enso_data)
#
#
# library(ggplot2)
#
# # Assign ENSO phase colors based on value
# assignENSOColors <- function(data) {
#   data <- data %>%
#     mutate(Color = case_when(
#       Values >= 0.5 & Values < 1.0 ~ '#F1959B',   # Weak El Niño
#       Values >= 1.0 & Values < 1.5 ~ '#F07470',   # Medium El Niño
#       Values >= 1.5 & Values < 2.0 ~ '#EA4C46',   # Strong El Niño
#       Values >= 2.0 ~ '#DC1C13',                  # Very Strong El Niño
#       Values <= -0.5 & Values > -1.0 ~ '#2A9DF4', # Weak La Niña
#       Values <= -1.0 & Values > -1.5 ~ '#1167B1', # Medium La Niña
#       Values <= -1.5 ~ '#003D80',                 # Strong La Niña
#       TRUE ~ '#d3d3d3'                            # ENSO Neutral
#     ))
#   return(data)
# }
#
#
# # Plotting function
# plotENSOIndex <- function(data) {
#   # Plot using ggplot
#   p <- ggplot(data, aes(x = abs(Values), y = YrMon, fill = Color)) +
#     geom_bar(stat = "identity", color = "black") +
#     scale_fill_identity() +
#     theme_minimal() +
#     theme(
#       axis.text.y = element_text(size = 10),
#       axis.title.y = element_text(size = 15),
#       axis.title.x = element_text(size = 15),
#       plot.title = element_text(size = 20, hjust = 0.5)
#     ) +
#     labs(
#       x = "Monthly Nino 3.4 Region Average",
#       y = "Time",
#       title = "Oceanic Niño Index, 2001-2021\nENSO Intensities by Month"
#     ) +
#     coord_flip() +
#     theme(panel.grid.minor = element_blank())
#
#   # Add legend
#   legend_labels <- c(
#     'Weak El Niño' = '#F1959B',
#     'Medium El Niño' = '#F07470',
#     'Strong El Niño' = '#EA4C46',
#     'Very Strong El Niño' = '#DC1C13',
#     'Weak La Niña' = '#2A9DF4',
#     'Medium La Niña' = '#1167B1',
#     'Strong La Niña' = '#003D80',
#     'ENSO Neutral' = '#d3d3d3'
#   )
#
#   p <- p + scale_fill_manual(
#     values = legend_labels,
#     breaks = names(legend_labels),
#     name = "ENSO Phase"
#   )
#
#   print(p)
# }
#
# # Main function which calls the subfunctions
# enso_main <- function(data) {
#   # Reshape data to long format
#   data_long <- data %>%
#     pivot_longer(
#       cols = -c(`ENSO Type`, Season),
#       names_to = "Month",
#       values_to = "Values"
#     ) %>%
#     mutate(
#       YrMon = paste(Season, Month, sep = "-"),
#       YrMon = factor(YrMon, levels = unique(YrMon))
#     )
#
#   # Assign colors for plotting
#   colored_data <- assignENSOColors(data_long)
#
#   # Plot the data
#   plotENSOIndex(colored_data)
# }
#
# # tester code
# enso_main(enso_data)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
