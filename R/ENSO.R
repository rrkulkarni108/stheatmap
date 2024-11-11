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
#' sample_data <- data.frame(
#' Season = c("2000-2001", "2001-2002"),
#' JJA = c(-0.6, -0.1),
#' JAS = c(-0.5, -0.1),
#' ASO = c(-0.5, -0.2),
#' SON = c(-0.6, -0.3),
#' OND = c(-0.7, -0.3),
#' NDJ = c(-0.7, -0.3),
#' DJF = c(-0.7, -0.1),
#' JFM = c(-0.5, 0.0),
#' FMA = c(-0.4, 0.2),
#' MAM = c(-0.3, 0.2),
#' AMJ = c(-0.3, 0.4),
#' MJJ = c(-0.1, 0.7)
#' )
#' print(sample_data)
#' enso_main(sample_data, "2000-2001", "2001-2002")


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
  print(enso_arr)
  for (i in 6:length(enso_arr)) {
    if (!is.na(enso_arr[i]) && !is.na(enso_arr[i + 1]) && !is.na(enso_arr[i + 2])) {
      avg_val <- mean(enso_arr[i:(i + 2)])
      #print(avg_val)
      val_arr <- c(val_arr, avg_val)
      #print(val_arr)
    } else {
      break
    }
  }
  #print(val_arr)
  val_arr <- round(val_arr, 2)


  return (val_arr)

}



## ONI value >= 0.5 and <1 is considered a weak El Nino, >=1.0 to <1.5 a moderate El Nino,
#  >=1.5 and <2 a strong El Nino, and >=2 a very strong El Nino;
#  similarly, values <= -0.5 and values > -1.0 is a weak La Nina,
#  values <= -1.0 and values > -1.5 a moderate La Nina, and <= -1.5 a strong La Nina

# Assign ENSO phase colors based on value
assignENSOColors <- function(data) {
  data <- data %>%
    mutate(Color = case_when(
      Values >= 0.5 & Values < 1.0 ~ '#F1959B',   # Weak El Nino
      Values >= 1.0 & Values < 1.5 ~ '#F07470',   # Medium El Nino
      Values >= 1.5 & Values < 2.0 ~ '#EA4C46',   # Strong El Nino
      Values >= 2.0 ~ '#DC1C13',                  # Very Strong El Nino
      Values <= -0.5 & Values > -1.0 ~ '#2A9DF4', # Weak La Nina
      Values <= -1.0 & Values > -1.5 ~ '#1167B1', # Medium La Nina
      Values <= -1.5 ~ '#003D80',                 # Strong La Nina
      TRUE ~ '#d3d3d3'                            # ENSO Neutral
    ),
    ENSO_Type = case_when(
      Values >= 0.5 & Values < 1.0 ~ 'Weak El Nino',
      Values >= 1.0 & Values < 1.5 ~ 'Medium El Nino',
      Values >= 1.5 & Values < 2.0 ~ 'Strong El Nino',
      Values >= 2.0 ~ 'Very Strong El Nino',
      Values <= -0.5 & Values > -1.0 ~ 'Weak La Nina',
      Values <= -1.0 & Values > -1.5 ~ 'Medium La Nina',
      Values <= -1.5 ~ 'Strong La Nina',
      TRUE ~ 'ENSO Neutral'
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
  legend_labels <- c(
    '#F1959B' = 'Weak El Niño',
    '#F07470' = 'Medium El Niño',
    '#EA4C46' = 'Strong El Niño',
    '#DC1C13' = 'Very Strong El Niño',
    '#2A9DF4' = 'Weak La Niña',
    '#1167B1' = 'Medium La Niña',
    '#003D80' = 'Strong La Niña',
    '#d3d3d3' = 'ENSO Neutral'
  )
  print("this is printed")
  print(data$ENSO_Type)
  options(repr.plot.width = 30, repr.plot.height = 200)
  ggplot(data, aes(y = reorder(YrMon, DATE), x = abs(Values), fill = Color)) +

    geom_bar(aes(x=abs(Values), y=reorder(YrMon, DATE), fill=Color), stat = "identity", show.legend = TRUE) +
    scale_fill_identity(name = "ENSO Phase", guide = "legend",
                        labels = legend_labels
                        ) +
    theme_minimal() +
    labs(
      x = "Monthly Nino 3.4 Region Average",
      y = "Time",
      title = "Oceanic Nino Index, 2001-2021\nENSO Intensities by Month"
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
  enso <- findDateSubset(data, StartDate, EndDate)

  # Extract only the relevant columns (exclude first two)
  if("EnsoType" %in% colnames(enso)){
    enso_subset <- enso %>% select(-c(EnsoType, Season))
  }
  enso_subset <- enso %>% select(-c(Season))
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
  q <- timeSeriesPlot(plot_data)
  q

  # Assign colors for plotting
  colored_data <- assignENSOColors(plot_data)
  #print(colored_data)

  # Plot 2- plot the data- main plot
  p <-  plotENSOSeries(colored_data)
  p
  return(list(q,p))
}

# tester code
#enso_data = enso_data[,2:11]
#enso_main(enso_data, "2000-2001",  "2020-2021")

sample_data <- data.frame(
  Season = c("2000-2001", "2001-2002"),
  JJA = c(-0.6, -0.1),
  JAS = c(-0.5, -0.1),
  ASO = c(-0.5, -0.2),
  SON = c(-0.6, -0.3),
  OND = c(-0.7, -0.3),
  NDJ = c(-0.7, -0.3),
  DJF = c(-0.7, -0.1),
  JFM = c(-0.5, 0.0),
  FMA = c(-0.4, 0.2),
  MAM = c(-0.3, 0.2),
  AMJ = c(-0.3, 0.4),
  MJJ = c(-0.1, 0.7)
)
print(sample_data)
enso_main(sample_data, "2000-2001", "2001-2002")













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
#       Values >= 0.5 & Values < 1.0 ~ '#F1959B',   # Weak El Nino
#       Values >= 1.0 & Values < 1.5 ~ '#F07470',   # Medium El Nino
#       Values >= 1.5 & Values < 2.0 ~ '#EA4C46',   # Strong El Nino
#       Values >= 2.0 ~ '#DC1C13',                  # Very Strong El Nino
#       Values <= -0.5 & Values > -1.0 ~ '#2A9DF4', # Weak La Nina
#       Values <= -1.0 & Values > -1.5 ~ '#1167B1', # Medium La Nina
#       Values <= -1.5 ~ '#003D80',                 # Strong La Nina
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
#       title = "Oceanic Nino Index, 2001-2021\nENSO Intensities by Month"
#     ) +
#     coord_flip() +
#     theme(panel.grid.minor = element_blank())
#
#   # Add legend
#   legend_labels <- c(
#     'Weak El Nino' = '#F1959B',
#     'Medium El Nino' = '#F07470',
#     'Strong El Nino' = '#EA4C46',
#     'Very Strong El Nino' = '#DC1C13',
#     'Weak La Nina' = '#2A9DF4',
#     'Medium La Nina' = '#1167B1',
#     'Strong La Nina' = '#003D80',
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
