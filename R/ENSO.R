

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
# #get the 3 month rolling averages of ONI to get avg ONI for individual month
# rollingAvg <- function(enso_arr) {
#   # Calculate 3-month rolling averages
#   val_arr <- c()
#   # Since my desired data range starts from January of 2001, I ignore the first five entries of 2000
#   # and start with the the 6th entry- which is the 3month average ONI for december 2000, january 2001 and february 2001.
#   # I take the rolling average of the three entries which contain the month January: NDJ, DJF, JFM to get an ONI value for January 2001
#   #print(enso_arr)
#   for (i in 6:length(enso_arr)) {
#     if (!is.na(enso_arr[i]) &&
#         !is.na(enso_arr[i + 1]) && !is.na(enso_arr[i + 2])) {
#       avg_val <- mean(enso_arr[i:(i + 2)])
#       val_arr <- c(val_arr, avg_val)
#     } else {
#       break
#     }
#   }
#   #print(val_arr)
#   val_arr <- round(val_arr, 2)
#
#
#   return (val_arr)
#
# }



# Obtain the 3 month rolling averages of ONI to get avg ONI for individual month
# For example: Suppose my desired data range starts from January of 2001, then I ignore the first five entries of 2000
# and start with the the 6th entry- which is the 3month average ONI for december 2000, january 2001 and february 2001.
# I take the rolling average of the three entries which contain the month January: NDJ, DJF, JFM to get an ONI value for January 2001

## Returns a list of average ONI values, one for each month desired by user
rollingAvgTest <- function(enso_arr, startMonth, endMonth ) {
  # Calculate 3-month rolling averages
  val_arr <- c() # Initialize vector

  # Loop through the array, starting from the first valid 3-month window (our rectangle of data always allows this from enso_main)
  for (i in 1:(length(enso_arr) - 2)) {
    # Check that none of the values are NA in our average
    if (!is.na(enso_arr[i]) &&
        !is.na(enso_arr[i + 1]) && !is.na(enso_arr[i + 2])) {
      avg_val <- mean(enso_arr[i:(i + 2)]) # Calculate mean
      val_arr <- c(val_arr, avg_val) # Add the mean to the existing vector

      # Else check if there are any NAs in any of the 3 values, add NA to the list
    } else if( is.na(enso_arr[i]) ||
              is.na(enso_arr[i + 1]) || is.na(enso_arr[i + 2])    ) {
      val_arr <-c(val_arr, NA) #Keep NAs for now, remove them from the list at the end
    }
      else{
        break
    }
  }
  #print(val_arr)
  val_arr_start <-  c() # Initialize start vector
  val_arr_result <- c() # Initialize result vector
  # Round the result to 2 decimal places for easy read of plot later
  val_arr <- round(val_arr, 2)

  # 5 Cases of month combinations, since each season has overlap of months fom two years
  # One season has months 7-12 and also 1-6 in the same row
  # Months 6 and 7 are on two rows so we have special edge case for that situation
  if (startMonth >= 8 && startMonth <= 12 ){
    val_arr_start <- val_arr[(startMonth-7):length(val_arr)]
  }
  else if (startMonth < 8 && startMonth >= 1){
    val_arr_start <- val_arr[(startMonth+5):length(val_arr)]
  }

  if (endMonth <= 5 && endMonth >= 1 ){
    val_arr_result <- val_arr_start[1:(length(val_arr_start) - (5-endMonth))]
  }
  else if (endMonth > 5 && endMonth <= 7){
    val_arr_result <- val_arr_start[1:(length(val_arr_start) - (12-(endMonth-5)))]
  }
  else if (endMonth > 7 && endMonth <= 12){
    val_arr_result <- val_arr_start[1:(length(val_arr_start) - (endMonth-7))]
  }
  #print(val_arr_start)
  # Remove the NAs from the list if there are any
  val_arr <- val_arr_result[!is.na(val_arr_result)]
  #print(val_arr)
  return (val_arr) # Returns a list of average oni values, one for each month desired by user

}


## ONI value >= 0.5 and <1 is considered a weak El Nino, >=1.0 to <1.5 a moderate El Nino,
#  >=1.5 and <2 a strong El Nino, and >=2 a very strong El Nino;
#  similarly, values <= -0.5 and values > -1.0 is a weak La Nina,
#  values <= -1.0 and values > -1.5 a moderate La Nina, and <= -1.5 a strong La Nina

# Assign ENSO phase colors based on value
assignENSOColors <- function(data) {
  data <- data %>%
    mutate(
      Color = case_when(
        Values >= 0.5 & Values < 1.0 ~ '#F1959B',
        # Weak El Nino
        Values >= 1.0 & Values < 1.5 ~ '#F07470',
        # Medium El Nino
        Values >= 1.5 & Values < 2.0 ~ '#EA4C46',
        # Strong El Nino
        Values >= 2.0 ~ '#DC1C13',
        # Very Strong El Nino
        Values <= -0.5 & Values > -1.0 ~ '#2A9DF4',
        # Weak La Nina
        Values <= -1.0 & Values > -1.5 ~ '#1167B1',
        # Medium La Nina
        Values <= -1.5 ~ '#003D80',
        # Strong La Nina
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
      )
    )
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
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        size = 10
      ),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(size = 10, hjust = 0.5)
    ) +
    scale_x_discrete(
      breaks = function(x)
        x[seq(1, length(x), by = 12)]
    ) +
    labs(x = "Time", y = "Average ONI Value", title = "Time Series of ONI Averages in NINO 3.4 Region")
}


## PLOT 2: MAIN PLOT
# Plotting function
plotENSOSeries <- function(data) {
  # Plot using ggplot
  # Color-coded time series barplot with legend
  legend_labels <- c(
    '#F1959B' = 'Weak El Nino',
    '#F07470' = 'Medium El Nino',
    '#EA4C46' = 'Strong El Nino',
    '#DC1C13' = 'Very Strong El Nino',
    '#2A9DF4' = 'Weak La Nina',
    '#1167B1' = 'Medium La Nina',
    '#003D80' = 'Strong La Nina',
    '#d3d3d3' = 'ENSO Neutral'
  )
  #print("this is printed")
  #print(data$ENSO_Type)
  options(repr.plot.width = 30, repr.plot.height = 200)
  ggplot(data, aes(
    y = reorder(YrMon, DATE),
    x = abs(Values),
    fill = Color
  )) +

    geom_bar(aes(
      x = abs(Values),
      y = reorder(YrMon, DATE),
      fill = Color
    ),
    stat = "identity",
    show.legend = TRUE) +
    scale_fill_identity(name = "ENSO Phase",
                        guide = "legend",
                        labels = legend_labels) +
    theme_minimal() +
    labs(x = "Monthly Nino 3.4 Region Average", y = "Time", title = "Oceanic Nino Index, 2001-2021\nENSO Intensities by Month") +
    theme(
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(size = 20, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    ) +  scale_y_discrete(
      limits = rev(unique(data$YrMon)),
      breaks = function(y)
        y[seq(1, length(y), by = 6)]
    )#scale_y_discrete(limits = rev(unique(data$YrMon)), breaks = function(y) y[seq(1, length(y), by = 12)])# scale_y_discrete(breaks = function(y) y[seq(1, length(y), by = 12)])
}

#' Functions that create a ENSO Time Series Plot of ONI values
#'
#' @param data n by p matrix containing ONI values, from National Weather Service, Climate Prediction Center: https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php
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
#' enso_main(sample_data, "2001-01-01", "2002-01-01")



# Main function which calls the subfunctions
enso_main <- function(data, StartDate, EndDate) {

  # Convert StartDate and EndDate to Date object
  start_date <- as.Date(StartDate, format = "%Y-%m-%d")
  end_date <- as.Date(EndDate, format = "%Y-%m-%d")

  if(start_date > end_date){
    stop("StartDate must be before EndDate. Please check your values.")
  }

  # Extract year and month for StartDate and EndDate
  startYear <- as.integer(format(start_date, "%Y"))
  startMonth <- as.integer(format(start_date, "%m"))

  endYear <- as.integer(format(end_date, "%Y"))
  endMonth <- as.integer(format(end_date, "%m"))

  # Determine the start season
  startSeason = "" #initialize endSeason
  if (startMonth >= 1 && startMonth <= 7) {
    startSeason <- paste(startYear - 1, startYear, sep = "-")
  } else {
    startSeason <- paste(startYear, startYear + 1, sep = "-")
  }

  # Determine the end season
  endSeason = "" #initialize endSeason
  if (endMonth >= 1 && endMonth <= 5) {
    endSeason <- paste(endYear - 1, endYear, sep = "-")
  } else {
    endSeason <- paste(endYear, endYear + 1, sep = "-")
  }

  # Reshape data to long format
  enso <- findDateSubset(data, startSeason, endSeason) #takes only the rows (seasons) which user wants

  # Remove season column so we can get array of just ONI values
  enso_subset <- enso %>% select(-c(Season))
  enso_arr <- as.vector(t(enso_subset)) #create array of ONI values
  val_arr <- rollingAvgTest(enso_arr, startMonth, endMonth) #get individual ONI average for each month
  #print(val_arr)

  # Extract years labels from Season column
  yrs <- c()
  for (season in enso$Season) {
    year2 <- unlist(strsplit(season, "-"))[2]
    yrs <- c(yrs, rep(year2, 12))
  }
  yrs <- yrs[1:length(val_arr)]
  #print(yrs)

  # Create dataframe to be used for plotting
  # Create data frame
  months <- rep(1:12, length.out = length(val_arr))
  plot_data <- data.frame(Year = yrs,
                          Month = months,
                          Values = val_arr)
  plot_data <- plot_data %>% mutate(DATE = make_date(Year, Month, 1),
                                    YrMon = format(DATE, "%Y-%m"))

  #Plot 1 - overall time series to get general picture
  q <- timeSeriesPlot(plot_data)

  # Assign colors for plotting
  colored_data <- assignENSOColors(plot_data)

  # Plot 2- plot the data- main plot
  p <-  plotENSOSeries(colored_data)
  return(list(q, p, val_arr = val_arr))
}


###############################################################################

# tester code
# Load the data

# load("Data/ENSO.Rda")
# ls()
# enso_data <- enso_data #name of data variable is enso_data


#Example 1: ENSO from 2001-2021
#enso_data <- readxl::read_excel("Data/ENSO.xlsx")
#enso_data = enso_data[,2:14] #do not need first column
#enso_main(enso_data, "2001-01-01",  "2021-06-01")


#Example 2: Sample data with only two years
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
#print(sample_data)
#enso_main(sample_data, "2001-01-01", "2002-01-01")



# #convert csv to Rda file
# enso_data <- readxl::read_excel("Data/ENSO.xlsx")
# enso_data = enso_data[,2:ncol(enso_data)]
# #
# # # Save the data as an Rda file
# save(enso_data, file = "ENSO.Rda")
# #
# # #check that it works
#load("Data/ENSO.Rda")
#print(enso_data)


# # TESTING THE user input date format conversion to seasonal data format
#
# # This is the edge case with NAs
# # StartDate <- "2021-01-01"
# # EndDate <- "2021-08-01"
#
#
# Regular case:
# StartDate <- "2001-07-01"
# EndDate <- "2002-04-01"
# StartDate <- "2001-01-01"
# EndDate <- "2021-06-01"
# # Convert StartDate and EndDate to Date object
# start_date <- as.Date(StartDate, format = "%Y-%m-%d")
# end_date <- as.Date(EndDate, format = "%Y-%m-%d")
# if(start_date > end_date){
#   stop("StartDate must be before EndDate. Please check your values.")
# }
#
# # Extract year and month for StartDate and EndDate
# startYear <- as.integer(format(start_date, "%Y"))
# startMonth <- as.integer(format(start_date, "%m"))
#
# endYear <- as.integer(format(end_date, "%Y"))
# endMonth <- as.integer(format(end_date, "%m"))
#
# #startMonth <- as.integer(format(as.Date(StartDate, format = "%Y-%m-%d"), "%m"))
# #endMonth <- as.integer(format(as.Date(EndDate, format = "%Y-%m-%d"), "%m"))
# # Determine the start season
# startSeason = "" #initialize endSeason
# if (startMonth >= 1 && startMonth <= 7) {
#   startSeason <- paste(startYear - 1, startYear, sep = "-")
# } else {
#   startSeason <- paste(startYear, startYear + 1, sep = "-")
# }
#
# # Determine the end season
# endSeason = "" #initialize endSeason
# if (endMonth >= 1 && endMonth <= 5) {
#   endSeason <- paste(endYear - 1, endYear, sep = "-")
# } else {
#   endSeason <- paste(endYear, endYear + 1, sep = "-")
# }
# startSeason
# endSeason
# options(tibble.width = Inf)
# # Reshape data to long format
# enso <- findDateSubset(enso_data, startSeason, endSeason)
# print(enso, n = 22)
# enso_subset <- enso %>% select(-c(Season))
# enso_arr <- as.vector(t(enso_subset)) #create array of ONI values
# val_arr <- rollingAvgTest(enso_arr, startMonth, endMonth) #get individual ONI average for each month
# val_arr




