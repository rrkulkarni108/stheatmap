% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Heatmap_Functions.R
\name{drought_main}
\alias{drought_main}
\title{Main function that visualizes drought data using heatmaps, calls all other functions}
\usage{
drought_main(drought_data, start_date, end_date)
}
\arguments{
\item{drought_data}{n by p matrix containing n data points to cluster.}

\item{start_date}{string of format "yyyy-mm-dd", starting date of time series}

\item{end_date}{string of format "yyyy-mm-dd", ending date of time series}
}
\value{
a heatmap plot of the state's county names on x axis, week time series on y axis and heatmap colored by drought severity category
}
\description{
Main function that visualizes drought data using heatmaps, calls all other functions
}
\examples{
# Give example
set.seed(123)
# Manually create a data frame
drought_data_example <- data.frame(
  MapDate = c(20210101, 20210201, 20210207, 20210304 , 20210315,
   20210404, 20210525, 20210518, 20210511, 20210504),
  FIPS = rep(6001, 10),
  County = c(rep("Alameda County", 5), rep("Alpine County", 5)),
  State = rep("CA", 10),
  None = c(0.00, 0.00, 0.00, 0.00, 0.00,
   0.00, 0.00, 0.00, 0.00, 0.00),
  D0 = c(0.00, 0.00, 0.00, 0.00, 0.00,
  0.00, 0.00, 0.00, 0.00, 0.00),
  D1 = c(0.00, 0.00, 0.00, 0.00, 0.00,
   0.00, 0.00, 0.00, 0.00, 0.00),
  D2 = c(0.00, 0.00, 0.00, 0.00, 0.00,
   0.00, 0.00, 0.00, 0.00, 0.00),
  D3 = c(36.14, 36.14, 60.04, 100.00, 100.00,
   36.14, 60.04, 100.00, 100.00, 100.00 ),
  D4 = c(63.86, 63.86, 39.96, 0.00, 0.00,
   63.86, 39.96, 0.00, 0.00, 0.00),
  ValidStart = as.Date(c("2021-01-01", "2021-02-01", "2021-02-07",
   "2021-03-04","2021-03-15", "2021-04-04", "2021-05-25", "2021-05-18",
    "2021-05-11", "2021-05-04"))
)

drought_data_example

# Run drought_main(), from January 2021 to May 2021, for two counties
drought_main(drought_data = drought_data_example,
start_date = "2021-01-01", end_date = "2021-05-25")
}
