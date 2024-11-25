#' Functions that visualize spatiotemporal data using heatmaps
#'
#' @param X n by p matrix containing n data points to cluster.
#'
#' @return a data matrix with correct format
#' @export
#'
#' @examples
#' # Give example



# Functions that visualize spatiotemporal data using heatmaps

# Function that subsets the data and reformats to be appropriate input for other functions
###############################################################################################
# Description of supplied parameters:
# X - n x p training data, input format is .Rda file
# y - a vector of size n of class labels
# OUTPUT

#convert csv to Rda file
#X <- read.csv("Data/Drought.csv")

# Save the data as an Rda file
#save(X, file = "Drought.Rda")

#check that it works
#load("Drought.Rda")
#print(X[1:10])

#load in the dataset
load("Data/Drought.Rda")
#ls()
X <- X #name of data variable is X



## Add a function that subsets the data to take only relevant columns
## and reshape format
SubsetData <- function(X) {
  library(reshape2)

  # select relevant columns
  X_subset <- X[, c("MapDate", "County", "None", "D0", "D1", "D2", "D3", "D4")]

  # reshape data to long format from wide format
  X_melt <- melt(X_subset, id.vars = c("MapDate", "County"),
                 variable.name = "Drought_Level", value.name = "Percentage")

  # change the type from MapDate to Date type
  X_melt$MapDate <- as.Date(as.character(X_melt$MapDate), format = "%Y%m%d")

  return(X_melt)
}



## Return output
##########################################################################




#' Functions that visualize spatiotemporal data using heatmaps
#'
#' @param X n by p matrix containing n data points to cluster.
#' @param K An integer specifying number of clusters.
#' @param M K by p matrix of cluster centers.
#'
#' @return Explain return
#' @export
#'
#' @examples
#' # Give example


# Function that assigns clusters and reformats to be appropriate input for other functions
###############################################################################################
# Description of supplied parameters:
# X - n x p training data
# y - a vector of size n of class labels
# OUTPUT


## Use hierarchical clustering with Euclidean metric to determine clusters
# there are supposed to be 6 clusters for 6 drought severity types
AssignClusters <- function(X, K = 6, M = NULL) {
  # Compute average percentages over time for each county
  X_avg <- X %>%
    group_by(County, Drought_Level) %>%
    summarise(Avg_Percentage = mean(Percentage, na.rm = TRUE)) %>%
    ungroup()

  # Reshape data to wide format
  X_wide = dcast(X_avg, County ~ Drought_Level, value.var = "Avg_Percentage")


}



## Return output
##########################################################################



# Function that creates heatmap from the data using other helper functions
###############################################################################################
# Description of supplied parameters:
# X - n x p training data
# y - a vector of size n of class labels
# OUTPUT

#'
#' @param X n by p matrix containing n data points to cluster. #change this
#'
#' @return a dataframe ready to be plotted
#' @export
#'
#' @examples
#' # Give example

CreateHeatmap <- function(X) {

}



## Return output
##########################################################################



# Function that plots data and allows for more control over plotting parameters
###############################################################################################
# Description of supplied parameters:
# X - n x p training data
# y - a vector of size n of class labels
# OUTPUT

PlotHeat <- function(X) {

}



## Return output
##########################################################################
