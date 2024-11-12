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
# X - n x p training data
# y - a vector of size n of class labels
# OUTPUT

#X <- readxl::read_excel("Data/Drought.xlsx")



## Add a function that subsets the data to take only relevant columns
## and reshape format
SubsetData <- function(X){

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
AssignClusters <- function(X, K, M = NULL){


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

CreateHeatmap <- function(X){


}



## Return output
##########################################################################



# Function that plots data and allows for more control over plotting parameters
###############################################################################################
# Description of supplied parameters:
# X - n x p training data
# y - a vector of size n of class labels
# OUTPUT

PlotHeat <- function(X){


}



## Return output
##########################################################################
