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

  # Name data with counties
  rownames(X_wide) <- X_wide$County

  # Extract data for clustering algorithm
  X_data <- X_wide[, c("None", "D0", "D1", "D2", "D3", "D4")]
  X_data <- as.matrix(X_data)

  # Compute the distance matrix
  dist_mat <- dist(X_data, method = "euclidean")

  # Hierarchical clustering
  hc <- hclust(dist_mat, method = "average")

  # Cut our dendrogram into K clusters
  clusters <- cutree(hc, k = K)

  # Assign clusters to X_wide
  X_wide$Cluster <- clusters

  # Compute cluster summaries
  cluster_summary <- X_wide %>%
    group_by(Cluster) %>%
    summarise_at(vars("None", "D0", "D1", "D2", "D3", "D4"), mean, na.rm = TRUE)

  # Determine Drought_Level labels for clusters
  # A new column Drought_Level is added to cluster_summary, indicating the dominant drought level for each cluster.
  cluster_summary$Drought_Level <- apply(cluster_summary[, c("None", "D0", "D1", "D2", "D3", "D4")], 1, function(x) {
    names(x)[which.max(x)]
  })

  # Merge Drought_Level labels back to X_wide
  X_wide <- merge(X_wide, cluster_summary[, c("Cluster", "Drought_Level")], by = "Cluster")

  return(list(X_wide = X_wide, hc = hc, cluster_summary = cluster_summary))

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
  # Subset the data to relevant columns and reshape to long format
  X_melt <- X %>%
    select(MapDate, County, Cluster) %>%
    mutate(Week = as.numeric(MapDate)) # Add a numeric week column for plotting

  # Convert the Cluster column into a factor for consistent coloring
  X_melt$Cluster <- as.factor(X_melt$Cluster)

  return(X_melt)
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
  # Plot the heatmap
  #reorder weeks from least to greatest
  X$Week <- factor(X$Week, levels = rev(sort(unique(X$Week))))
  library(stringr)
  X$Week <- str_wrap(X$Week, width = 5)
  heatmap_plot <- ggplot(data = X, aes(x = County, y = Week, fill = Cluster)) +
    geom_tile() +
    scale_fill_manual(
      values = c(
        "1" = "green",
        "2" = "yellowgreen",
        "3" = "yellow",
        "4" = "orange",
        "5" = "red",
        "6" = "darkred"
      ),
      name = "Severity"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 8),  # set font size
      plot.margin = margin(10, 10, 10, 20)  # make left margin bigger to fit  y-axis labels
    ) +
    labs(
      title = "Heatmap of County-Level Drought Severity",
      x = "County",
      y = "Week"
    )

  print(heatmap_plot)
}



## Return output
##########################################################################









AssignClustersByWeek <- function(X, K = 6) {
  # Compute average percentages over time for each week in each county
  #Week <- X$MapDate
  X_avg <- X %>%
    group_by(County, MapDate, Drought_Level) %>%
    summarise(Avg_Percentage = mean(Percentage, na.rm = TRUE)) %>%
    ungroup()

  # Reshape data to wide format
  X_wide = dcast(X_avg, County + MapDate ~ Drought_Level, value.var = "Avg_Percentage")

  # Extract relevant columns for clustering
  X_data <- X_wide[, c("None", "D0", "D1", "D2", "D3", "D4")]
  X_data <- as.matrix(X_data)

  # Compute the distance matrix
  dist_mat <- dist(X_data, method = "euclidean")

  # Perform hierarchical clustering
  hc <- hclust(dist_mat, method = "average")

  # Cut the dendrogram into K clusters
  clusters <- cutree(hc, k = K)

  # Assign clusters to the wide-format data
  X_wide$Cluster <- clusters

  # Compute cluster summaries
  cluster_summary <- X_wide %>%
    group_by(Cluster) %>%
    summarise_at(vars("None", "D0", "D1", "D2", "D3", "D4"), mean, na.rm = TRUE)

  # Determine dominant drought levels for clusters
  cluster_summary$Drought_Level <- apply(cluster_summary[, c("None", "D0", "D1", "D2", "D3", "D4")], 1, function(x) {
    names(x)[which.max(x)]
  })

  # Merge cluster labels back into the data
  X_wide <- merge(X_wide, cluster_summary[, c("Cluster", "Drought_Level")], by = "Cluster")

  # Assign cluster categories for each week in each county
  X_with_clusters <- X %>%
    left_join(X_wide[, c("County", "MapDate", "Cluster", "Drought_Level")], by = c("County", "MapDate"))

  return(list(X_with_clusters = X_with_clusters, hc = hc, cluster_summary = cluster_summary))
}






# This function identifies which counties are in each cluster severity

county_severities <- function(cluster_results, K = 6, pretty_print = TRUE) {
  #cluster_df <- AssignClustersByWeek(X, K = 6)
  # Extract relevant data from cluster_results
  X_with_clusters <- cluster_results$X_with_clusters
  cluster_summary <- cluster_results$cluster_summary

  # Group by Cluster and include Severity
  counties_by_cluster <- X_with_clusters %>%
    group_by(Cluster) %>%
    summarise(
      Severity = first(Drought_Level),  # Get the drought level for the cluster
      Counties = paste(unique(County), collapse = ", ")  # List unique counties in the cluster
    )

  # Print the results
  print(counties_by_cluster)
  if (pretty_print == TRUE){
    # Loop to print each cluster's details
    for (i in seq_len(nrow(counties_by_cluster))) {
      cat(paste("Cluster", counties_by_cluster$Cluster[i],
                "(Severity:", counties_by_cluster$Severity[i], "):\n"))
      cat(counties_by_cluster$Counties[i], "\n\n")
    }
  }


}






#Test code with dataset
#load in the dataset
load("Data/Drought.Rda")
#ls()
X <- X #name of data variable is X
X_melt <- SubsetData(X)
cluster_results <- AssignClusters(X_melt, K= 6)
by_week <- AssignClustersByWeek(X_melt, K = 6)
X_with_clusters <- by_week$X_with_clusters
heatmap_data <- CreateHeatmap(X_with_clusters)
PlotHeat(heatmap_data)

# vals <- county_severities(X_melt, K = 6, pretty_print = TRUE)
# vals
#
#
# county_severities(by_week, K = 6, pretty_print = TRUE)
#












# # Group by Cluster to summarize counties and dominant drought severity
# counties_by_cluster <- X_with_clusters %>%
#   group_by(Cluster) %>%
#   summarise(
#     Severity = first(Drought_Level.y),  # Dominant drought level for the cluster
#     Counties = paste(unique(County), collapse = ", ")  # List unique counties in the cluster
#   )
#
# # Print the results
# print(counties_by_cluster)
#
# # Pretty-print the results for better readability
# for (i in seq_len(nrow(counties_by_cluster))) {
#   cat(paste("Cluster", counties_by_cluster$Cluster[i],
#             "(Severity:", counties_by_cluster$Severity[i], "):\n"))
#   cat(counties_by_cluster$Counties[i], "\n\n")
# }

