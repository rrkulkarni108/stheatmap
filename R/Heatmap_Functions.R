


# Function that subsets the data and reformats to be appropriate input for other functions
###############################################################################################
# Description of supplied parameters:
# X - n x p cleaned training data, input format is .Rda file
# start_date string of format "yyyy-mm-dd", starting date of time series
# end_date string of format "yyyy-mm-dd", ending date of time series
# OUTPUT


# Add a function that subsets the data to take only relevant columns (MapDate, County, None, D0-D4) and reshape format
# Returns a data frame subsetted by the start and end date of MapDate, and having additional column Week
SubsetData <- function(X, start_date, end_date) {
  # Select relevant columns
  X_subset <- X[, c("MapDate", "County", "None", "D0", "D1", "D2", "D3", "D4")]

  # Convert MapDate to Date type and filter by start_date and end_date
  X_subset <- X_subset %>%
    mutate(MapDate = as.Date(as.character(MapDate), format = "%Y%m%d")) %>%
    filter(MapDate >= as.Date(start_date) &
             MapDate <= as.Date(end_date))

  # Reshape data to long format
  X_melt <- reshape2::melt(
    X_subset,
    id.vars = c("MapDate", "County"),
    variable.name = "Drought_Level",
    value.name = "Percentage"
  )

  # Add a numeric Week column
  X_melt <- X_melt %>%
    mutate(Week = as.numeric(difftime(MapDate, min(MapDate), units = "weeks")))

  return(X_melt) # Return dataframe
}


## Return output
##########################################################################




## Use hierarchical clustering with Euclidean metric to determine clusters
# there are supposed to be 6 clusters for 6 drought severity types
AssignClusters <- function(X, K = 6) {
  # Compute average percentages over time for each county
  X_avg <- X %>%
    group_by(County, Drought_Level) %>%
    summarise(Avg_Percentage = mean(Percentage, na.rm = TRUE)) %>%
    ungroup()

  # Reshape data to wide format
  X_wide = reshape2::dcast(X_avg, County ~ Drought_Level, value.var = "Avg_Percentage")

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

  return(list(
    X_wide = X_wide,
    hc = hc,
    cluster_summary = cluster_summary
  ))

}

## Return output
##########################################################################


# Function that creates the dataframe structure for the heatmap;
# returns the reformatted dataframe to be input to PlotHeat() function
###############################################################################################
# Description of supplied parameters:
# X - dataframe of X_with_clusters taken from cluster output of AssignClustersbyWeek() function
# OUTPUT dataframe with columns MapDate, County, Cluster, Week


# Function which takes the X_with_clusters dataframe from AssignClustersbyWeek() function output
# and selects columns MapDate, County, Cluster, Week, and makes Cluster a factor
# Returns a n x 4 dataframe for input to PlotHeat() function
CreateHeatmap <- function(X) {
  # Subset the data to relevant columns and reshape to long format
  X_melt <- X %>%
    select(MapDate, County, Cluster) %>%
    mutate(Week = as.numeric(MapDate)) # Add a numeric week column for plotting

  # Convert the Cluster column into a factor for consistent coloring
  X_melt$Cluster <- as.factor(X_melt$Cluster)

  return(X_melt)
}


## Return output: n x 4 dataframe for input to PlotHeat() function
##########################################################################



# Function that plots data and allows for more control over plotting parameters
###############################################################################################
# Description of supplied parameters:
# X - n x 4 dataframe from CreateHeatmap() function
# palette - a vector of size 6 of color and hexcode for clusters, which can be specified by user. Default vector color scheme given.
# OUTPUT - heatmap of the drought clusters with y-axis the weeks from 1 to Week n and X-axis the labels of all the counties in the state. Colored by palette scheme.

PlotHeat <- function(X,
                     palette = c(
                       "1" = "#008000",
                       #None severity
                       "2" = "#66BD63",
                       #D0 severity
                       "3" = "yellow",
                       #D1 severity
                       "4" = "orange",
                       #D2 severity
                       "5" =  "red",
                       #D3 severity
                       "6" =  "darkred" #D4 severity
                     )) {
  # Plot the heatmap
  X$County <- stringr::str_remove(X$County, " County") #remove the word county since it takes a lot of space in X label
  #reorder weeks from least to greatest
  X$Week <- as.integer(factor(X$Week, levels = (sort(unique(
    X$Week
  )))))#X$Week <- factor(X$Week, levels = rev(sort(unique(X$Week))))
  heatmap_plot <- ggplot(data = X, aes(x = County, y = Week, fill = Cluster)) +
    geom_tile() +
    scale_fill_manual(values = palette, name = "Drought Severity") +
    scale_y_reverse(breaks = seq(0, max(X$Week), by = 50)) +  # This flips the Y-axis
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 8),
      # set font size
      #plot.margin = margin(10, 10, 10, 20)  # make left margin bigger to fit  y-axis labels
    ) +
    labs(title = "Heatmap of County-Level Drought Severity", x = "County", y = "Week")

  print(heatmap_plot)
}



## Return output - heatmap of the drought clusters with y-axis the weeks from 1 to Week n and X-axis the labels of all the counties in the state. Colored by palette scheme.
##########################################################################








# Function that assigns clusters and reformats to be appropriate input for other functions
###############################################################################################
# Description of supplied parameters:
# X - n x 5 dataframe with columns MapDate, County, Drought_Level, Percentage, Week
# K - an integer value of number of clusters, for Drought data typically 6 by default
# OUTPUT list of clusters for each week, hierarchical clustering method description, cluster summary


# Input: Dataframe with columns MapDate, County, Drought_Level, Percentage, Week
AssignClustersByWeek <- function(X, K = 6) {
  # Compute average percentages over time for each week in each county
  X_avg <- X %>%
    group_by(County, MapDate, Drought_Level) %>%
    summarise(Avg_Percentage = mean(Percentage, na.rm = TRUE)) %>%
    ungroup()

  # Reshape data to wide format
  X_wide = reshape2::dcast(X_avg, County + MapDate ~ Drought_Level, value.var = "Avg_Percentage")

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

  return(list(
    X_with_clusters = X_with_clusters,
    hc = hc,
    cluster_summary = cluster_summary
  ))
}


#' Main function that visualizes drought data using heatmaps, calls all other functions
#'
#' @param drought_data n by p matrix containing n data points to cluster.
#' @param start_date string of format "yyyy-mm-dd", starting date of time series
#' @param end_date string of format "yyyy-mm-dd", ending date of time series
#' @return a heatmap plot of the state's county names on x axis, week time series on y axis and heatmap colored by drought severity category
#' @export
#'
#' @examples
#' # Give example
#' set.seed(123)
#' # Manually create a data frame
#' drought_data_example <- data.frame(
#'   MapDate = c(20210101, 20210201, 20210207, 20210304 , 20210315,
#'    20210404, 20210525, 20210518, 20210511, 20210504),
#'   FIPS = rep(6001, 10),
#'   County = c(rep("Alameda County", 5), rep("Alpine County", 5)),
#'   State = rep("CA", 10),
#'   None = c(0.00, 0.00, 0.00, 0.00, 0.00,
#'    0.00, 0.00, 0.00, 0.00, 0.00),
#'   D0 = c(0.00, 0.00, 0.00, 0.00, 0.00,
#'   0.00, 0.00, 0.00, 0.00, 0.00),
#'   D1 = c(0.00, 0.00, 0.00, 0.00, 0.00,
#'    0.00, 0.00, 0.00, 0.00, 0.00),
#'   D2 = c(0.00, 0.00, 0.00, 0.00, 0.00,
#'    0.00, 0.00, 0.00, 0.00, 0.00),
#'   D3 = c(36.14, 36.14, 60.04, 100.00, 100.00,
#'    36.14, 60.04, 100.00, 100.00, 100.00 ),
#'   D4 = c(63.86, 63.86, 39.96, 0.00, 0.00,
#'    63.86, 39.96, 0.00, 0.00, 0.00),
#'   ValidStart = as.Date(c("2021-01-01", "2021-02-01", "2021-02-07",
#'    "2021-03-04","2021-03-15", "2021-04-04", "2021-05-25", "2021-05-18",
#'     "2021-05-11", "2021-05-04"))
#' )
#'
#' drought_data_example
#'
#' # Run drought_main(), from January 2021 to May 2021, for two counties
#' drought_main(drought_data = drought_data_example,
#' start_date = "2021-01-01", end_date = "2021-05-25")

drought_main <- function(drought_data, start_date, end_date) {
  drought_data_subset <- SubsetData(drought_data, start_date, end_date)  # Subset and reshape drought data
  drought_clusters <- AssignClustersByWeek(drought_data_subset, K = 6)  # Assign clusters
  drought_heatmap_data <- CreateHeatmap(drought_clusters$X_with_clusters)  # Prepare data for heatmap

  # Generate the drought heatmap plot
  drought_plot <- PlotHeat(drought_heatmap_data)

  return(plot = drought_plot)
}
