## Package `stheatmap`: Heatmap for Large Spatiotemporal Series 
## Includes ENSO SpatioTemporal Data Visualizations 

### Radhika Kulkarni


#### When we have a large volume of data associated with geographic locations, taken over a period of time, it is often difficult to see the relevant patterns inherent in the data. In this package we will visualize such data using a function that creates heat maps generated using agglomerative hierarchical clustering with euclidean distance metric on the data. This bigger picture visualization can show us how the variables interrelate with each other on the grand scale and can reveal some interesting associations in specific geographical areas or time points. In this package will also be two plotting functions in plotENSOSeries() which are specifically for visualizing [El Nino Southern Oscillation Data](https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php). 

I will then also apply this function on a large dataset which involves drought (US Drought Monitor) and wildfires (NASA) in California over the time period of 20 years, and El Nino/La Nina oscillation cycle, which can be useful in analysis of how these global and local weather patterns are associated. If time permits I will also implement some plotting functions which show the data on the map of California, which can be extended also to any geographic location.  

## Installation Instructions:
#### Install the `stheatmap` package using install.packages("stheatmap")
### Using the package: You must use as input to enso_main() function- the datatable from [National Weather Service, Climate Prediction Center](https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php). 

* If link breaks, here is URL : https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php

## Still need to complete:
* There are bugs in the heatmap code so I need to complete the functions and fix the errors
* need to ensure R style is correct across all files
* fix warnings
* make vignette on my sample dataset if time permits, or make faster my functions using C++
* add more plotting using shp files if time permits


