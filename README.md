## Package `stheatmap`: Heatmap for Large Spatiotemporal Series 
## Includes ENSO SpatioTemporal Data Visualizations 

### Radhika Kulkarni


#### When we have a large volume of data associated with geographic locations, taken over a period of time, it is often difficult to see the relevant patterns inherent in the data. In this package we will visualize such data using a function that creates heat maps generated using agglomerative hierarchical clustering with euclidean distance metric on the data. This bigger picture visualization can show us how the variables interrelate with each other on the grand scale and can reveal some interesting associations in specific geographical areas or time points. In this package will also be two plotting functions in `enso_main()` which are specifically for visualizing [El Nino Southern Oscillation Data](https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php). 

I will then also apply this function on a large dataset which involves drought ([US Drought Monitor](https://droughtmonitor.unl.edu/DmData/DataDownload/ComprehensiveStatistics.aspx)) and wildfires ([NASA Earthdata](https://firms.modaps.eosdis.nasa.gov/active_fire/)) in California over the time period of 20 years, and El Nino/La Nina oscillation cycle, which can be useful in analysis of how these global and local weather patterns are associated. If time permits I will also implement some plotting functions which show the data on the map of California, which can be extended also to any geographic location.  

## Installation Instructions:
### Step 1: Download [Rtools](https://cran.rstudio.com/bin/windows/Rtools/) for Windows or [Xcode](https://apps.apple.com/us/app/xcode/id497799835?mt=12) for Mac.
### Step 2: Install R package `devtools`:
Install `devtools` : 
```html
install.packages("devtools")
```
### Step 3: Install `stheatmap` using devtools
Install the `stheatmap` package using:
```
devtools::install_github("rrkulkarni108/stheatmap")
```

## Using the package: 
Overall there is a heatmap function (TBD) and two plotting functions for ENSO data. 
* You must use as input to enso_main() function- the datatable from [National Weather Service, Climate Prediction Center](https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php).
* Input to heatmap function must come from [US Drought Monitor](https://droughtmonitor.unl.edu/DmData/DataDownload/ComprehensiveStatistics.aspx) data download.

* If link breaks, here is URL : https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php

## Still need to complete:
* There are bugs in the heatmap code so I need to complete the functions and fix the errors
* need to ensure R style is correct across all files
* fix warnings
* make vignette on my sample dataset if time permits, or make my functions faster using C++
* add more plotting using shp files if time permits


