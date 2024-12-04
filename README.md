## Package `stheatmap`: Heatmap for Large Spatiotemporal Series 
### Includes Drought Heatmap and ENSO Barplot SpatioTemporal Data Visualizations 

### Radhika Kulkarni


#### When we have a large volume of data associated with geographic locations, taken over a period of time, it is often difficult to see the relevant patterns inherent in the data. In this package we will visualize California Drought data taken from US Drought Monitor ([US Drought Monitor](https://droughtmonitor.unl.edu/DmData/DataDownload/ComprehensiveStatistics.aspx))  using a function that creates heatmaps generated using agglomerative hierarchical clustering with euclidean distance metric on the data. We also visualize [El Nino Southern Oscillation Data](https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php) (ENSO) data using a rolling average smoother which captures the cyclical nature of ENSO (seasonal trends). These bigger picture visualizations can show us how the global and local variables interrelate with each other on the grand scale and can reveal some interesting associations in specific geographical areas or time points.  

Please see the vignette on the analysis and comparison of these two data on their specialized plots. The Drought data is subset to be spatially in California, and the El Nino/La Nina oscillation cycle values are a global weather pattern. To investigate how these global and local weather patterns are associated, time series will be over the time period of 20 years, from 2001-2021. If wishing to visually compare another state's drought patterns with the ENSO effect, you may download the data for that specific state. 

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
Overall there is a Drought heatmap function and a plotting function which contains two plots for ENSO data. 

### ENSO Data Usage:
* You must use as input to enso_main() function- the datatable from [National Weather Service, Climate Prediction Center](https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php).

* If link breaks, here is URL : https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php

### Drought Heatmap Usage:

* You must use as input to drought_main() function-the **cleaned** data from US Drought Monitor:
* Download the data from from ([US Drought Monitor](https://droughtmonitor.unl.edu/DmData/DataDownload/ComprehensiveStatistics.aspx)):
    * your specified start and end date
    * Area Type: State 
    * Statistics category: Percent Area (the percentage of the area of that county which has the specified drought severity category)
    * Statistics type: Cumulative (percentage of area adds up to 100%)
    * Output format : CSV
* Clean the data:
     * Drop all columns except MapDate, None, D0, D1, D2, D3, D4, ValidStart
     * Your data is now ready to be input

## Still need to complete:
* Add the example for drought_main()
* Need to ensure R style is correct across all files
* Make vignette on my sample dataset


