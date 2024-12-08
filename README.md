## Package `stheatmap`: Heatmap for Large Spatiotemporal Series 
### Includes Drought Heatmap and ENSO Barplot Spatiotemporal Data Visualizations 

### Radhika Kulkarni


#### When we have a large volume of data associated with geographic locations, taken over a period of time, it is often difficult to see the relevant patterns inherent in the data. 
In this package we will visualize California Drought data taken from [US Drought Monitor](https://droughtmonitor.unl.edu/DmData/DataDownload/ComprehensiveStatistics.aspx)  using a function that creates heatmaps generated using agglomerative hierarchical clustering with euclidean distance metric. 
We also visualize [El Nino Southern Oscillation Data](https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php) (ENSO) data using a rolling average smoother which captures the cyclical nature of ENSO (seasonal trends). These bigger picture visualizations can show us how the global and local variables interrelate with each other on the grand scale and can reveal some interesting associations in specific geographical areas or time points.  

Please see the [vignette](https://github.com/rrkulkarni108/stheatmap/blob/master/vignettes/stheatmap-vignette.Rmd) on the analysis and comparison of these two data on their specialized plots. The Drought data is subset to be spatially in California, and the El Nino/La Nina oscillation cycle values are a global weather pattern. To investigate how these global and local weather patterns are associated, time series will be over the time period of 20 years, from 2001-2021. If wishing to visually compare another state's drought patterns with the ENSO effect, you may download the data for that specific state. 

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

Build the Vignette:

```html
devtools::install_github("rrkulkarni108/stheatmap", build_vignettes = TRUE)
```

## Using The Package: 

### Two Functions, Three Plots
* `drought_main(drought_data, start_date, end_date)`
  * Plot: Heatmap for Drought data
  * **Example** usage:
    ```
    # load dataset that comes with package
    load(Data/Drought.rda)

    # user is interested to subset drought data from January 1, 2001 to June 1, 2021
    drought_main(Drought, start_date = "2001-01-01", end_date = "2021-06-01")

    ```
    ![image](https://github.com/user-attachments/assets/37afb80d-d7a6-45f9-9957-a911c8f13eb3)
* `enso_main(data, StartDate, EndDate)`
   * Plot 1 : Time Series Line Plot for Overall Trend at a glance
   * Plot 2:  ENSO barplot using Rolling Avg Smoother
   *   **Example** usage:
        ```
        # load dataset that comes with package
        load(Data/ENSO.rda)
    
        # user is interested to subset ENSO data from January 1, 2021 to January 1, 2006
        enso_main(ENSO, StartDate = "2001-01-01", EndDate = "2006-01-01")
        ```

### ENSO Data Usage:
* You must use the datatable from [National Weather Service, Climate Prediction Center](https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php) as input to the `enso_main()` function.

* If the link breaks, here is the URL : https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php

### Drought Heatmap Usage:

You must use the ***cleaned*** data from US Drought Monitor as input to `drought_main()` function:
   * Download the data from from ([US Drought Monitor](https://droughtmonitor.unl.edu/DmData/DataDownload/ComprehensiveStatistics.aspx)):
       * Start and End date (specified by user)
       * Area Type: State 
       * Statistics category: Percent Area (the percentage of the area of that county which has the specified drought severity category)
       * Statistics type: Cumulative (percentage of area adds up to 100%)
       * Output format : CSV
   * Clean the data:
        * Drop all columns except MapDate, None, D0, D1, D2, D3, D4, ValidStart
        * Your data is now ready to be input

## Vignette
For more information on how to use the package and analyses, see the vignette ([here](https://github.com/rrkulkarni108/stheatmap/blob/master/vignettes/stheatmap-vignette.Rmd)).




