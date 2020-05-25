# tidylodes
#### An R Package for extracting, processing and 'spatialising' data from the US Census Bureau's LODES database

## Description: 
This package enables users to extract datasets directly from the US Census Bureau's database of Longitudinal
Origin-Destination Employment Statistics (LODES).

The database contains three different datasets:
  
  1. **Workplace Area Characteristics (WAC)** - Total and sector-specific job estimates at census block level for census blocks classified as 'workplace areas.' The data set does not contain data for every census block, only those classified as workplace areas.
  
  2. **Residence Area Characteristics (RAC)** - Total and sector-specific job estimates at census block level for census blocks classified as 'residence areas.' The dataset does not contain data for every census block, only those classified as residence areas.
  
  3. **Origin-Destination Job Flows (OD)** - Estimated job flows at census block level, from residence areas to workplace areas. The dataset only includes flows of greater than or equal to 1. 

*tidylodes* allows users to download data from LODES (WAC, RAC or OD) for specific states and for specific years. The package cleans the datasets and improves the recording of census geographies within them. The package also offers users the opportunity to convert these datasets into spatial formats for spatial exploration/manipulation/analysis. It is important to stress that the datasets downloaded with this package contain only information on workplace areas, residence areas or those with valid OD flows, thus not every census block is accounted for. 

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

## Installation

```{r}
## Need to link to github here!
```



## Usage 

```{r setup}
## Setup
library(tidylodes)
```

Each dataset (WAC, RAC and OD) has its own set of functions that can be used to extract, manipulate and/or convert to a spatial format:

```{r}
## List all available functions
ls("package:tidylodes")
```

As an example, let's go through how to use the various functions specific to **WAC** data:

```{r}
## Extract WAC data for Delaware, from 2013
de_wac <- get_wac_data("de", "2013")
de_wac[1:2, ]
```

```{r}
## Reduce the dimensionality of de_wac to focus on one job sector - e.g. Retail Trade
de_wac_rt <- get_jobsector_wac(de_wac, job_code = "Retail_Trade",
                               job_proportion = T)
de_wac_rt[1:2, ]
```

```{r echo=TRUE, message=TRUE, warning=FALSE, results='hide', fig.keep='all'}
## Convert the simple features
de_wac_rt_sf <- get_wac_spatial(de_wac_rt)
```

```{r}
de_wac_rt_sf[1:2, ]
```


The **WAC** and **RAC** have identical functions, so the above steps can be repeated to obtain LODES data for Residential Areas (RAC) by substituting the wac function as below:

| Function Purpose                | WAC                   | RAC                 |
| --------------------------------|:---------------------:| -------------------:|
| Download full dataset           | *get_wac_data*        | *get_rac_data*      |
| Subset to specific job sector   | *get_jobsector_wac*   | *get_jobsector_rac* |
| Convert to spatial format       | *get_wac_spatial*     | *get_rac_spatial*   |


The **OD** datasets have different functions:

```{r}
## Download OD data for Delaware, from 2013
de_od <- get_od_data("de", "2013", main = T)
de_od[1:2, ]
```

```{r}
## Subset data to include only those rows of data where flows exceed a certain threshold
de_od_sub <- get_od_subset(de_od, flow_threshold = 30)
de_od_sub[1:2, ]
```

```{r warning=FALSE}
## Convert to a format that enables plotting of flow lines between census block centroids
de_od_sub_sp <- get_od_spatial(de_od_sub)
de_od_sub_sp[1:2, ]
```

## Examples and Suggested Application

**Plotting choropleths of WAC/RAC data**

```{r echo=FALSE, message=FALSE, warning=FALSE, results='hide', fig.keep='all'}
## Get RAC data for Vermont, from 2017 
vt_rac <- get_rac_data("vt", "2017")
## Convert to simple feature
vt_rac_sf <- get_rac_spatial(vt_rac)
```

These WAC/RAC datasets are large, and mapping these will take considerable time. However, the nature of converting WAC/RAC to sf means that they become easy to manipulate using the tidyverse, so areas of particular interest can be selected (e.g. specific tracts), and then mapped using tmap.

```{r message = FALSE, warning=FALSE }
#install.packages("tidyverse")
library(tidyverse)
#install.packages("tmap")
library(tmap)
## Subset dataset to specific County & Tract
area_of_interest <- vt_rac_sf %>%
  filter(CountyID == "007", TractID == "000200")
## Map 
tm_shape(area_of_interest) +
  tm_fill(col = "Retail_Trade", style = "jenks",
          title = "Retail Trade Jobs", palette = "YlOrRd", n = 5) +
  tm_layout(legend.outside = T, frame = FALSE) 
```
