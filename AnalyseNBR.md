LandTrendr-Paramo NBR Analysis
================
James Millington
2020-07-22

  - [Aim and Rationale](#aim-and-rationale)
  - [Data](#data)

## Aim and Rationale

We want to compare/characterise (pixel-by-pixel) pre-disturbance value
(within Paramo) for LandTrendR outputs for 144 different parameters sets
and for time window used to create the outputs (full year vs 4 months).

We need this because we will want to filter LandTrenR results by some
pre-isturbance value at some point, but we don’t know what that value is
yet.

We will compare pairs of pixels that have a disturbance in both full
year and 4-month windows *in the same year* (but we can also do summary
stats \[below\] for *all* pixels).

Ultimately we want to examine boxplots (and summary stats) of
pre-disturbance values to compare full vs 4m-month for each pair of 144
images. To do this we likely need a table that contains the following
columns:

  - parameterSet (1-144)
  - pixelid,
  - year,
  - subset(full vs 4month),
  - pre-D,
  - Magnitude,
  - Duration, DSNR, Rate \[if getting these for each pixel doesn’t add
    to much processing time\]

## Data

``` r
library(raster)
library(sf)


LW1 <- raster("Data/NBRanalysis/LTR_AllYear_1.tif")

GISpath <- "/home/james/OneDrive/Research/Projects/ColombiaBIO/Fire/Fire GIS Files/" #linux
#GISpath <- "E:/OneDrive - King's College London/Research/Projects/ColombiaBIO/Fire/Fire GIS Files/"  #windows

buf <- st_read(paste0(GISpath,"500mbufferOfComplejos/GroundTruthingBuffer500m.shp"))
```

    ## Reading layer `GroundTruthingBuffer500m' from data source `/home/james/OneDrive/Research/Projects/ColombiaBIO/Fire/Fire GIS Files/500mbufferOfComplejos/GroundTruthingBuffer500m.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 17 features and 11 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 1024933 ymin: 1060489 xmax: 1197690 ymax: 1202938
    ## CRS:            3116

``` r
#set crs of shp to same as for raster
buf <- st_transform(buf, "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

plot(LW1)
plot(st_geometry(buf), add=T)
```

![](AnalyseNBR_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
names(LW1)
```

    ## [1] "LTR_AllYear_1"

``` r
SW1 <- raster("Data/NBRanalysis/LTR_JanApr_1.tif")
plot(SW1)
plot(st_geometry(buf), add=T)
```

![](AnalyseNBR_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->