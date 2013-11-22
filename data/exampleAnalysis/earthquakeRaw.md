Raw Earthquake Analysis
========================

------

### Load libraries


```r
library(maps)
library(Hmisc)
```

```
## Loading required package: survival Loading required package: splines
## Loading required package: Formula Hmisc library by Frank E Harrell Jr
## 
## Type library(help='Hmisc'), ?Overview, or ?Hmisc.Overview') to see overall
## documentation.
## 
## Attaching package: 'Hmisc'
## 
## The following object is masked from 'package:survival':
## 
## untangle.specials
## 
## The following object is masked from 'package:base':
## 
## format.pval, round.POSIXt, trunc.POSIXt, units
```




------

## Processing

Download the data, read the data in and save the raw data as an RDA file


```r
getwd()
```

```
## [1] "/Users/RCHM/Rwork1/GitLocal/data_analysis/data/exampleAnalysis"
```

```r
download.file("http://earthquake.usgs.gov/earthquakes/catalogs/eqs7day-M1.txt", 
    destfile = "../../data/earthquakes.csv", method = "curl")
dateDownloaded <- date()
dateDownloaded
```

```
## [1] "Sun Nov 17 18:10:37 2013"
```

```r
quakesRaw <- read.csv("../../data/earthquakes.csv")
save(quakesRaw, dateDownloaded, file = "../../data/quakesRaw.rda")
```



### Look at the data set


```r
head(quakesRaw)
```

```
##                                                                                                                                                                                                                                                                                                                                                     Src
## 1 This USGS data file has been deprecated. To continue receiving updates for earthquake information you must switch to the new data format [http://earthquake.usgs.gov/earthquakes/feed/]. In the future, data feeds will be updated and deprecated following our official deprecation policy [http://earthquake.usgs.gov/earthquakes/feed/policy.php].
## 2                                                                                                                                                                                                                                                                                                                                                    nc
## 3                                                                                                                                                                                                                                                                                                                                                    ak
## 4                                                                                                                                                                                                                                                                                                                                                    nc
## 5                                                                                                                                                                                                                                                                                                                                                    nc
## 6                                                                                                                                                                                                                                                                                                                                                    nn
##       Eqid Version                               Datetime   Lat    Lon
## 1                                                            NA     NA
## 2 72109121       0 Monday, November 18, 2013 00:05:07 UTC 38.86 -123.3
## 3 10854321       1 Sunday, November 17, 2013 23:15:08 UTC 63.27 -151.1
## 4 72109096       6 Sunday, November 17, 2013 22:45:06 UTC 40.30 -124.7
## 5 72109086       0 Sunday, November 17, 2013 22:39:39 UTC 38.96 -123.1
## 6 00429374       1 Sunday, November 17, 2013 22:28:26 UTC 37.36 -115.5
##   Magnitude Depth NST                       Region
## 1        NA    NA  NA                             
## 2       1.6   2.8   8          Northern California
## 3       1.6  11.5  14               Central Alaska
## 4       3.7   5.0  47 offshore Northern California
## 5       1.9   0.0  13          Northern California
## 6       1.2  12.0   7                       Nevada
```

```r
summary(quakesRaw)
```

```
##       Src            Eqid        Version   
##  ak     :393   13316000:  2   2      :360  
##  nc     :186   13320000:  2   1      :213  
##  ci     :143           :  1   0      :143  
##  uw     : 36   00428903:  1   4      : 65  
##  nn     : 30   00428912:  1   3      : 57  
##  pr     : 30   00428927:  1   5      : 30  
##  (Other): 75   (Other) :885   (Other): 25  
##                                      Datetime        Lat       
##  Saturday, November 16, 2013 05:12:48 UTC:  2   Min.   :-60.4  
##                                          :  1   1st Qu.: 36.6  
##  Friday, November 15, 2013 00:04:34 UTC  :  1   Median : 40.8  
##  Friday, November 15, 2013 00:05:16 UTC  :  1   Mean   : 46.0  
##  Friday, November 15, 2013 00:22:57 UTC  :  1   3rd Qu.: 60.6  
##  Friday, November 15, 2013 00:33:19 UTC  :  1   Max.   : 69.5  
##  (Other)                                 :886   NA's   :1      
##       Lon         Magnitude        Depth             NST       
##  Min.   :-179   Min.   :1.00   Min.   :  0.00   Min.   :  3.0  
##  1st Qu.:-151   1st Qu.:1.30   1st Qu.:  3.20   1st Qu.: 11.0  
##  Median :-123   Median :1.60   Median :  9.85   Median : 17.0  
##  Mean   :-134   Mean   :1.77   Mean   : 25.13   Mean   : 22.1  
##  3rd Qu.:-118   3rd Qu.:2.10   3rd Qu.: 26.40   3rd Qu.: 28.0  
##  Max.   : 180   Max.   :7.80   Max.   :300.00   Max.   :219.0  
##  NA's   :1      NA's   :1      NA's   :1        NA's   :1      
##                                          Region   
##  Central Alaska                             :144  
##  Northern California                        :130  
##  Southern Alaska                            :106  
##  Southern California                        : 82  
##  Andreanof Islands, Aleutian Islands, Alaska: 75  
##  Central California                         : 59  
##  (Other)                                    :297
```

```r
sapply(quakesRaw[1, ], class)
```

```
##       Src      Eqid   Version  Datetime       Lat       Lon Magnitude 
##  "factor"  "factor"  "factor"  "factor" "numeric" "numeric" "numeric" 
##     Depth       NST    Region 
## "numeric" "integer"  "factor"
```


### Find out about missing values

```r
sum(is.na(quakesRaw))
```

```
## [1] 5
```


### Find minimum and maximum times

```r
timeF = strptime(quakesRaw$Datetime, format = "%A, %B %e, %Y %H:%M:%S")
min(timeF)
```

```
## [1] NA
```

```r
max(timeF)
```

```
## [1] NA
```



------

## Exploratory analysis

### Make some univariate plots/summaries


```r
hist(quakesRaw$Magnitude, breaks = 100)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-51.png) 

```r
quantile(quakesRaw$Magnitude)
```

```
## Error: missing values and NaN's not allowed if 'na.rm' is FALSE
```

```r
hist(quakesRaw$Depth, breaks = 100)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-52.png) 

```r
quantile(quakesRaw$Depth)
```

```
## Error: missing values and NaN's not allowed if 'na.rm' is FALSE
```

```r
hist(quakesRaw$Lat, breaks = 100)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-53.png) 

```r
hist(quakesRaw$Lon, breaks = 100)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-54.png) 


### Make some univariate tables

```r
table(quakesRaw$Src)
```

```
## 
##                                                                                                                                                                                                                                                                                                                                                    ak 
##                                                                                                                                                                                                                                                                                                                                                   393 
##                                                                                                                                                                                                                                                                                                                                                    at 
##                                                                                                                                                                                                                                                                                                                                                     2 
##                                                                                                                                                                                                                                                                                                                                                    ci 
##                                                                                                                                                                                                                                                                                                                                                   143 
##                                                                                                                                                                                                                                                                                                                                                    hv 
##                                                                                                                                                                                                                                                                                                                                                    23 
##                                                                                                                                                                                                                                                                                                                                                    mb 
##                                                                                                                                                                                                                                                                                                                                                     9 
##                                                                                                                                                                                                                                                                                                                                                    nc 
##                                                                                                                                                                                                                                                                                                                                                   186 
##                                                                                                                                                                                                                                                                                                                                                    nm 
##                                                                                                                                                                                                                                                                                                                                                     8 
##                                                                                                                                                                                                                                                                                                                                                    nn 
##                                                                                                                                                                                                                                                                                                                                                    30 
##                                                                                                                                                                                                                                                                                                                                                    pr 
##                                                                                                                                                                                                                                                                                                                                                    30 
##                                                                                                                                                                                                                                                                                                                                                    pt 
##                                                                                                                                                                                                                                                                                                                                                     2 
##                                                                                                                                                                                                                                                                                                                                                    se 
##                                                                                                                                                                                                                                                                                                                                                     1 
## This USGS data file has been deprecated. To continue receiving updates for earthquake information you must switch to the new data format [http://earthquake.usgs.gov/earthquakes/feed/]. In the future, data feeds will be updated and deprecated following our official deprecation policy [http://earthquake.usgs.gov/earthquakes/feed/policy.php]. 
##                                                                                                                                                                                                                                                                                                                                                     1 
##                                                                                                                                                                                                                                                                                                                                                    uu 
##                                                                                                                                                                                                                                                                                                                                                    29 
##                                                                                                                                                                                                                                                                                                                                                    uw 
##                                                                                                                                                                                                                                                                                                                                                    36
```

```r
table(quakesRaw$Version)
```

```
## 
##       0   1   2   3   4   5   6   7   8   A   B 
##   1 143 213 360  57  65  30   7   4   2   8   3
```

```r
table(quakesRaw$Region)
```

```
## 
##                                                             
##                                                           1 
##                                            Alaska Peninsula 
##                                                           6 
##                 Andreanof Islands, Aleutian Islands, Alaska 
##                                                          75 
##                                                    Arkansas 
##                                                           6 
##                                     Baja California, Mexico 
##                                                           3 
##                                    British Columbia, Canada 
##                                                           3 
##                                              Central Alaska 
##                                                         144 
##                                          Central California 
##                                                          59 
##                          Channel Islands region, California 
##                                                           1 
##                                          Dominican Republic 
##                                                           1 
##                                           eastern Tennessee 
##                                                           1 
##                       Fox Islands, Aleutian Islands, Alaska 
##                                                          13 
##                        Greater Los Angeles area, California 
##                                                          29 
##                                              Gulf of Alaska 
##                                                           2 
##                                                    Illinois 
##                                                           1 
##                                    Island of Hawaii, Hawaii 
##                                                          23 
##                                     Kenai Peninsula, Alaska 
##                                                          17 
##                                Kodiak Island region, Alaska 
##                                                           2 
##                                Long Valley area, California 
##                                                           1 
##                                   Mona Passage, Puerto Rico 
##                                                           1 
##      near the east coast of the Kamchatka Peninsula, Russia 
##                                                           1 
## near the south coast of Chukotskiy Avtonomnyy Okrug, Russia 
##                                                           1 
##                                                      Nevada 
##                                                          30 
##                               Newberry Caldera area, Oregon 
##                                                           7 
##                                             northern Alaska 
##                                                           7 
##                                         Northern California 
##                                                         130 
##                        off the coast of Southeastern Alaska 
##                                                           1 
##                                 offshore Central California 
##                                                           1 
##                                offshore Northern California 
##                                                           7 
##                               Olympic Peninsula, Washington 
##                                                           2 
##                                                      Oregon 
##                                                           5 
##                                                 Puerto Rico 
##                                                           2 
##                                          Puerto Rico region 
##                                                          11 
##                       Rat Islands, Aleutian Islands, Alaska 
##                                                           1 
##                     San Diego County urban area, California 
##                                                           2 
##                          San Francisco Bay area, California 
##                                                          14 
##                         San Juan Islands region, Washington 
##                                                           1 
##                                                  Scotia Sea 
##                                                           2 
##                       Seattle-Tacoma urban area, Washington 
##                                                           3 
##                                             south of Alaska 
##                                                           2 
##                                         Southeastern Alaska 
##                                                           1 
##                                       southeastern Missouri 
##                                                           1 
##                                             Southern Alaska 
##                                                         106 
##                                         Southern California 
##                                                          82 
##                            Southern Yukon Territory, Canada 
##                                                           6 
##                                Unimak Island region, Alaska 
##                                                           9 
##                                                        Utah 
##                                                          27 
##                                       Virgin Islands region 
##                                                          15 
##                                                  Washington 
##                                                          16 
##                                             western Montana 
##                                                          10 
##                          Yellowstone National Park, Wyoming 
##                                                           1
```

```r
length(unique(quakesRaw$NST))
```

```
## [1] 78
```

```r
length(unique(quakesRaw$Eqid))
```

```
## [1] 891
```



### Plot the earthquakes on the globe

```r
map("world")
points(quakesRaw$Lon, quakesRaw$Lat, pch = 19, col = "blue")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 



### Plot the earthquakes on the globe/sized by relative magnitude 

```r
map("world")
points(quakesRaw$Lon, quakesRaw$Lat, pch = 19, col = "blue", cex = quakesRaw$Magnitude/max(quakesRaw$Magnitude))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 



### Plot the earthquakes on the globe/sized by relative depth

```r
map("world")
points(quakesRaw$Lon, quakesRaw$Lat, pch = 19, col = "blue", cex = quakesRaw$Depth/max(quakesRaw$Depth))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 



### Plot depth versus magnitude

```r
plot(quakesRaw$Depth, quakesRaw$Magnitude, pch = 19)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


### Looks weird, let's try a transform - need to add one to avoid log(0)

```r
summary(log10(quakesRaw$Depth), useNA = "ifany")
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    -Inf       1       1    -Inf       1       2       1
```

```r
summary(log10(quakesRaw$Depth + 1))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   0.623   1.040   1.050   1.440   2.480       1
```

```r
quakesRaw$log10Depth <- log10(quakesRaw$Depth + 1)
```


### Plot w/transform

```r
plot(quakesRaw$log10Depth, quakesRaw$Magnitude, pch = 19)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 




### Color by Latitute 

```r
latCut = cut2(quakesRaw$Lat, g = 5)
plot(quakesRaw$log10Depth, quakesRaw$Magnitude, pch = 19, col = latCut)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 


Might be a lot of black in the top right quadrant

### Color by longitude 


```r
lonCut = cut2(quakesRaw$Lon, g = 5)
plot(quakesRaw$log10Depth, quakesRaw$Magnitude, pch = 19, col = lonCut)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 

Definitely a lot of light blue in the upper right hand quadrant


### Color by NST



```r
nstCut = cut2(quakesRaw$NST, g = 5)
plot(quakesRaw$log10Depth, quakesRaw$Magnitude, pch = 19, col = nstCut)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 



### Check out relationship with Src

```r
boxplot(quakesRaw$log10Depth ~ quakesRaw$Src)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-161.png) 

```r
boxplot(quakesRaw$Magnitude ~ quakesRaw$Src)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-162.png) 


### How do lat/lon correlated with source


```r

boxplot(quakesRaw$Lat ~ quakesRaw$Src)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-171.png) 

```r
boxplot(quakesRaw$Lon ~ quakesRaw$Src)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-172.png) 




### Get the formatted time, see earthquakes over time


```r
timeF = strptime(quakesRaw$Datetime, format = "%A, %B %e, %Y %H:%M:%S")
plot(timeF, quakesRaw$log10Depth)
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-181.png) 

```r
plot(timeF, quakesRaw$Magnitude)
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-182.png) 


------

## Modeling


## Fit a basic model relating depth to time


```r
lm1 <- lm(quakesRaw$Depth ~ quakesRaw$Magnitude)
summary(lm1)
```

```
## 
## Call:
## lm(formula = quakesRaw$Depth ~ quakesRaw$Magnitude)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -49.34 -20.72 -14.42   0.21 268.68 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            13.31       3.12    4.27  2.2e-05 ***
## quakesRaw$Magnitude     6.67       1.63    4.10  4.6e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 35.4 on 890 degrees of freedom
##   (1 observation deleted due to missingness)
## Multiple R-squared:  0.0185,	Adjusted R-squared:  0.0174 
## F-statistic: 16.8 on 1 and 890 DF,  p-value: 4.56e-05
```



## Plot the observed ('black') and fitted ('red') points


```r
lm1 <- lm(quakesRaw$Magnitude ~ quakesRaw$Depth)
plot(quakesRaw$Depth, quakesRaw$Magnitude, pch = 19)
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20.png) 

```r
points(quakesRaw$Depth, lm1$fitted, pch = 19, col = "red")
```

```
## Error: 'x' and 'y' lengths differ
```



## Look at residuals versus observations, residuals versus fitted values


```r
lm1 <- lm(quakesRaw$Magnitude ~ quakesRaw$Depth)
par(mfrow = c(1, 2))
plot(quakesRaw$Depth, lm1$residuals, pch = 19)
```

```
## Error: 'x' and 'y' lengths differ
```

```r
plot(lm1$fitted, lm1$residuals, pch = 19)
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21.png) 


## Try the transform


```r
lm2 <- lm(quakesRaw$Magnitude ~ quakesRaw$log10Depth)
plot(quakesRaw$log10Depth, quakesRaw$Magnitude, pch = 19)
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22.png) 

```r
points(quakesRaw$log10Depth, lm2$fitted, pch = 19, col = "red")
```

```
## Error: 'x' and 'y' lengths differ
```



## Plot the residuals versus the observed and fitted (little better here)


```r
lm2 <- lm(quakesRaw$Magnitude ~ quakesRaw$log10Depth)
par(mfrow = c(1, 2))
plot(quakesRaw$log10Depth, lm2$residuals, pch = 19)
```

```
## Error: 'x' and 'y' lengths differ
```

```r
plot(lm2$fitted, lm2$residuals, pch = 19)
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23.png) 


## What if we color residuals by lat (see a pattern)


```r
lm2 <- lm(quakesRaw$Magnitude ~ quakesRaw$log10Depth)
latCut = cut2(quakesRaw$Lat, g = 5)
par(mfrow = c(1, 2))
plot(quakesRaw$log10Depth, lm2$residuals, pch = 19, col = latCut)
```

```
## Error: 'x' and 'y' lengths differ
```

```r
plot(lm2$fitted, lm2$residuals, pch = 19, col = latCut)
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24.png) 



## What if we color residuals by lon (see a pattern)


```r
lm2 <- lm(quakesRaw$Magnitude ~ quakesRaw$log10Depth)
lonCut = cut2(quakesRaw$Lon, g = 5)
par(mfrow = c(1, 2))
plot(quakesRaw$log10Depth, lm2$residuals, pch = 19, col = lonCut)
```

```
## Error: 'x' and 'y' lengths differ
```

```r
plot(lm2$fitted, lm2$residuals, pch = 19, col = lonCut)
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25.png) 


## Now try fitting a new model with Lat in there


```r
latCut = cut2(quakesRaw$Lat, g = 5)
lm3 <- lm(quakesRaw$Magnitude ~ quakesRaw$log10Depth + latCut)
par(mfrow = c(1, 2))
plot(quakesRaw$log10Depth, lm3$residuals, pch = 19, col = latCut)
```

```
## Error: 'x' and 'y' lengths differ
```

```r
plot(lm3$fitted, lm3$residuals, pch = 19, col = latCut)
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26.png) 


## How about Lat/Lon


```r
latCut = cut2(quakesRaw$Lat, g = 5)
lonCut = cut2(quakesRaw$Lon, g = 5)
lm4 <- lm(quakesRaw$Magnitude ~ quakesRaw$log10Depth + latCut + lonCut)
par(mfrow = c(1, 2))
plot(quakesRaw$log10Depth, lm4$residuals, pch = 19, col = latCut)
```

```
## Error: 'x' and 'y' lengths differ
```

```r
plot(lm4$fitted, lm4$residuals, pch = 19, col = latCut)
```

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27.png) 



## Color that model by NST


```r
latCut = cut2(quakesRaw$Lat, g = 5)
lonCut = cut2(quakesRaw$Lon, g = 5)
nstCut = cut2(quakesRaw$NST, g = 5)
lm4 <- lm(quakesRaw$Magnitude ~ quakesRaw$log10Depth + latCut + lonCut)
par(mfrow = c(1, 2))
plot(quakesRaw$log10Depth, lm4$residuals, pch = 19, col = nstCut)
```

```
## Error: 'x' and 'y' lengths differ
```

```r
plot(lm4$fitted, lm4$residuals, pch = 19, col = nstCut)
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28.png) 


## Include NST 


```r
latCut = cut2(quakesRaw$Lat, g = 5)
lonCut = cut2(quakesRaw$Lon, g = 5)
nstCut = cut2(quakesRaw$NST, g = 5)
lm5 <- lm(quakesRaw$Magnitude ~ quakesRaw$log10Depth + latCut + lonCut + nstCut)
par(mfrow = c(1, 2))
plot(quakesRaw$log10Depth, lm5$residuals, pch = 19, col = nstCut)
```

```
## Error: 'x' and 'y' lengths differ
```

```r
plot(lm5$fitted, lm5$residuals, pch = 19, col = nstCut)
```

![plot of chunk lm5Chunk](figure/lm5Chunk.png) 




## Let's use model 5


```r
summary(lm5)
```

```
## 
## Call:
## lm(formula = quakesRaw$Magnitude ~ quakesRaw$log10Depth + latCut + 
##     lonCut + nstCut)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -1.150 -0.387 -0.129  0.217  5.813 
## 
## Coefficients:
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            2.0210     0.1145   17.65  < 2e-16 ***
## quakesRaw$log10Depth   0.0722     0.0482    1.50  0.13461    
## latCut[ 35.3,38.8)     0.1211     0.0805    1.51  0.13247    
## latCut[ 38.8,51.4)     0.2020     0.0790    2.56  0.01068 *  
## latCut[ 51.4,61.6)     0.3130     0.0986    3.17  0.00156 ** 
## latCut[ 61.6,69.5]     0.2417     0.1153    2.10  0.03635 *  
## lonCut[-152,-148)     -0.8327     0.0913   -9.12  < 2e-16 ***
## lonCut[-148,-123)     -0.6777     0.0819   -8.28  4.7e-16 ***
## lonCut[-123,-117)     -0.6147     0.0882   -6.97  6.2e-12 ***
## lonCut[-117, 180]     -0.4299     0.0894   -4.81  1.8e-06 ***
## nstCut[10, 15)        -0.0760     0.0681   -1.12  0.26452    
## nstCut[15, 21)        -0.1051     0.0678   -1.55  0.12168    
## nstCut[21, 32)        -0.0398     0.0700   -0.57  0.57035    
## nstCut[32,219]         0.2784     0.0722    3.86  0.00012 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.644 on 878 degrees of freedom
##   (1 observation deleted due to missingness)
## Multiple R-squared:  0.231,	Adjusted R-squared:  0.22 
## F-statistic: 20.3 on 13 and 878 DF,  p-value: <2e-16
```

```r
confint(lm5)
```

```
##                         2.5 %   97.5 %
## (Intercept)           1.79630  2.24580
## quakesRaw$log10Depth -0.02242  0.16677
## latCut[ 35.3,38.8)   -0.03675  0.27905
## latCut[ 38.8,51.4)    0.04703  0.35696
## latCut[ 51.4,61.6)    0.11938  0.50660
## latCut[ 61.6,69.5]    0.01540  0.46806
## lonCut[-152,-148)    -1.01183 -0.65350
## lonCut[-148,-123)    -0.83842 -0.51696
## lonCut[-123,-117)    -0.78773 -0.44161
## lonCut[-117, 180]    -0.60542 -0.25445
## nstCut[10, 15)       -0.20971  0.05763
## nstCut[15, 21)       -0.23826  0.02805
## nstCut[21, 32)       -0.17722  0.09770
## nstCut[32,219]        0.13678  0.42001
```


## Let's plot fitted versus observed magnitudes on the map


```r
par(mfrow = c(2, 1))
map("world")
lm5fitted <- lm5$fitted
points(quakesRaw$Lon, quakesRaw$Lat, pch = 19, col = "blue", cex = lm5fitted/max(lm5fitted))
map("world")
points(quakesRaw$Lon, quakesRaw$Lat, pch = 19, col = "blue", cex = quakesRaw$Magnitude/max(quakesRaw$Magnitude))
```

![plot of chunk unnamed-chunk-30](figure/unnamed-chunk-30.png) 



