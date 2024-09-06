---
title: '**Response Surface Methodology (RSM) Applied to Building Simulations**'
subtitle: '**Optimizing Thermal Comfort and Daylight **'
author: "Juan Gamero-Salinas - Postdoc Researcher @ DATAI University of Navarra"
date: '2024-09-06'
output:
  html_document:
    toc: true
    toc_float: true
    toc_depth: 4
    theme: united
    keep_md: true
    
---



# **1. Initial steps**





### Set working directory



### Install and load the FrF2 and DoE.base packages


``` r
suppressPackageStartupMessages(library(FrF2))
```

# **2. Create the fractional factorial design**

### Factors taken into account


``` r
factors <- read.csv("final_factors.csv", sep=';', header = FALSE)
colnames(factors) <- c("Factor", "-1", "+1")
factors
```

```
##                            Factor   -1   +1
## 1                         Factors -1.0  1.0
## 2 Window-to-wall ratio (%), North  5.0 40.0
## 3 Window-to-wall ratio (%), South  5.0 40.0
## 4  Window-to-wall ratio (%), East  5.0 40.0
## 5  Window-to-wall ratio (%), West  5.0 40.0
## 6 Roof Overhang Length (m), North  0.5  2.5
## 7 Roof Overhang Length (m), South  0.5  2.5
## 8  Roof Overhang Length (m), East  0.5  2.5
## 9  Roof Overhang Length (m), West  0.5  2.5
```


### Create a resolution V design with 8 two-level (0,1) factors


``` r
frac_design_rhino <- FrF2(nfactors = 8, 
                          randomize = FALSE,
                          resolution = 5,
                          default.levels = c(0, 1),
                          factor.names=c("WWR_N","WWR_S","WWR_E","WWR_W","RoofOvrhng_N",
                                 "RoofOvrhng_S","RoofOvrhng_E","RoofOvrhng_W"))
```


``` r
frac_design_rhino <- as.data.frame(frac_design_rhino)
head(frac_design_rhino)
```

```
##   WWR_N WWR_S WWR_E WWR_W RoofOvrhng_N RoofOvrhng_S RoofOvrhng_E RoofOvrhng_W
## 1     0     0     0     0            0            0            1            1
## 2     1     0     0     0            0            0            0            0
## 3     0     1     0     0            0            0            0            0
## 4     1     1     0     0            0            0            1            1
## 5     0     0     1     0            0            0            0            1
## 6     1     0     1     0            0            0            1            0
```


### Check the number of combinations


``` r
nrow(frac_design_rhino)
```

```
## [1] 64
```


### Save the fractional factorial design for running the energy and dyalight simulations in Rhino + Grasshopper/Honeybee/Radiance


``` r
write.csv(frac_design_rhino, "frac_design_2k8_R5.csv")
```

### Confirm that it has 64

``` r
frac_design_confirm <- FrF2(nfactors = 8, 
                      randomize = FALSE,
                      nruns = 64,
                      default.levels = c(0, 1),
                      factor.names=c("WWR_N","WWR_S","WWR_E","WWR_W","RoofOvrhng_N",
                                 "RoofOvrhng_S","RoofOvrhng_E","RoofOvrhng_W"))
nrow(frac_design_confirm)
```

```
## [1] 64
```


### Create the same fractional factorial design but with -1 and 1 levels


``` r
frac_design_rhino <- FrF2(nfactors = 8, 
                          randomize = FALSE,
                          resolution = 5,
                          default.levels = c(-1, 1),
                          factor.names=c("WWR_N","WWR_S","WWR_E","WWR_W","RoofOvrhng_N",
                                 "RoofOvrhng_S","RoofOvrhng_E","RoofOvrhng_W"))
```



``` r
head(frac_design_rhino)
```

```
##   WWR_N WWR_S WWR_E WWR_W RoofOvrhng_N RoofOvrhng_S RoofOvrhng_E RoofOvrhng_W
## 1    -1    -1    -1    -1           -1           -1            1            1
## 2     1    -1    -1    -1           -1           -1           -1           -1
## 3    -1     1    -1    -1           -1           -1           -1           -1
## 4     1     1    -1    -1           -1           -1            1            1
## 5    -1    -1     1    -1           -1           -1           -1            1
## 6     1    -1     1    -1           -1           -1            1           -1
```





### Load simulation results into Rstudio


``` r
data_ioh <- read.csv("data_IOH_64.csv")
data_da <- read.csv("data_DA_64.csv")
```

### Convert results to datafrane


``` r
df_ioh <- as.data.frame(data_ioh)
df_da <- as.data.frame(data_da)
```

### Append simulation results with fractional factorial design


``` r
frac_design_rhino$IOH  <- df_ioh$out.IOH 
frac_design_rhino$UDI  <- df_da$out.UDI
frac_design_rhino$DA  <- df_da$out.DA 
frac_design_rhino$cDA  <- df_da$out.cDA
frac_design_rhino$sDA50  <- df_da$out.sDA50
frac_design_rhino$sDA80  <- df_da$out.sDA80
```



### Print dataframe


``` r
df = frac_design_rhino
head(df)
```

```
##   WWR_N WWR_S WWR_E WWR_W RoofOvrhng_N RoofOvrhng_S RoofOvrhng_E RoofOvrhng_W
## 1    -1    -1    -1    -1           -1           -1            1            1
## 2     1    -1    -1    -1           -1           -1           -1           -1
## 3    -1     1    -1    -1           -1           -1           -1           -1
## 4     1     1    -1    -1           -1           -1            1            1
## 5    -1    -1     1    -1           -1           -1           -1            1
## 6     1    -1     1    -1           -1           -1            1           -1
##        IOH      UDI       DA      cDA    sDA50    sDA80
## 1 0.166911 80.54431 61.08474 76.12886 0.803508 0.072138
## 2 0.178483 73.72197 79.09149 83.77467 0.995834 0.525049
## 3 0.158333 59.24195 71.77930 80.28757 0.853353 0.557168
## 4 0.147522 55.27759 85.57550 86.60934 1.000000 0.970845
## 5 0.118730 71.28322 72.19053 80.37454 0.884407 0.503704
## 6 0.119295 66.93795 82.79622 85.45728 1.000000 0.721652
```

# **3. Distribution**


``` r
# Assuming 'IOH' is the column name in your dataframe 'df'
hist(df$IOH, main="Histogram with Density Curve", xlab="IOH Values", col="lightblue", border="black", probability=TRUE)

# Add a density curve
lines(density(df$IOH), col="darkred", lwd=2)
```

![](RSM_thermal_daylight_optimization_files/figure-html/unnamed-chunk-14-1.png)<!-- -->



``` r
# Assuming 'UDI' is the column name in your dataframe 'df'
hist(df$UDI, main="Histogram with Density Curve", xlab="sDA Values", col="lightblue", border="black", probability=TRUE)

# Add a density curve
lines(density(df$UDI), col="darkred", lwd=2)
```

![](RSM_thermal_daylight_optimization_files/figure-html/unnamed-chunk-15-1.png)<!-- -->



# **4. Correlations**


``` r
ks.test(df$IOH, "pnorm")
```

```
## 
## 	Exact one-sample Kolmogorov-Smirnov test
## 
## data:  df$IOH
## D = 0.53647, p-value = 2.22e-16
## alternative hypothesis: two-sided
```

``` r
ks.test(df$UDI, "pnorm")
```

```
## 
## 	Exact one-sample Kolmogorov-Smirnov test
## 
## data:  df$UDI
## D = 1, p-value = 2.22e-16
## alternative hypothesis: two-sided
```



``` r
cor.test(df$IOH, df$UDI, method="spearman")
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  df$IOH and df$UDI
## S = 60314, p-value = 0.002042
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## -0.380815
```

``` r
cor.test(df$IOH, df$DA, method="spearman")
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  df$IOH and df$DA
## S = 41394, p-value = 0.6806
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##        rho 
## 0.05233516
```

``` r
cor.test(df$IOH, df$cDA, method="spearman")
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  df$IOH and df$cDA
## S = 41922, p-value = 0.7516
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##        rho 
## 0.04024725
```

``` r
cor.test(df$IOH, df$sDA50, method="spearman")
```

```
## Warning in cor.test.default(df$IOH, df$sDA50, method = "spearman"): Cannot
## compute exact p-value with ties
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  df$IOH and df$sDA50
## S = 43031, p-value = 0.9072
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##        rho 
## 0.01486546
```

``` r
cor.test(df$IOH, df$sDA80, method="spearman")
```

```
## Warning in cor.test.default(df$IOH, df$sDA80, method = "spearman"): Cannot
## compute exact p-value with ties
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  df$IOH and df$sDA80
## S = 41790, p-value = 0.7342
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##        rho 
## 0.04327121
```




``` r
cor.test(df$UDI, df$DA, method="spearman")
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  df$UDI and df$DA
## S = 68934, p-value = 9.483e-07
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##        rho 
## -0.5781593
```

``` r
cor.test(df$UDI, df$cDA, method="spearman")
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  df$UDI and df$cDA
## S = 67062, p-value = 7.322e-06
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##        rho 
## -0.5353022
```

``` r
cor.test(df$UDI, df$sDA50, method="spearman")
```

```
## Warning in cor.test.default(df$UDI, df$sDA50, method = "spearman"): Cannot
## compute exact p-value with ties
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  df$UDI and df$sDA50
## S = 62177, p-value = 0.0004892
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##        rho 
## -0.4234702
```


# **5. Desirability Values**



``` r
min(df$IOH)
```

```
## [1] 0.091551
```

``` r
max(df$IOH)
```

```
## [1] 0.193161
```


``` r
min(df$UDI)
```

```
## [1] 35.25077
```

``` r
max(df$UDI)
```

```
## [1] 81.21243
```



``` r
min(df$sDA50)
```

```
## [1] 0.494473
```

``` r
max(df$sDA50)
```

```
## [1] 1
```



``` r
min(df$sDA80)
```

```
## [1] 0.014012
```

``` r
max(df$sDA80)
```

```
## [1] 0.970845
```





``` r
predOutcomes <- data.frame(
  ioh = df$IOH,
  udi = df$UDI
)
head(predOutcomes)
```

```
##        ioh      udi
## 1 0.166911 80.54431
## 2 0.178483 73.72197
## 3 0.158333 59.24195
## 4 0.147522 55.27759
## 5 0.118730 71.28322
## 6 0.119295 66.93795
```


``` r
library(desirability)
```


``` r
iohD <- dMin(0, 0.193161, scale = 1)
udiD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, udiD)
head(overallD)
```

```
## $d
## $d[[1]]
## Smaller-is-better desirability function
## 
## Call: dMin.default(low = 0, high = 0.193161, scale = 1)
## 
## Non-informative value: 0.5 
## 
## $d[[2]]
## Larger-is-better desirability function
## 
## Call: dMax.default(low = 35.23524, high = 100, scale = 1)
## 
## Non-informative value: 0.5 
## 
## 
## $call
## dOverall.default(iohD, udiD)
```


``` r
predict(iohD, predOutcomes[1])
```

```
##  [1] 0.135896998 0.075988424 0.180305548 0.236274403 0.385331407 0.382406386
##  [7] 0.202038714 0.199869539 0.245556815 0.100688027 0.016670032 0.132651001
## [13] 0.390441135 0.181428963 0.000000000 0.152665393 0.095749142 0.121603222
## [19] 0.221633767 0.201184504 0.347176708 0.437003329 0.248512899 0.165551017
## [25] 0.082397585 0.260430418 0.156071878 0.004509192 0.227017876 0.366015914
## [31] 0.121815480 0.036534290 0.231630609 0.266295991 0.497957662 0.473392662
## [37] 0.433881581 0.525282019 0.522978241 0.436242306 0.173787669 0.355936240
## [43] 0.422901103 0.230476131 0.309803739 0.439136265 0.394101294 0.251572522
## [49] 0.296291694 0.211900953 0.456147980 0.526037865 0.491708989 0.484295484
## [55] 0.472217477 0.484202298 0.345954929 0.202463230 0.260477011 0.410258800
## [61] 0.471741190 0.274563706 0.218061617 0.442138941
```


``` r
predict(udiD, predOutcomes[2])
```

```
##  [1] 0.6995944863 0.5942542210 0.3706755186 0.3094638658 0.5565986966
##  [6] 0.4895055737 0.2832158878 0.1033350853 0.6195421708 0.4249570445
## [11] 0.2130736685 0.1286041977 0.5430966624 0.2136224854 0.0509557204
## [16] 0.0002397137 0.6496304472 0.7099105748 0.3310000531 0.3862719325
## [21] 0.5221867571 0.6145432485 0.2577500171 0.1702026225 0.5038511530
## [26] 0.5916687717 0.2487576886 0.1657078634 0.4351290270 0.3794919799
## [31] 0.1005835427 0.0466784406 0.6819034611 0.5925369136 0.6488637339
## [36] 0.5508944834 0.5691736525 0.5199677726 0.5540631047 0.2947420789
## [41] 0.5484675771 0.5029106261 0.5349290108 0.2892987946 0.4924099001
## [46] 0.3239061953 0.3092621049 0.1436121434 0.6242919915 0.6804988701
## [51] 0.5797830641 0.6918267743 0.5328541478 0.6126889839 0.4966502462
## [56] 0.4288185736 0.6077675267 0.5231579489 0.3975541174 0.4725604634
## [61] 0.5704903716 0.3258714307 0.1843281284 0.3391216458
```


``` r
predict(overallD, predOutcomes)
```

```
##  [1] 0.308338759 0.212500451 0.258524375 0.270404124 0.463114413 0.432654663
##  [7] 0.239208222 0.143713381 0.390042052 0.206852813 0.059598195 0.130611927
## [13] 0.460485914 0.196868753 0.000000000 0.006049462 0.249402402 0.293815271
## [19] 0.270851968 0.278768591 0.425782901 0.518225284 0.253089320 0.167860708
## [25] 0.203755045 0.392541139 0.197038269 0.027335116 0.314296146 0.372693043
## [31] 0.110691610 0.041296049 0.397428879 0.397228152 0.568424725 0.510675441
## [37] 0.496944629 0.522618141 0.538296338 0.358579090 0.308734355 0.423088782
## [43] 0.475628078 0.258217867 0.390577045 0.377145803 0.349114015 0.190075956
## [49] 0.430084331 0.379734590 0.514263428 0.603263690 0.511868317 0.544722414
## [55] 0.484279801 0.455669769 0.458541352 0.325453911 0.321797620 0.440309083
## [61] 0.518771440 0.299119487 0.200486632 0.387219428
```


``` r
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
head(overall_desirability)
```

```
##           D1        D2   Overall
## 1 0.13589700 0.6995945 0.3083388
## 2 0.07598842 0.5942542 0.2125005
## 3 0.18030555 0.3706755 0.2585244
## 4 0.23627440 0.3094639 0.2704041
## 5 0.38533141 0.5565987 0.4631144
## 6 0.38240639 0.4895056 0.4326547
```


``` r
df$d1 <- overall_desirability$D1
df$d2 <- overall_desirability$D2
df$Overall <- overall_desirability$Overall
df
```

```
##    WWR_N WWR_S WWR_E WWR_W RoofOvrhng_N RoofOvrhng_S RoofOvrhng_E RoofOvrhng_W
## 1     -1    -1    -1    -1           -1           -1            1            1
## 2      1    -1    -1    -1           -1           -1           -1           -1
## 3     -1     1    -1    -1           -1           -1           -1           -1
## 4      1     1    -1    -1           -1           -1            1            1
## 5     -1    -1     1    -1           -1           -1           -1            1
## 6      1    -1     1    -1           -1           -1            1           -1
## 7     -1     1     1    -1           -1           -1            1           -1
## 8      1     1     1    -1           -1           -1           -1            1
## 9     -1    -1    -1     1           -1           -1           -1            1
## 10     1    -1    -1     1           -1           -1            1           -1
## 11    -1     1    -1     1           -1           -1            1           -1
## 12     1     1    -1     1           -1           -1           -1            1
## 13    -1    -1     1     1           -1           -1            1            1
## 14     1    -1     1     1           -1           -1           -1           -1
## 15    -1     1     1     1           -1           -1           -1           -1
## 16     1     1     1     1           -1           -1            1            1
## 17    -1    -1    -1    -1            1           -1            1           -1
## 18     1    -1    -1    -1            1           -1           -1            1
## 19    -1     1    -1    -1            1           -1           -1            1
## 20     1     1    -1    -1            1           -1            1           -1
## 21    -1    -1     1    -1            1           -1           -1           -1
## 22     1    -1     1    -1            1           -1            1            1
## 23    -1     1     1    -1            1           -1            1            1
## 24     1     1     1    -1            1           -1           -1           -1
## 25    -1    -1    -1     1            1           -1           -1           -1
## 26     1    -1    -1     1            1           -1            1            1
## 27    -1     1    -1     1            1           -1            1            1
## 28     1     1    -1     1            1           -1           -1           -1
## 29    -1    -1     1     1            1           -1            1           -1
## 30     1    -1     1     1            1           -1           -1            1
## 31    -1     1     1     1            1           -1           -1            1
## 32     1     1     1     1            1           -1            1           -1
## 33    -1    -1    -1    -1           -1            1            1           -1
## 34     1    -1    -1    -1           -1            1           -1            1
## 35    -1     1    -1    -1           -1            1           -1            1
## 36     1     1    -1    -1           -1            1            1           -1
## 37    -1    -1     1    -1           -1            1           -1           -1
## 38     1    -1     1    -1           -1            1            1            1
## 39    -1     1     1    -1           -1            1            1            1
## 40     1     1     1    -1           -1            1           -1           -1
## 41    -1    -1    -1     1           -1            1           -1           -1
## 42     1    -1    -1     1           -1            1            1            1
## 43    -1     1    -1     1           -1            1            1            1
## 44     1     1    -1     1           -1            1           -1           -1
## 45    -1    -1     1     1           -1            1            1           -1
## 46     1    -1     1     1           -1            1           -1            1
## 47    -1     1     1     1           -1            1           -1            1
## 48     1     1     1     1           -1            1            1           -1
## 49    -1    -1    -1    -1            1            1            1            1
## 50     1    -1    -1    -1            1            1           -1           -1
## 51    -1     1    -1    -1            1            1           -1           -1
## 52     1     1    -1    -1            1            1            1            1
## 53    -1    -1     1    -1            1            1           -1            1
## 54     1    -1     1    -1            1            1            1           -1
## 55    -1     1     1    -1            1            1            1           -1
## 56     1     1     1    -1            1            1           -1            1
## 57    -1    -1    -1     1            1            1           -1            1
## 58     1    -1    -1     1            1            1            1           -1
## 59    -1     1    -1     1            1            1            1           -1
## 60     1     1    -1     1            1            1           -1            1
## 61    -1    -1     1     1            1            1            1            1
## 62     1    -1     1     1            1            1           -1           -1
## 63    -1     1     1     1            1            1           -1           -1
## 64     1     1     1     1            1            1            1            1
##         IOH      UDI       DA      cDA    sDA50    sDA80          d1
## 1  0.166911 80.54431 61.08474 76.12886 0.803508 0.072138 0.135896998
## 2  0.178483 73.72197 79.09149 83.77467 0.995834 0.525049 0.075988424
## 3  0.158333 59.24195 71.77930 80.28757 0.853353 0.557168 0.180305548
## 4  0.147522 55.27759 85.57550 86.60934 1.000000 0.970845 0.236274403
## 5  0.118730 71.28322 72.19053 80.37454 0.884407 0.503704 0.385331407
## 6  0.119295 66.93795 82.79622 85.45728 1.000000 0.721652 0.382406386
## 7  0.154135 53.57765 75.91649 81.75132 0.897111 0.688444 0.202038714
## 8  0.154554 41.92771 85.96148 86.71351 1.000000 0.970117 0.199869539
## 9  0.145729 75.35974 72.25147 80.50013 0.894994 0.373332 0.245556815
## 10 0.173712 62.75748 82.79772 85.45770 1.000000 0.690549 0.100688027
## 11 0.189941 49.03491 74.46327 81.59084 0.878032 0.677857 0.016670032
## 12 0.167538 43.56426 85.97747 86.71186 1.000000 0.968659 0.132651001
## 13 0.117743 70.40877 78.23883 82.80558 0.938047 0.798639 0.390441135
## 14 0.158116 49.07045 85.29769 86.49110 1.000000 0.970845 0.181428963
## 15 0.193161 38.53538 80.35257 83.62522 0.937318 0.856420 0.000000000
## 16 0.163672 35.25077 86.22319 86.77890 1.000000 0.969388 0.152665393
## 17 0.174666 77.30840 57.17507 72.99107 0.734607 0.050165 0.095749142
## 18 0.169672 81.21243 76.26162 82.62971 0.983126 0.385354 0.121603222
## 19 0.150350 56.67238 64.85392 76.26079 0.749962 0.514821 0.221633767
## 20 0.154300 60.25205 83.34910 85.69143 0.995627 0.823258 0.201184504
## 21 0.126100 69.05454 70.84846 78.70883 0.855830 0.488573 0.347176708
## 22 0.108749 75.03599 80.53517 84.51364 0.993477 0.588566 0.437003329
## 23 0.145158 51.92836 70.83710 78.67796 0.831127 0.642568 0.248512899
## 24 0.161183 46.25837 84.29933 86.05691 0.996356 0.869157 0.165551017
## 25 0.177245 67.86704 69.83017 78.63561 0.847360 0.386080 0.082397585
## 26 0.142856 73.55453 80.42911 84.44062 0.994203 0.577481 0.260430418
## 27 0.163014 51.34597 69.78055 78.47686 0.817717 0.612925 0.156071878
## 28 0.192290 45.96727 84.54547 86.12362 0.996356 0.923502 0.004509192
## 29 0.149310 63.41627 77.52074 81.76394 0.927114 0.778784 0.227017876
## 30 0.122461 59.81295 83.69959 85.87547 0.993440 0.930631 0.366015914
## 31 0.169631 41.74951 78.74401 82.15081 0.927114 0.783724 0.121815480
## 32 0.186104 38.25836 85.04475 86.31050 0.995627 0.943947 0.036534290
## 33 0.148419 79.39855 54.67681 73.84740 0.677252 0.047129 0.231630609
## 34 0.141723 73.61075 70.47375 80.89523 0.854069 0.484721 0.266295991
## 35 0.096975 77.25874 68.90761 79.36668 0.837802 0.483610 0.497957662
## 36 0.101720 70.91379 83.95954 86.09783 1.000000 0.897919 0.473392662
## 37 0.109352 72.09763 72.56136 80.38277 0.897817 0.422509 0.433881581
## 38 0.091697 68.91083 80.49915 84.47696 0.996540 0.615453 0.525282019
## 39 0.092142 71.11900 73.60170 81.02653 0.882290 0.673110 0.522978241
## 40 0.108896 54.32414 85.23323 86.50721 1.000000 0.965199 0.436242306
## 41 0.159592 70.75661 72.03467 80.36688 0.894265 0.382665 0.173787669
## 42 0.124408 67.80613 80.13554 84.25098 0.996523 0.589919 0.355936240
## 43 0.111473 69.87979 72.99838 81.09135 0.877326 0.651784 0.422901103
## 44 0.148642 53.97161 85.28629 86.51663 1.000000 0.968692 0.230476131
## 45 0.133319 67.12605 78.38405 82.88167 0.938776 0.812851 0.309803739
## 46 0.108337 56.21295 84.61029 86.16385 1.000000 0.944460 0.439136265
## 47 0.117036 55.26453 79.59107 83.34740 0.938047 0.833129 0.394101294
## 48 0.144567 44.53625 85.84397 86.68078 1.000000 0.970845 0.251572522
## 49 0.135929 75.66736 42.98319 68.16411 0.494473 0.014012 0.296291694
## 50 0.152230 79.30759 71.77680 80.93225 0.937250 0.366760 0.211900953
## 51 0.105051 72.78475 65.60848 76.68638 0.776782 0.465621 0.456147980
## 52 0.091551 80.04124 81.27368 84.97672 0.994898 0.715498 0.526037865
## 53 0.098182 69.74541 67.18034 77.18774 0.830449 0.372615 0.491708989
## 54 0.099614 74.91590 79.34285 84.00884 0.993514 0.520691 0.484295484
## 55 0.101947 67.40067 72.07417 79.23653 0.855124 0.642665 0.472217477
## 56 0.099632 63.00757 83.18310 85.68297 0.995627 0.850253 0.484202298
## 57 0.126336 74.59716 67.53279 77.40817 0.845264 0.264136 0.345954929
## 58 0.154053 69.11744 79.63435 84.08218 0.993540 0.558153 0.202463230
## 59 0.142847 60.98274 69.38670 78.49519 0.817717 0.637669 0.260477011
## 60 0.113915 65.84050 83.33111 85.76683 0.996356 0.910842 0.410258800
## 61 0.102039 72.18291 76.11422 81.07088 0.927114 0.651169 0.471741190
## 62 0.140126 56.34022 83.91127 85.93203 0.995627 0.946133 0.274563706
## 63 0.151040 47.17321 78.94803 82.33495 0.927114 0.812662 0.218061617
## 64 0.107757 57.19837 84.18864 86.06779 0.994169 0.931266 0.442138941
##              d2     Overall
## 1  0.6995944863 0.308338759
## 2  0.5942542210 0.212500451
## 3  0.3706755186 0.258524375
## 4  0.3094638658 0.270404124
## 5  0.5565986966 0.463114413
## 6  0.4895055737 0.432654663
## 7  0.2832158878 0.239208222
## 8  0.1033350853 0.143713381
## 9  0.6195421708 0.390042052
## 10 0.4249570445 0.206852813
## 11 0.2130736685 0.059598195
## 12 0.1286041977 0.130611927
## 13 0.5430966624 0.460485914
## 14 0.2136224854 0.196868753
## 15 0.0509557204 0.000000000
## 16 0.0002397137 0.006049462
## 17 0.6496304472 0.249402402
## 18 0.7099105748 0.293815271
## 19 0.3310000531 0.270851968
## 20 0.3862719325 0.278768591
## 21 0.5221867571 0.425782901
## 22 0.6145432485 0.518225284
## 23 0.2577500171 0.253089320
## 24 0.1702026225 0.167860708
## 25 0.5038511530 0.203755045
## 26 0.5916687717 0.392541139
## 27 0.2487576886 0.197038269
## 28 0.1657078634 0.027335116
## 29 0.4351290270 0.314296146
## 30 0.3794919799 0.372693043
## 31 0.1005835427 0.110691610
## 32 0.0466784406 0.041296049
## 33 0.6819034611 0.397428879
## 34 0.5925369136 0.397228152
## 35 0.6488637339 0.568424725
## 36 0.5508944834 0.510675441
## 37 0.5691736525 0.496944629
## 38 0.5199677726 0.522618141
## 39 0.5540631047 0.538296338
## 40 0.2947420789 0.358579090
## 41 0.5484675771 0.308734355
## 42 0.5029106261 0.423088782
## 43 0.5349290108 0.475628078
## 44 0.2892987946 0.258217867
## 45 0.4924099001 0.390577045
## 46 0.3239061953 0.377145803
## 47 0.3092621049 0.349114015
## 48 0.1436121434 0.190075956
## 49 0.6242919915 0.430084331
## 50 0.6804988701 0.379734590
## 51 0.5797830641 0.514263428
## 52 0.6918267743 0.603263690
## 53 0.5328541478 0.511868317
## 54 0.6126889839 0.544722414
## 55 0.4966502462 0.484279801
## 56 0.4288185736 0.455669769
## 57 0.6077675267 0.458541352
## 58 0.5231579489 0.325453911
## 59 0.3975541174 0.321797620
## 60 0.4725604634 0.440309083
## 61 0.5704903716 0.518771440
## 62 0.3258714307 0.299119487
## 63 0.1843281284 0.200486632
## 64 0.3391216458 0.387219428
## class=design, type= FrF2
```


``` r
# Assuming df is your dataframe containing the variables IOH, sDA, and Overall
library(ggplot2)
```



``` r
# Scatterplot with hue based on Overall
ggplot(df, aes(x = IOH, y = UDI, color = Overall)) +
  geom_point(size = 3) +  # Set the size of points to 3 (adjust as needed)
  labs(x = "Indoor Overheating Hours (IOH)", y = "Useful Daylight Illuminance (UDI)", color = "Overall Desirability") +
  scale_color_gradient2(low = "red", high = "blue", mid = "grey", midpoint = 0.5, guide = "legend") +
  scale_x_continuous(breaks = seq(0, 0.2, by = 0.02), limits = c(0, 0.2)) +  # Set x-axis ticks from 0 to 20
  scale_y_continuous(breaks = seq(30, 100, by = 5), limits = c(30, 100)) +  # Set y-axis ticks from 40 to 100
  theme_minimal()  +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background color to white
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Show axis lines
    axis.title.x = element_text(size = 14),  # Increase font size of x-axis title
    axis.title.y = element_text(size = 14),  # Increase font size of y-axis title
    axis.text.x = element_text(size = 12),  # Increase font size of x-axis tick labels
    axis.text.y = element_text(size = 12),   # Increase font size of y-axis tick labels
    axis.ticks = element_line(color = "black") # Show axis ticks
  )
```

![](RSM_thermal_daylight_optimization_files/figure-html/unnamed-chunk-32-1.png)<!-- -->





``` r
# Calculate the density of the 'Overall' column
density_values <- density(df$Overall)

# Assuming 'Overall' is the column name in your dataframe 'df'
hist(df$Overall, 
     main="Histogram with Density Curve", 
     xlab="Overall Desirability Values", 
     col="lightblue", 
     border="black", 
     probability=TRUE, 
     ylim=c(0, max(density_values$y)))

# Add a density curve
lines(density(df$Overall), col="darkred", lwd=2)
```

![](RSM_thermal_daylight_optimization_files/figure-html/unnamed-chunk-33-1.png)<!-- -->





``` r
min(df$Overall)
```

```
## [1] 0
```

``` r
max(df$Overall)
```

```
## [1] 0.6032637
```





``` r
cor.test(df$Overall, df$UDI, method="kendall")
```

```
## 
## 	Kendall's rank correlation tau
## 
## data:  df$Overall and df$UDI
## z = 6.4773, p-value = 9.34e-11
## alternative hypothesis: true tau is not equal to 0
## sample estimates:
##       tau 
## 0.5545635
```

``` r
cor.test(df$Overall, df$IOH, method="kendall")
```

```
## 
## 	Kendall's rank correlation tau
## 
## data:  df$Overall and df$IOH
## z = -8.4819, p-value < 2.2e-16
## alternative hypothesis: true tau is not equal to 0
## sample estimates:
##        tau 
## -0.7261905
```






# **6. Stepwise regression with Overall Desirability**


``` r
library(MASS)
library(dplyr)
```

```
## 
## Adjuntando el paquete: 'dplyr'
```

```
## The following object is masked from 'package:MASS':
## 
##     select
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```




``` r
# Specify the variables to drop
variables_to_drop <- c("d1", "d2", "DA", "cDA", "sDA50", "sDA80")
```



``` r
# Drop the specified variables from the dataframe
df_sw <- df[, !names(df) %in% variables_to_drop]
```




``` r
library(corrplot)
```

```
## corrplot 0.92 loaded
```

``` r
# Convert selected features to numeric, handling non-numeric values
df_sw[] <- lapply(df_sw, function(col) {
  as.numeric(as.character(col))
})

# Calculate the correlation matrix
correlation_matrix <- cor(df_sw, method="spearman", use = "pairwise.complete.obs")
correlation_matrix
```

```
##                     WWR_N      WWR_S       WWR_E      WWR_W RoofOvrhng_N
## WWR_N         1.000000000  0.0000000  0.00000000  0.0000000   0.00000000
## WWR_S         0.000000000  1.0000000  0.00000000  0.0000000   0.00000000
## WWR_E         0.000000000  0.0000000  1.00000000  0.0000000   0.00000000
## WWR_W         0.000000000  0.0000000  0.00000000  1.0000000   0.00000000
## RoofOvrhng_N  0.000000000  0.0000000  0.00000000  0.0000000   1.00000000
## RoofOvrhng_S  0.000000000  0.0000000  0.00000000  0.0000000   0.00000000
## RoofOvrhng_E  0.000000000  0.0000000  0.00000000  0.0000000   0.00000000
## RoofOvrhng_W  0.000000000  0.0000000  0.00000000  0.0000000   0.00000000
## IOH          -0.001691662  0.0541332 -0.29773258  0.2774326  -0.02706660
## UDI          -0.169166238 -0.5870068 -0.37893237 -0.4195323   0.08796644
## Overall      -0.115033042 -0.3078826  0.04567488 -0.3687824   0.05920818
##              RoofOvrhng_S RoofOvrhng_E RoofOvrhng_W          IOH         UDI
## WWR_N           0.0000000    0.0000000    0.0000000 -0.001691662 -0.16916624
## WWR_S           0.0000000    0.0000000    0.0000000  0.054133196 -0.58700685
## WWR_E           0.0000000    0.0000000    0.0000000 -0.297732579 -0.37893237
## WWR_W           0.0000000    0.0000000    0.0000000  0.277432631 -0.41953227
## RoofOvrhng_N    0.0000000    0.0000000    0.0000000 -0.027066598  0.08796644
## RoofOvrhng_S    1.0000000    0.0000000    0.0000000 -0.652981679  0.28758260
## RoofOvrhng_E    0.0000000    1.0000000    0.0000000 -0.116724704  0.11334138
## RoofOvrhng_W    0.0000000    0.0000000    1.0000000 -0.336640814  0.15732460
## IOH            -0.6529817   -0.1167247   -0.3366408  1.000000000 -0.38081502
## UDI             0.2875826    0.1133414    0.1573246 -0.380815018  1.00000000
## Overall         0.5768569    0.1539413    0.2943493 -0.894780220  0.71300366
##                  Overall
## WWR_N        -0.11503304
## WWR_S        -0.30788255
## WWR_E         0.04567488
## WWR_W        -0.36878240
## RoofOvrhng_N  0.05920818
## RoofOvrhng_S  0.57685687
## RoofOvrhng_E  0.15394128
## RoofOvrhng_W  0.29434925
## IOH          -0.89478022
## UDI           0.71300366
## Overall       1.00000000
```

``` r
# Initialize an empty matrix to store p-values
p_value_matrix <- matrix(NA, nrow = ncol(df_sw), ncol = ncol(df_sw))
rownames(p_value_matrix) <- colnames(df_sw)
colnames(p_value_matrix) <- colnames(df_sw)

# Calculate the p-values for each pair of variables
for(i in 1:ncol(df_sw)) {
  for(j in 1:ncol(df_sw)) {
    if(i <= j) {
      test_result <- cor.test(df_sw[[i]], df_sw[[j]], method = "kendall", use = "pairwise.complete.obs")
      p_value_matrix[i, j] <- test_result$p.value
      p_value_matrix[j, i] <- test_result$p.value
    }
  }
}


p_value_matrix
```

```
##                     WWR_N        WWR_S        WWR_E        WWR_W RoofOvrhng_N
## WWR_N        2.067066e-15 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
## WWR_S        1.000000e+00 2.067066e-15 1.000000e+00 1.000000e+00 1.000000e+00
## WWR_E        1.000000e+00 1.000000e+00 2.067066e-15 1.000000e+00 1.000000e+00
## WWR_W        1.000000e+00 1.000000e+00 1.000000e+00 2.067066e-15 1.000000e+00
## RoofOvrhng_N 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 2.067066e-15
## RoofOvrhng_S 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
## RoofOvrhng_E 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
## RoofOvrhng_W 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
## IOH          9.892870e-01 6.674365e-01 1.811891e-02 2.766155e-02 8.298964e-01
## UDI          1.793641e-01 3.174061e-06 2.632480e-03 8.686652e-04 4.850446e-01
## Overall      3.612181e-01 1.453582e-02 7.169537e-01 3.421173e-03 6.383905e-01
##              RoofOvrhng_S RoofOvrhng_E RoofOvrhng_W          IOH          UDI
## WWR_N        1.000000e+00 1.000000e+00 1.000000e+00 9.892870e-01 1.793641e-01
## WWR_S        1.000000e+00 1.000000e+00 1.000000e+00 6.674365e-01 3.174061e-06
## WWR_E        1.000000e+00 1.000000e+00 1.000000e+00 1.811891e-02 2.632480e-03
## WWR_W        1.000000e+00 1.000000e+00 1.000000e+00 2.766155e-02 8.686652e-04
## RoofOvrhng_N 1.000000e+00 1.000000e+00 1.000000e+00 8.298964e-01 4.850446e-01
## RoofOvrhng_S 2.067066e-15 1.000000e+00 1.000000e+00 2.184840e-07 2.245299e-02
## RoofOvrhng_E 1.000000e+00 2.067066e-15 1.000000e+00 3.541999e-01 3.683229e-01
## RoofOvrhng_W 1.000000e+00 1.000000e+00 2.067066e-15 7.539983e-03 2.117656e-01
## IOH          2.184840e-07 3.541999e-01 7.539983e-03 1.614050e-31 1.041055e-03
## UDI          2.245299e-02 3.683229e-01 2.117656e-01 1.041055e-03 1.614050e-31
## Overall      4.679653e-06 2.217564e-01 1.947432e-02 2.216167e-17 9.339723e-11
##                   Overall
## WWR_N        3.612181e-01
## WWR_S        1.453582e-02
## WWR_E        7.169537e-01
## WWR_W        3.421173e-03
## RoofOvrhng_N 6.383905e-01
## RoofOvrhng_S 4.679653e-06
## RoofOvrhng_E 2.217564e-01
## RoofOvrhng_W 1.947432e-02
## IOH          2.216167e-17
## UDI          9.339723e-11
## Overall      1.614050e-31
```

``` r
# Open a PNG device
png("correlogram.png", height = 5, width = 5, units = "in", res = 300)


# Plot the correlogram using corrplot
corrplot(correlation_matrix, method = "circle", type = "upper",  tl.col = "black", tl.srt = 45, addCoef.col = 1,  number.cex = 0.6, tl.cex = 0.75, number.font = 1, p.mat = p_value_matrix, sig.level = 0.05, insig = "blank", addgrid.col="white")


# Close the PNG device
dev.off()
```

```
## png 
##   2
```




``` r
# Specify the variables to drop
variables_to_drop <- c("UDI", "IOH")
```


``` r
# Drop the specified variables from the dataframe
df_sw <- df_sw[, !names(df_sw) %in% variables_to_drop]
```






``` r
#F1
fullModel = lm(Overall ~ ., data = df_sw) 
nullModel = lm(Overall ~ 1, data = df_sw)
n = nrow(df_sw)

stepwise_model <- lm(Overall ~ 1, data = df_sw) %>%
  stepAIC(trace = FALSE, direction = "both", scope = list(upper = fullModel, lower = nullModel), k = log(n))

summary(stepwise_model)
```

```
## 
## Call:
## lm.default(formula = Overall ~ RoofOvrhng_S + WWR_W + WWR_S + 
##     RoofOvrhng_W, data = df_sw)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.17767 -0.06263 -0.01038  0.06702  0.17951 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   0.33336    0.01107  30.117  < 2e-16 ***
## RoofOvrhng_S  0.08659    0.01107   7.823 1.09e-10 ***
## WWR_W        -0.05728    0.01107  -5.175 2.87e-06 ***
## WWR_S        -0.04863    0.01107  -4.393 4.71e-05 ***
## RoofOvrhng_W  0.04286    0.01107   3.872 0.000272 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.08855 on 59 degrees of freedom
## Multiple R-squared:  0.6745,	Adjusted R-squared:  0.6525 
## F-statistic: 30.57 on 4 and 59 DF,  p-value: 8.688e-14
```

``` r
stepwise_model$anova
```

```
## Stepwise Model Path 
## Analysis of Deviance Table
## 
## Initial Model:
## Overall ~ 1
## 
## Final Model:
## Overall ~ RoofOvrhng_S + WWR_W + WWR_S + RoofOvrhng_W
## 
## 
##             Step Df  Deviance Resid. Df Resid. Dev       AIC
## 1                                    63  1.4213966 -239.5047
## 2 + RoofOvrhng_S  1 0.4798943        62  0.9415023 -261.7086
## 3        + WWR_W  1 0.2099816        61  0.7315207 -273.7002
## 4        + WWR_S  1 0.1513296        60  0.5801911 -284.3744
## 5 + RoofOvrhng_W  1 0.1175788        59  0.4626122 -294.7095
```

``` r
model_summary <- summary(stepwise_model)
```



``` r
# Extract p-values from the model coefficients
p_values <- coef(model_summary)[, "Pr(>|t|)"]

# Apply FDR adjustment using the Benjamini-Hochberg (BH) method
adjusted_p_values <- p.adjust(p_values, method = "BH")

# Identify significant coefficients based on FDR-adjusted p-values (threshold: 0.05)
significant <- adjusted_p_values < 0.05

# Display summary with only significant coefficients
significant_coefficients <- coef(model_summary)[significant, ]
print(significant_coefficients)
```

```
##                 Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)   0.33335583  0.0110686 30.117262 1.623863e-37
## RoofOvrhng_S  0.08659300  0.0110686  7.823305 1.089281e-10
## WWR_W        -0.05727969  0.0110686 -5.174973 2.871747e-06
## WWR_S        -0.04862638  0.0110686 -4.393184 4.713275e-05
## RoofOvrhng_W  0.04286222  0.0110686  3.872416 2.724323e-04
```

``` r
# Print the adjusted R-squared value of the model
cat("Adjusted R-squared:", summary(stepwise_model)$adj.r.squared, "\n")
```

```
## Adjusted R-squared: 0.6524715
```




#### Normality


``` r
resst=rstandard(stepwise_model)
ks.test(resst, "pnorm")
```

```
## 
## 	Exact one-sample Kolmogorov-Smirnov test
## 
## data:  resst
## D = 0.094063, p-value = 0.5898
## alternative hypothesis: two-sided
```

``` r
shapiro.test(resst)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  resst
## W = 0.96871, p-value = 0.1036
```

``` r
plot(stepwise_model, 2)
```

![](RSM_thermal_daylight_optimization_files/figure-html/unnamed-chunk-44-1.png)<!-- -->


### Homocedascticity

``` r
plot(stepwise_model, 3)
```

![](RSM_thermal_daylight_optimization_files/figure-html/unnamed-chunk-45-1.png)<!-- -->

``` r
bartlett.test(Overall ~ RoofOvrhng_S, data = df_sw)
```

```
## 
## 	Bartlett test of homogeneity of variances
## 
## data:  Overall by RoofOvrhng_S
## Bartlett's K-squared = 2.6304, df = 1, p-value = 0.1048
```

``` r
bartlett.test(Overall ~ WWR_E, data = df_sw)
```

```
## 
## 	Bartlett test of homogeneity of variances
## 
## data:  Overall by WWR_E
## Bartlett's K-squared = 1.0196, df = 1, p-value = 0.3126
```

``` r
bartlett.test(Overall ~ RoofOvrhng_W, data = df_sw)
```

```
## 
## 	Bartlett test of homogeneity of variances
## 
## data:  Overall by RoofOvrhng_W
## Bartlett's K-squared = 0.0020581, df = 1, p-value = 0.9638
```

``` r
bartlett.test(Overall ~ WWR_N, data = df_sw)
```

```
## 
## 	Bartlett test of homogeneity of variances
## 
## data:  Overall by WWR_N
## Bartlett's K-squared = 0.12534, df = 1, p-value = 0.7233
```



RSM
- WWR en este y norte: 15% (como el paper de Overheatinf Risk 2020) aprox 17%mean
- Overhang este y norte: 0.5m


# **7. Lasso Regression with multivariate response**

### Install and load glmnet package


``` r
library(glmnet)
```

```
## Warning: package 'glmnet' was built under R version 4.4.1
```

```
## Cargando paquete requerido: Matrix
```

```
## Loaded glmnet 4.1-8
```

### Define the response variables


``` r
Y <- data.matrix(df[, c("Overall")])
x <- data.matrix(df[, c("RoofOvrhng_N", "RoofOvrhng_S", "RoofOvrhng_E", "RoofOvrhng_W", "WWR_N", "WWR_S", "WWR_E", "WWR_W")])
```


``` r
Y
```

```
##        Overall
## 1  0.308338759
## 2  0.212500451
## 3  0.258524375
## 4  0.270404124
## 5  0.463114413
## 6  0.432654663
## 7  0.239208222
## 8  0.143713381
## 9  0.390042052
## 10 0.206852813
## 11 0.059598195
## 12 0.130611927
## 13 0.460485914
## 14 0.196868753
## 15 0.000000000
## 16 0.006049462
## 17 0.249402402
## 18 0.293815271
## 19 0.270851968
## 20 0.278768591
## 21 0.425782901
## 22 0.518225284
## 23 0.253089320
## 24 0.167860708
## 25 0.203755045
## 26 0.392541139
## 27 0.197038269
## 28 0.027335116
## 29 0.314296146
## 30 0.372693043
## 31 0.110691610
## 32 0.041296049
## 33 0.397428879
## 34 0.397228152
## 35 0.568424725
## 36 0.510675441
## 37 0.496944629
## 38 0.522618141
## 39 0.538296338
## 40 0.358579090
## 41 0.308734355
## 42 0.423088782
## 43 0.475628078
## 44 0.258217867
## 45 0.390577045
## 46 0.377145803
## 47 0.349114015
## 48 0.190075956
## 49 0.430084331
## 50 0.379734590
## 51 0.514263428
## 52 0.603263690
## 53 0.511868317
## 54 0.544722414
## 55 0.484279801
## 56 0.455669769
## 57 0.458541352
## 58 0.325453911
## 59 0.321797620
## 60 0.440309083
## 61 0.518771440
## 62 0.299119487
## 63 0.200486632
## 64 0.387219428
```


``` r
x
```

```
##    RoofOvrhng_N RoofOvrhng_S RoofOvrhng_E RoofOvrhng_W WWR_N WWR_S WWR_E WWR_W
## 1             1            1            2            2     1     1     1     1
## 2             1            1            1            1     2     1     1     1
## 3             1            1            1            1     1     2     1     1
## 4             1            1            2            2     2     2     1     1
## 5             1            1            1            2     1     1     2     1
## 6             1            1            2            1     2     1     2     1
## 7             1            1            2            1     1     2     2     1
## 8             1            1            1            2     2     2     2     1
## 9             1            1            1            2     1     1     1     2
## 10            1            1            2            1     2     1     1     2
## 11            1            1            2            1     1     2     1     2
## 12            1            1            1            2     2     2     1     2
## 13            1            1            2            2     1     1     2     2
## 14            1            1            1            1     2     1     2     2
## 15            1            1            1            1     1     2     2     2
## 16            1            1            2            2     2     2     2     2
## 17            2            1            2            1     1     1     1     1
## 18            2            1            1            2     2     1     1     1
## 19            2            1            1            2     1     2     1     1
## 20            2            1            2            1     2     2     1     1
## 21            2            1            1            1     1     1     2     1
## 22            2            1            2            2     2     1     2     1
## 23            2            1            2            2     1     2     2     1
## 24            2            1            1            1     2     2     2     1
## 25            2            1            1            1     1     1     1     2
## 26            2            1            2            2     2     1     1     2
## 27            2            1            2            2     1     2     1     2
## 28            2            1            1            1     2     2     1     2
## 29            2            1            2            1     1     1     2     2
## 30            2            1            1            2     2     1     2     2
## 31            2            1            1            2     1     2     2     2
## 32            2            1            2            1     2     2     2     2
## 33            1            2            2            1     1     1     1     1
## 34            1            2            1            2     2     1     1     1
## 35            1            2            1            2     1     2     1     1
## 36            1            2            2            1     2     2     1     1
## 37            1            2            1            1     1     1     2     1
## 38            1            2            2            2     2     1     2     1
## 39            1            2            2            2     1     2     2     1
## 40            1            2            1            1     2     2     2     1
## 41            1            2            1            1     1     1     1     2
## 42            1            2            2            2     2     1     1     2
## 43            1            2            2            2     1     2     1     2
## 44            1            2            1            1     2     2     1     2
## 45            1            2            2            1     1     1     2     2
## 46            1            2            1            2     2     1     2     2
## 47            1            2            1            2     1     2     2     2
## 48            1            2            2            1     2     2     2     2
## 49            2            2            2            2     1     1     1     1
## 50            2            2            1            1     2     1     1     1
## 51            2            2            1            1     1     2     1     1
## 52            2            2            2            2     2     2     1     1
## 53            2            2            1            2     1     1     2     1
## 54            2            2            2            1     2     1     2     1
## 55            2            2            2            1     1     2     2     1
## 56            2            2            1            2     2     2     2     1
## 57            2            2            1            2     1     1     1     2
## 58            2            2            2            1     2     1     1     2
## 59            2            2            2            1     1     2     1     2
## 60            2            2            1            2     2     2     1     2
## 61            2            2            2            2     1     1     2     2
## 62            2            2            1            1     2     1     2     2
## 63            2            2            1            1     1     2     2     2
## 64            2            2            2            2     2     2     2     2
```




### Create models


``` r
set.seed(123)  # Set seed for reproducibility
```


#### perform k-fold cross-validation to find optimal lambda value


``` r
cv_model <- cv.glmnet(x, Y, alpha = 1, family = "gaussian", nfolds = 3)
```

#### find optimal lambda value that minimizes test MSE


``` r
best_lambda <- cv_model$lambda.min
best_lambda
```

```
## [1] 0.003040446
```


``` r
best_lambda2 <- cv_model$lambda.1se
best_lambda2
```

```
## [1] 0.02144975
```


#### produce plot of test MSE by lambda value


``` r
plot(cv_model) 
```

![](RSM_thermal_daylight_optimization_files/figure-html/unnamed-chunk-54-1.png)<!-- -->

#### find coefficients of best model


``` r
best_model <- glmnet(x, Y, alpha = 1, lambda = best_lambda2, family = "gaussian")
coef(best_model)
```

```
## 9 x 1 sparse Matrix of class "dgCMatrix"
##                       s0
## (Intercept)   0.26270838
## RoofOvrhng_N  .         
## RoofOvrhng_S  0.13028651
## RoofOvrhng_E  .         
## RoofOvrhng_W  0.04282493
## WWR_N         .         
## WWR_S        -0.05435327
## WWR_E         .         
## WWR_W        -0.07165988
```





# **8. Response Surface Models**

Load `rsm`package


``` r
library(rsm)
```


## **First-order models**


### **Experiment 1**


``` r
dsg1_fo <- cube(3, n0=3, randomize=FALSE)
dsg1_fo
```

```
##    run.order std.order x1.as.is x2.as.is x3.as.is
## 1          1         1       -1       -1       -1
## 2          2         2        1       -1       -1
## 3          3         3       -1        1       -1
## 4          4         4        1        1       -1
## 5          5         5       -1       -1        1
## 6          6         6        1       -1        1
## 7          7         7       -1        1        1
## 8          8         8        1        1        1
## 9          9         9        0        0        0
## 10        10        10        0        0        0
## 11        11        11        0        0        0
## 
## Data are stored in coded form using these coding formulas ...
## x1 ~ x1.as.is
## x2 ~ x2.as.is
## x3 ~ x3.as.is
```


``` r
as.data.frame(dsg1_fo)
```

```
##    run.order std.order x1 x2 x3
## 1          1         1 -1 -1 -1
## 2          2         2  1 -1 -1
## 3          3         3 -1  1 -1
## 4          4         4  1  1 -1
## 5          5         5 -1 -1  1
## 6          6         6  1 -1  1
## 7          7         7 -1  1  1
## 8          8         8  1  1  1
## 9          9         9  0  0  0
## 10        10        10  0  0  0
## 11        11        11  0  0  0
```


##### Add random values for other variables

##### Generate a random number from the beta distribution

``` r
WWR_N_random <- rnorm(11, mean = 0, sd=2)
WWR_E_random <- rnorm(11, mean = 0, sd=2)
RoofOvrhng_N_random <- rnorm(11, mean = 0, sd=2)
RoofOvrhng_E_random <- rnorm(11, mean = 0, sd=2)
```

##### Save into csv

``` r
write.csv(WWR_N_random, "WWR_N_random.csv")
write.csv(WWR_E_random, "WWR_E_random.csv")
write.csv(RoofOvrhng_N_random, "RoofOvrhng_N_random.csv")
write.csv(RoofOvrhng_E_random, "RoofOvrhng_E_random.csv")
```

##### Load random numbers

``` r
WWR_N_random <- read.csv('randomNumbers/dsg1_fo/WWR_N_random.csv')
WWR_E_random <- read.csv('randomNumbers/dsg1_fo/WWR_E_random.csv')
RoofOvrhng_N_random <- read.csv('randomNumbers/dsg1_fo/RoofOvrhng_N_random.csv')
RoofOvrhng_E_random <- read.csv('randomNumbers/dsg1_fo/RoofOvrhng_E_random.csv')
```

##### Append random numbers to Rhino file

``` r
dsg1_fo$WWR_N <- ((WWR_N_random$x * 2) + 15)/100
dsg1_fo$WWR_E <- ((WWR_E_random$x * 2) + 15)/100
dsg1_fo$RoofOvrhng_N <- ((RoofOvrhng_N_random$x * 2) + 50)/100
dsg1_fo$RoofOvrhng_E <- ((RoofOvrhng_E_random$x * 2) + 50)/100
```


##### Data Collection (Rhino) - Simulations

``` r
dsg1_fo <- as.data.frame(dsg1_fo)
write.csv(dsg1_fo, "dsg_fo_1.csv")
dsg1_fo
```

```
##    run.order std.order x1 x2 x3      WWR_N     WWR_E RoofOvrhng_N RoofOvrhng_E
## 1          1         1 -1 -1 -1 0.08567435 0.1937083    0.5232264    0.5237708
## 2          2         2  1 -1 -1 0.19610136 0.1370468    0.4765137    0.5881314
## 3          3         3 -1  1 -1 0.22044151 0.1290086    0.5128033    0.3951510
## 4          4         4  1  1 -1 0.11820489 0.1979854    0.5206644    0.5073799
## 5          5         5 -1 -1  1 0.18962634 0.1250927    0.4799168    0.5074037
## 6          6         6  1 -1  1 0.11943519 0.1393275    0.5233646    0.5074908
## 7          7         7 -1  1  1 0.05900656 0.1696052    0.5048154    0.5417652
## 8          8         8  1  1  1 0.08892342 0.1334582    0.4999819    0.4155448
## 9          9         9  0  0  0 0.17190987 0.2004578    0.5412329    0.5007336
## 10        10        10  0  0  0 0.21092834 0.1525342    0.5463859    0.4803053
## 11        11        11  0  0  0 0.18711350 0.1611472    0.4319177    0.5240263
```

We upload simulation results




#### Load simulation results and append

``` r
ioh <- read.csv('CCD/FO_data_ioh_1.csv')
sda <- read.csv('CCD/FO_data_udi_1.csv')

ioh <- as.data.frame(ioh)
sda <- as.data.frame(sda)

print(ioh)
```

```
##    in.FO  out.IOH      img
## 1      1 0.099511  FO1.png
## 2      2 0.090010  FO2.png
## 3      3 0.111297  FO3.png
## 4      4 0.098355  FO4.png
## 5      5 0.104487  FO5.png
## 6      6 0.093274  FO6.png
## 7      7 0.117959  FO7.png
## 8      8 0.103315  FO8.png
## 9      9 0.102112  FO9.png
## 10    10 0.101614 FO10.png
## 11    11 0.101543 FO11.png
```

``` r
print(sda)
```

```
##    in.FO   out.DA  out.cDA  out.UDI out.sDA50 out.sDA80      img
## 1      1 77.35512 83.09721 75.76564  0.948505  0.663101  FO1.png
## 2      2 81.23193 84.94230 78.74894  0.990525  0.700240  FO2.png
## 3      3 83.72288 85.97175 68.39721  0.998542  0.933560  FO3.png
## 4      4 80.61795 84.73569 72.97912  0.966472  0.815305  FO4.png
## 5      5 82.78230 85.59602 69.54944  0.988338  0.839578  FO5.png
## 6      6 79.17700 84.12568 74.63984  0.964355  0.694334  FO6.png
## 7      7 78.75404 83.24934 61.95183  0.940962  0.741378  FO7.png
## 8      8 79.50897 84.08124 69.63323  0.956268  0.760554  FO8.png
## 9      9 82.67288 85.58726 68.62591  0.983236  0.860563  FO9.png
## 10    10 83.32293 85.82678 69.58266  0.994898  0.900410 FO10.png
## 11    11 83.02640 85.71698 69.78006  0.988338  0.882626 FO11.png
```

##### Append simulation results with fractional factorial design

``` r
dsg1_fo$IOH  <- ioh$out.IOH 
dsg1_fo$sDA  <- sda$out.UDI
dsg1_fo
```

```
##    run.order std.order x1 x2 x3      WWR_N     WWR_E RoofOvrhng_N RoofOvrhng_E
## 1          1         1 -1 -1 -1 0.08567435 0.1937083    0.5232264    0.5237708
## 2          2         2  1 -1 -1 0.19610136 0.1370468    0.4765137    0.5881314
## 3          3         3 -1  1 -1 0.22044151 0.1290086    0.5128033    0.3951510
## 4          4         4  1  1 -1 0.11820489 0.1979854    0.5206644    0.5073799
## 5          5         5 -1 -1  1 0.18962634 0.1250927    0.4799168    0.5074037
## 6          6         6  1 -1  1 0.11943519 0.1393275    0.5233646    0.5074908
## 7          7         7 -1  1  1 0.05900656 0.1696052    0.5048154    0.5417652
## 8          8         8  1  1  1 0.08892342 0.1334582    0.4999819    0.4155448
## 9          9         9  0  0  0 0.17190987 0.2004578    0.5412329    0.5007336
## 10        10        10  0  0  0 0.21092834 0.1525342    0.5463859    0.4803053
## 11        11        11  0  0  0 0.18711350 0.1611472    0.4319177    0.5240263
##         IOH      sDA
## 1  0.099511 75.76564
## 2  0.090010 78.74894
## 3  0.111297 68.39721
## 4  0.098355 72.97912
## 5  0.104487 69.54944
## 6  0.093274 74.63984
## 7  0.117959 61.95183
## 8  0.103315 69.63323
## 9  0.102112 68.62591
## 10 0.101614 69.58266
## 11 0.101543 69.78006
```

##### Create a new dataframe with only the results

``` r
predOutcomes <- data.frame(
  ioh = dsg1_fo$IOH,
  sda = dsg1_fo$sDA
)
predOutcomes
```

```
##         ioh      sda
## 1  0.099511 75.76564
## 2  0.090010 78.74894
## 3  0.111297 68.39721
## 4  0.098355 72.97912
## 5  0.104487 69.54944
## 6  0.093274 74.63984
## 7  0.117959 61.95183
## 8  0.103315 69.63323
## 9  0.102112 68.62591
## 10 0.101614 69.58266
## 11 0.101543 69.78006
```

##### Calcular Desirability values

``` r
library(desirability)
```


##### Desirability target and limit

``` r
iohD <- dMin(0, 0.193161, scale = 1)
sdaD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, sdaD)
head(overallD)
```

```
## $d
## $d[[1]]
## Smaller-is-better desirability function
## 
## Call: dMin.default(low = 0, high = 0.193161, scale = 1)
## 
## Non-informative value: 0.5 
## 
## $d[[2]]
## Larger-is-better desirability function
## 
## Call: dMax.default(low = 35.23524, high = 100, scale = 1)
## 
## Non-informative value: 0.5 
## 
## 
## $call
## dOverall.default(iohD, sdaD)
```

##### Calculate the Overall Desirability values

``` r
predict(iohD, predOutcomes[1])
```

```
##  [1] 0.4848287 0.5340157 0.4238123 0.4908134 0.4590678 0.5171178 0.3893229
##  [8] 0.4651353 0.4713633 0.4739414 0.4743090
```

``` r
predict(sdaD, predOutcomes[2])
```

```
##  [1] 0.6258095 0.6718731 0.5120373 0.5827842 0.5298282 0.6084265 0.4125174
##  [8] 0.5311221 0.5155685 0.5303412 0.5333891
```

``` r
predict(overallD, predOutcomes)
```

```
##  [1] 0.5508270 0.5989915 0.4658408 0.5348254 0.4931806 0.5609173 0.4007524
##  [8] 0.4970348 0.4929706 0.5013488 0.5029823
```

``` r
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
overall_desirability
```

```
##           D1        D2   Overall
## 1  0.4848287 0.6258095 0.5508270
## 2  0.5340157 0.6718731 0.5989915
## 3  0.4238123 0.5120373 0.4658408
## 4  0.4908134 0.5827842 0.5348254
## 5  0.4590678 0.5298282 0.4931806
## 6  0.5171178 0.6084265 0.5609173
## 7  0.3893229 0.4125174 0.4007524
## 8  0.4651353 0.5311221 0.4970348
## 9  0.4713633 0.5155685 0.4929706
## 10 0.4739414 0.5303412 0.5013488
## 11 0.4743090 0.5333891 0.5029823
```

##### Append all results in the factorial design

``` r
dsg1_fo$d1 <- overall_desirability$D1
dsg1_fo$d2 <- overall_desirability$D2
dsg1_fo$Overall <- overall_desirability$Overall
dsg1_fo
```

```
##    run.order std.order x1 x2 x3      WWR_N     WWR_E RoofOvrhng_N RoofOvrhng_E
## 1          1         1 -1 -1 -1 0.08567435 0.1937083    0.5232264    0.5237708
## 2          2         2  1 -1 -1 0.19610136 0.1370468    0.4765137    0.5881314
## 3          3         3 -1  1 -1 0.22044151 0.1290086    0.5128033    0.3951510
## 4          4         4  1  1 -1 0.11820489 0.1979854    0.5206644    0.5073799
## 5          5         5 -1 -1  1 0.18962634 0.1250927    0.4799168    0.5074037
## 6          6         6  1 -1  1 0.11943519 0.1393275    0.5233646    0.5074908
## 7          7         7 -1  1  1 0.05900656 0.1696052    0.5048154    0.5417652
## 8          8         8  1  1  1 0.08892342 0.1334582    0.4999819    0.4155448
## 9          9         9  0  0  0 0.17190987 0.2004578    0.5412329    0.5007336
## 10        10        10  0  0  0 0.21092834 0.1525342    0.5463859    0.4803053
## 11        11        11  0  0  0 0.18711350 0.1611472    0.4319177    0.5240263
##         IOH      sDA        d1        d2   Overall
## 1  0.099511 75.76564 0.4848287 0.6258095 0.5508270
## 2  0.090010 78.74894 0.5340157 0.6718731 0.5989915
## 3  0.111297 68.39721 0.4238123 0.5120373 0.4658408
## 4  0.098355 72.97912 0.4908134 0.5827842 0.5348254
## 5  0.104487 69.54944 0.4590678 0.5298282 0.4931806
## 6  0.093274 74.63984 0.5171178 0.6084265 0.5609173
## 7  0.117959 61.95183 0.3893229 0.4125174 0.4007524
## 8  0.103315 69.63323 0.4651353 0.5311221 0.4970348
## 9  0.102112 68.62591 0.4713633 0.5155685 0.4929706
## 10 0.101614 69.58266 0.4739414 0.5303412 0.5013488
## 11 0.101543 69.78006 0.4743090 0.5333891 0.5029823
```

##### Subset unnecesary columns and code data

``` r
dsg1_fo = subset(dsg1_fo, select = -c(run.order, std.order,RoofOvrhng_N, RoofOvrhng_E, WWR_N, WWR_E))
dsg1_fo <- coded.data(
  dsg1_fo, formulas = list(x1 ~ (RoofOvrhng_S_W - 2.5)/.5,
                  x2 ~ (WWR_W - 0.15)/.1,
                  x3 ~ (WWR_S - 0.4)/.1)
  )

dsg1_fo
```

```
##    RoofOvrhng_S_W WWR_W WWR_S      IOH      sDA        d1        d2   Overall
## 1             2.0  0.05   0.3 0.099511 75.76564 0.4848287 0.6258095 0.5508270
## 2             3.0  0.05   0.3 0.090010 78.74894 0.5340157 0.6718731 0.5989915
## 3             2.0  0.25   0.3 0.111297 68.39721 0.4238123 0.5120373 0.4658408
## 4             3.0  0.25   0.3 0.098355 72.97912 0.4908134 0.5827842 0.5348254
## 5             2.0  0.05   0.5 0.104487 69.54944 0.4590678 0.5298282 0.4931806
## 6             3.0  0.05   0.5 0.093274 74.63984 0.5171178 0.6084265 0.5609173
## 7             2.0  0.25   0.5 0.117959 61.95183 0.3893229 0.4125174 0.4007524
## 8             3.0  0.25   0.5 0.103315 69.63323 0.4651353 0.5311221 0.4970348
## 9             2.5  0.15   0.4 0.102112 68.62591 0.4713633 0.5155685 0.4929706
## 10            2.5  0.15   0.4 0.101614 69.58266 0.4739414 0.5303412 0.5013488
## 11            2.5  0.15   0.4 0.101543 69.78006 0.4743090 0.5333891 0.5029823
## 
## Data are stored in coded form using these coding formulas ...
## x1 ~ (RoofOvrhng_S_W - 2.5)/0.5
## x2 ~ (WWR_W - 0.15)/0.1
## x3 ~ (WWR_S - 0.4)/0.1
```

We create the response surface model and identify the most significant terms.

##### Model adjustment and summary

``` r
anal1_fo <- rsm(Overall ~ FO(x1, x2, x3), data = dsg1_fo)
summary(anal1_fo)
```

```
## 
## Call:
## rsm(formula = Overall ~ FO(x1, x2, x3), data = dsg1_fo)
## 
##               Estimate Std. Error  t value  Pr(>|t|)    
## (Intercept)  0.5090611  0.0037103 137.2003 2.883e-13 ***
## x1           0.0351460  0.0043508   8.0781 8.566e-05 ***
## x2          -0.0381829  0.0043508  -8.7761 5.024e-05 ***
## x3          -0.0248250  0.0043508  -5.7059 0.0007309 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Multiple R-squared:  0.9615,	Adjusted R-squared:  0.945 
## F-statistic: 58.28 on 3 and 7 DF,  p-value: 2.567e-05
## 
## Analysis of Variance Table
## 
## Response: Overall
##                Df    Sum Sq   Mean Sq F value    Pr(>F)
## FO(x1, x2, x3)  3 0.0264756 0.0088252 58.2777 2.567e-05
## Residuals       7 0.0010600 0.0001514                  
## Lack of fit     5 0.0010023 0.0002005  6.9487    0.1306
## Pure error      2 0.0000577 0.0000288                  
## 
## Direction of steepest ascent (at radius 1):
##         x1         x2         x3 
##  0.6109394 -0.6637284 -0.4315294 
## 
## Corresponding increment in original units:
## RoofOvrhng_S_W          WWR_W          WWR_S 
##     0.30546971    -0.06637284    -0.04315294
```





### **Experiment 2**

#### Implement steepest ascent on experiment 1



``` r
steepest(anal1_fo, descent = FALSE)
```

```
## Path of steepest ascent from ridge analysis:
```

```
##    dist    x1     x2     x3 | RoofOvrhng_S_W   WWR_W  WWR_S |  yhat
## 1   0.0 0.000  0.000  0.000 |         2.5000  0.1500 0.4000 | 0.509
## 2   0.5 0.306 -0.332 -0.216 |         2.6530  0.1168 0.3784 | 0.538
## 3   1.0 0.611 -0.664 -0.432 |         2.8055  0.0836 0.3568 | 0.567
## 4   1.5 0.916 -0.995 -0.647 |         2.9580  0.0505 0.3353 | 0.595
## 5   2.0 1.221 -1.326 -0.862 |         3.1105  0.0174 0.3138 | 0.624
## 6   2.5 1.528 -1.660 -1.079 |         3.2640 -0.0160 0.2921 | 0.653
## 7   3.0 1.832 -1.991 -1.294 |         3.4160 -0.0491 0.2706 | 0.682
## 8   3.5 2.140 -2.325 -1.512 |         3.5700 -0.0825 0.2488 | 0.711
## 9   4.0 2.441 -2.652 -1.724 |         3.7205 -0.1152 0.2276 | 0.739
## 10  4.5 2.750 -2.988 -1.942 |         3.8750 -0.1488 0.2058 | 0.768
## 11  5.0 3.060 -3.324 -2.161 |         4.0300 -0.1824 0.1839 | 0.797
```


``` r
steepest(anal1_fo, dist = (1:12)/8, descent = FALSE)
```

```
## Path of steepest ascent from ridge analysis:
```

```
##     dist    x1     x2     x3 | RoofOvrhng_S_W  WWR_W  WWR_S |  yhat
## 1  0.125 0.076 -0.083 -0.054 |         2.5380 0.1417 0.3946 | 0.516
## 2  0.250 0.153 -0.166 -0.108 |         2.5765 0.1334 0.3892 | 0.523
## 3  0.375 0.229 -0.249 -0.162 |         2.6145 0.1251 0.3838 | 0.531
## 4  0.500 0.306 -0.332 -0.216 |         2.6530 0.1168 0.3784 | 0.538
## 5  0.625 0.382 -0.415 -0.270 |         2.6910 0.1085 0.3730 | 0.545
## 6  0.750 0.458 -0.498 -0.324 |         2.7290 0.1002 0.3676 | 0.552
## 7  0.875 0.535 -0.581 -0.378 |         2.7675 0.0919 0.3622 | 0.559
## 8  1.000 0.611 -0.664 -0.432 |         2.8055 0.0836 0.3568 | 0.567
## 9  1.125 0.688 -0.747 -0.486 |         2.8440 0.0753 0.3514 | 0.574
## 10 1.250 0.764 -0.830 -0.540 |         2.8820 0.0670 0.3460 | 0.581
## 11 1.375 0.840 -0.913 -0.593 |         2.9200 0.0587 0.3407 | 0.588
## 12 1.500 0.916 -0.995 -0.647 |         2.9580 0.0505 0.3353 | 0.595
```


``` r
dsg2_fo = dupe(steepest(anal1_fo, dist = (1:12)/8, descent = FALSE),randomize=FALSE)
```

```
## Path of steepest ascent from ridge analysis:
```


``` r
dsg2_fo
```

```
##    run.order std.order  dist    x1     x2     x3 | RoofOvrhng_S_W  WWR_W  WWR_S
## 1          1         1 0.125 0.076 -0.083 -0.054 |         2.5380 0.1417 0.3946
## 2          2         2 0.250 0.153 -0.166 -0.108 |         2.5765 0.1334 0.3892
## 3          3         3 0.375 0.229 -0.249 -0.162 |         2.6145 0.1251 0.3838
## 4          4         4 0.500 0.306 -0.332 -0.216 |         2.6530 0.1168 0.3784
## 5          5         5 0.625 0.382 -0.415 -0.270 |         2.6910 0.1085 0.3730
## 6          6         6 0.750 0.458 -0.498 -0.324 |         2.7290 0.1002 0.3676
## 7          7         7 0.875 0.535 -0.581 -0.378 |         2.7675 0.0919 0.3622
## 8          8         8 1.000 0.611 -0.664 -0.432 |         2.8055 0.0836 0.3568
## 9          9         9 1.125 0.688 -0.747 -0.486 |         2.8440 0.0753 0.3514
## 10        10        10 1.250 0.764 -0.830 -0.540 |         2.8820 0.0670 0.3460
## 11        11        11 1.375 0.840 -0.913 -0.593 |         2.9200 0.0587 0.3407
## 12        12        12 1.500 0.916 -0.995 -0.647 |         2.9580 0.0505 0.3353
##    |.1  yhat
## 1    | 0.516
## 2    | 0.523
## 3    | 0.531
## 4    | 0.538
## 5    | 0.545
## 6    | 0.552
## 7    | 0.559
## 8    | 0.567
## 9    | 0.574
## 10   | 0.581
## 11   | 0.588
## 12   | 0.595
```


``` r
dsg2_fo <- data.frame(
  run.order = dsg2_fo$run.order,
  std.order = dsg2_fo$std.order,
  dist = dsg2_fo$dist,
  x1 = dsg2_fo$x1,
  x2 = dsg2_fo$x2,
  x3 = dsg2_fo$x3
)

dsg2_fo
```

```
##    run.order std.order  dist    x1     x2     x3
## 1          1         1 0.125 0.076 -0.083 -0.054
## 2          2         2 0.250 0.153 -0.166 -0.108
## 3          3         3 0.375 0.229 -0.249 -0.162
## 4          4         4 0.500 0.306 -0.332 -0.216
## 5          5         5 0.625 0.382 -0.415 -0.270
## 6          6         6 0.750 0.458 -0.498 -0.324
## 7          7         7 0.875 0.535 -0.581 -0.378
## 8          8         8 1.000 0.611 -0.664 -0.432
## 9          9         9 1.125 0.688 -0.747 -0.486
## 10        10        10 1.250 0.764 -0.830 -0.540
## 11        11        11 1.375 0.840 -0.913 -0.593
## 12        12        12 1.500 0.916 -0.995 -0.647
```



##### Add random values for other variables

##### Generate a random number from the beta distribution

``` r
WWR_N_random <- rnorm(12, mean = 0, sd=2)
WWR_E_random <- rnorm(12, mean = 0, sd=2)
RoofOvrhng_N_random <- rnorm(12, mean = 0, sd=2)
RoofOvrhng_E_random <- rnorm(12, mean = 0, sd=2)
```

##### Save into csv

``` r
write.csv(WWR_N_random, "WWR_N_random.csv")
write.csv(WWR_E_random, "WWR_E_random.csv")
write.csv(RoofOvrhng_N_random, "RoofOvrhng_N_random.csv")
write.csv(RoofOvrhng_E_random, "RoofOvrhng_E_random.csv")
```

##### Load random numbers

``` r
WWR_N_random <- read.csv('randomNumbers/dsg2_fo/WWR_N_random.csv')
WWR_E_random <- read.csv('randomNumbers/dsg2_fo/WWR_E_random.csv')
RoofOvrhng_N_random <- read.csv('randomNumbers/dsg2_fo/RoofOvrhng_N_random.csv')
RoofOvrhng_E_random <- read.csv('randomNumbers/dsg2_fo/RoofOvrhng_E_random.csv')
```

##### Append random numbers to Rhino file

``` r
dsg2_fo$WWR_N <- ((WWR_N_random$x * 2) + 15)/100
dsg2_fo$WWR_E <- ((WWR_E_random$x * 2) + 15)/100
dsg2_fo$RoofOvrhng_N <- ((RoofOvrhng_N_random$x * 2) + 50)/100
dsg2_fo$RoofOvrhng_E <- ((RoofOvrhng_E_random$x * 2) + 50)/100
```



##### Data Collection (Rhino) - Simulations

``` r
dsg2_fo <- as.data.frame(dsg2_fo)
write.csv(dsg2_fo, "dsg_fo_2.csv")
dsg2_fo
```

```
##    run.order std.order  dist    x1     x2     x3      WWR_N      WWR_E
## 1          1         1 0.125 0.076 -0.083 -0.054 0.10857976 0.13835972
## 2          2         2 0.250 0.153 -0.166 -0.108 0.08647103 0.22066873
## 3          3         3 0.375 0.229 -0.249 -0.162 0.15542364 0.15354896
## 4          4         4 0.500 0.306 -0.332 -0.216 0.18366017 0.06239945
## 5          5         5 0.625 0.382 -0.415 -0.270 0.11288892 0.10103762
## 6          6         6 0.750 0.458 -0.498 -0.324 0.15531837 0.15885006
## 7          7         7 0.875 0.535 -0.581 -0.378 0.23391473 0.12672328
## 8          8         8 1.000 0.611 -0.664 -0.432 0.17148897 0.14366290
## 9          9         9 1.125 0.688 -0.747 -0.486 0.15346038 0.14262755
## 10        10        10 1.250 0.764 -0.830 -0.540 0.12284271 0.18569210
## 11        11        11 1.375 0.840 -0.913 -0.593 0.12361458 0.18878875
## 12        12        12 1.500 0.916 -0.995 -0.647 0.07020651 0.12307643
##    RoofOvrhng_N RoofOvrhng_E
## 1     0.5358421    0.4471885
## 2     0.5212598    0.4975095
## 3     0.4808588    0.4802932
## 4     0.4996827    0.4655160
## 5     0.5447087    0.4807657
## 6     0.5195915    0.4660929
## 7     0.4809640    0.5476727
## 8     0.4284646    0.5003154
## 9     0.4937831    0.4924279
## 10    0.4447171    0.5274688
## 11    0.5069236    0.5133593
## 12    0.4853186    0.4210843
```



We upload simulation results 

#### Load simulation results and append

``` r
ioh <- read.csv('CCD/FO_data_ioh_2.csv')
sda <- read.csv('CCD/FO_data_udi_2.csv')

ioh <- as.data.frame(ioh)
sda <- as.data.frame(sda)

print(ioh)
```

```
##    in.SA1  out.IOH       img
## 1       1 0.100487  SA11.png
## 2       2 0.100394  SA12.png
## 3       3 0.098588  SA13.png
## 4       4 0.097842  SA14.png
## 5       5 0.097185  SA15.png
## 6       6 0.096163  SA16.png
## 7       7 0.095041  SA17.png
## 8       8 0.094777  SA18.png
## 9       9 0.093070  SA19.png
## 10     10 0.093192 SA110.png
## 11     11 0.091778 SA111.png
## 12     12 0.089795 SA112.png
```

``` r
print(sda)
```

```
##    in.SA1   out.DA  out.cDA  out.UDI out.sDA50 out.sDA80       img
## 1       1 79.77740 84.33241 73.62174  0.964286  0.752970  SA11.png
## 2       2 79.11469 83.83107 70.87649  0.956268  0.748569  SA12.png
## 3       3 81.70712 85.22388 73.33279  0.978134  0.819106  SA13.png
## 4       4 81.63199 85.20157 77.85907  0.986880  0.767082  SA14.png
## 5       5 78.97641 84.04576 77.96548  0.965015  0.679634  SA15.png
## 6       6 81.28688 85.05147 75.09046  0.978863  0.785615  SA16.png
## 7       7 82.85368 85.64396 75.00836  1.000000  0.855557  SA17.png
## 8       8 81.64039 85.16511 76.40577  0.983236  0.779059  SA18.png
## 9       9 80.74533 84.81964 77.49173  0.978134  0.734682  SA19.png
## 10     10 79.63634 84.31465 76.45283  0.970140  0.713148 SA110.png
## 11     11 79.37032 84.19865 76.66741  0.967976  0.693525 SA111.png
## 12     12 74.41217 81.75148 79.23680  0.928535  0.528164 SA112.png
```

##### Append simulation results with fractional factorial design

``` r
dsg2_fo$IOH  <- ioh$out.IOH 
dsg2_fo$sDA  <- sda$out.UDI
dsg2_fo
```

```
##    run.order std.order  dist    x1     x2     x3      WWR_N      WWR_E
## 1          1         1 0.125 0.076 -0.083 -0.054 0.10857976 0.13835972
## 2          2         2 0.250 0.153 -0.166 -0.108 0.08647103 0.22066873
## 3          3         3 0.375 0.229 -0.249 -0.162 0.15542364 0.15354896
## 4          4         4 0.500 0.306 -0.332 -0.216 0.18366017 0.06239945
## 5          5         5 0.625 0.382 -0.415 -0.270 0.11288892 0.10103762
## 6          6         6 0.750 0.458 -0.498 -0.324 0.15531837 0.15885006
## 7          7         7 0.875 0.535 -0.581 -0.378 0.23391473 0.12672328
## 8          8         8 1.000 0.611 -0.664 -0.432 0.17148897 0.14366290
## 9          9         9 1.125 0.688 -0.747 -0.486 0.15346038 0.14262755
## 10        10        10 1.250 0.764 -0.830 -0.540 0.12284271 0.18569210
## 11        11        11 1.375 0.840 -0.913 -0.593 0.12361458 0.18878875
## 12        12        12 1.500 0.916 -0.995 -0.647 0.07020651 0.12307643
##    RoofOvrhng_N RoofOvrhng_E      IOH      sDA
## 1     0.5358421    0.4471885 0.100487 73.62174
## 2     0.5212598    0.4975095 0.100394 70.87649
## 3     0.4808588    0.4802932 0.098588 73.33279
## 4     0.4996827    0.4655160 0.097842 77.85907
## 5     0.5447087    0.4807657 0.097185 77.96548
## 6     0.5195915    0.4660929 0.096163 75.09046
## 7     0.4809640    0.5476727 0.095041 75.00836
## 8     0.4284646    0.5003154 0.094777 76.40577
## 9     0.4937831    0.4924279 0.093070 77.49173
## 10    0.4447171    0.5274688 0.093192 76.45283
## 11    0.5069236    0.5133593 0.091778 76.66741
## 12    0.4853186    0.4210843 0.089795 79.23680
```

##### Create a new dataframe with only the results

``` r
predOutcomes <- data.frame(
  ioh = dsg2_fo$IOH,
  sda = dsg2_fo$sDA
)
predOutcomes
```

```
##         ioh      sda
## 1  0.100487 73.62174
## 2  0.100394 70.87649
## 3  0.098588 73.33279
## 4  0.097842 77.85907
## 5  0.097185 77.96548
## 6  0.096163 75.09046
## 7  0.095041 75.00836
## 8  0.094777 76.40577
## 9  0.093070 77.49173
## 10 0.093192 76.45283
## 11 0.091778 76.66741
## 12 0.089795 79.23680
```

##### Calcular Desirability values

``` r
library(desirability)
```


##### Desirability target and limit

``` r
iohD <- dMin(0, 0.193161, scale = 1)
sdaD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, sdaD)
head(overallD)
```

```
## $d
## $d[[1]]
## Smaller-is-better desirability function
## 
## Call: dMin.default(low = 0, high = 0.193161, scale = 1)
## 
## Non-informative value: 0.5 
## 
## $d[[2]]
## Larger-is-better desirability function
## 
## Call: dMax.default(low = 35.23524, high = 100, scale = 1)
## 
## Non-informative value: 0.5 
## 
## 
## $call
## dOverall.default(iohD, sdaD)
```

##### Calculate the Overall Desirability values

``` r
predict(iohD, predOutcomes[1])
```

```
##  [1] 0.4797759 0.4802574 0.4896071 0.4934692 0.4968705 0.5021614 0.5079700
##  [8] 0.5093368 0.5181740 0.5175424 0.5248627 0.5351287
```

``` r
predict(sdaD, predOutcomes[2])
```

```
##  [1] 0.5927066 0.5503186 0.5882451 0.6581330 0.6597761 0.6153843 0.6141167
##  [8] 0.6356934 0.6524612 0.6364201 0.6397333 0.6794059
```

``` r
predict(overallD, predOutcomes)
```

```
##  [1] 0.5332601 0.5140959 0.5366647 0.5698845 0.5725585 0.5558977 0.5585274
##  [8] 0.5690185 0.5814537 0.5739115 0.5794585 0.6029673
```

``` r
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
overall_desirability
```

```
##           D1        D2   Overall
## 1  0.4797759 0.5927066 0.5332601
## 2  0.4802574 0.5503186 0.5140959
## 3  0.4896071 0.5882451 0.5366647
## 4  0.4934692 0.6581330 0.5698845
## 5  0.4968705 0.6597761 0.5725585
## 6  0.5021614 0.6153843 0.5558977
## 7  0.5079700 0.6141167 0.5585274
## 8  0.5093368 0.6356934 0.5690185
## 9  0.5181740 0.6524612 0.5814537
## 10 0.5175424 0.6364201 0.5739115
## 11 0.5248627 0.6397333 0.5794585
## 12 0.5351287 0.6794059 0.6029673
```

##### Append all results in the factorial design

``` r
dsg2_fo$d1 <- overall_desirability$D1
dsg2_fo$d2 <- overall_desirability$D2
dsg2_fo$Overall <- overall_desirability$Overall
dsg2_fo
```

```
##    run.order std.order  dist    x1     x2     x3      WWR_N      WWR_E
## 1          1         1 0.125 0.076 -0.083 -0.054 0.10857976 0.13835972
## 2          2         2 0.250 0.153 -0.166 -0.108 0.08647103 0.22066873
## 3          3         3 0.375 0.229 -0.249 -0.162 0.15542364 0.15354896
## 4          4         4 0.500 0.306 -0.332 -0.216 0.18366017 0.06239945
## 5          5         5 0.625 0.382 -0.415 -0.270 0.11288892 0.10103762
## 6          6         6 0.750 0.458 -0.498 -0.324 0.15531837 0.15885006
## 7          7         7 0.875 0.535 -0.581 -0.378 0.23391473 0.12672328
## 8          8         8 1.000 0.611 -0.664 -0.432 0.17148897 0.14366290
## 9          9         9 1.125 0.688 -0.747 -0.486 0.15346038 0.14262755
## 10        10        10 1.250 0.764 -0.830 -0.540 0.12284271 0.18569210
## 11        11        11 1.375 0.840 -0.913 -0.593 0.12361458 0.18878875
## 12        12        12 1.500 0.916 -0.995 -0.647 0.07020651 0.12307643
##    RoofOvrhng_N RoofOvrhng_E      IOH      sDA        d1        d2   Overall
## 1     0.5358421    0.4471885 0.100487 73.62174 0.4797759 0.5927066 0.5332601
## 2     0.5212598    0.4975095 0.100394 70.87649 0.4802574 0.5503186 0.5140959
## 3     0.4808588    0.4802932 0.098588 73.33279 0.4896071 0.5882451 0.5366647
## 4     0.4996827    0.4655160 0.097842 77.85907 0.4934692 0.6581330 0.5698845
## 5     0.5447087    0.4807657 0.097185 77.96548 0.4968705 0.6597761 0.5725585
## 6     0.5195915    0.4660929 0.096163 75.09046 0.5021614 0.6153843 0.5558977
## 7     0.4809640    0.5476727 0.095041 75.00836 0.5079700 0.6141167 0.5585274
## 8     0.4284646    0.5003154 0.094777 76.40577 0.5093368 0.6356934 0.5690185
## 9     0.4937831    0.4924279 0.093070 77.49173 0.5181740 0.6524612 0.5814537
## 10    0.4447171    0.5274688 0.093192 76.45283 0.5175424 0.6364201 0.5739115
## 11    0.5069236    0.5133593 0.091778 76.66741 0.5248627 0.6397333 0.5794585
## 12    0.4853186    0.4210843 0.089795 79.23680 0.5351287 0.6794059 0.6029673
```

##### Plot Distance-Overall plot

``` r
library(ggplot2)
```


``` r
# Create a scatterplot
p <- ggplot(dsg2_fo, aes(x = dist, y = Overall)) +
  geom_point() +
  
  # Fit a quadratic regression model
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              se = FALSE, color = "blue") +
  
  # Add labels and a title
  labs(x = "Distance (in coded units)", y = "Overall Desirability (D)", title = "Design of Experiment 2 (DoE 2)") 


ggsave("experiment 2 plot.png", plot= p, height = 5 , width = 5)
```


``` r
p
```

![](RSM_thermal_daylight_optimization_files/figure-html/unnamed-chunk-92-1.png)<!-- -->




### **Experiment 3**


``` r
dsg3_fo <- cube(3, n0=3, randomize=FALSE)
dsg3_fo
```

```
##    run.order std.order x1.as.is x2.as.is x3.as.is
## 1          1         1       -1       -1       -1
## 2          2         2        1       -1       -1
## 3          3         3       -1        1       -1
## 4          4         4        1        1       -1
## 5          5         5       -1       -1        1
## 6          6         6        1       -1        1
## 7          7         7       -1        1        1
## 8          8         8        1        1        1
## 9          9         9        0        0        0
## 10        10        10        0        0        0
## 11        11        11        0        0        0
## 
## Data are stored in coded form using these coding formulas ...
## x1 ~ x1.as.is
## x2 ~ x2.as.is
## x3 ~ x3.as.is
```


``` r
as.data.frame(dsg3_fo)
```

```
##    run.order std.order x1 x2 x3
## 1          1         1 -1 -1 -1
## 2          2         2  1 -1 -1
## 3          3         3 -1  1 -1
## 4          4         4  1  1 -1
## 5          5         5 -1 -1  1
## 6          6         6  1 -1  1
## 7          7         7 -1  1  1
## 8          8         8  1  1  1
## 9          9         9  0  0  0
## 10        10        10  0  0  0
## 11        11        11  0  0  0
```


##### Add random values for other variables

##### Generate a random number from the beta distribution

``` r
WWR_N_random <- rnorm(11, mean = 0, sd=2)
WWR_E_random <- rnorm(11, mean = 0, sd=2)
RoofOvrhng_N_random <- rnorm(11, mean = 0, sd=2)
RoofOvrhng_E_random <- rnorm(11, mean = 0, sd=2)
```

##### Save into csv

``` r
write.csv(WWR_N_random, "WWR_N_random.csv")
write.csv(WWR_E_random, "WWR_E_random.csv")
write.csv(RoofOvrhng_N_random, "RoofOvrhng_N_random.csv")
write.csv(RoofOvrhng_E_random, "RoofOvrhng_E_random.csv")
```

##### Load random numbers

``` r
WWR_N_random <- read.csv('randomNumbers/dsg3_fo/WWR_N_random.csv')
WWR_E_random <- read.csv('randomNumbers/dsg3_fo/WWR_E_random.csv')
RoofOvrhng_N_random <- read.csv('randomNumbers/dsg3_fo/RoofOvrhng_N_random.csv')
RoofOvrhng_E_random <- read.csv('randomNumbers/dsg3_fo/RoofOvrhng_E_random.csv')
```

##### Append random numbers to Rhino file

``` r
dsg3_fo$WWR_N <- ((WWR_N_random$x * 2) + 15)/100
dsg3_fo$WWR_E <- ((WWR_E_random$x * 2) + 15)/100
dsg3_fo$RoofOvrhng_N <- ((RoofOvrhng_N_random$x * 2) + 50)/100
dsg3_fo$RoofOvrhng_E <- ((RoofOvrhng_E_random$x * 2) + 50)/100
```


##### Data Collection (Rhino) - Simulations

``` r
dsg3_fo <- as.data.frame(dsg3_fo)
write.csv(dsg3_fo, "dsg_fo_3.csv")
dsg3_fo
```

```
##    run.order std.order x1 x2 x3     WWR_N     WWR_E RoofOvrhng_N RoofOvrhng_E
## 1          1         1 -1 -1 -1 0.1893065 0.1855726    0.4177128    0.5337593
## 2          2         2  1 -1 -1 0.1737174 0.1466804    0.5469480    0.5379537
## 3          3         3 -1  1 -1 0.1838173 0.1322550    0.4514541    0.4239182
## 4          4         4  1  1 -1 0.1659429 0.2166245    0.5265096    0.4890155
## 5          5         5 -1 -1  1 0.1288507 0.2097377    0.4959998    0.4576768
## 6          6         6  1 -1  1 0.1549597 0.1408065    0.4327998    0.4869709
## 7          7         7 -1  1  1 0.2361433 0.1328428    0.5167358    0.4251059
## 8          8         8  1  1  1 0.1022055 0.1187210    0.5532268    0.5195924
## 9          9         9  0  0  0 0.1316436 0.1369325    0.5506240    0.4870025
## 10        10        10  0  0  0 0.1621468 0.1641334    0.5154475    0.4746483
## 11        11        11  0  0  0 0.1422357 0.1043477    0.5048208    0.4950779
```

We upload simulation results





#### Load simulation results and append

``` r
ioh <- read.csv('CCD/FO_data_ioh_3.csv')
sda <- read.csv('CCD/FO_data_udi_3.csv')

ioh <- as.data.frame(ioh)
sda <- as.data.frame(sda)

print(ioh)
```

```
##    in.FO  out.IOH      img
## 1      1 0.092187  FO1.png
## 2      2 0.086442  FO2.png
## 3      3 0.094551  FO3.png
## 4      4 0.087198  FO4.png
## 5      5 0.097980  FO5.png
## 6      6 0.088557  FO6.png
## 7      7 0.099709  FO7.png
## 8      8 0.089949  FO8.png
## 9      9 0.089601  FO9.png
## 10    10 0.090457 FO10.png
## 11    11 0.089695 FO11.png
```

``` r
print(sda)
```

```
##    in.FO   out.DA  out.cDA  out.UDI out.sDA50 out.sDA80      img
## 1      1 80.97801 84.81970 77.23921  0.989067  0.662610  FO1.png
## 2      2 78.98224 83.96332 79.98071  0.983236  0.525078  FO2.png
## 3      3 81.09247 84.89364 79.04618  0.986880  0.686944  FO3.png
## 4      4 80.37323 84.56586 76.95032  0.982507  0.646240  FO4.png
## 5      5 79.96027 84.45959 71.27977  0.968751  0.737810  FO5.png
## 6      6 80.26508 84.59961 76.50810  0.976723  0.690641  FO6.png
## 7      7 83.39223 85.85478 70.87782  1.000000  0.890818  FO7.png
## 8      8 77.81506 83.46222 77.67497  0.959253  0.625785  FO8.png
## 9      9 79.05794 84.09598 79.12041  0.970163  0.647645  FO9.png
## 10    10 80.70919 84.77352 77.33072  0.981050  0.715375 FO10.png
## 11    11 79.32568 84.23173 80.41685  0.973784  0.629684 FO11.png
```

##### Append simulation results with fractional factorial design

``` r
dsg3_fo$IOH  <- ioh$out.IOH 
dsg3_fo$sDA  <- sda$out.UDI
dsg3_fo
```

```
##    run.order std.order x1 x2 x3     WWR_N     WWR_E RoofOvrhng_N RoofOvrhng_E
## 1          1         1 -1 -1 -1 0.1893065 0.1855726    0.4177128    0.5337593
## 2          2         2  1 -1 -1 0.1737174 0.1466804    0.5469480    0.5379537
## 3          3         3 -1  1 -1 0.1838173 0.1322550    0.4514541    0.4239182
## 4          4         4  1  1 -1 0.1659429 0.2166245    0.5265096    0.4890155
## 5          5         5 -1 -1  1 0.1288507 0.2097377    0.4959998    0.4576768
## 6          6         6  1 -1  1 0.1549597 0.1408065    0.4327998    0.4869709
## 7          7         7 -1  1  1 0.2361433 0.1328428    0.5167358    0.4251059
## 8          8         8  1  1  1 0.1022055 0.1187210    0.5532268    0.5195924
## 9          9         9  0  0  0 0.1316436 0.1369325    0.5506240    0.4870025
## 10        10        10  0  0  0 0.1621468 0.1641334    0.5154475    0.4746483
## 11        11        11  0  0  0 0.1422357 0.1043477    0.5048208    0.4950779
##         IOH      sDA
## 1  0.092187 77.23921
## 2  0.086442 79.98071
## 3  0.094551 79.04618
## 4  0.087198 76.95032
## 5  0.097980 71.27977
## 6  0.088557 76.50810
## 7  0.099709 70.87782
## 8  0.089949 77.67497
## 9  0.089601 79.12041
## 10 0.090457 77.33072
## 11 0.089695 80.41685
```

##### Create a new dataframe with only the results

``` r
predOutcomes <- data.frame(
  ioh = dsg3_fo$IOH,
  sda = dsg3_fo$sDA
)
predOutcomes
```

```
##         ioh      sda
## 1  0.092187 77.23921
## 2  0.086442 79.98071
## 3  0.094551 79.04618
## 4  0.087198 76.95032
## 5  0.097980 71.27977
## 6  0.088557 76.50810
## 7  0.099709 70.87782
## 8  0.089949 77.67497
## 9  0.089601 79.12041
## 10 0.090457 77.33072
## 11 0.089695 80.41685
```

##### Calcular Desirability values

``` r
library(desirability)
```


##### Desirability target and limit

``` r
iohD <- dMin(0, 0.193161, scale = 1)
sdaD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, sdaD)
head(overallD)
```

```
## $d
## $d[[1]]
## Smaller-is-better desirability function
## 
## Call: dMin.default(low = 0, high = 0.193161, scale = 1)
## 
## Non-informative value: 0.5 
## 
## $d[[2]]
## Larger-is-better desirability function
## 
## Call: dMax.default(low = 35.23524, high = 100, scale = 1)
## 
## Non-informative value: 0.5 
## 
## 
## $call
## dOverall.default(iohD, sdaD)
```

##### Calculate the Overall Desirability values

``` r
predict(iohD, predOutcomes[1])
```

```
##  [1] 0.5227453 0.5524873 0.5105068 0.5485735 0.4927547 0.5415379 0.4838037
##  [8] 0.5343315 0.5361331 0.5317015 0.5356464
```

``` r
predict(sdaD, predOutcomes[2])
```

```
##  [1] 0.6485621 0.6908922 0.6764626 0.6441016 0.5565454 0.6372734 0.5503391
##  [8] 0.6552904 0.6776087 0.6499751 0.6976264
```

``` r
predict(overallD, predOutcomes)
```

```
##  [1] 0.5822652 0.6178262 0.5876553 0.5944216 0.5236797 0.5874587 0.5160001
##  [8] 0.5917282 0.6027341 0.5878714 0.6112946
```

``` r
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
overall_desirability
```

```
##           D1        D2   Overall
## 1  0.5227453 0.6485621 0.5822652
## 2  0.5524873 0.6908922 0.6178262
## 3  0.5105068 0.6764626 0.5876553
## 4  0.5485735 0.6441016 0.5944216
## 5  0.4927547 0.5565454 0.5236797
## 6  0.5415379 0.6372734 0.5874587
## 7  0.4838037 0.5503391 0.5160001
## 8  0.5343315 0.6552904 0.5917282
## 9  0.5361331 0.6776087 0.6027341
## 10 0.5317015 0.6499751 0.5878714
## 11 0.5356464 0.6976264 0.6112946
```

##### Append all results in the factorial design

``` r
dsg3_fo$d1 <- overall_desirability$D1
dsg3_fo$d2 <- overall_desirability$D2
dsg3_fo$Overall <- overall_desirability$Overall
dsg3_fo
```

```
##    run.order std.order x1 x2 x3     WWR_N     WWR_E RoofOvrhng_N RoofOvrhng_E
## 1          1         1 -1 -1 -1 0.1893065 0.1855726    0.4177128    0.5337593
## 2          2         2  1 -1 -1 0.1737174 0.1466804    0.5469480    0.5379537
## 3          3         3 -1  1 -1 0.1838173 0.1322550    0.4514541    0.4239182
## 4          4         4  1  1 -1 0.1659429 0.2166245    0.5265096    0.4890155
## 5          5         5 -1 -1  1 0.1288507 0.2097377    0.4959998    0.4576768
## 6          6         6  1 -1  1 0.1549597 0.1408065    0.4327998    0.4869709
## 7          7         7 -1  1  1 0.2361433 0.1328428    0.5167358    0.4251059
## 8          8         8  1  1  1 0.1022055 0.1187210    0.5532268    0.5195924
## 9          9         9  0  0  0 0.1316436 0.1369325    0.5506240    0.4870025
## 10        10        10  0  0  0 0.1621468 0.1641334    0.5154475    0.4746483
## 11        11        11  0  0  0 0.1422357 0.1043477    0.5048208    0.4950779
##         IOH      sDA        d1        d2   Overall
## 1  0.092187 77.23921 0.5227453 0.6485621 0.5822652
## 2  0.086442 79.98071 0.5524873 0.6908922 0.6178262
## 3  0.094551 79.04618 0.5105068 0.6764626 0.5876553
## 4  0.087198 76.95032 0.5485735 0.6441016 0.5944216
## 5  0.097980 71.27977 0.4927547 0.5565454 0.5236797
## 6  0.088557 76.50810 0.5415379 0.6372734 0.5874587
## 7  0.099709 70.87782 0.4838037 0.5503391 0.5160001
## 8  0.089949 77.67497 0.5343315 0.6552904 0.5917282
## 9  0.089601 79.12041 0.5361331 0.6776087 0.6027341
## 10 0.090457 77.33072 0.5317015 0.6499751 0.5878714
## 11 0.089695 80.41685 0.5356464 0.6976264 0.6112946
```

##### Subset unnecesary columns and code data

``` r
dsg3_fo = subset(dsg3_fo, select = -c(run.order, std.order,RoofOvrhng_N, RoofOvrhng_E, WWR_N, WWR_E))
dsg3_fo <- coded.data(
  dsg3_fo, formulas = list(x1 ~ (RoofOvrhng_S_W - 2.9580)/.5,
                  x2 ~ (WWR_W - 0.0505)/.025,
                  x3 ~ (WWR_S - 0.3353)/.1)
  )

dsg3_fo
```

```
##    RoofOvrhng_S_W  WWR_W  WWR_S      IOH      sDA        d1        d2   Overall
## 1           2.458 0.0255 0.2353 0.092187 77.23921 0.5227453 0.6485621 0.5822652
## 2           3.458 0.0255 0.2353 0.086442 79.98071 0.5524873 0.6908922 0.6178262
## 3           2.458 0.0755 0.2353 0.094551 79.04618 0.5105068 0.6764626 0.5876553
## 4           3.458 0.0755 0.2353 0.087198 76.95032 0.5485735 0.6441016 0.5944216
## 5           2.458 0.0255 0.4353 0.097980 71.27977 0.4927547 0.5565454 0.5236797
## 6           3.458 0.0255 0.4353 0.088557 76.50810 0.5415379 0.6372734 0.5874587
## 7           2.458 0.0755 0.4353 0.099709 70.87782 0.4838037 0.5503391 0.5160001
## 8           3.458 0.0755 0.4353 0.089949 77.67497 0.5343315 0.6552904 0.5917282
## 9           2.958 0.0505 0.3353 0.089601 79.12041 0.5361331 0.6776087 0.6027341
## 10          2.958 0.0505 0.3353 0.090457 77.33072 0.5317015 0.6499751 0.5878714
## 11          2.958 0.0505 0.3353 0.089695 80.41685 0.5356464 0.6976264 0.6112946
## 
## Data are stored in coded form using these coding formulas ...
## x1 ~ (RoofOvrhng_S_W - 2.958)/0.5
## x2 ~ (WWR_W - 0.0505)/0.025
## x3 ~ (WWR_S - 0.3353)/0.1
```

We create the response surface model and identify the most significant terms.

##### Model adjustment and summary

``` r
anal3_fo <- rsm(Overall ~ FO(x1, x2, x3), data = dsg3_fo)
summary(anal3_fo)
```

```
## 
## Call:
## rsm(formula = Overall ~ FO(x1, x2, x3), data = dsg3_fo)
## 
##               Estimate Std. Error t value  Pr(>|t|)    
## (Intercept)  0.5820850  0.0063963 91.0037 5.096e-12 ***
## x1           0.0227293  0.0075003  3.0305    0.0191 *  
## x2          -0.0026781  0.0075003 -0.3571    0.7316    
## x3          -0.0204127  0.0075003 -2.7216    0.0297 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Multiple R-squared:  0.7049,	Adjusted R-squared:  0.5784 
## F-statistic: 5.573 on 3 and 7 DF,  p-value: 0.02852
## 
## Analysis of Variance Table
## 
## Response: Overall
##                Df    Sum Sq    Mean Sq F value  Pr(>F)
## FO(x1, x2, x3)  3 0.0075238 0.00250793  5.5727 0.02852
## Residuals       7 0.0031503 0.00045004                
## Lack of fit     5 0.0028693 0.00057386  4.0852 0.20827
## Pure error      2 0.0002809 0.00014047                
## 
## Direction of steepest ascent (at radius 1):
##          x1          x2          x3 
##  0.74116229 -0.08732698 -0.66562186 
## 
## Corresponding increment in original units:
## RoofOvrhng_S_W          WWR_W          WWR_S 
##    0.370581143   -0.002183175   -0.066562186
```




### **Experiment 4**

#### Implement steepest ascent on experiment 3



``` r
steepest(anal3_fo, descent = FALSE)
```

```
## Path of steepest ascent from ridge analysis:
```

```
##    dist    x1     x2     x3 | RoofOvrhng_S_W    WWR_W  WWR_S |  yhat
## 1   0.0 0.000  0.000  0.000 |         2.9580 0.050500 0.3353 | 0.582
## 2   0.5 0.371 -0.044 -0.333 |         3.1435 0.049400 0.3020 | 0.597
## 3   1.0 0.741 -0.087 -0.666 |         3.3285 0.048325 0.2687 | 0.613
## 4   1.5 1.110 -0.131 -0.997 |         3.5130 0.047225 0.2356 | 0.628
## 5   2.0 1.477 -0.174 -1.327 |         3.6965 0.046150 0.2026 | 0.643
## 6   2.5 1.853 -0.218 -1.664 |         3.8845 0.045050 0.1689 | 0.659
## 7   3.0 2.221 -0.262 -1.995 |         4.0685 0.043950 0.1358 | 0.674
## 8   3.5 2.579 -0.304 -2.316 |         4.2475 0.042900 0.1037 | 0.689
## 9   4.0 2.968 -0.350 -2.666 |         4.4420 0.041750 0.0687 | 0.705
## 10  4.5 3.348 -0.394 -3.007 |         4.6320 0.040650 0.0346 | 0.721
## 11  5.0 3.678 -0.433 -3.303 |         4.7970 0.039675 0.0050 | 0.734
```


``` r
steepest(anal3_fo, dist = (1:12)/8, descent = FALSE)
```

```
## Path of steepest ascent from ridge analysis:
```

```
##     dist    x1     x2     x3 | RoofOvrhng_S_W    WWR_W  WWR_S |  yhat
## 1  0.125 0.093 -0.011 -0.083 |         3.0045 0.050225 0.3270 | 0.586
## 2  0.250 0.185 -0.022 -0.166 |         3.0505 0.049950 0.3187 | 0.590
## 3  0.375 0.278 -0.033 -0.250 |         3.0970 0.049675 0.3103 | 0.594
## 4  0.500 0.371 -0.044 -0.333 |         3.1435 0.049400 0.3020 | 0.597
## 5  0.625 0.463 -0.055 -0.416 |         3.1895 0.049125 0.2937 | 0.601
## 6  0.750 0.557 -0.066 -0.500 |         3.2365 0.048850 0.2853 | 0.605
## 7  0.875 0.649 -0.076 -0.583 |         3.2825 0.048600 0.2770 | 0.609
## 8  1.000 0.741 -0.087 -0.666 |         3.3285 0.048325 0.2687 | 0.613
## 9  1.125 0.833 -0.098 -0.748 |         3.3745 0.048050 0.2605 | 0.617
## 10 1.250 0.927 -0.109 -0.832 |         3.4215 0.047775 0.2521 | 0.620
## 11 1.375 1.019 -0.120 -0.915 |         3.4675 0.047500 0.2438 | 0.624
## 12 1.500 1.110 -0.131 -0.997 |         3.5130 0.047225 0.2356 | 0.628
```


``` r
dsg4_fo = dupe(steepest(anal3_fo, dist = (1:12)/8, descent = FALSE),randomize=FALSE)
```

```
## Path of steepest ascent from ridge analysis:
```


``` r
dsg4_fo
```

```
##    run.order std.order  dist    x1     x2     x3 | RoofOvrhng_S_W    WWR_W
## 1          1         1 0.125 0.093 -0.011 -0.083 |         3.0045 0.050225
## 2          2         2 0.250 0.185 -0.022 -0.166 |         3.0505 0.049950
## 3          3         3 0.375 0.278 -0.033 -0.250 |         3.0970 0.049675
## 4          4         4 0.500 0.371 -0.044 -0.333 |         3.1435 0.049400
## 5          5         5 0.625 0.463 -0.055 -0.416 |         3.1895 0.049125
## 6          6         6 0.750 0.557 -0.066 -0.500 |         3.2365 0.048850
## 7          7         7 0.875 0.649 -0.076 -0.583 |         3.2825 0.048600
## 8          8         8 1.000 0.741 -0.087 -0.666 |         3.3285 0.048325
## 9          9         9 1.125 0.833 -0.098 -0.748 |         3.3745 0.048050
## 10        10        10 1.250 0.927 -0.109 -0.832 |         3.4215 0.047775
## 11        11        11 1.375 1.019 -0.120 -0.915 |         3.4675 0.047500
## 12        12        12 1.500 1.110 -0.131 -0.997 |         3.5130 0.047225
##     WWR_S |.1  yhat
## 1  0.3270   | 0.586
## 2  0.3187   | 0.590
## 3  0.3103   | 0.594
## 4  0.3020   | 0.597
## 5  0.2937   | 0.601
## 6  0.2853   | 0.605
## 7  0.2770   | 0.609
## 8  0.2687   | 0.613
## 9  0.2605   | 0.617
## 10 0.2521   | 0.620
## 11 0.2438   | 0.624
## 12 0.2356   | 0.628
```


``` r
dsg4_fo <- data.frame(
  run.order = dsg4_fo$run.order,
  std.order = dsg4_fo$std.order,
  dist = dsg4_fo$dist,
  x1 = dsg4_fo$x1,
  x2 = dsg4_fo$x2,
  x3 = dsg4_fo$x3
)

dsg4_fo
```

```
##    run.order std.order  dist    x1     x2     x3
## 1          1         1 0.125 0.093 -0.011 -0.083
## 2          2         2 0.250 0.185 -0.022 -0.166
## 3          3         3 0.375 0.278 -0.033 -0.250
## 4          4         4 0.500 0.371 -0.044 -0.333
## 5          5         5 0.625 0.463 -0.055 -0.416
## 6          6         6 0.750 0.557 -0.066 -0.500
## 7          7         7 0.875 0.649 -0.076 -0.583
## 8          8         8 1.000 0.741 -0.087 -0.666
## 9          9         9 1.125 0.833 -0.098 -0.748
## 10        10        10 1.250 0.927 -0.109 -0.832
## 11        11        11 1.375 1.019 -0.120 -0.915
## 12        12        12 1.500 1.110 -0.131 -0.997
```




##### Add random values for other variables

##### Generate a random number from the beta distribution

``` r
WWR_N_random <- rnorm(12, mean = 0, sd=2)
WWR_E_random <- rnorm(12, mean = 0, sd=2)
RoofOvrhng_N_random <- rnorm(12, mean = 0, sd=2)
RoofOvrhng_E_random <- rnorm(12, mean = 0, sd=2)
```

##### Save into csv

``` r
write.csv(WWR_N_random, "WWR_N_random.csv")
write.csv(WWR_E_random, "WWR_E_random.csv")
write.csv(RoofOvrhng_N_random, "RoofOvrhng_N_random.csv")
write.csv(RoofOvrhng_E_random, "RoofOvrhng_E_random.csv")
```

##### Load random numbers

``` r
WWR_N_random <- read.csv('randomNumbers/dsg4_fo/WWR_N_random.csv')
WWR_E_random <- read.csv('randomNumbers/dsg4_fo/WWR_E_random.csv')
RoofOvrhng_N_random <- read.csv('randomNumbers/dsg4_fo/RoofOvrhng_N_random.csv')
RoofOvrhng_E_random <- read.csv('randomNumbers/dsg4_fo/RoofOvrhng_E_random.csv')
```

##### Append random numbers to Rhino file

``` r
dsg4_fo$WWR_N <- ((WWR_N_random$x * 2) + 15)/100
dsg4_fo$WWR_E <- ((WWR_E_random$x * 2) + 15)/100
dsg4_fo$RoofOvrhng_N <- ((RoofOvrhng_N_random$x * 2) + 50)/100
dsg4_fo$RoofOvrhng_E <- ((RoofOvrhng_E_random$x * 2) + 50)/100
```


##### Data Collection (Rhino) - Simulations

``` r
dsg4_fo <- as.data.frame(dsg4_fo)
write.csv(dsg4_fo, "dsg_fo_4.csv")
dsg4_fo
```

```
##    run.order std.order  dist    x1     x2     x3      WWR_N      WWR_E
## 1          1         1 0.125 0.093 -0.011 -0.083 0.14006301 0.20518334
## 2          2         2 0.250 0.185 -0.022 -0.166 0.10403294 0.18340645
## 3          3         3 0.375 0.278 -0.033 -0.250 0.17898823 0.12869522
## 4          4         4 0.500 0.371 -0.044 -0.333 0.09165283 0.15494876
## 5          5         5 0.625 0.463 -0.055 -0.416 0.20946871 0.15944341
## 6          6         6 0.750 0.557 -0.066 -0.500 0.18457654 0.17436250
## 7          7         7 0.875 0.649 -0.076 -0.583 0.12231689 0.22241284
## 8          8         8 1.000 0.741 -0.087 -0.666 0.04259101 0.13254980
## 9          9         9 1.125 0.833 -0.098 -0.748 0.15348474 0.07784832
## 10        10        10 1.250 0.927 -0.109 -0.832 0.13970472 0.18840897
## 11        11        11 1.375 1.019 -0.120 -0.915 0.11541727 0.10714510
## 12        12        12 1.500 1.110 -0.131 -0.997 0.13875848 0.15876657
##    RoofOvrhng_N RoofOvrhng_E
## 1     0.5035128    0.5378773
## 2     0.5286753    0.5244125
## 3     0.4404444    0.5020310
## 4     0.5422277    0.5231496
## 5     0.4688710    0.5225594
## 6     0.4361960    0.5042661
## 7     0.4296577    0.5134591
## 8     0.4830271    0.5106423
## 9     0.5003041    0.4521977
## 10    0.4268295    0.5878176
## 11    0.4945933    0.4665472
## 12    0.4744100    0.4603371
```


We upload simulation results 

#### Load simulation results and append

``` r
ioh <- read.csv('CCD/FO_data_ioh_4.csv')
sda <- read.csv('CCD/FO_data_udi_4.csv')

ioh <- as.data.frame(ioh)
sda <- as.data.frame(sda)

print(ioh)
```

```
##    in.SA1  out.IOH       img
## 1       1 0.090115  SA11.png
## 2       2 0.088773  SA12.png
## 3       3 0.089518  SA13.png
## 4       4 0.088596  SA14.png
## 5       5 0.088231  SA15.png
## 6       6 0.087646  SA16.png
## 7       7 0.087254  SA17.png
## 8       8 0.086928  SA18.png
## 9       9 0.087557  SA19.png
## 10     10 0.086271 SA110.png
## 11     11 0.087049 SA111.png
## 12     12 0.086613 SA112.png
```

``` r
print(sda)
```

```
##    in.SA1   out.DA  out.cDA  out.UDI out.sDA50 out.sDA80       img
## 1       1 79.97549 84.46961 76.33044  0.971621  0.708687  SA11.png
## 2       2 77.75365 83.41536 77.82834  0.957841  0.621331  SA12.png
## 3       3 80.79322 84.77565 79.24173  0.986880  0.673373  SA13.png
## 4       4 76.33954 82.72100 79.35930  0.949962  0.543584  SA14.png
## 5       5 81.51752 85.04201 77.43691  0.995627  0.708324  SA15.png
## 6       6 80.99802 84.82791 77.51757  0.987609  0.677840  SA16.png
## 7       7 78.80474 83.92257 77.01310  0.966542  0.624258  SA17.png
## 8       8 68.33253 78.71428 78.42003  0.851733  0.387308  SA18.png
## 9       9 78.15863 83.68898 82.68728  0.978134  0.453972  SA19.png
## 10     10 79.01380 84.01829 78.66263  0.973078  0.584431 SA110.png
## 11     11 76.38586 82.87645 82.07670  0.964355  0.414785 SA111.png
## 12     12 78.32879 83.71931 79.86506  0.971621  0.521687 SA112.png
```

##### Append simulation results with fractional factorial design

``` r
dsg4_fo$IOH  <- ioh$out.IOH 
dsg4_fo$sDA  <- sda$out.UDI
dsg4_fo
```

```
##    run.order std.order  dist    x1     x2     x3      WWR_N      WWR_E
## 1          1         1 0.125 0.093 -0.011 -0.083 0.14006301 0.20518334
## 2          2         2 0.250 0.185 -0.022 -0.166 0.10403294 0.18340645
## 3          3         3 0.375 0.278 -0.033 -0.250 0.17898823 0.12869522
## 4          4         4 0.500 0.371 -0.044 -0.333 0.09165283 0.15494876
## 5          5         5 0.625 0.463 -0.055 -0.416 0.20946871 0.15944341
## 6          6         6 0.750 0.557 -0.066 -0.500 0.18457654 0.17436250
## 7          7         7 0.875 0.649 -0.076 -0.583 0.12231689 0.22241284
## 8          8         8 1.000 0.741 -0.087 -0.666 0.04259101 0.13254980
## 9          9         9 1.125 0.833 -0.098 -0.748 0.15348474 0.07784832
## 10        10        10 1.250 0.927 -0.109 -0.832 0.13970472 0.18840897
## 11        11        11 1.375 1.019 -0.120 -0.915 0.11541727 0.10714510
## 12        12        12 1.500 1.110 -0.131 -0.997 0.13875848 0.15876657
##    RoofOvrhng_N RoofOvrhng_E      IOH      sDA
## 1     0.5035128    0.5378773 0.090115 76.33044
## 2     0.5286753    0.5244125 0.088773 77.82834
## 3     0.4404444    0.5020310 0.089518 79.24173
## 4     0.5422277    0.5231496 0.088596 79.35930
## 5     0.4688710    0.5225594 0.088231 77.43691
## 6     0.4361960    0.5042661 0.087646 77.51757
## 7     0.4296577    0.5134591 0.087254 77.01310
## 8     0.4830271    0.5106423 0.086928 78.42003
## 9     0.5003041    0.4521977 0.087557 82.68728
## 10    0.4268295    0.5878176 0.086271 78.66263
## 11    0.4945933    0.4665472 0.087049 82.07670
## 12    0.4744100    0.4603371 0.086613 79.86506
```

##### Create a new dataframe with only the results

``` r
predOutcomes <- data.frame(
  ioh = dsg4_fo$IOH,
  sda = dsg4_fo$sDA
)
predOutcomes
```

```
##         ioh      sda
## 1  0.090115 76.33044
## 2  0.088773 77.82834
## 3  0.089518 79.24173
## 4  0.088596 79.35930
## 5  0.088231 77.43691
## 6  0.087646 77.51757
## 7  0.087254 77.01310
## 8  0.086928 78.42003
## 9  0.087557 82.68728
## 10 0.086271 78.66263
## 11 0.087049 82.07670
## 12 0.086613 79.86506
```

##### Calcular Desirability values

``` r
library(desirability)
```


##### Desirability target and limit

``` r
iohD <- dMin(0, 0.193161, scale = 1)
sdaD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, sdaD)
head(overallD)
```

```
## $d
## $d[[1]]
## Smaller-is-better desirability function
## 
## Call: dMin.default(low = 0, high = 0.193161, scale = 1)
## 
## Non-informative value: 0.5 
## 
## $d[[2]]
## Larger-is-better desirability function
## 
## Call: dMax.default(low = 35.23524, high = 100, scale = 1)
## 
## Non-informative value: 0.5 
## 
## 
## $call
## dOverall.default(iohD, sdaD)
```

##### Calculate the Overall Desirability values

``` r
predict(iohD, predOutcomes[1])
```

```
##  [1] 0.5334721 0.5404196 0.5365628 0.5413360 0.5432256 0.5462542 0.5482836
##  [8] 0.5499713 0.5467149 0.5533726 0.5493448 0.5516020
```

``` r
predict(sdaD, predOutcomes[2])
```

```
##  [1] 0.6345303 0.6576586 0.6794821 0.6812973 0.6516147 0.6528601 0.6450708
##  [8] 0.6667946 0.7326831 0.6705405 0.7232553 0.6891065
```

``` r
predict(overallD, predOutcomes)
```

```
##  [1] 0.5818111 0.5961641 0.6038086 0.6072979 0.5949570 0.5971830 0.5947114
##  [8] 0.6055724 0.6329050 0.6091459 0.6303305 0.6165327
```

``` r
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
overall_desirability
```

```
##           D1        D2   Overall
## 1  0.5334721 0.6345303 0.5818111
## 2  0.5404196 0.6576586 0.5961641
## 3  0.5365628 0.6794821 0.6038086
## 4  0.5413360 0.6812973 0.6072979
## 5  0.5432256 0.6516147 0.5949570
## 6  0.5462542 0.6528601 0.5971830
## 7  0.5482836 0.6450708 0.5947114
## 8  0.5499713 0.6667946 0.6055724
## 9  0.5467149 0.7326831 0.6329050
## 10 0.5533726 0.6705405 0.6091459
## 11 0.5493448 0.7232553 0.6303305
## 12 0.5516020 0.6891065 0.6165327
```

##### Append all results in the factorial design

``` r
dsg4_fo$d1 <- overall_desirability$D1
dsg4_fo$d2 <- overall_desirability$D2
dsg4_fo$Overall <- overall_desirability$Overall
dsg4_fo
```

```
##    run.order std.order  dist    x1     x2     x3      WWR_N      WWR_E
## 1          1         1 0.125 0.093 -0.011 -0.083 0.14006301 0.20518334
## 2          2         2 0.250 0.185 -0.022 -0.166 0.10403294 0.18340645
## 3          3         3 0.375 0.278 -0.033 -0.250 0.17898823 0.12869522
## 4          4         4 0.500 0.371 -0.044 -0.333 0.09165283 0.15494876
## 5          5         5 0.625 0.463 -0.055 -0.416 0.20946871 0.15944341
## 6          6         6 0.750 0.557 -0.066 -0.500 0.18457654 0.17436250
## 7          7         7 0.875 0.649 -0.076 -0.583 0.12231689 0.22241284
## 8          8         8 1.000 0.741 -0.087 -0.666 0.04259101 0.13254980
## 9          9         9 1.125 0.833 -0.098 -0.748 0.15348474 0.07784832
## 10        10        10 1.250 0.927 -0.109 -0.832 0.13970472 0.18840897
## 11        11        11 1.375 1.019 -0.120 -0.915 0.11541727 0.10714510
## 12        12        12 1.500 1.110 -0.131 -0.997 0.13875848 0.15876657
##    RoofOvrhng_N RoofOvrhng_E      IOH      sDA        d1        d2   Overall
## 1     0.5035128    0.5378773 0.090115 76.33044 0.5334721 0.6345303 0.5818111
## 2     0.5286753    0.5244125 0.088773 77.82834 0.5404196 0.6576586 0.5961641
## 3     0.4404444    0.5020310 0.089518 79.24173 0.5365628 0.6794821 0.6038086
## 4     0.5422277    0.5231496 0.088596 79.35930 0.5413360 0.6812973 0.6072979
## 5     0.4688710    0.5225594 0.088231 77.43691 0.5432256 0.6516147 0.5949570
## 6     0.4361960    0.5042661 0.087646 77.51757 0.5462542 0.6528601 0.5971830
## 7     0.4296577    0.5134591 0.087254 77.01310 0.5482836 0.6450708 0.5947114
## 8     0.4830271    0.5106423 0.086928 78.42003 0.5499713 0.6667946 0.6055724
## 9     0.5003041    0.4521977 0.087557 82.68728 0.5467149 0.7326831 0.6329050
## 10    0.4268295    0.5878176 0.086271 78.66263 0.5533726 0.6705405 0.6091459
## 11    0.4945933    0.4665472 0.087049 82.07670 0.5493448 0.7232553 0.6303305
## 12    0.4744100    0.4603371 0.086613 79.86506 0.5516020 0.6891065 0.6165327
```

##### Plot Distance-Overall plot

``` r
library(ggplot2)
```


``` r
# Create a scatterplot
p <- ggplot(dsg4_fo, aes(x = dist, y = Overall)) +
  geom_point() +
  
  # Fit a quadratic regression model
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              se = FALSE, color = "blue") +
  
  # Add labels and a title
  labs(x = "Distance (in coded units)", y = "Overall Desirability (D)", title = "Design of Experiment 4 (DoE 4)") 


ggsave("experiment 4 plot.png", plot= p, height = 5 , width = 5)
```


``` r
p
```

![](RSM_thermal_daylight_optimization_files/figure-html/unnamed-chunk-128-1.png)<!-- -->








### **Experiment 5**


``` r
dsg5_fo <- cube(3, n0=3, randomize=FALSE)
dsg5_fo
```

```
##    run.order std.order x1.as.is x2.as.is x3.as.is
## 1          1         1       -1       -1       -1
## 2          2         2        1       -1       -1
## 3          3         3       -1        1       -1
## 4          4         4        1        1       -1
## 5          5         5       -1       -1        1
## 6          6         6        1       -1        1
## 7          7         7       -1        1        1
## 8          8         8        1        1        1
## 9          9         9        0        0        0
## 10        10        10        0        0        0
## 11        11        11        0        0        0
## 
## Data are stored in coded form using these coding formulas ...
## x1 ~ x1.as.is
## x2 ~ x2.as.is
## x3 ~ x3.as.is
```


``` r
as.data.frame(dsg5_fo)
```

```
##    run.order std.order x1 x2 x3
## 1          1         1 -1 -1 -1
## 2          2         2  1 -1 -1
## 3          3         3 -1  1 -1
## 4          4         4  1  1 -1
## 5          5         5 -1 -1  1
## 6          6         6  1 -1  1
## 7          7         7 -1  1  1
## 8          8         8  1  1  1
## 9          9         9  0  0  0
## 10        10        10  0  0  0
## 11        11        11  0  0  0
```


##### Add random values for other variables

##### Generate a random number from the beta distribution

``` r
WWR_N_random <- rnorm(11, mean = 0, sd=2)
WWR_E_random <- rnorm(11, mean = 0, sd=2)
RoofOvrhng_N_random <- rnorm(11, mean = 0, sd=2)
RoofOvrhng_E_random <- rnorm(11, mean = 0, sd=2)
```

##### Save into csv

``` r
write.csv(WWR_N_random, "WWR_N_random.csv")
write.csv(WWR_E_random, "WWR_E_random.csv")
write.csv(RoofOvrhng_N_random, "RoofOvrhng_N_random.csv")
write.csv(RoofOvrhng_E_random, "RoofOvrhng_E_random.csv")
```

##### Load random numbers

``` r
WWR_N_random <- read.csv('randomNumbers/dsg5_fo/WWR_N_random.csv')
WWR_E_random <- read.csv('randomNumbers/dsg5_fo/WWR_E_random.csv')
RoofOvrhng_N_random <- read.csv('randomNumbers/dsg5_fo/RoofOvrhng_N_random.csv')
RoofOvrhng_E_random <- read.csv('randomNumbers/dsg5_fo/RoofOvrhng_E_random.csv')
```

##### Append random numbers to Rhino file

``` r
dsg5_fo$WWR_N <- ((WWR_N_random$x * 2) + 15)/100
dsg5_fo$WWR_E <- ((WWR_E_random$x * 2) + 15)/100
dsg5_fo$RoofOvrhng_N <- ((RoofOvrhng_N_random$x * 2) + 50)/100
dsg5_fo$RoofOvrhng_E <- ((RoofOvrhng_E_random$x * 2) + 50)/100
```


##### Data Collection (Rhino) - Simulations

``` r
dsg5_fo <- as.data.frame(dsg5_fo)
write.csv(dsg5_fo, "dsg_fo_5.csv")
dsg5_fo
```

```
##    run.order std.order x1 x2 x3     WWR_N     WWR_E RoofOvrhng_N RoofOvrhng_E
## 1          1         1 -1 -1 -1 0.1149092 0.1252200    0.4972664    0.5425179
## 2          2         2  1 -1 -1 0.1546059 0.1855303    0.5360377    0.5296485
## 3          3         3 -1  1 -1 0.1381577 0.1429491    0.4855203    0.4894969
## 4          4         4  1  1 -1 0.1676761 0.1664764    0.4953594    0.4814060
## 5          5         5 -1 -1  1 0.2228511 0.1741813    0.4857285    0.4206535
## 6          6         6  1 -1  1 0.1479519 0.1948198    0.4879594    0.5116764
## 7          7         7 -1  1  1 0.1468679 0.1482289    0.4534016    0.4991399
## 8          8         8  1  1  1 0.1779018 0.1313431    0.4540365    0.4330856
## 9          9         9  0  0  0 0.2183046 0.1613787    0.5157652    0.5047305
## 10        10        10  0  0  0 0.1284651 0.1640527    0.4916823    0.5454376
## 11        11        11  0  0  0 0.1556434 0.1009766    0.4495917    0.5165347
```

We upload simulation results






#### Load simulation results and append

``` r
ioh <- read.csv('CCD/FO_data_ioh_5.csv')
sda <- read.csv('CCD/FO_data_udi_5.csv')

ioh <- as.data.frame(ioh)
sda <- as.data.frame(sda)

print(ioh)
```

```
##    in.FO  out.IOH      img
## 1      1 0.098070  FO1.png
## 2      2 0.091099  FO2.png
## 3      3 0.096368  FO3.png
## 4      4 0.092038  FO4.png
## 5      5 0.087298  FO5.png
## 6      6 0.083260  FO6.png
## 7      7 0.088204  FO7.png
## 8      8 0.085546  FO8.png
## 9      9 0.086292  FO9.png
## 10    10 0.086292 FO10.png
## 11    11 0.087364 FO11.png
```

``` r
print(sda)
```

```
##    in.FO   out.DA  out.cDA  out.UDI out.sDA50 out.sDA80      img
## 1      1 73.54490 81.54243 81.46817  0.957349  0.321006  FO1.png
## 2      2 76.32886 82.77022 78.97493  0.971865  0.424680  FO2.png
## 3      3 77.23512 83.16813 80.87491  0.973032  0.429354  FO3.png
## 4      4 78.37342 83.60510 79.42589  0.981086  0.471830  FO4.png
## 5      5 81.81532 85.17620 75.67243  0.998542  0.730551  FO5.png
## 6      6 79.33172 84.19243 77.17188  0.973830  0.621823  FO6.png
## 7      7 80.15563 84.55612 78.46937  0.977405  0.688030  FO7.png
## 8      8 80.78425 84.76690 78.94207  0.984694  0.676193  FO8.png
## 9      9 80.75055 84.67295 78.07220  0.997813  0.618147  FO9.png
## 10    10 77.81310 83.49478 79.97647  0.969457  0.508812 FO10.png
## 11    11 78.29003 83.69189 81.96771  0.977428  0.463409 FO11.png
```

##### Append simulation results with fractional factorial design

``` r
dsg5_fo$IOH  <- ioh$out.IOH 
dsg5_fo$sDA  <- sda$out.UDI
dsg5_fo
```

```
##    run.order std.order x1 x2 x3     WWR_N     WWR_E RoofOvrhng_N RoofOvrhng_E
## 1          1         1 -1 -1 -1 0.1149092 0.1252200    0.4972664    0.5425179
## 2          2         2  1 -1 -1 0.1546059 0.1855303    0.5360377    0.5296485
## 3          3         3 -1  1 -1 0.1381577 0.1429491    0.4855203    0.4894969
## 4          4         4  1  1 -1 0.1676761 0.1664764    0.4953594    0.4814060
## 5          5         5 -1 -1  1 0.2228511 0.1741813    0.4857285    0.4206535
## 6          6         6  1 -1  1 0.1479519 0.1948198    0.4879594    0.5116764
## 7          7         7 -1  1  1 0.1468679 0.1482289    0.4534016    0.4991399
## 8          8         8  1  1  1 0.1779018 0.1313431    0.4540365    0.4330856
## 9          9         9  0  0  0 0.2183046 0.1613787    0.5157652    0.5047305
## 10        10        10  0  0  0 0.1284651 0.1640527    0.4916823    0.5454376
## 11        11        11  0  0  0 0.1556434 0.1009766    0.4495917    0.5165347
##         IOH      sDA
## 1  0.098070 81.46817
## 2  0.091099 78.97493
## 3  0.096368 80.87491
## 4  0.092038 79.42589
## 5  0.087298 75.67243
## 6  0.083260 77.17188
## 7  0.088204 78.46937
## 8  0.085546 78.94207
## 9  0.086292 78.07220
## 10 0.086292 79.97647
## 11 0.087364 81.96771
```

##### Create a new dataframe with only the results

``` r
predOutcomes <- data.frame(
  ioh = dsg5_fo$IOH,
  sda = dsg5_fo$sDA
)
predOutcomes
```

```
##         ioh      sda
## 1  0.098070 81.46817
## 2  0.091099 78.97493
## 3  0.096368 80.87491
## 4  0.092038 79.42589
## 5  0.087298 75.67243
## 6  0.083260 77.17188
## 7  0.088204 78.46937
## 8  0.085546 78.94207
## 9  0.086292 78.07220
## 10 0.086292 79.97647
## 11 0.087364 81.96771
```

##### Calcular Desirability values

``` r
library(desirability)
```


##### Desirability target and limit

``` r
iohD <- dMin(0, 0.193161, scale = 1)
sdaD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, sdaD)
head(overallD)
```

```
## $d
## $d[[1]]
## Smaller-is-better desirability function
## 
## Call: dMin.default(low = 0, high = 0.193161, scale = 1)
## 
## Non-informative value: 0.5 
## 
## $d[[2]]
## Larger-is-better desirability function
## 
## Call: dMax.default(low = 35.23524, high = 100, scale = 1)
## 
## Non-informative value: 0.5 
## 
## 
## $call
## dOverall.default(iohD, sdaD)
```

##### Calculate the Overall Desirability values

``` r
predict(iohD, predOutcomes[1])
```

```
##  [1] 0.4922888 0.5283779 0.5011001 0.5235167 0.5480558 0.5689606 0.5433654
##  [8] 0.5571259 0.5532639 0.5532639 0.5477141
```

``` r
predict(sdaD, predOutcomes[2])
```

```
##  [1] 0.7138593 0.6753624 0.7046992 0.6823255 0.6243703 0.6475225 0.6675565
##  [8] 0.6748551 0.6614240 0.6908268 0.7215725
```

``` r
predict(overallD, predOutcomes)
```

```
##  [1] 0.5928111 0.5973664 0.5942431 0.5976694 0.5849699 0.6069718 0.6022683
##  [8] 0.6131715 0.6049314 0.6182309 0.6286616
```

``` r
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
overall_desirability
```

```
##           D1        D2   Overall
## 1  0.4922888 0.7138593 0.5928111
## 2  0.5283779 0.6753624 0.5973664
## 3  0.5011001 0.7046992 0.5942431
## 4  0.5235167 0.6823255 0.5976694
## 5  0.5480558 0.6243703 0.5849699
## 6  0.5689606 0.6475225 0.6069718
## 7  0.5433654 0.6675565 0.6022683
## 8  0.5571259 0.6748551 0.6131715
## 9  0.5532639 0.6614240 0.6049314
## 10 0.5532639 0.6908268 0.6182309
## 11 0.5477141 0.7215725 0.6286616
```

##### Append all results in the factorial design

``` r
dsg5_fo$d1 <- overall_desirability$D1
dsg5_fo$d2 <- overall_desirability$D2
dsg5_fo$Overall <- overall_desirability$Overall
dsg5_fo
```

```
##    run.order std.order x1 x2 x3     WWR_N     WWR_E RoofOvrhng_N RoofOvrhng_E
## 1          1         1 -1 -1 -1 0.1149092 0.1252200    0.4972664    0.5425179
## 2          2         2  1 -1 -1 0.1546059 0.1855303    0.5360377    0.5296485
## 3          3         3 -1  1 -1 0.1381577 0.1429491    0.4855203    0.4894969
## 4          4         4  1  1 -1 0.1676761 0.1664764    0.4953594    0.4814060
## 5          5         5 -1 -1  1 0.2228511 0.1741813    0.4857285    0.4206535
## 6          6         6  1 -1  1 0.1479519 0.1948198    0.4879594    0.5116764
## 7          7         7 -1  1  1 0.1468679 0.1482289    0.4534016    0.4991399
## 8          8         8  1  1  1 0.1779018 0.1313431    0.4540365    0.4330856
## 9          9         9  0  0  0 0.2183046 0.1613787    0.5157652    0.5047305
## 10        10        10  0  0  0 0.1284651 0.1640527    0.4916823    0.5454376
## 11        11        11  0  0  0 0.1556434 0.1009766    0.4495917    0.5165347
##         IOH      sDA        d1        d2   Overall
## 1  0.098070 81.46817 0.4922888 0.7138593 0.5928111
## 2  0.091099 78.97493 0.5283779 0.6753624 0.5973664
## 3  0.096368 80.87491 0.5011001 0.7046992 0.5942431
## 4  0.092038 79.42589 0.5235167 0.6823255 0.5976694
## 5  0.087298 75.67243 0.5480558 0.6243703 0.5849699
## 6  0.083260 77.17188 0.5689606 0.6475225 0.6069718
## 7  0.088204 78.46937 0.5433654 0.6675565 0.6022683
## 8  0.085546 78.94207 0.5571259 0.6748551 0.6131715
## 9  0.086292 78.07220 0.5532639 0.6614240 0.6049314
## 10 0.086292 79.97647 0.5532639 0.6908268 0.6182309
## 11 0.087364 81.96771 0.5477141 0.7215725 0.6286616
```

##### Subset unnecesary columns and code data

``` r
dsg5_fo = subset(dsg5_fo, select = -c(run.order, std.order,RoofOvrhng_N, RoofOvrhng_E, WWR_N, WWR_E))
dsg5_fo <- coded.data(
  dsg5_fo, formulas = list(x1 ~ (RoofOvrhng_S_W - 3.5130)/.25,
                  x2 ~ (WWR_W - 0.047225)/.025,
                  x3 ~ (WWR_S - 0.2356)/.1)
  )

dsg5_fo
```

```
##    RoofOvrhng_S_W   WWR_W  WWR_S      IOH      sDA        d1        d2
## 1           3.263 0.02222 0.1356 0.098070 81.46817 0.4922888 0.7138593
## 2           3.763 0.02222 0.1356 0.091099 78.97493 0.5283779 0.6753624
## 3           3.263 0.07222 0.1356 0.096368 80.87491 0.5011001 0.7046992
## 4           3.763 0.07222 0.1356 0.092038 79.42589 0.5235167 0.6823255
## 5           3.263 0.02222 0.3356 0.087298 75.67243 0.5480558 0.6243703
## 6           3.763 0.02222 0.3356 0.083260 77.17188 0.5689606 0.6475225
## 7           3.263 0.07222 0.3356 0.088204 78.46937 0.5433654 0.6675565
## 8           3.763 0.07222 0.3356 0.085546 78.94207 0.5571259 0.6748551
## 9           3.513 0.04722 0.2356 0.086292 78.07220 0.5532639 0.6614240
## 10          3.513 0.04722 0.2356 0.086292 79.97647 0.5532639 0.6908268
## 11          3.513 0.04722 0.2356 0.087364 81.96771 0.5477141 0.7215725
##      Overall
## 1  0.5928111
## 2  0.5973664
## 3  0.5942431
## 4  0.5976694
## 5  0.5849699
## 6  0.6069718
## 7  0.6022683
## 8  0.6131715
## 9  0.6049314
## 10 0.6182309
## 11 0.6286616
## 
## Data are stored in coded form using these coding formulas ...
## x1 ~ (RoofOvrhng_S_W - 3.513)/0.25
## x2 ~ (WWR_W - 0.047225)/0.025
## x3 ~ (WWR_S - 0.2356)/0.1
```

We create the response surface model and identify the most significant terms.

##### Model adjustment and summary

``` r
anal5_fo <- rsm(Overall ~ FO(x1, x2, x3), data = dsg5_fo)
summary(anal5_fo)
```

```
## 
## Call:
## rsm(formula = Overall ~ FO(x1, x2, x3), data = dsg5_fo)
## 
##              Estimate Std. Error  t value  Pr(>|t|)    
## (Intercept) 0.6037541  0.0039559 152.6222 1.368e-13 ***
## x1          0.0051109  0.0046387   1.1018    0.3070    
## x2          0.0031541  0.0046387   0.6800    0.5184    
## x3          0.0031614  0.0046387   0.6815    0.5174    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Multiple R-squared:  0.2342,	Adjusted R-squared:  -0.094 
## F-statistic: 0.7136 on 3 and 7 DF,  p-value: 0.5743
## 
## Analysis of Variance Table
## 
## Response: Overall
##                Df     Sum Sq    Mean Sq F value Pr(>F)
## FO(x1, x2, x3)  3 0.00036851 0.00012284  0.7136 0.5743
## Residuals       7 0.00120497 0.00017214               
## Lack of fit     5 0.00092203 0.00018441  1.3035 0.4878
## Pure error      2 0.00028293 0.00014147               
## 
## Direction of steepest ascent (at radius 1):
##        x1        x2        x3 
## 0.7530294 0.4647290 0.4658044 
## 
## Corresponding increment in original units:
## RoofOvrhng_S_W          WWR_W          WWR_S 
##     0.18825734     0.01161822     0.04658044
```






## **SO: Second-order models**


### **Experiment 6: Adding stars**

The value of making rotatability (or near-rotatability) a design goal becomes clear when you consider that the experimenter doesn’t know the location of the optimum point within the region of interest before the experiment is conducted, so it’s desirable that all points a given distance (r) from the center point in any direction have the same magnitude of prediction error: https://www.qualitydigest.com/june01/html/doe.html

Because the purpose of RAM is optimization and the location of the optimum is unkwnown prior to running the experiment… it makes sense


``` r
dsg1_so = ccd(3, n0 = c(0,3), randomize = FALSE, alpha = "rotatable",
            coding = c(x1 ~ (RoofOvrhng_S_W - 3.5130)/.25,
                  x2 ~ (WWR_W - 0.047225)/.025,
                  x3 ~ (WWR_S - 0.2356)/.1))
dsg1_so
```

```
##    run.order std.order RoofOvrhng_S_W       WWR_W      WWR_S Block
## 1          1         1       3.263000 0.022220000 0.13560000     1
## 2          2         2       3.763000 0.022220000 0.13560000     1
## 3          3         3       3.263000 0.072220000 0.13560000     1
## 4          4         4       3.763000 0.072220000 0.13560000     1
## 5          5         5       3.263000 0.022220000 0.33560000     1
## 6          6         6       3.763000 0.022220000 0.33560000     1
## 7          7         7       3.263000 0.072220000 0.33560000     1
## 8          8         8       3.763000 0.072220000 0.33560000     1
## 9          1         1       3.092552 0.047220000 0.23560000     2
## 10         2         2       3.933448 0.047220000 0.23560000     2
## 11         3         3       3.513000 0.005175179 0.23560000     2
## 12         4         4       3.513000 0.089264821 0.23560000     2
## 13         5         5       3.513000 0.047220000 0.06742072     2
## 14         6         6       3.513000 0.047220000 0.40377928     2
## 15         7         7       3.513000 0.047220000 0.23560000     2
## 16         8         8       3.513000 0.047220000 0.23560000     2
## 17         9         9       3.513000 0.047220000 0.23560000     2
## 
## Data are stored in coded form using these coding formulas ...
## x1 ~ (RoofOvrhng_S_W - 3.513)/0.25
## x2 ~ (WWR_W - 0.047225)/0.025
## x3 ~ (WWR_S - 0.2356)/0.1
```


``` r
as.data.frame(dsg1_so)
```

```
##    run.order std.order        x1        x2        x3 Block
## 1          1         1 -1.000000 -1.000000 -1.000000     1
## 2          2         2  1.000000 -1.000000 -1.000000     1
## 3          3         3 -1.000000  1.000000 -1.000000     1
## 4          4         4  1.000000  1.000000 -1.000000     1
## 5          5         5 -1.000000 -1.000000  1.000000     1
## 6          6         6  1.000000 -1.000000  1.000000     1
## 7          7         7 -1.000000  1.000000  1.000000     1
## 8          8         8  1.000000  1.000000  1.000000     1
## 9          1         1 -1.681793  0.000000  0.000000     2
## 10         2         2  1.681793  0.000000  0.000000     2
## 11         3         3  0.000000 -1.681793  0.000000     2
## 12         4         4  0.000000  1.681793  0.000000     2
## 13         5         5  0.000000  0.000000 -1.681793     2
## 14         6         6  0.000000  0.000000  1.681793     2
## 15         7         7  0.000000  0.000000  0.000000     2
## 16         8         8  0.000000  0.000000  0.000000     2
## 17         9         9  0.000000  0.000000  0.000000     2
```




##### Add random values for other variables

##### Generate a random number from the beta distribution

``` r
WWR_N_random <- rnorm(17, mean = 0, sd=2)
WWR_E_random <- rnorm(17, mean = 0, sd=2)
RoofOvrhng_N_random <- rnorm(17, mean = 0, sd=2)
RoofOvrhng_E_random <- rnorm(17, mean = 0, sd=2)
```

##### Save into csv

``` r
write.csv(WWR_N_random, "WWR_N_random.csv")
write.csv(WWR_E_random, "WWR_E_random.csv")
write.csv(RoofOvrhng_N_random, "RoofOvrhng_N_random.csv")
write.csv(RoofOvrhng_E_random, "RoofOvrhng_E_random.csv")
```

##### Load random numbers

``` r
WWR_N_random <- read.csv('randomNumbers/dsg1_so/WWR_N_random.csv')
WWR_E_random <- read.csv('randomNumbers/dsg1_so/WWR_E_random.csv')
RoofOvrhng_N_random <- read.csv('randomNumbers/dsg1_so/RoofOvrhng_N_random.csv')
RoofOvrhng_E_random <- read.csv('randomNumbers/dsg1_so/RoofOvrhng_E_random.csv')
```

##### Append random numbers to Rhino file

``` r
dsg1_so$WWR_N <- ((WWR_N_random$x * 2) + 15)/100
dsg1_so$WWR_E <- ((WWR_E_random$x * 2) + 15)/100
dsg1_so$RoofOvrhng_N <- ((RoofOvrhng_N_random$x * 2) + 50)/100
dsg1_so$RoofOvrhng_E <- ((RoofOvrhng_E_random$x * 2) + 50)/100
```


##### Drop "Block" column

``` r
# Specify the variables to drop
variables_to_drop <- c("Block")
```


``` r
# Drop the specified variables from the dsg1_so
dsg1_so <- dsg1_so[, !names(dsg1_so) %in% variables_to_drop]
dsg1_so
```

```
##    run.order std.order RoofOvrhng_S_W       WWR_W      WWR_S      WWR_N
## 1          1         1       3.263000 0.022220000 0.13560000 0.19721523
## 2          2         2       3.763000 0.022220000 0.13560000 0.04306110
## 3          3         3       3.263000 0.072220000 0.13560000 0.15007979
## 4          4         4       3.763000 0.072220000 0.13560000 0.23041072
## 5          5         5       3.263000 0.022220000 0.33560000 0.15235623
## 6          6         6       3.763000 0.022220000 0.33560000 0.16810612
## 7          7         7       3.263000 0.072220000 0.33560000 0.19672111
## 8          8         8       3.763000 0.072220000 0.33560000 0.07992755
## 9          1         1       3.092552 0.047220000 0.23560000 0.21643810
## 10         2         2       3.933448 0.047220000 0.23560000 0.17753114
## 11         3         3       3.513000 0.005175179 0.23560000 0.18273749
## 12         4         4       3.513000 0.089264821 0.23560000 0.21511257
## 13         5         5       3.513000 0.047220000 0.06742072 0.18386619
## 14         6         6       3.513000 0.047220000 0.40377928 0.13429855
## 15         7         7       3.513000 0.047220000 0.23560000 0.13408318
## 16         8         8       3.513000 0.047220000 0.23560000 0.18962349
## 17         9         9       3.513000 0.047220000 0.23560000 0.11355122
##        WWR_E RoofOvrhng_N RoofOvrhng_E
## 1  0.1697546    0.5445526    0.5253962
## 2  0.1679785    0.4730640    0.4319493
## 3  0.2070079    0.4617041    0.5389445
## 4  0.2058411    0.5154634    0.4799910
## 5  0.1238387    0.5091187    0.5268058
## 6  0.1194760    0.5448359    0.4959477
## 7  0.1350558    0.4483948    0.5007742
## 8  0.1253084    0.4607031    0.4745517
## 9  0.1796870    0.4740034    0.4910256
## 10 0.1647595    0.5253768    0.4987337
## 11 0.1748863    0.5351508    0.4501293
## 12 0.1565717    0.4765441    0.5481755
## 13 0.1181618    0.5156447    0.5136777
## 14 0.1806150    0.5243866    0.4652325
## 15 0.1226210    0.5509605    0.4513711
## 16 0.1283304    0.5389319    0.5350802
## 17 0.1456654    0.5382432    0.4616005
## 
## Data are stored in coded form using these coding formulas ...
## x1 ~ (RoofOvrhng_S_W - 3.513)/0.25
## x2 ~ (WWR_W - 0.047225)/0.025
## x3 ~ (WWR_S - 0.2356)/0.1
```


##### Data Collection (Rhino) - Simulations

``` r
dsg1_so <- as.data.frame(dsg1_so)
write.csv(dsg1_so, "dsg_so_1.csv")
dsg1_so
```

```
##    run.order std.order        x1        x2        x3      WWR_N     WWR_E
## 1          1         1 -1.000000 -1.000000 -1.000000 0.19721523 0.1697546
## 2          2         2  1.000000 -1.000000 -1.000000 0.04306110 0.1679785
## 3          3         3 -1.000000  1.000000 -1.000000 0.15007979 0.2070079
## 4          4         4  1.000000  1.000000 -1.000000 0.23041072 0.2058411
## 5          5         5 -1.000000 -1.000000  1.000000 0.15235623 0.1238387
## 6          6         6  1.000000 -1.000000  1.000000 0.16810612 0.1194760
## 7          7         7 -1.000000  1.000000  1.000000 0.19672111 0.1350558
## 8          8         8  1.000000  1.000000  1.000000 0.07992755 0.1253084
## 9          1         1 -1.681793  0.000000  0.000000 0.21643810 0.1796870
## 10         2         2  1.681793  0.000000  0.000000 0.17753114 0.1647595
## 11         3         3  0.000000 -1.681793  0.000000 0.18273749 0.1748863
## 12         4         4  0.000000  1.681793  0.000000 0.21511257 0.1565717
## 13         5         5  0.000000  0.000000 -1.681793 0.18386619 0.1181618
## 14         6         6  0.000000  0.000000  1.681793 0.13429855 0.1806150
## 15         7         7  0.000000  0.000000  0.000000 0.13408318 0.1226210
## 16         8         8  0.000000  0.000000  0.000000 0.18962349 0.1283304
## 17         9         9  0.000000  0.000000  0.000000 0.11355122 0.1456654
##    RoofOvrhng_N RoofOvrhng_E
## 1     0.5445526    0.5253962
## 2     0.4730640    0.4319493
## 3     0.4617041    0.5389445
## 4     0.5154634    0.4799910
## 5     0.5091187    0.5268058
## 6     0.5448359    0.4959477
## 7     0.4483948    0.5007742
## 8     0.4607031    0.4745517
## 9     0.4740034    0.4910256
## 10    0.5253768    0.4987337
## 11    0.5351508    0.4501293
## 12    0.4765441    0.5481755
## 13    0.5156447    0.5136777
## 14    0.5243866    0.4652325
## 15    0.5509605    0.4513711
## 16    0.5389319    0.5350802
## 17    0.5382432    0.4616005
```

We upload simulation results




#### Load simulation results and append

``` r
ioh <- read.csv('CCD/SO_data_ioh_1.csv')
sda <- read.csv('CCD/SO_data_udi_1.csv')

ioh <- as.data.frame(ioh)
sda <- as.data.frame(sda)

print(ioh)
```

```
##    in.SO  out.IOH      img
## 1      1 0.094866  SO1.png
## 2      2 0.093126  SO2.png
## 3      3 0.093109  SO3.png
## 4      4 0.090905  SO4.png
## 5      5 0.086149  SO5.png
## 6      6 0.083425  SO6.png
## 7      7 0.088032  SO7.png
## 8      8 0.085232  SO8.png
## 9      9 0.088574  SO9.png
## 10    10 0.084304 SO10.png
## 11    11 0.086083 SO11.png
## 12    12 0.087171 SO12.png
## 13    13 0.114817 SO13.png
## 14    14 0.089264 SO14.png
## 15    15 0.087171 SO15.png
## 16    16 0.086706 SO16.png
## 17    17 0.086320 SO17.png
```

``` r
print(sda)
```

```
##    in.SO   out.DA  out.cDA  out.UDI out.sDA50 out.sDA80      img
## 1      1 78.12549 83.45837 78.74421  0.988412  0.477114  SO1.png
## 2      2 62.77590 76.11549 77.11412  0.805923  0.259187  SO2.png
## 3      3 78.68424 83.79511 78.39834  0.978134  0.502882  SO3.png
## 4      4 80.28228 84.38279 76.18637  0.999271  0.567819  SO4.png
## 5      5 79.19081 84.15757 79.91685  0.976017  0.596555  SO5.png
## 6      6 79.37305 84.19581 80.18285  0.980367  0.578226  SO6.png
## 7      7 81.62342 85.11741 77.86703  0.991983  0.741432  SO7.png
## 8      8 75.58545 82.29728 80.18154  0.948482  0.506312  SO8.png
## 9      9 81.26222 84.90523 77.03490  0.997813  0.667500  SO9.png
## 10    10 79.53145 84.16676 79.07523  0.983965  0.546257 SO10.png
## 11    11 78.92103 83.92368 78.29105  0.984740  0.533544 SO11.png
## 12    12 81.23080 84.88361 78.00496  0.997813  0.669073 SO12.png
## 13    13 73.43403 81.73444 80.36717  0.947316  0.386462 SO13.png
## 14    14 79.61392 84.32452 75.76610  0.971621  0.695938 SO14.png
## 15    15 77.42256 83.34011 81.43926  0.970892  0.462894 SO15.png
## 16    16 79.72222 84.25002 80.42025  0.987609  0.551314 SO16.png
## 17    17 76.64651 82.95321 80.67720  0.962215  0.458782 SO17.png
```

##### Append simulation results with fractional factorial design

``` r
dsg1_so$IOH  <- ioh$out.IOH 
dsg1_so$sDA  <- sda$out.UDI
dsg1_so
```

```
##    run.order std.order        x1        x2        x3      WWR_N     WWR_E
## 1          1         1 -1.000000 -1.000000 -1.000000 0.19721523 0.1697546
## 2          2         2  1.000000 -1.000000 -1.000000 0.04306110 0.1679785
## 3          3         3 -1.000000  1.000000 -1.000000 0.15007979 0.2070079
## 4          4         4  1.000000  1.000000 -1.000000 0.23041072 0.2058411
## 5          5         5 -1.000000 -1.000000  1.000000 0.15235623 0.1238387
## 6          6         6  1.000000 -1.000000  1.000000 0.16810612 0.1194760
## 7          7         7 -1.000000  1.000000  1.000000 0.19672111 0.1350558
## 8          8         8  1.000000  1.000000  1.000000 0.07992755 0.1253084
## 9          1         1 -1.681793  0.000000  0.000000 0.21643810 0.1796870
## 10         2         2  1.681793  0.000000  0.000000 0.17753114 0.1647595
## 11         3         3  0.000000 -1.681793  0.000000 0.18273749 0.1748863
## 12         4         4  0.000000  1.681793  0.000000 0.21511257 0.1565717
## 13         5         5  0.000000  0.000000 -1.681793 0.18386619 0.1181618
## 14         6         6  0.000000  0.000000  1.681793 0.13429855 0.1806150
## 15         7         7  0.000000  0.000000  0.000000 0.13408318 0.1226210
## 16         8         8  0.000000  0.000000  0.000000 0.18962349 0.1283304
## 17         9         9  0.000000  0.000000  0.000000 0.11355122 0.1456654
##    RoofOvrhng_N RoofOvrhng_E      IOH      sDA
## 1     0.5445526    0.5253962 0.094866 78.74421
## 2     0.4730640    0.4319493 0.093126 77.11412
## 3     0.4617041    0.5389445 0.093109 78.39834
## 4     0.5154634    0.4799910 0.090905 76.18637
## 5     0.5091187    0.5268058 0.086149 79.91685
## 6     0.5448359    0.4959477 0.083425 80.18285
## 7     0.4483948    0.5007742 0.088032 77.86703
## 8     0.4607031    0.4745517 0.085232 80.18154
## 9     0.4740034    0.4910256 0.088574 77.03490
## 10    0.5253768    0.4987337 0.084304 79.07523
## 11    0.5351508    0.4501293 0.086083 78.29105
## 12    0.4765441    0.5481755 0.087171 78.00496
## 13    0.5156447    0.5136777 0.114817 80.36717
## 14    0.5243866    0.4652325 0.089264 75.76610
## 15    0.5509605    0.4513711 0.087171 81.43926
## 16    0.5389319    0.5350802 0.086706 80.42025
## 17    0.5382432    0.4616005 0.086320 80.67720
```

##### Create a new dataframe with only the results

``` r
predOutcomes <- data.frame(
  ioh = dsg1_so$IOH,
  sda = dsg1_so$sDA
)
predOutcomes
```

```
##         ioh      sda
## 1  0.094866 78.74421
## 2  0.093126 77.11412
## 3  0.093109 78.39834
## 4  0.090905 76.18637
## 5  0.086149 79.91685
## 6  0.083425 80.18285
## 7  0.088032 77.86703
## 8  0.085232 80.18154
## 9  0.088574 77.03490
## 10 0.084304 79.07523
## 11 0.086083 78.29105
## 12 0.087171 78.00496
## 13 0.114817 80.36717
## 14 0.089264 75.76610
## 15 0.087171 81.43926
## 16 0.086706 80.42025
## 17 0.086320 80.67720
```

##### Calcular Desirability values

``` r
library(desirability)
```


##### Desirability target and limit

``` r
iohD <- dMin(0, 0.193161, scale = 1)
sdaD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, sdaD)
head(overallD)
```

```
## $d
## $d[[1]]
## Smaller-is-better desirability function
## 
## Call: dMin.default(low = 0, high = 0.193161, scale = 1)
## 
## Non-informative value: 0.5 
## 
## $d[[2]]
## Larger-is-better desirability function
## 
## Call: dMax.default(low = 35.23524, high = 100, scale = 1)
## 
## Non-informative value: 0.5 
## 
## 
## $call
## dOverall.default(iohD, sdaD)
```

##### Calculate the Overall Desirability values

``` r
predict(iohD, predOutcomes[1])
```

```
##  [1] 0.5088760 0.5178840 0.5179721 0.5293822 0.5540042 0.5681064 0.5442558
##  [8] 0.5587515 0.5414499 0.5635558 0.5543459 0.5487132 0.4055891 0.5378777
## [15] 0.5487132 0.5511206 0.5531189
```

``` r
predict(sdaD, predOutcomes[2])
```

```
##  [1] 0.6718001 0.6466307 0.6664596 0.6323058 0.6899062 0.6940134 0.6582560
##  [8] 0.6939931 0.6454074 0.6769111 0.6648030 0.6603857 0.6968594 0.6258166
## [15] 0.7134130 0.6976789 0.7016464
```

``` r
predict(overallD, predOutcomes)
```

```
##  [1] 0.5846905 0.5786879 0.5875436 0.5785598 0.6182321 0.6279120 0.5985480
##  [8] 0.6227116 0.5911478 0.6176384 0.6070674 0.6019654 0.5316377 0.5801835
## [15] 0.6256670 0.6200848 0.6229718
```

``` r
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
overall_desirability
```

```
##           D1        D2   Overall
## 1  0.5088760 0.6718001 0.5846905
## 2  0.5178840 0.6466307 0.5786879
## 3  0.5179721 0.6664596 0.5875436
## 4  0.5293822 0.6323058 0.5785598
## 5  0.5540042 0.6899062 0.6182321
## 6  0.5681064 0.6940134 0.6279120
## 7  0.5442558 0.6582560 0.5985480
## 8  0.5587515 0.6939931 0.6227116
## 9  0.5414499 0.6454074 0.5911478
## 10 0.5635558 0.6769111 0.6176384
## 11 0.5543459 0.6648030 0.6070674
## 12 0.5487132 0.6603857 0.6019654
## 13 0.4055891 0.6968594 0.5316377
## 14 0.5378777 0.6258166 0.5801835
## 15 0.5487132 0.7134130 0.6256670
## 16 0.5511206 0.6976789 0.6200848
## 17 0.5531189 0.7016464 0.6229718
```

##### Append all results in the factorial design

``` r
dsg1_so$d1 <- overall_desirability$D1
dsg1_so$d2 <- overall_desirability$D2
dsg1_so$Overall <- overall_desirability$Overall
dsg1_so
```

```
##    run.order std.order        x1        x2        x3      WWR_N     WWR_E
## 1          1         1 -1.000000 -1.000000 -1.000000 0.19721523 0.1697546
## 2          2         2  1.000000 -1.000000 -1.000000 0.04306110 0.1679785
## 3          3         3 -1.000000  1.000000 -1.000000 0.15007979 0.2070079
## 4          4         4  1.000000  1.000000 -1.000000 0.23041072 0.2058411
## 5          5         5 -1.000000 -1.000000  1.000000 0.15235623 0.1238387
## 6          6         6  1.000000 -1.000000  1.000000 0.16810612 0.1194760
## 7          7         7 -1.000000  1.000000  1.000000 0.19672111 0.1350558
## 8          8         8  1.000000  1.000000  1.000000 0.07992755 0.1253084
## 9          1         1 -1.681793  0.000000  0.000000 0.21643810 0.1796870
## 10         2         2  1.681793  0.000000  0.000000 0.17753114 0.1647595
## 11         3         3  0.000000 -1.681793  0.000000 0.18273749 0.1748863
## 12         4         4  0.000000  1.681793  0.000000 0.21511257 0.1565717
## 13         5         5  0.000000  0.000000 -1.681793 0.18386619 0.1181618
## 14         6         6  0.000000  0.000000  1.681793 0.13429855 0.1806150
## 15         7         7  0.000000  0.000000  0.000000 0.13408318 0.1226210
## 16         8         8  0.000000  0.000000  0.000000 0.18962349 0.1283304
## 17         9         9  0.000000  0.000000  0.000000 0.11355122 0.1456654
##    RoofOvrhng_N RoofOvrhng_E      IOH      sDA        d1        d2   Overall
## 1     0.5445526    0.5253962 0.094866 78.74421 0.5088760 0.6718001 0.5846905
## 2     0.4730640    0.4319493 0.093126 77.11412 0.5178840 0.6466307 0.5786879
## 3     0.4617041    0.5389445 0.093109 78.39834 0.5179721 0.6664596 0.5875436
## 4     0.5154634    0.4799910 0.090905 76.18637 0.5293822 0.6323058 0.5785598
## 5     0.5091187    0.5268058 0.086149 79.91685 0.5540042 0.6899062 0.6182321
## 6     0.5448359    0.4959477 0.083425 80.18285 0.5681064 0.6940134 0.6279120
## 7     0.4483948    0.5007742 0.088032 77.86703 0.5442558 0.6582560 0.5985480
## 8     0.4607031    0.4745517 0.085232 80.18154 0.5587515 0.6939931 0.6227116
## 9     0.4740034    0.4910256 0.088574 77.03490 0.5414499 0.6454074 0.5911478
## 10    0.5253768    0.4987337 0.084304 79.07523 0.5635558 0.6769111 0.6176384
## 11    0.5351508    0.4501293 0.086083 78.29105 0.5543459 0.6648030 0.6070674
## 12    0.4765441    0.5481755 0.087171 78.00496 0.5487132 0.6603857 0.6019654
## 13    0.5156447    0.5136777 0.114817 80.36717 0.4055891 0.6968594 0.5316377
## 14    0.5243866    0.4652325 0.089264 75.76610 0.5378777 0.6258166 0.5801835
## 15    0.5509605    0.4513711 0.087171 81.43926 0.5487132 0.7134130 0.6256670
## 16    0.5389319    0.5350802 0.086706 80.42025 0.5511206 0.6976789 0.6200848
## 17    0.5382432    0.4616005 0.086320 80.67720 0.5531189 0.7016464 0.6229718
```

##### Subset unnecesary columns and code data

``` r
dsg1_so = subset(dsg1_so, select = -c(run.order, std.order,RoofOvrhng_N, RoofOvrhng_E, WWR_N, WWR_E))
dsg1_so <- coded.data(
  dsg1_so, formulas = list(x1 ~ (RoofOvrhng_S_W - 3.5130)/.25,
                  x2 ~ (WWR_W - 0.047225)/.025,
                  x3 ~ (WWR_S - 0.2356)/.1)
  )

dsg1_so
```

```
##    RoofOvrhng_S_W       WWR_W      WWR_S      IOH      sDA        d1        d2
## 1        3.263000 0.022220000 0.13560000 0.094866 78.74421 0.5088760 0.6718001
## 2        3.763000 0.022220000 0.13560000 0.093126 77.11412 0.5178840 0.6466307
## 3        3.263000 0.072220000 0.13560000 0.093109 78.39834 0.5179721 0.6664596
## 4        3.763000 0.072220000 0.13560000 0.090905 76.18637 0.5293822 0.6323058
## 5        3.263000 0.022220000 0.33560000 0.086149 79.91685 0.5540042 0.6899062
## 6        3.763000 0.022220000 0.33560000 0.083425 80.18285 0.5681064 0.6940134
## 7        3.263000 0.072220000 0.33560000 0.088032 77.86703 0.5442558 0.6582560
## 8        3.763000 0.072220000 0.33560000 0.085232 80.18154 0.5587515 0.6939931
## 9        3.092552 0.047220000 0.23560000 0.088574 77.03490 0.5414499 0.6454074
## 10       3.933448 0.047220000 0.23560000 0.084304 79.07523 0.5635558 0.6769111
## 11       3.513000 0.005175179 0.23560000 0.086083 78.29105 0.5543459 0.6648030
## 12       3.513000 0.089264821 0.23560000 0.087171 78.00496 0.5487132 0.6603857
## 13       3.513000 0.047220000 0.06742072 0.114817 80.36717 0.4055891 0.6968594
## 14       3.513000 0.047220000 0.40377928 0.089264 75.76610 0.5378777 0.6258166
## 15       3.513000 0.047220000 0.23560000 0.087171 81.43926 0.5487132 0.7134130
## 16       3.513000 0.047220000 0.23560000 0.086706 80.42025 0.5511206 0.6976789
## 17       3.513000 0.047220000 0.23560000 0.086320 80.67720 0.5531189 0.7016464
##      Overall
## 1  0.5846905
## 2  0.5786879
## 3  0.5875436
## 4  0.5785598
## 5  0.6182321
## 6  0.6279120
## 7  0.5985480
## 8  0.6227116
## 9  0.5911478
## 10 0.6176384
## 11 0.6070674
## 12 0.6019654
## 13 0.5316377
## 14 0.5801835
## 15 0.6256670
## 16 0.6200848
## 17 0.6229718
## 
## Data are stored in coded form using these coding formulas ...
## x1 ~ (RoofOvrhng_S_W - 3.513)/0.25
## x2 ~ (WWR_W - 0.047225)/0.025
## x3 ~ (WWR_S - 0.2356)/0.1
```

We create the response surface model and identify the most significant terms.

##### Model adjustment and summary

``` r
anal1_so <- rsm(Overall ~ SO(x1, x2, x3), data = dsg1_so)
summary(anal1_so)
```

```
## 
## Call:
## rsm(formula = Overall ~ SO(x1, x2, x3), data = dsg1_so)
## 
##               Estimate Std. Error  t value  Pr(>|t|)    
## (Intercept)  0.6220418  0.0060103 103.4963 2.072e-12 ***
## x1           0.0046430  0.0028225   1.6450 0.1439656    
## x2          -0.0022509  0.0028225  -0.7975 0.4513701    
## x3           0.0160773  0.0028225   5.6962 0.0007383 ***
## x1:x2        0.0014378  0.0036877   0.3899 0.7082097    
## x1:x3        0.0061037  0.0036877   1.6551 0.1418699    
## x2:x3       -0.0034512  0.0036877  -0.9359 0.3805023    
## x1^2        -0.0035626  0.0031065  -1.1468 0.2891467    
## x2^2        -0.0035190  0.0031065  -1.1328 0.2946142    
## x3^2        -0.0207038  0.0031065  -6.6646 0.0002866 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Multiple R-squared:  0.9241,	Adjusted R-squared:  0.8265 
## F-statistic: 9.466 on 9 and 7 DF,  p-value: 0.003633
## 
## Analysis of Variance Table
## 
## Response: Overall
##                 Df    Sum Sq    Mean Sq F value   Pr(>F)
## FO(x1, x2, x3)   3 0.0038936 0.00129788 11.9295 0.003851
## TWI(x1, x2, x3)  3 0.0004099 0.00013662  1.2558 0.360371
## PQ(x1, x2, x3)   3 0.0049657 0.00165524 15.2142 0.001890
## Residuals        7 0.0007616 0.00010880                 
## Lack of fit      5 0.0007460 0.00014920 19.1445 0.050383
## Pure error       2 0.0000156 0.00000779                 
## 
## Stationary point of response surface:
##         x1         x2         x3 
##  1.0690572 -0.3848121  0.5779291 
## 
## Stationary point in original units:
## RoofOvrhng_S_W          WWR_W          WWR_S 
##      3.7802643      0.0375997      0.2933929 
## 
## Eigenanalysis:
## eigen() decomposition
## $values
## [1] -0.002745706 -0.003623011 -0.021416661
## 
## $vectors
##          [,1]       [,2]       [,3]
## x1 0.81609683  0.5518550  0.1715867
## x2 0.57181532 -0.8140951 -0.1013727
## x3 0.08374483  0.1808458 -0.9799396
```

##### Graphical representation

``` r
par(mfrow = c(1, 1), pty = 's', mar = c(5, 5, 2, 2))  # Adjust the margin as needed

# Define your custom color gradient
my_colors <- colorRampPalette(c("darkgreen","gold", "orange","red","white"))

# Generate the desired number of colors from this palette
nbcol <- 100  # Adjust the number of colors as needed
custom_color <- my_colors(nbcol)

contour(anal1_so, ~ x1 + x2, at = xs(anal1_so), image = TRUE, img.col = custom_color)
```

![](RSM_thermal_daylight_optimization_files/figure-html/unnamed-chunk-163-1.png)<!-- -->


``` r
par(mfrow = c(1, 1), pty = 's', mar = c(5, 5, 2, 2))  # Adjust the margin as needed

# Define your custom color gradient
my_colors <- colorRampPalette(c("darkgreen","gold", "orange","red","white"))

# Generate the desired number of colors from this palette
nbcol <- 100  # Adjust the number of colors as needed
custom_color <- my_colors(nbcol)

contour(anal1_so, ~ x2 + x3, at = xs(anal1_so), image = TRUE, img.col = custom_color)
```

![](RSM_thermal_daylight_optimization_files/figure-html/unnamed-chunk-164-1.png)<!-- -->



``` r
par(mfrow = c(1, 1), pty = 's', mar = c(5, 5, 2, 2))  # Adjust the margin as needed

# Define your custom color gradient
my_colors <- colorRampPalette(c("darkgreen","gold", "orange","red","white"))

# Generate the desired number of colors from this palette
nbcol <- 100  # Adjust the number of colors as needed
custom_color <- my_colors(nbcol)

contour(anal1_so, ~ x1 + x3, at = xs(anal1_so), image = TRUE, img.col = custom_color)
```

![](RSM_thermal_daylight_optimization_files/figure-html/unnamed-chunk-165-1.png)<!-- -->

##### 3D Perspective plot

``` r
# Define your custom color gradient
my_colors <- colorRampPalette(c("darkgreen","gold", "orange","red","white"))

# Generate the desired number of colors from this palette
nbcol <- 50  # Adjust the number of colors as needed
custom_color <- my_colors(nbcol)

# Open a PNG device
png("rsm_1_plot.png", height = 5, width = 5, units = "in", res = 300)

# Create the contour plot and save it directly to the file
persp(anal1_so, ~ x1 + x3, at = xs(anal1_so), col = custom_color, contours = "colors", zlab = "Overall Desirability", cex.axis = 0.7, border = NA)

# Close the PNG device
dev.off()
```

```
## png 
##   2
```


``` r
# Define your custom color gradient
my_colors <- colorRampPalette(c("darkgreen","gold", "orange","red","white"))

# Generate the desired number of colors from this palette
nbcol <- 50  # Adjust the number of colors as needed
custom_color <- my_colors(nbcol)

# Open a PNG device
png("rsm_2_plot.png", height = 5, width = 5, units = "in", res = 300)

# Create the contour plot and save it directly to the file
persp(anal1_so, ~ x1 + x2, at = xs(anal1_so), col = custom_color, contours = "colors", zlab = "Overall Desirability", cex.axis = 0.7, border = NA)

# Close the PNG device
dev.off()
```

```
## png 
##   2
```


``` r
# Define your custom color gradient
my_colors <- colorRampPalette(c("darkgreen","gold", "orange","red","white"))

# Generate the desired number of colors from this palette
nbcol <- 50  # Adjust the number of colors as needed
custom_color <- my_colors(nbcol)

# Open a PNG device
png("rsm_3_plot.png", height = 5, width = 5, units = "in", res = 300)

# Create the contour plot and save it directly to the file
persp(anal1_so, ~ x2 + x3, at = xs(anal1_so), col = custom_color, contours = "colors", zlab = "Overall Desirability", cex.axis = 0.7, border = NA)

# Close the PNG device
dev.off()
```

```
## png 
##   2
```


##### Metrics for model 5


``` r
summary_model5 <- summary(anal5_fo)
adj_r_squared5 <- summary_model5$adj.r.squared
sigma5 <- summary_model5$sigma
AIC5 <- AIC(anal5_fo)
BIC5 <- BIC(anal5_fo)
p_value5 <- summary_model5$coefficients["(Intercept)", "Pr(>|t|)"]
print(AIC5)
```

```
## [1] -59.09453
```

``` r
print(BIC5)
```

```
## [1] -57.10505
```

##### Metrics for model 6


``` r
summary_model6 <- summary(anal1_so)
adj_r_squared6 <- summary_model6$adj.r.squared
sigma6 <- summary_model6$sigma
AIC6 <- AIC(anal1_so)
BIC6 <- BIC(anal1_so)
p_value6 <- summary_model6$coefficients["(Intercept)", "Pr(>|t|)"]
print(AIC6)
```

```
## [1] -99.98289
```

``` r
print(BIC6)
```

```
## [1] -90.81754
```




# **8. Robustness**

## **Bootstrapping**


``` r
# Extracting Stationary Point in Original Units from your model summary
stationary_point <- c(RoofOvrhng_S_W = 3.7802643, WWR_W = 0.0375997, WWR_S = 0.2933929)

# Set seed for reproducibility
set.seed(123)  # You can use any seed value you like

# Simulating Bootstrap Replications
fits <- predict(anal1_so)
resids <- resid(anal1_so)
```





``` r
# Converting coded values to original units
boot <- code2val(as.data.frame(t(boot.raw)), codings = codings(anal1_so))

  
# Calculate confidence intervals
ci_95 <- function(x) {
  quantile(x, probs = c(0.025, 0.975))
}

ci_RoofOvrhng_S_W <- ci_95(boot$RoofOvrhng_S_W)
ci_WWR_W <- ci_95(boot$WWR_W)
ci_WWR_S <- ci_95(boot$WWR_S)
```



``` r
print(paste("95% CI for RoofOvrhng_S_W:", ci_RoofOvrhng_S_W))
```

```
## [1] "95% CI for RoofOvrhng_S_W: 3.47082264598494"
## [2] "95% CI for RoofOvrhng_S_W: 3.94036451956231"
```

``` r
print(paste("95% CI for WWR_W:", ci_WWR_W))
```

```
## [1] "95% CI for WWR_W: 0.015384813929249" 
## [2] "95% CI for WWR_W: 0.0559413454783458"
```

``` r
print(paste("95% CI for WWR_S:", ci_WWR_S))
```

```
## [1] "95% CI for WWR_S: 0.26192249250929"  "95% CI for WWR_S: 0.317833456923142"
```


## **Plots**



``` r
# Plot for WWR_W vs. WWR_S with individual dots and ablines for confidence intervals
p <- ggplot(boot, aes(x = WWR_W, y = WWR_S)) +
  geom_point(alpha = 0.05, color = "black", size = 3) +  # Plot individual points with transparency
  geom_point(data = data.frame(WWR_W = stationary_point["WWR_W"], WWR_S = stationary_point["WWR_S"]), 
             aes(WWR_W, WWR_S), color = "red", shape = 17, size = 3) +  # Plot the stationary point
  geom_vline(xintercept = ci_WWR_W, color = "blue", linetype = "dashed") +  # Vertical lines for ci_WWR_W
  geom_hline(yintercept = ci_WWR_S, color = "blue", linetype = "dashed") +  # Horizontal lines for ci_WWR_S
  labs(x = "WWR_W", y = "WWR_S") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),  # Add back x and y ticks
    axis.text.x = element_text(size = 11),  # Increase x-axis tick label size
    axis.text.y = element_text(size = 11)   # Increase y-axis tick label size
  ) +
  #theme(aspect.ratio = 1) +
  coord_cartesian(xlim = c(0.00, 0.10), ylim = c(0.25, 0.35)) +
  scale_x_continuous(breaks = seq(0.00, 0.10, by = 0.05)) +
  scale_y_continuous(breaks = seq(0.25, 0.35, by = 0.01))

ggsave("robustness 1 plot.png", plot= p, height = 5 , width = 2.5)


p
```

![](RSM_thermal_daylight_optimization_files/figure-html/unnamed-chunk-175-1.png)<!-- -->



``` r
# Plot for RoofOvrhng_S_W vs. WWR_S with individual dots and ablines for confidence intervals
p <- ggplot(boot, aes(x = RoofOvrhng_S_W, y = WWR_S)) +
  geom_point(alpha = 0.05, color = "black", size = 3) +  # Plot individual points with transparency
  geom_point(data = data.frame(RoofOvrhng_S_W = stationary_point["RoofOvrhng_S_W"], WWR_S = stationary_point["WWR_S"]), 
             aes(RoofOvrhng_S_W, WWR_S), color = "red", shape = 17, size = 3) +  # Plot the stationary point
  geom_vline(xintercept = ci_RoofOvrhng_S_W, color = "blue", linetype = "dashed") +  # Vertical lines for ci_RoofOvrhng_S_W
  geom_hline(yintercept = ci_WWR_S, color = "blue", linetype = "dashed") +  # Horizontal lines for ci_WWR_S
  labs(x = "RoofOvrhng_S_W", y = "WWR_S") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),  # Add back x and y ticks
    axis.text.x = element_text(size = 11),  # Increase x-axis tick label size
    axis.text.y = element_text(size = 11)   # Increase y-axis tick label size
  ) +
  #theme(aspect.ratio = 1) +
  coord_cartesian(xlim = c(3.00, 4.20), ylim = c(0.25, 0.35)) +
  scale_x_continuous(breaks = seq(3.00, 4.20, by =0.2)) +
  scale_y_continuous(breaks = seq(0.25, 0.35, by = 0.01))

ggsave("robustness 2 plot.png", plot= p, height = 5 , width = 2.5)


p
```

![](RSM_thermal_daylight_optimization_files/figure-html/unnamed-chunk-176-1.png)<!-- -->




``` r
# Plot for RoofOvrhng_S_W vs. WWR_W with individual dots and ablines for confidence intervals
p <- ggplot(boot, aes(x = RoofOvrhng_S_W, y = WWR_W)) +
  geom_point(alpha = 0.05, color = "black", size = 3) +  # Plot individual points with transparency
  geom_point(data = data.frame(RoofOvrhng_S_W = stationary_point["RoofOvrhng_S_W"], WWR_W = stationary_point["WWR_W"]), 
             aes(RoofOvrhng_S_W, WWR_W), color = "red", shape = 17, size = 3) +  # Plot the stationary point
  geom_vline(xintercept = ci_RoofOvrhng_S_W, color = "blue", linetype = "dashed") +  # Vertical lines for ci_RoofOvrhng_S_W
  geom_hline(yintercept = ci_WWR_W, color = "blue", linetype = "dashed") +  # Horizontal lines for ci_WWR_W
  labs(x = "RoofOvrhng_S_W", y = "WWR_W") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),  # Add back x and y ticks
    axis.text.x = element_text(size = 11),  # Increase x-axis tick label size
    axis.text.y = element_text(size = 11)   # Increase y-axis tick label size
  ) +
  #theme(aspect.ratio = 1) +
  coord_cartesian(xlim = c(3.00, 4.20), ylim = c(0.00, 0.10)) +
  scale_x_continuous(breaks = seq(3.00, 4.20, by = 0.20)) +
  scale_y_continuous(breaks = seq(0.00, 0.10, by = 0.01))

ggsave("robustness 3 plot.png", plot= p, height = 5 , width = 2.5)


p
```

![](RSM_thermal_daylight_optimization_files/figure-html/unnamed-chunk-177-1.png)<!-- -->


## **Optimal experimental point**

### Estimation


``` r
opt_point <- summary(anal1_so)$canonical$xs
opt_point
```

```
##         x1         x2         x3 
##  1.0690572 -0.3848121  0.5779291
```


``` r
op_point_ru <- code2val(
  opt_point,                     # Optimal point in coded units
  codings = codings(anal1_so)  # Formulas to convert to factor units
)

op_point_ru
```

```
## RoofOvrhng_S_W          WWR_W          WWR_S 
##      3.7802643      0.0375997      0.2933929
```


``` r
opt_point_df <- data.frame(  # predict() needs a data frame with the points 
  x1 = opt_point[1],         # to be predicted 
  x2 = opt_point[2],
  x3 = opt_point[3]
  )

best_response <- predict(
  anal1_so,             # Our model
  opt_point_df             # Data frame with points to be predicted 
  )

names(best_response) <- "Best Overall" # A nice name to our best point

best_response
```

```
## Best Overall 
##    0.6296024
```


### Simulation

##### Generate a random number from the beta distribution

``` r
WWR_N_random <- rnorm(1, mean = 0, sd=2)
WWR_E_random <- rnorm(1, mean = 0, sd=2)
RoofOvrhng_N_random <- rnorm(1, mean = 0, sd=2)
RoofOvrhng_E_random <- rnorm(1, mean = 0, sd=2)
```

##### Save into csv

``` r
write.csv(WWR_N_random, "WWR_N_random.csv")
write.csv(WWR_E_random, "WWR_E_random.csv")
write.csv(RoofOvrhng_N_random, "RoofOvrhng_N_random.csv")
write.csv(RoofOvrhng_E_random, "RoofOvrhng_E_random.csv")
```

##### Load random numbers

``` r
WWR_N_random <- read.csv('randomNumbers/dsgOpt_so/WWR_N_random.csv')
WWR_E_random <- read.csv('randomNumbers/dsgOpt_so/WWR_E_random.csv')
RoofOvrhng_N_random <- read.csv('randomNumbers/dsgOpt_so/RoofOvrhng_N_random.csv')
RoofOvrhng_E_random <- read.csv('randomNumbers/dsgOpt_so/RoofOvrhng_E_random.csv')
```

##### Convert optimal point to df

``` r
dsgOpt_so <- data.frame(t(opt_point))
colnames(dsgOpt_so) <- c("RoofOvrhng_S_W ", "WWR_W", "WWR_S")
dsgOpt_so
```

```
##   RoofOvrhng_S_W       WWR_W     WWR_S
## 1        1.069057 -0.3848121 0.5779291
```


##### Append random numbers to Rhino file

``` r
dsgOpt_so$WWR_N <- ((WWR_N_random$x * 2) + 15)/100
dsgOpt_so$WWR_E <- ((WWR_E_random$x * 2) + 15)/100
dsgOpt_so$RoofOvrhng_N <- ((RoofOvrhng_N_random$x * 2) + 50)/100
dsgOpt_so$RoofOvrhng_E <- ((RoofOvrhng_E_random$x * 2) + 50)/100
```


##### Data Collection (Rhino) - Simulations

``` r
dsgOpt_so <- as.data.frame(dsgOpt_so)
write.csv(dsgOpt_so, "dsg_so_Opt.csv")
dsgOpt_so
```

```
##   RoofOvrhng_S_W       WWR_W     WWR_S     WWR_N     WWR_E RoofOvrhng_N
## 1        1.069057 -0.3848121 0.5779291 0.1228586 0.1579244    0.4655916
##   RoofOvrhng_E
## 1    0.5249001
```

We upload simulation results




#### Load simulation results and append

``` r
ioh <- read.csv('CCD/SO_data_ioh_Opt.csv')
sda <- read.csv('CCD/SO_data_udi_Opt.csv')

ioh <- as.data.frame(ioh)
sda <- as.data.frame(sda)

print(ioh)
```

```
##   in.Opt  out.IOH      img
## 1      1 0.083288 Opt1.png
```

``` r
print(sda)
```

```
##   in.Opt  out.UDI      img
## 1      1 79.66763 Opt1.png
```

##### Append simulation results with fractional factorial design

``` r
dsgOpt_so$IOH  <- ioh$out.IOH 
dsgOpt_so$sDA  <- sda$out.UDI
dsgOpt_so
```

```
##   RoofOvrhng_S_W       WWR_W     WWR_S     WWR_N     WWR_E RoofOvrhng_N
## 1        1.069057 -0.3848121 0.5779291 0.1228586 0.1579244    0.4655916
##   RoofOvrhng_E      IOH      sDA
## 1    0.5249001 0.083288 79.66763
```

##### Create a new dataframe with only the results

``` r
predOutcomes <- data.frame(
  ioh = dsgOpt_so$IOH,
  sda = dsgOpt_so$sDA
)
predOutcomes
```

```
##        ioh      sda
## 1 0.083288 79.66763
```

##### Calcular Desirability values

``` r
library(desirability)
```


##### Desirability target and limit

``` r
iohD <- dMin(0, 0.193161, scale = 1)
sdaD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, sdaD)
head(overallD)
```

```
## $d
## $d[[1]]
## Smaller-is-better desirability function
## 
## Call: dMin.default(low = 0, high = 0.193161, scale = 1)
## 
## Non-informative value: 0.5 
## 
## $d[[2]]
## Larger-is-better desirability function
## 
## Call: dMax.default(low = 35.23524, high = 100, scale = 1)
## 
## Non-informative value: 0.5 
## 
## 
## $call
## dOverall.default(iohD, sdaD)
```

##### Calculate the Overall Desirability values

``` r
predict(iohD, predOutcomes[1])
```

```
## [1] 0.5688157
```

``` r
predict(sdaD, predOutcomes[2])
```

```
## [1] 0.6860581
```

``` r
predict(overallD, predOutcomes)
```

```
## [1] 0.6246924
```

``` r
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
overall_desirability
```

```
##          D1        D2   Overall
## 1 0.5688157 0.6860581 0.6246924
```

##### Append all results in the factorial design

``` r
dsgOpt_so$d1 <- overall_desirability$D1
dsgOpt_so$d2 <- overall_desirability$D2
dsgOpt_so$Overall <- overall_desirability$Overall
dsgOpt_so
```

```
##   RoofOvrhng_S_W       WWR_W     WWR_S     WWR_N     WWR_E RoofOvrhng_N
## 1        1.069057 -0.3848121 0.5779291 0.1228586 0.1579244    0.4655916
##   RoofOvrhng_E      IOH      sDA        d1        d2   Overall
## 1    0.5249001 0.083288 79.66763 0.5688157 0.6860581 0.6246924
```


##### Concatenating



``` r
# Select only the required columns and add an identifier column to each dataframe
df_subset <- df[, c("IOH", "UDI", "Overall")]
df_subset$identifier <- "Fractional factorial design (n = 64)"
dsg1_fo_subset <- dsg1_fo[, c("IOH", "sDA", "Overall")]
dsg1_fo_subset$identifier <- "Response surface designs (n = 74)"
dsg2_fo_subset <- dsg2_fo[, c("IOH", "sDA", "Overall")]
dsg2_fo_subset$identifier <- "Response surface designs (n = 74)"
dsg3_fo_subset <- dsg3_fo[, c("IOH", "sDA", "Overall")]
dsg3_fo_subset$identifier <- "Response surface designs (n = 74)"
dsg4_fo_subset <- dsg4_fo[, c("IOH", "sDA", "Overall")]
dsg4_fo_subset$identifier <- "Response surface designs (n = 74)"
dsg5_fo_subset <- dsg5_fo[, c("IOH", "sDA", "Overall")]
dsg5_fo_subset$identifier <- "Response surface designs (n = 74)"
dsg1_so_subset <- dsg1_so[, c("IOH", "sDA", "Overall")]
dsg1_so_subset$identifier <- "Response surface designs (n = 74)"

# Replace the column name "UDI" with "sDA" in df_subset
colnames(df_subset)[colnames(df_subset) == "UDI"] <- "sDA"
```


``` r
# Concatenate the dataframes
df_combined <- rbind(df_subset, dsg1_fo_subset, dsg2_fo_subset, dsg3_fo_subset, dsg4_fo_subset, dsg5_fo_subset, dsg1_so_subset)

# View the combined dataframe
print(df_combined)
```

```
##          IOH      sDA     Overall                           identifier
## 1   0.166911 80.54431 0.308338759 Fractional factorial design (n = 64)
## 2   0.178483 73.72197 0.212500451 Fractional factorial design (n = 64)
## 3   0.158333 59.24195 0.258524375 Fractional factorial design (n = 64)
## 4   0.147522 55.27759 0.270404124 Fractional factorial design (n = 64)
## 5   0.118730 71.28322 0.463114413 Fractional factorial design (n = 64)
## 6   0.119295 66.93795 0.432654663 Fractional factorial design (n = 64)
## 7   0.154135 53.57765 0.239208222 Fractional factorial design (n = 64)
## 8   0.154554 41.92771 0.143713381 Fractional factorial design (n = 64)
## 9   0.145729 75.35974 0.390042052 Fractional factorial design (n = 64)
## 10  0.173712 62.75748 0.206852813 Fractional factorial design (n = 64)
## 11  0.189941 49.03491 0.059598195 Fractional factorial design (n = 64)
## 12  0.167538 43.56426 0.130611927 Fractional factorial design (n = 64)
## 13  0.117743 70.40877 0.460485914 Fractional factorial design (n = 64)
## 14  0.158116 49.07045 0.196868753 Fractional factorial design (n = 64)
## 15  0.193161 38.53538 0.000000000 Fractional factorial design (n = 64)
## 16  0.163672 35.25077 0.006049462 Fractional factorial design (n = 64)
## 17  0.174666 77.30840 0.249402402 Fractional factorial design (n = 64)
## 18  0.169672 81.21243 0.293815271 Fractional factorial design (n = 64)
## 19  0.150350 56.67238 0.270851968 Fractional factorial design (n = 64)
## 20  0.154300 60.25205 0.278768591 Fractional factorial design (n = 64)
## 21  0.126100 69.05454 0.425782901 Fractional factorial design (n = 64)
## 22  0.108749 75.03599 0.518225284 Fractional factorial design (n = 64)
## 23  0.145158 51.92836 0.253089320 Fractional factorial design (n = 64)
## 24  0.161183 46.25837 0.167860708 Fractional factorial design (n = 64)
## 25  0.177245 67.86704 0.203755045 Fractional factorial design (n = 64)
## 26  0.142856 73.55453 0.392541139 Fractional factorial design (n = 64)
## 27  0.163014 51.34597 0.197038269 Fractional factorial design (n = 64)
## 28  0.192290 45.96727 0.027335116 Fractional factorial design (n = 64)
## 29  0.149310 63.41627 0.314296146 Fractional factorial design (n = 64)
## 30  0.122461 59.81295 0.372693043 Fractional factorial design (n = 64)
## 31  0.169631 41.74951 0.110691610 Fractional factorial design (n = 64)
## 32  0.186104 38.25836 0.041296049 Fractional factorial design (n = 64)
## 33  0.148419 79.39855 0.397428879 Fractional factorial design (n = 64)
## 34  0.141723 73.61075 0.397228152 Fractional factorial design (n = 64)
## 35  0.096975 77.25874 0.568424725 Fractional factorial design (n = 64)
## 36  0.101720 70.91379 0.510675441 Fractional factorial design (n = 64)
## 37  0.109352 72.09763 0.496944629 Fractional factorial design (n = 64)
## 38  0.091697 68.91083 0.522618141 Fractional factorial design (n = 64)
## 39  0.092142 71.11900 0.538296338 Fractional factorial design (n = 64)
## 40  0.108896 54.32414 0.358579090 Fractional factorial design (n = 64)
## 41  0.159592 70.75661 0.308734355 Fractional factorial design (n = 64)
## 42  0.124408 67.80613 0.423088782 Fractional factorial design (n = 64)
## 43  0.111473 69.87979 0.475628078 Fractional factorial design (n = 64)
## 44  0.148642 53.97161 0.258217867 Fractional factorial design (n = 64)
## 45  0.133319 67.12605 0.390577045 Fractional factorial design (n = 64)
## 46  0.108337 56.21295 0.377145803 Fractional factorial design (n = 64)
## 47  0.117036 55.26453 0.349114015 Fractional factorial design (n = 64)
## 48  0.144567 44.53625 0.190075956 Fractional factorial design (n = 64)
## 49  0.135929 75.66736 0.430084331 Fractional factorial design (n = 64)
## 50  0.152230 79.30759 0.379734590 Fractional factorial design (n = 64)
## 51  0.105051 72.78475 0.514263428 Fractional factorial design (n = 64)
## 52  0.091551 80.04124 0.603263690 Fractional factorial design (n = 64)
## 53  0.098182 69.74541 0.511868317 Fractional factorial design (n = 64)
## 54  0.099614 74.91590 0.544722414 Fractional factorial design (n = 64)
## 55  0.101947 67.40067 0.484279801 Fractional factorial design (n = 64)
## 56  0.099632 63.00757 0.455669769 Fractional factorial design (n = 64)
## 57  0.126336 74.59716 0.458541352 Fractional factorial design (n = 64)
## 58  0.154053 69.11744 0.325453911 Fractional factorial design (n = 64)
## 59  0.142847 60.98274 0.321797620 Fractional factorial design (n = 64)
## 60  0.113915 65.84050 0.440309083 Fractional factorial design (n = 64)
## 61  0.102039 72.18291 0.518771440 Fractional factorial design (n = 64)
## 62  0.140126 56.34022 0.299119487 Fractional factorial design (n = 64)
## 63  0.151040 47.17321 0.200486632 Fractional factorial design (n = 64)
## 64  0.107757 57.19837 0.387219428 Fractional factorial design (n = 64)
## 65  0.099511 75.76564 0.550827030    Response surface designs (n = 74)
## 66  0.090010 78.74894 0.598991452    Response surface designs (n = 74)
## 67  0.111297 68.39721 0.465840845    Response surface designs (n = 74)
## 68  0.098355 72.97912 0.534825440    Response surface designs (n = 74)
## 69  0.104487 69.54944 0.493180594    Response surface designs (n = 74)
## 70  0.093274 74.63984 0.560917304    Response surface designs (n = 74)
## 71  0.117959 61.95183 0.400752391    Response surface designs (n = 74)
## 72  0.103315 69.63323 0.497034844    Response surface designs (n = 74)
## 73  0.102112 68.62591 0.492970632    Response surface designs (n = 74)
## 74  0.101614 69.58266 0.501348842    Response surface designs (n = 74)
## 75  0.101543 69.78006 0.502982346    Response surface designs (n = 74)
## 76  0.100487 73.62174 0.533260124    Response surface designs (n = 74)
## 77  0.100394 70.87649 0.514095879    Response surface designs (n = 74)
## 78  0.098588 73.33279 0.536664689    Response surface designs (n = 74)
## 79  0.097842 77.85907 0.569884515    Response surface designs (n = 74)
## 80  0.097185 77.96548 0.572558533    Response surface designs (n = 74)
## 81  0.096163 75.09046 0.555897692    Response surface designs (n = 74)
## 82  0.095041 75.00836 0.558527404    Response surface designs (n = 74)
## 83  0.094777 76.40577 0.569018457    Response surface designs (n = 74)
## 84  0.093070 77.49173 0.581453698    Response surface designs (n = 74)
## 85  0.093192 76.45283 0.573911457    Response surface designs (n = 74)
## 86  0.091778 76.66741 0.579458465    Response surface designs (n = 74)
## 87  0.089795 79.23680 0.602967341    Response surface designs (n = 74)
## 88  0.092187 77.23921 0.582265196    Response surface designs (n = 74)
## 89  0.086442 79.98071 0.617826174    Response surface designs (n = 74)
## 90  0.094551 79.04618 0.587655273    Response surface designs (n = 74)
## 91  0.087198 76.95032 0.594421591    Response surface designs (n = 74)
## 92  0.097980 71.27977 0.523679653    Response surface designs (n = 74)
## 93  0.088557 76.50810 0.587458666    Response surface designs (n = 74)
## 94  0.099709 70.87782 0.516000059    Response surface designs (n = 74)
## 95  0.089949 77.67497 0.591728231    Response surface designs (n = 74)
## 96  0.089601 79.12041 0.602734146    Response surface designs (n = 74)
## 97  0.090457 77.33072 0.587871390    Response surface designs (n = 74)
## 98  0.089695 80.41685 0.611294620    Response surface designs (n = 74)
## 99  0.090115 76.33044 0.581811131    Response surface designs (n = 74)
## 100 0.088773 77.82834 0.596164115    Response surface designs (n = 74)
## 101 0.089518 79.24173 0.603808559    Response surface designs (n = 74)
## 102 0.088596 79.35930 0.607297917    Response surface designs (n = 74)
## 103 0.088231 77.43691 0.594956978    Response surface designs (n = 74)
## 104 0.087646 77.51757 0.597182990    Response surface designs (n = 74)
## 105 0.087254 77.01310 0.594711449    Response surface designs (n = 74)
## 106 0.086928 78.42003 0.605572370    Response surface designs (n = 74)
## 107 0.087557 82.68728 0.632905025    Response surface designs (n = 74)
## 108 0.086271 78.66263 0.609145879    Response surface designs (n = 74)
## 109 0.087049 82.07670 0.630330520    Response surface designs (n = 74)
## 110 0.086613 79.86506 0.616532700    Response surface designs (n = 74)
## 111 0.098070 81.46817 0.592811069    Response surface designs (n = 74)
## 112 0.091099 78.97493 0.597366364    Response surface designs (n = 74)
## 113 0.096368 80.87491 0.594243076    Response surface designs (n = 74)
## 114 0.092038 79.42589 0.597669447    Response surface designs (n = 74)
## 115 0.087298 75.67243 0.584969883    Response surface designs (n = 74)
## 116 0.083260 77.17188 0.606971836    Response surface designs (n = 74)
## 117 0.088204 78.46937 0.602268277    Response surface designs (n = 74)
## 118 0.085546 78.94207 0.613171494    Response surface designs (n = 74)
## 119 0.086292 78.07220 0.604931377    Response surface designs (n = 74)
## 120 0.086292 79.97647 0.618230931    Response surface designs (n = 74)
## 121 0.087364 81.96771 0.628661625    Response surface designs (n = 74)
## 122 0.094866 78.74421 0.584690472    Response surface designs (n = 74)
## 123 0.093126 77.11412 0.578687910    Response surface designs (n = 74)
## 124 0.093109 78.39834 0.587543578    Response surface designs (n = 74)
## 125 0.090905 76.18637 0.578559798    Response surface designs (n = 74)
## 126 0.086149 79.91685 0.618232084    Response surface designs (n = 74)
## 127 0.083425 80.18285 0.627911963    Response surface designs (n = 74)
## 128 0.088032 77.86703 0.598547964    Response surface designs (n = 74)
## 129 0.085232 80.18154 0.622711566    Response surface designs (n = 74)
## 130 0.088574 77.03490 0.591147850    Response surface designs (n = 74)
## 131 0.084304 79.07523 0.617638403    Response surface designs (n = 74)
## 132 0.086083 78.29105 0.607067362    Response surface designs (n = 74)
## 133 0.087171 78.00496 0.601965431    Response surface designs (n = 74)
## 134 0.114817 80.36717 0.531637651    Response surface designs (n = 74)
## 135 0.089264 75.76610 0.580183451    Response surface designs (n = 74)
## 136 0.087171 81.43926 0.625666985    Response surface designs (n = 74)
## 137 0.086706 80.42025 0.620084823    Response surface designs (n = 74)
## 138 0.086320 80.67720 0.622971813    Response surface designs (n = 74)
```






##### Plot

``` r
# Open a PNG device
png("scatterplot.png", height = 5, width = 7.5, units = "in", res = 300)

# Adding an optimal point (black triangle) annotation
optimal_point <- data.frame(IOH = 0.083288, sDA = 79.66763)

# Plotting
ggplot(df_combined, aes(x = IOH, y = sDA, color = Overall, shape = identifier)) +
  geom_point(size = 2, alpha = 0.7) +
  annotate("point", x = optimal_point$IOH, y = optimal_point$sDA, color = "black", shape = 17, size = 3) +
  labs(x = "Indoor Overheating Hours (IOH)", y = "Useful Daylight Illuminance (UDI)", color = "Overall Desirability", shape = "Design Type") +
  scale_color_gradient2(low = "red", high = "blue", mid = "grey", midpoint = 0.5) +
  scale_x_continuous(breaks = seq(0.075, 0.2, by = 0.025), limits = c(0.075, 0.2)) +
  scale_y_continuous(breaks = seq(30, 85, by = 5), limits = c(30, 85)) +
  scale_shape_manual(name = "Design Type", 
                     values = c("Fractional factorial design (n = 64)" = 15, "Response surface designs (n = 74)" = 10, "Optimal Point" = 17)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.ticks = element_line(color = "black")
  )

# Close the PNG device
dev.off()
```

```
## png 
##   2
```




``` r
library(plotly)
```

```
## Warning: package 'plotly' was built under R version 4.4.1
```

```
## 
## Adjuntando el paquete: 'plotly'
```

```
## The following object is masked from 'package:MASS':
## 
##     select
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```


``` r
# Sample data
WWR_W <- boot$WWR_W
WWR_S <- boot$WWR_S
RoofOvrhng_S_W <- boot$RoofOvrhng_S_W

# Stationary point values
stationary_point <- data.frame(
  RoofOvrhng_S_W = 3.7802643,
  WWR_W = 0.0375997,
  WWR_S = 0.2933929
)

# Create 3D plot
plot_ly() %>%
  add_trace(
    x = ~WWR_W, y = ~WWR_S, z = ~RoofOvrhng_S_W, type = 'scatter3d', mode = 'markers',
    marker = list(size = 3, color = 'black', opacity = 0.15)
  ) %>%
  add_trace(
    x = stationary_point$WWR_W, y = stationary_point$WWR_S, z = stationary_point$RoofOvrhng_S_W,
    type = 'scatter3d', mode = 'markers',
    marker = list(size = 5, color = 'red', symbol = 'cross')
  ) %>%
  layout(
    scene = list(
      xaxis = list(title = 'WWR_W'),
      yaxis = list(title = 'WWR_S'),
      zaxis = list(title = 'RoofOvrhng_S_W')
    )
  )
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-13a05e6dd88e9b5df13e" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-13a05e6dd88e9b5df13e">{"x":{"visdat":{"51ac21e71418":["function () ","plotlyVisDat"]},"cur_data":"51ac21e71418","attrs":{"51ac21e71418":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"z":{},"type":"scatter3d","mode":"markers","marker":{"size":3,"color":"black","opacity":0.14999999999999999},"inherit":true},"51ac21e71418.1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":0.0375997,"y":0.29339290000000001,"z":3.7802642999999998,"type":"scatter3d","mode":"markers","marker":{"size":5,"color":"red","symbol":"cross"},"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"scene":{"xaxis":{"title":"WWR_W"},"yaxis":{"title":"WWR_S"},"zaxis":{"title":"RoofOvrhng_S_W"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[0.047870472989598394,0.018147280422165631,0.032226669490628551,0.053919235527676274,0.033779506309956929,0.033103109905999753,0.018792882167980942,0.032545587040453246,0.037579239714976176,0.021134466371665401,0.045190823927408287,0.045214274492806059,0.023559119215101731,0.043478972263792444,0.024126586657565702,0.040646644054205504,0.03458015276817903,0.033545796077409748,0.034786952233731044,0.032313367854623909,0.042025849235779754,0.048779813652335927,0.0394284323809692,0.048189122575358727,0.039017418092649664,0.030951808420980689,0.045787835947766503,0.042318169771916561,0.05216815028519841,0.030105487169115586,0.045046364105317038,0.039482625403952726,0.0061378614905699472,0.039077402493064586,0.043660725237317732,0.038505843654363874,0.02416657557739298,0.044940575005698119,0.045169158663912037,0.042982389646811517,0.027481764054215561,0.035908024231427318,0.046251355729821086,0.048383981633015216,0.054386724554534166,0.042912906200604443,0.040130244752132213,0.036034883456508321,0.029790434365243552,0.024181754438165698,0.024798519835433176,0.042346363112058402,0.024572833274605643,0.037037333201863622,0.051511072325188964,0.04308209848843108,0.026754567753468697,0.038996507189465679,-0.0023608383908830036,0.038492845803879669,0.047884151581048764,0.01946594248462117,0.05418134910597,0.034932860090379804,0.045911226106998188,0.029920786910647767,0.039216394109260151,0.037237415170807822,0.048857389930708407,0.051353848506430308,0.048839588525092903,0.028026903488660427,0.039676889874972067,0.031596360442259684,0.029405744421107195,0.045055391431057384,0.044253653586507069,0.025714781774080139,0.023850539811390681,0.0095272821777369482,0.051091995991091967,0.0491704846968457,0.035710100074478661,0.034937740220365276,0.039776139843455856,0.047871843597140203,0.051091915037107222,0.052167239362007518,0.033192094245696406,0.034046255482590569,0.046267307811331482,0.045202674119345643,0.034125711047364747,0.037057228115988765,0.034753252528473194,0.050751421021820876,0.050098741895178822,0.04840997398704254,0.038802901841818394,0.044987413368781405,0.033043509285693023,0.055932506657642253,0.044133383848971419,0.045397764047583437,0.036267575490417703,0.04654356898253624,0.03988386381084047,0.039090931085808849,0.038114260169682439,0.050134648889898034,0.019386896282490091,0.038450744752653199,0.041217901734745092,0.054557638344927828,0.04838489641088186,0.048061946988883407,0.049158242449772287,0.021170856257408846,0.036433641233990434,0.036832360485431037,0.03572449436901922,0.032097964102654947,0.036311215643476752,0.031433925429042414,0.039372483612695144,0.048732135424771318,0.048213233429000017,0.02563655258431425,0.040083326402357747,0.041863069685066014,0.03583463547681516,0.014669720587326941,0.04837745484835515,0.028873553272740472,0.033689108614692265,0.024470073426042353,0.038506678881008685,0.038969487259052825,0.047316800476679383,0.039344076255236504,0.040190309742778541,0.034630960645919008,0.048443712098721815,0.048402442139608731,0.03801642093354516,0.027586339172997035,0.027586542427465083,0.047589700034089105,0.032343017572554748,0.037808256547768634,0.040243006800631559,0.014373425864989089,0.015022815642783571,0.044723287369353514,0.022609595671017193,0.042771223517883421,0.029640711619197836,0.048789887196243859,0.063577585095769301,0.030404610423918153,0.048516869230840677,0.04850454611696884,0.048460492079224045,0.038525908769746611,0.0494344353702003,0.033041912972503376,0.043186477096770412,0.051816796203626669,0.043481302378174769,0.041590860133819504,0.02995260620213815,0.034786055056149831,0.047929086489117291,0.040871323923596395,0.029650539951440336,0.038143650630048674,0.028949096353871242,0.039265834181012274,0.039220021892962663,0.034695371602179823,0.044449585527339075,0.035914388219847021,0.03717870154328675,0.018646772484989318,0.028001289504853054,0.039282124022226465,0.051215163866659023,0.035911085894005705,0.043109611181640473,0.040554231292378257,0.024779216881609587,0.04025205638394562,0.022524395705310767,0.052812212177940798,0.03862409151008437,0.032408314354701526,0.039117080313589174,0.033845377013987668,0.051290459213351014,0.03769655489330466,0.036771402176013095,0.040549395583784155,0.027988058481578582,0.047165546284031988,0.030734883239160606,0.049003742842909696,0.057200531052697456,0.027853253334212,0.035193230595628247,0.027207405646915424,0.0470924512196194,0.036883167468539632,0.039808118211059243,0.01144776187234544,0.054103047792469904,0.049003061344137677,0.048236490533966864,0.050998497331077884,0.048475847644290006,0.035534085928705279,0.039825780305630151,0.038128503719903915,0.062500266762135895,0.035995289657157044,0.038621493847216554,0.041213103093044733,0.039101097824751976,0.034432656904438634,0.051101039747179715,0.042583502877962594,0.037127600210133796,0.039331539445131208,0.024922490160407273,0.052909405093504946,0.047482426738634183,0.035421296479640429,0.045120703035610082,0.032695105656768211,0.055153782691781825,0.030716329289437176,0.034647122787614441,0.02695211666357461,0.011765143807297256,0.038415292754438335,0.034090498609042404,0.028603930278561347,0.043605911175191955,0.048070200399139798,0.046374320980528412,0.04129367450518294,0.04653698828281734,0.054130740047705829,0.035408778273367747,0.028510670997232358,0.01902047243177829,0.027501963838103678,0.049607948880751158,0.030289260487614756,0.044362407842779991,0.039453732924465329,0.039973534710216474,0.042634824664304245,0.037271279552931164,0.044641892606300991,0.021626244505796578,0.05079394479942842,0.037441335755225513,0.036234439868241132,0.038226904997627126,0.041172609967217132,0.03829428659156351,0.047722733703164689,0.048215063717072286,0.038645963237983742,0.037306144492993312,0.047104643538017778,0.047542245771227523,0.043598315390608468,0.021243686239141121,0.054180425064697793,0.042742169462032992,0.015422447683245723,0.032490048787618304,0.048347760034292267,0.035712872973002699,0.046308429729839583,0.039276860946840228,0.045224423386952768,0.033975369763174813,0.031606246326195804,0.035173508519672832,0.048944953227329571,0.03970681328817291,0.052719523386646747,0.037997162334755641,0.025423702239992817,0.050833823210259692,0.041458643467248896,0.038590485374472669,0.033079585111549605,0.048597279898714818,0.057383308071166039,0.042231441118136051,0.029216037269671136,0.038940644857721994,0.028333647954864538,0.028577128545013323,0.039421050618582198,0.035122741603487988,0.044844755075027873,0.041949676919309224,0.050772348419782849,0.047559722193968382,0.033207033727984198,0.04327436330342059,0.044855295406083302,0.052774152191473078,0.047638168060911694,0.031037862325187522,0.03452639323889372,0.023048798605623977,0.014110007370756315,0.051237738799475928,0.037745221652710867,0.022432020018590505,0.035426730192769075,0.021762702786778377,0.045268776640422038,0.03203548446999168,0.055722205619624016,0.035935666600512658,0.052793710393854255,0.051265386669335965,0.030961272233775539,0.039309186610685852,0.03490974886195846,0.019955869437129927,0.02823282933582574,0.032962885563379077,0.049647597179063203,0.043926327432140318,0.034607853594931778,0.02494905412490931,0.046144845223147241,0.043910984895881223,0.040469978884622254,0.030084509914864416,0.044246415431374975,0.046372631026644164,0.037682214964858414,0.03602616536650715,0.038731685541688363,0.036486544494733368,0.036415504042950707,0.045387602692139359,0.034255793000496776,0.036534870084037135,0.030368039442363746,0.032632567100860363,0.043292223429405828,0.028462693004421027,0.047553611944045296,0.046338329557851561,0.031603632765028075,0.050532312420178194,0.041016318431311032,0.048238628386567541,0.030134432647547154,0.027115674693399691,0.039294897899864074,0.012361273355748707,0.035101949093684331,0.025656535365197415,0.038993549526167078,0.048773240666423398,0.032338362250106514,0.04263006211432064,0.049828106371441083,0.03613135211692823,0.052134175881005093,0.017623354402042303,0.040042390906771773,0.048730357491871691,0.044720869348601512,0.049391446378152085,0.02805753450042435,0.048888847629025664,0.04463571433374703,0.018962799342246456,0.047410064702065914,0.042097007207376219,0.035520701836648065,0.0015413415087765503,0.040591962078028093,0.033296327512558704,0.044199488821444428,0.036223797802202098,0.046029582031077404,0.024010732442124275,0.04105425575383969,0.051352573141907082,0.037781247929660623,0.03237851379530747,0.027859309666105843,0.040214376451526895,0.048081479026601814,0.042332534066671104,0.034377586794701355,0.046738404858444299,0.045137447096639888,0.0077084352186085128,0.039330129753489915,0.04778239998219478,0.016982266087328238,0.044559866672004282,0.041337191496355986,0.040030982195340334,0.037891309260247814,0.036265596670420147,0.02945618194756278,0.036443189817769187,0.041540635885030569,0.038817485267544645,0.056550404299183295,0.037205461868356327,0.037405942963939003,0.035198888327604275,0.039579975990691935,0.039642568490618875,0.034914633213129098,0.031117824284430235,0.022612709932868749,0.037371142798131148,0.045093727781464213,0.043854558105500097,0.017436570420276479,0.043775773307924815,0.047631996272312425,0.036941761726081664,0.050368261708874866,0.026959173738403487,0.043317359256291856,0.027111881290716372,0.0442194999047964,0.048684256999744012,0.026098831491092794,0.038497260817304889,0.033354618615263093,0.041755582046787892,0.037576045545875729,0.047178326395791219,0.037977245865960914,0.031851414326367039,0.04750778322101646,0.048599258457653352,0.029509042749849088,0.047976162607543553,0.046230759168434742,0.027391862138091038,0.053305379681867215,0.021067517833265815,0.043875771004081118,0.038380851355567458,0.041111566170850704,0.047061361002684973,0.02520949170759337,0.038491170758839247,0.050737106697423304,0.034351348253808062,0.037686723529389496,0.048756810028649282,0.02477726763631783,0.041758577004431571,0.051506483179232801,0.036932643231875222,0.040833434279816179,0.049129341974340261,0.047811168521895611,0.042061834797657023,0.058560360886164564,0.052907908881867256,0.04847786088360416,0.032752057448555222,0.057691213135993222,0.035662305765324608,0.018065368452814191,0.027225645807101753,0.034510547031606734,0.046536690177154436,0.065968982649242613,0.024283114217589549,0.022973349816548229,0.039278114943932817,0.021755189540459165,0.035765654560778626,0.03603410500983735,0.031952402764759608,0.044130757432015201,0.038792715298621716,0.051474935074097151,0.029176868440077802,0.019045876402702441,0.028366781504933852,0.025791802818669204,0.03742411754170006,0.018892078748924002,0.038414128632545369,0.039606271594351941,0.051065124917828435,0.037767217683597365,0.047384093502355305,0.036635605899376769,0.037246781638560406,0.040390974285991935,0.047305503842714397,0.058949615152632688,0.015673933928215283,0.052178279402807046,0.03034176869496347,0.034943373560218469,0.029963124792251567,0.042406649798264147,0.047966070473494812,0.012091121455190902,0.020417980288383736,0.02991305631147536,0.044020737790383208,0.05820996051234948,0.051160545084886461,0.014181377965609337,0.049153422347088555,0.046625600329383558,0.049755647318597343,0.031281588862226817,0.046930687160343355,0.030900568690368124,0.049304110644681591,0.043602264502646336,0.044306557727336743,0.0097354061974112768,0.052576228064784708,0.027385350377422225,0.042679019767655473,0.031053936653678605,0.038809413668069614,0.053979793417456214,0.029336225857175425,0.047350491509629042,0.036540704378635214,0.042987812935533179,0.04814763985131295,0.031560693689268851,0.046673370577417723,0.016675046737067812,0.054082035949554921,0.038664937308050845,0.04594925954602129,0.048217290023421289,0.048961835429198027,0.040228141784648529,0.023982070975655193,0.0090907535962325287,0.028850239581594151,0.054675125979415616,0.021793525146338578,0.056325084275045165,0.05963085762551712,0.045510438575629718,0.047762105772468408,0.027823682571986717,0.035821583709703855,0.024886959704224089,0.035124176019056839,0.03919339790327768,0.040407862709360615,0.040978391088967628,0.031889173589051439,0.026775955410709314,0.041821086728548069,0.0132606222820447,0.025866938426860209,0.04382299756481544,0.018914461191118611,0.033600064665844906,0.033873000012638349,0.040365818255070171,0.034898576011030663,0.04204093976977881,0.044896234024857692,0.04055343521057888,0.049937714212087099,0.060293082713032478,0.039375036932799617,0.057517881944597907,0.02613498131930482,0.028578084753924801,0.055928818987337828,0.051132964008738652,0.042005426621762466,0.043262237447005462,0.037463545406737311,0.038656216707526274,0.02461247569884464,0.037521666942825257,0.043548284349759728,0.018637155887652058,0.048668161204639508,0.050245882985988227,0.017098208892464883,0.030856174561625125,0.03470220980959033,0.017151711137875583,0.043551850022356411,0.048417301006756404,0.047710538035263997,0.027340899174602154,0.020582132069781864,0.047986911677920921,0.043793113538719459,0.038000202888896738,0.039657779768595344,0.038577783172416828,0.029131168853507007,0.054317946651457165,0.033540634067169006,0.043277584398766354,0.038837303570533076,0.045742047659005067,0.031855831466895301,0.03486762415860236,0.039830652357783852,0.04365320060761492,0.043615807013493081,0.0410624882767541,0.032138486127839885,0.029666243681421962,0.047751427636757965,0.033679001599024629,0.040673544741597706,0.040769339018593087,0.043382375002337402,0.042090159773073899,0.033697695744600013,0.045406797914325309,0.038736850455581458,0.044995987685396414,0.020007945605489394,0.039170974490904127,0.018927788711313245,0.03258156028559038,0.025337299842818784,0.022664862344486194,0.042703073035846209,0.038579936230892058,0.015392005229251847,0.047861232782004465,0.041352262888083968,0.036305420967048856,0.045223658310952632,0.044539870930685663,0.034550111539946783,0.034074285513227276,0.048505636987567331,0.045437927167893702,0.015985700985792897,0.043823303125361472,0.036751117242859371,0.04735411839830516,0.046147177644561554,0.040191549811084817,0.021054504332632226,0.047647926205718762,0.038187219652079364,0.028596019349446507,0.031970101680377887,0.037784315253034849,0.058021506963893264,0.026889545310651027,0.043945076371977511,0.033383834381412202,0.037697152545291351,0.053473644340805065,0.033935299432357444,0.031608383676786041,0.050677376926369179,0.041910808231770111,0.032185116327667997,0.038796553986127795,0.035429788882819085,0.042459506297782966,0.035242032234851943,0.051672769138206176,0.052544283052675417,0.047250537720817679,0.039849175586922164,0.040911216503180782,0.03159209429588912,0.0043862614141024631,0.042957811393414091,0.028034805263509533,0.032067619033975495,0.048114532519255272,0.023561160209217034,0.030598054215147066,0.043026928106032002,0.036946013748517033,0.063208755470354899,0.021488766757904561,0.028407866788538155,0.037853549769970393,0.037058035687644562,0.022139756628171949,0.049295152893173051,0.046584916117959646,0.029038773611456445,0.03975835922411261,0.035091432984482619,0.049039732427065649,0.045649350171903434,0.049179542358127527,0.028003641312646786,0.045844314547640423,0.021307446576800233,0.043764382877364526,0.035139578366895879,0.041872721295478005,0.046787016867528806,0.049497623127890966,0.03513816295782992,0.041905757470167025,0.039208662562237921,0.034311562133173427,0.047341459900559199,0.042020429268645298,0.027112352792279495,0.046134225158588969,0.033701747067568288,0.049583530135865486,0.039869917165975863,0.040402952354949256,0.040431815044590563,0.050302512879824693,0.042921155992257834,0.047883700969312666,0.01541978601523275,0.032801276445429212,0.047755578716857448,0.047852898469592085,0.0310637104595776,0.029195754513382011,0.045310472679154987,0.046773968568504853,0.037790748170560391,0.040301101026593344,0.031654055896991495,0.048301200757919383,0.032406606779761232,0.033004151761670861,0.050763917662688389,0.051876180981878531,0.038379618409355637,0.038401294093312006,0.040083777949719332,0.033994356374903764,0.037836636298609304,0.026226060619146764,0.04149930954841128,0.033540724600987282,0.033500606204527279,0.046456022139236851,0.056515558719628087,0.043469559204555142,0.025197249221286824,0.042573277981165544,0.033499587871449063,0.046627445869808319,0.032924954510000472,0.040332918581034563,0.05150434583077506,0.040193680737387757,0.02844873576459165,0.032177866000162952,0.055388322738887052,0.022626417562543726,0.047333078417691452,0.051343908605115625,0.032267987534915214,0.033095670069700857,0.042372841832042617,0.0492328376694341,0.048453710946359876,0.049101049275380927,0.045643705777355326,0.026582378420851895,0.048238369300895013,0.037719543339472875,0.040153461185322578,0.023077538887517565,0.030326083240889829,0.048685839799161711,0.041113783827670648,0.049262794840429128,0.052306292200600978,0.04106172384024831,0.04873091874672595,0.029170363200885032,0.042051316040738876,0.034894154675650005,0.013288904135004269,0.051606262791418049,0.051152434056363882,0.034769723979577602,0.025584091294588076,0.032585655010115185,0.029133960749360435,0.045304282785494691,0.048200591031086773,0.023507061775198745,0.034589201648842084,0.047928299490038552,0.031406873003820029,0.036304926095532944,0.052025385869996714,0.030334909121946061,0.040814362150398589,0.037177841171027043,0.044200904714446008,0.034909327533540598,0.014540404688456611,0.04065137486615282,0.03816547485389514,0.024196349513322904,0.027475492321563655,0.049395890453404173,0.037763965510772782,0.050720014989215932,0.04868509219387282,0.02135575801982045,0.031016216154286711,0.038352436187138517,0.05989108056368278,0.042896883807352133,0.03667794730903387,0.029083687797969656,0.047645088286785112,0.037487251746099445,0.050820966412017901,0.036922425901517592,0.039090051846954653,0.037372448780633845,0.043613180565303389,0.024509936534737979,0.045795750551916564,0.034552508389618312,0.036202999242101477,0.021502123909450638,0.044604059296829819,0.04803710442690641,0.035481055006597227,0.039664207957432376,0.015904015347286388,0.048570333993836985,0.041246581656398836,0.035999273503050731,0.04730807420917988,0.044261294469353618,0.04018112781124162,0.036468968930961576,0.033224378187706571,0.019940354400569016,0.046146881451007045,0.048215311354472677,0.029356394028280376,0.039638589511058801,0.0072744068244460086,0.048613359241074593,0.046419504477047575,0.044794734365791268,0.023399266614211225,0.024887064739752506,0.043444189497568758,0.040123150666307443,0.035292685745349994,0.044265120425656196,0.065924250985992167,0.031651579105859237,0.052146331347535803,0.035431351215540585,0.035330442849116531,0.03501798039701047,0.03314239113001459,0.041558587881751645,0.043642668187347487,0.037645261989805057,0.036405519600438592,0.049466316825083095,0.04244214633631381,0.058007832767001437,0.035911521715174727,0.03172051111059343,0.041544153992167356,0.034183060201965175,0.035805512083003074,0.036808845866370031,0.054207926600846001,0.039878133705398604,0.057786040080480591,0.0033898346642515093,0.048650649339598173,0.03901129556399565,0.03701087999435005,0.045648353977421785,0.030428011768758874,0.050697332226223198,0.049882377303746346,0.014641966809593135,0.040198269060533399,0.047713171992150354,0.044294329430635632,0.043869944723586969,0.043177904709025335,0.031206067882895261,0.043503226383878872,0.01951734144441555,0.045648270115309135,0.043513707900635037,0.037270437526430229,0.034174470523493115,0.048680609376927864,0.046360508885971118,0.036182666524727611,0.036110519247091515,0.027877293849685614,0.037812606642776428,0.03426335061385373,0.063864691789257189,0.034003324810481697,0.0186207566827719,0.045158386864034943,0.035636845732582259,0.035528673698072152,0.027316974946444181,0.05131867444579509,0.042386080079078972,0.032490320920066158,0.033454463380063679,0.050899649384535291,0.049976827030855521,0.038184692659669485,0.026964450932542905,0.029817103817758876,0.033065429297521404,0.025077269962307289,0.037849618155656725,0.049187966785372433,0.035957163117362509,0.015104353229136148,0.036795488990539638,0.042873430083193713,0.032043308980074937,0.043639260300564565,0.048721688309653018,0.037664867523478376,0.043202835632696862,0.044036525220814832,0.045101177237271528,0.058728115144658338,0.054358839236294469,0.030419734655833482,0.04925727701380047,0.047401913065063021,0.048202399076042998,0.050634139397449957,0.035484457249678872,0.025526131654092763,0.031476966537148959,0.04904355176886073,0.031718184695743834,0.030607929853596082,0.03786337538790964,0.054245407549299678,0.028270757743831934,0.05628605948578265,0.049131744882435109,0.039851076913464144,0.02903451666619113,0.040342331607655844,0.0051372301254498792,0.027862586385054129,0.040812772333672975,0.044987009347690121,0.03473109567015667,0.025431561451054973,0.033925161034894874,0.047597316535784868,0.05874573645071085,0.029721014129161276,0.047210386342966322,0.028854670433247749,0.019467059425425461],"y":[0.25727028834239657,0.29194329897815641,0.2936073391945701,0.266390587016963,0.3038045429534042,0.28221179034059268,0.29897180118761563,0.283942190915978,0.28272851458127712,0.32688011086940449,0.28724236837265887,0.27783125444768092,0.28022122555315732,0.2938776697629899,0.2935260519391279,0.27742470287894994,0.28347533460717178,0.28336922698492223,0.28037479844988294,0.29581422405915209,0.28021857945810591,0.27544522086702183,0.29883891721593203,0.26712929759061638,0.28526323857861757,0.28148198652526324,0.30246869974094243,0.30054511295917108,0.26809930918826214,0.29395960458123466,0.28260534858723774,0.28209588199393199,0.29678618038418586,0.26008018442210418,0.29382674963231259,0.28607687436117124,0.29376180588002765,0.27869711768536309,0.29920822726548757,0.25915396835347815,0.30871419390020993,0.27763146993505416,0.2571836616466508,0.27523161948261388,0.28090079419924258,0.27812896338360427,0.27439730345468655,0.27525623393455995,0.29460164341688777,0.28003508338865718,0.28930980842581766,0.28439272450826286,0.30559047889762175,0.27914167213473795,0.28679334614546398,0.28620299600299892,0.29067132903912174,0.3178241572675588,0.31856356030172683,0.27210441065227065,0.27742139718362108,0.30101033475636579,0.28737298748909446,0.27691903819190639,0.29237277700538666,0.27670721090025407,0.27062760983629963,0.27678344136765859,0.27362233493281696,0.27541326567172958,0.29641630476262004,0.27665072929341239,0.27518748034723556,0.26896204328012063,0.28201242171201696,0.28057902884321151,0.29408766706731532,0.28765102226624079,0.28161309045900945,0.30901245448295239,0.25896760457851148,0.31364726962071621,0.27492364484683385,0.28302115921745769,0.29261566653200916,0.26167222119155853,0.2792179949147986,0.2768309010107578,0.28298625617851025,0.30503380653156625,0.27946869390700591,0.27005510264304305,0.29179368131936345,0.28580520365514905,0.27289475790901263,0.26493656331549792,0.26728631637987621,0.26915118088395423,0.26436951794947988,0.2913771746802537,0.27048814010151928,0.30813591723971806,0.28990129430017525,0.28376888312240217,0.27876652485348313,0.27668452106593266,0.27746215581383449,0.30105367673226829,0.27909493120062556,0.26825650909831056,0.31109812428141415,0.27823621396446224,0.29447658240083674,0.28782517922570622,0.26039947879190944,0.26713254506353584,0.26443835800991866,0.29729556282241743,0.27653321303022976,0.28140787726379324,0.2874862438704896,0.2895684463137046,0.28851060298782083,0.28099331873203021,0.28025045331623349,0.26150522886174493,0.28479354430735399,0.26336205862700263,0.27940486817075161,0.2719973748700138,0.27593331177757918,0.30031807146522738,0.2726822877719916,0.28758028692308357,0.29190640996282813,0.28576518801241702,0.27208364725304857,0.26715412496189239,0.26005656344065753,0.28928624964759331,0.28346707850573521,0.30333545924051408,0.27989852711799679,0.28729191918184188,0.29220076279836016,0.30142648153035023,0.28269911413053694,0.26813696441225687,0.3079254729380394,0.27554170475661216,0.28065390742714835,0.31610919135581317,0.30799011334579973,0.28539238797583721,0.28784890325191848,0.2716709806407388,0.29771045627782833,0.27357144546942258,0.27995286855523094,0.26970308716684543,0.27775566826319403,0.2728559941067446,0.26321936947100261,0.27836909944128674,0.27497519643007329,0.29877456267379221,0.28927054039516431,0.26408940196203307,0.28582849502821955,0.27947948764402292,0.30532595914528848,0.27518957036345482,0.271441962800406,0.26517886310717753,0.29297905126481211,0.27870442090339292,0.28192045717888642,0.28844313367063734,0.29764688477495072,0.29606676091725903,0.25647263295921047,0.27697598449294364,0.26439116652154171,0.29427625816929442,0.29644283202044736,0.2836031174348822,0.27309278478231491,0.28897616483665678,0.28433290392149979,0.29772726868624488,0.30217437930682883,0.33173954169121017,0.29281087203022294,0.27434745690721013,0.28387355572892087,0.28763651063095408,0.26967267604219536,0.32856230457806535,0.27243990119495631,0.27895586615356532,0.26787176898459431,0.2822502705105206,0.29697618821226035,0.2786490097933168,0.28402060682257274,0.29105428007648904,0.26918270031991787,0.30576099843712679,0.2889705691432996,0.28749388799091813,0.2610414131824414,0.29054794758684305,0.28727866205786884,0.29411773793080054,0.22442014886608372,0.2716094587697665,0.2698356243334028,0.26969932829382265,0.26454036665001351,0.28479452843384317,0.32109542846735328,0.28832554155705536,0.28413825402836335,0.27194528735263224,0.29589756682665158,0.27681706036005438,0.31439690671806308,0.28741729212324646,0.27827238424814577,0.27359308504757979,0.28299095104402228,0.27440136580048841,0.28820452297325649,0.27483412935128193,0.27802021233636665,0.27622693890227146,0.28675688214234918,0.28617699107768146,0.27469788211140161,0.27944085025193849,0.27844271722472214,0.28786976459267666,0.31402090858141923,0.32431204457821566,0.2874256357544609,0.29301633946812045,0.27116419510747075,0.27134077836215453,0.28628090227735725,0.28454529827772501,0.26583237230746071,0.33355196133268578,0.3039999832058507,0.31819614349089642,0.29383564668216056,0.29123124128535793,0.27280771544499643,0.28461484095829359,0.28199775958442719,0.2916606239997,0.26824993092182653,0.2764881530872953,0.27629965873336992,0.27589750672971258,0.31498097424000143,0.26063081484370632,0.26099996080514182,0.29904222270276232,0.26808737106924047,0.27258443836984902,0.27383776744855487,0.28973743805773722,0.267623792948529,0.27794231222253624,0.27605572377060805,0.26852452072616573,0.27260301906256157,0.26823064145777198,0.2987623826091369,0.28001993101410522,0.27598019267670737,0.31583319588490499,0.27108208670651596,0.26932931994174941,0.27918115301783647,0.26860373036455942,0.27591485285548351,0.26676005995015933,0.30405062673048022,0.29793715512482438,0.2903378164122723,0.27775228951903347,0.26932574245889929,0.27259993813875083,0.27233825907396808,0.2793581188033798,0.2690599436729687,0.29483002484616422,0.28630451724862804,0.29180074438690712,0.2706808064414219,0.2892368313528314,0.29374562166190765,0.29212264373349195,0.27541394736253072,0.2814999290883563,0.29156868861008023,0.27765507954667001,0.2756112342528429,0.28424333418784986,0.30127417788197114,0.28571200769810773,0.27501955731622146,0.29009914423227123,0.27210748485819891,0.28218478803107622,0.26868023182723894,0.2677349009804732,0.29729785827273891,0.27963842637945008,0.30044150249025536,0.30416611727648735,0.26780400428912893,0.28535509385453689,0.29585789553569652,0.27309045481021837,0.27891964701583288,0.28502639328856672,0.28046616406650088,0.27735699156537458,0.29687946654213965,0.27182043219851187,0.27784322455009675,0.28087892891049371,0.29080397788985018,0.2882757370226855,0.2948870394467934,0.30500115009743273,0.29578939318735586,0.26748897786964809,0.29549829824704188,0.28372518402183866,0.29886323074709337,0.27769939138670335,0.27760760163019388,0.27672120812716972,0.26920850858356193,0.26740726351113714,0.2809396073984573,0.29825031223144838,0.27284543829554581,0.27526638247122021,0.27542812364999575,0.30284083328954514,0.27998407406398829,0.28384586601032896,0.28751437608852087,0.28812512584831335,0.29583704957988133,0.27321759265044759,0.31364945028105995,0.2721521627444537,0.29571208784053926,0.2818318548830635,0.28577112098645507,0.26819662399219368,0.26241956929587706,0.28379306546403038,0.29877819569290104,0.27511204837540842,0.30229351518594311,0.26903215683924342,0.29170519963042774,0.29554235976856802,0.26708629650297416,0.29506701812237573,0.28062777496720803,0.28218011602010568,0.28360614691842245,0.28942456810273409,0.30532871496527136,0.27530390773448238,0.27166034071196354,0.29163226406772169,0.2642101514765437,0.30449792353469846,0.27268660628346353,0.27893744169745477,0.29330522231445799,0.27013154868193806,0.27471649283084471,0.27169455958829525,0.27395221345537935,0.31760073137635236,0.31696535585446883,0.27852462514355358,0.28206150852199707,0.26866555303357281,0.27315686378826365,0.28967719040941653,0.27658693326206563,0.27575770163433438,0.28511201573970929,0.33430283215807133,0.27708914699086717,0.27072718476989438,0.28442561608678812,0.29975527172219973,0.28927752843318788,0.27840858268068691,0.34834908441880069,0.27802469187666112,0.27112280934046901,0.30868118193999589,0.27358840147851377,0.27739279062931488,0.25760086675710753,0.29372217084319885,0.27466097362880837,0.28756430460247395,0.29879051321606975,0.28443761931674866,0.27026905611865509,0.26280428060896432,0.28552700369267675,0.27660621806106056,0.28268906638025187,0.28369352143572907,0.27187832401253825,0.28616232093276034,0.28845538852558072,0.2832330990106709,0.27789315460961328,0.28925766022287974,0.26329828931814497,0.29939723005095359,0.27032306877018353,0.26900409227911898,0.28199208794651082,0.28349714004217386,0.29339554726407147,0.27294620035893946,0.28405934582222592,0.27334547596191849,0.26682652721600303,0.34212055078137354,0.26084211330128604,0.27872270364524548,0.27947594630103317,0.29938570484408716,0.28869889924079184,0.26952711301671378,0.29288208585944808,0.26867971470879343,0.26686357975418928,0.28845539273793913,0.27700713786918724,0.27777334278855548,0.27040200853437335,0.27323391620594095,0.28630999875288549,0.26912055668003404,0.28884425795036484,0.270834072788951,0.33307748591005953,0.30289923200598656,0.27438879663475324,0.27774831618591894,0.2857478160275792,0.27098557825062985,0.26385854035518297,0.27436570255988002,0.29812271726833894,0.27200556977930823,0.29121835798834517,0.28079008667464916,0.27229615343679431,0.26681069401779012,0.27376730247968117,0.27025531066997471,0.28103284668950845,0.29836776084124328,0.29005485434888673,0.29698648302270758,0.27079065188505996,0.30131772190623091,0.28979739165832613,0.27431657236602353,0.27229689393301071,0.27547347130665989,0.30382650341652473,0.30105566741199785,0.27296449202215484,0.29653635151542673,0.29380131932617515,0.29348473813129594,0.33816921948791917,0.28614334521683849,0.27744702817037703,0.30678825956831002,0.3061560261674578,0.29133493289449269,0.29344686923629654,0.28597248420025467,0.29779820036632881,0.2990683816123651,0.29040090267348728,0.28173159776224604,0.29676136553861893,0.26836889313059648,0.26698678789330998,0.26719795336919527,0.2782812364776544,0.29103960390356098,0.26606951580396543,0.27602914287145219,0.31688094372224296,0.28773709510861412,0.28689824971509226,0.29024804210976696,0.29127601333774167,0.28033312615634731,0.26782945068812702,0.3087901157420182,0.28727541663518036,0.27652784202876085,0.26993190775256121,0.27767824678157804,0.27609783925478487,0.28174567214404639,0.27770356531660423,0.27394909765264175,0.30350337452642917,0.27732605531450238,0.2628657340527869,0.27674592726274744,0.28172849984415038,0.2791566142657832,0.28143161746666689,0.30006915964434155,0.28434873137221561,0.30057899095875967,0.27156293115818225,0.2728892237747565,0.2740284284696945,0.2720531357918301,0.27993233169987164,0.26614825787295193,0.27181262842155229,0.28101364325476275,0.28529955410810776,0.27525361045473234,0.27021756394092,0.30985878382469445,0.28635621151494572,0.29051242929780074,0.30135790022911818,0.27122031298844379,0.27963832339847583,0.28086216828103455,0.30652846079357787,0.30935335148709797,0.28403476524278481,0.32649348018390784,0.29728371544397814,0.26579230947897814,0.2980659862073653,0.29011961914348616,0.2728390299091572,0.29030134504073946,0.29533737101466506,0.28394952898744019,0.28251778790403975,0.30405168600084631,0.28146064039376295,0.27176194802673947,0.30030917084038383,0.27843433794896,0.27493303429479576,0.31832509048429053,0.28980475204704093,0.2734402545108125,0.28249925827509914,0.28603008778319694,0.30946531261490973,0.28785963772915701,0.28450671767305308,0.27986515100987852,0.28952378431685583,0.29051461598800682,0.27946115962438112,0.28142898039194042,0.31046971067691076,0.27764484189020722,0.29927080854443172,0.29098627291008961,0.2864886748367334,0.27535301513956578,0.28165425212122092,0.29200619230635216,0.28103894945372693,0.29409086761947095,0.30543902665257222,0.27085469812443258,0.26909419264521539,0.29855676268874642,0.26118442494372901,0.26726101403962071,0.31618487284015628,0.28758119636963175,0.27616095901773752,0.31578142857478742,0.26454725586491512,0.26299604108223867,0.26785636848914124,0.28350474987624752,0.28271354499444123,0.25558781421886706,0.27156647306462028,0.27895584369800608,0.30681140142985802,0.27283073763465066,0.30136241156808491,0.2755091354420181,0.27202481313431814,0.29607168055448096,0.26226586680088365,0.28456519470969321,0.29383794174439576,0.2879546558947203,0.27393527139362922,0.26781577091215059,0.27508561719592572,0.26323793079931856,0.29345559464819976,0.28324478594638935,0.25876772851229435,0.28506783073083652,0.30125063121815943,0.28426310934150478,0.28738883654399083,0.27965330172215824,0.3046906850103539,0.27334702712831316,0.26934668116588439,0.27804106274039264,0.27696190763815992,0.29315847631467978,0.30647356419505556,0.26787922005826775,0.28982243312890593,0.30260731509857497,0.27081090829869031,0.26743540547088718,0.31208898917819572,0.26077798749452313,0.28688657851963656,0.27888342581657216,0.29677116534819564,0.28241566112859978,0.28948591833379239,0.28242229811267222,0.26547545767281278,0.27550429260324361,0.28166160267076251,0.28772623746826864,0.28035597487727082,0.27995936743562244,0.27907222803353854,0.29691331183834824,0.30447335982076079,0.26271891405246828,0.29571912303906234,0.28592810303576743,0.30179453301700243,0.28136909682645517,0.2824118807236754,0.28967921399134533,0.28261959862341268,0.29020551663301719,0.28647384898382977,0.28791646802619197,0.29098919533404916,0.27416055957788577,0.27371613639105724,0.2690368653828743,0.29040213885156491,0.26659433521971643,0.29156997031203796,0.27563002731629738,0.28045079308483328,0.27683819771190193,0.27209209702060289,0.26716979181444761,0.27531020644866278,0.27071755446290696,0.29310780305141348,0.3280143440209915,0.28338438604053001,0.2842533952849145,0.29972829551052366,0.27202350460519847,0.30242670218127499,0.3033489357433003,0.26891538874210785,0.28074394599435726,0.32352586365316727,0.28950474109360991,0.28861899032752619,0.29480618357584798,0.29962187704822718,0.31182810566840652,0.28063624751734206,0.30509836866533813,0.28739852053725262,0.28926328578678495,0.28515120744569272,0.27723002302290284,0.26875642592360588,0.26238912123146929,0.28627240928489234,0.27275586102119448,0.29702762821711864,0.27061004652431864,0.29417415343445985,0.27162743722441018,0.26795331613089457,0.27854912612324267,0.26945957648048846,0.28410686152421005,0.28503079461559039,0.29687497008100711,0.27539458391371524,0.27815999297412575,0.29250538777900598,0.28113756943910267,0.2833958557036772,0.28255821431253014,0.28667294941761012,0.2686622719185271,0.28506514551115736,0.26917514768417994,0.29799140178444739,0.28061521428001768,0.25248256392880059,0.2842457349633386,0.26328090724838005,0.25926174547986747,0.28996366828718517,0.2932232362908298,0.28596014086457477,0.26716071828642513,0.29323329364633277,0.27125178615999773,0.27828953325590844,0.27031437908480033,0.28788904226457351,0.2687973032658898,0.30035348955168728,0.27303997114020589,0.29846031112513016,0.28305472235428758,0.29009518334279433,0.27199370061804901,0.29248463611857134,0.28135778483430385,0.2884400817123578,0.26899984782484632,0.29333154445408161,0.30643385808000084,0.26780988221487778,0.26672248109988872,0.28055549324064522,0.27753134951187669,0.28047670347398368,0.29053413652189108,0.28785893669103102,0.29144463417827293,0.27868055126063807,0.28822399598523396,0.31205128531238618,0.29222480772614562,0.26692400965285445,0.29276756793477454,0.26948650886800152,0.28063915858519756,0.29071239179037822,0.27725742091578298,0.29352435448871228,0.27777353139188127,0.26820827206148656,0.27411045026773306,0.28820588867414287,0.30326288711267047,0.26967068745928069,0.28163055464172315,0.28177507821938907,0.32519830270254246,0.29084472270049005,0.26815279998467217,0.28319198375034205,0.27661145465479819,0.26440966933090704,0.29576725448820074,0.27615218134153441,0.27910273631734445,0.27986592372854591,0.26962464228072797,0.33532639812973364,0.27815582288055463,0.27536181448769248,0.26522389134532903,0.30760819278100882,0.28555358448373103,0.29047972086151003,0.26727354785021396,0.26567184881796513,0.3122078416729418,0.28196794688805449,0.27629546615518552,0.2986497736548227,0.30332016667385719,0.29082779741680043,0.31680890753186919,0.28123856787709578,0.28135716956529877,0.27466851814558141,0.26185933352142449,0.28409466800958932,0.2700421946731682,0.28227508192835182,0.27403423670035421,0.29365081591359649,0.2619241119705169,0.28428627540951396,0.29233650258900268,0.27215694951753483,0.29495093128398053,0.29210742526704314,0.26991102509143411,0.27266970702266224,0.27134190905718203,0.28432873399438924,0.29296682558088472,0.27449240215002096,0.31779931856729471,0.29479451977994464,0.28246177204336875,0.27555995579788317,0.29046455413274208,0.26789153761870638,0.33373290026887975,0.28435964469334191,0.27825116821752904,0.2953907231939501,0.30827637812343467,0.28912448658475809,0.27615293024696785,0.2854168674300111,0.26914660689492342,0.29446312208982961,0.26822885000251623,0.27009703309330357,0.27697335256489392,0.27334085891277632,0.29407548782753196,0.26945591081381415,0.29109004244547854,0.33286053907631719,0.32034849978752516,0.28709589821009851,0.27354677007915373,0.28605152141496842,0.27653128384170128,0.30630576394423231,0.27113976873419121,0.28843738563385168,0.27909141930968479,0.28788857510607929,0.30532206191733052,0.28247272837869408,0.31055795729294394,0.26816868543243344,0.26742116179380088,0.30742264573623118,0.2953358775124707,0.26415969087231345,0.28875714858052315,0.28922704328756882,0.27376894690928966,0.27340713260865956,0.26964333516168276,0.28547164282349469,0.26998671469157531,0.29189183920405204,0.24602301130397497,0.26873633885486925,0.27258681368821513,0.27343204444737618,0.29662725376389409,0.20194945154018937,0.29067477438976325,0.29474786533398278,0.27461493345110977,0.27503083347974072,0.29561396425594472,0.28589529204083197,0.35040279617980069,0.2756943140576299,0.28393862536873959,0.28799468994118621,0.29756898048448938,0.27636763365272587,0.27799681370270102,0.29047901570046009,0.29013876209376749,0.2781771130555184,0.2691073924373214,0.27332532968792528,0.29045261618922219,0.27139106394916618,0.2964106457946501,0.27994391037604605,0.31534255853878829,0.28722143015151025,0.27500113466910764,0.26823148249650203,0.27583066289498476,0.28631496304082371,0.28151532933484325,0.28467105008933513,0.28025902044244583,0.30946609228722644,0.28610417577063035,0.2888476424235657,0.2822908483343749,0.27063400633644646,0.31460345725292643,0.2753200508822064,0.2872844673639493,0.27229162676266822,0.2799826650559869,0.27053398803166234,0.27345381065412439,0.28469284661983169,0.27385922026103121,0.26615598685419711,0.27005849258031189,0.29281537541690783,0.28446961554861727,0.28462334846700393,0.30351668921437919,0.28747899013997369,0.27614264605042693,0.26536085265415571,0.27239210470734765,0.28514542441595525,0.2727345522264556,0.27816555920093727,0.28276149977035936,0.27790990543300198,0.26982699878001359,0.26948889734999509,0.27826729919796622,0.27231230883690594,0.31115372313861578,0.27589983756548558,0.26941018433778618,0.27690330334067781,0.27298033580348069,0.28341091428028697,0.27353144579591482,0.30707861136089259,0.27460380728380951,0.28617327610198806,0.27787551520249504,0.28490084570156643,0.29590038398250151,0.27982263963346715,0.268811728971251,0.29651480539359271,0.30665854804918008,0.26686339014619564,0.27421663862714035,0.30427989588340881,0.29374613985495146,0.28601749148160494,0.33087756634832871,0.27834251695374546,0.28272328753909842,0.27259733603944725,0.27366158147715308,0.3337550724579601,0.30888841610274026,0.27523488375398375,0.27876927691798847,0.29214524029882255,0.26952586085639496,0.29230028341909653,0.28616749139304898],"z":[3.494710258212514,3.6616444199908775,3.7839211261013768,3.575809152373322,3.8951021537889008,3.6469627564840148,3.7224752296127024,3.7879491996323442,3.7904200952223484,3.7028163497749227,3.7152140634720734,3.6930574326609329,3.6050278916046383,3.7615770631312579,3.6270443942793249,3.5833071997379862,3.6350013233420309,3.6937774700637727,3.7614521270743375,3.7566494737443774,3.6042211615776707,3.6878967982938677,3.7754870082608059,3.4923975136622931,3.7224589890516602,3.6104492323611663,3.7700453700114056,3.6969019346353122,3.5175718973491721,3.7032640057320729,3.7375285576146524,3.51772478058755,3.6654766291719958,3.4963773745589197,3.6605810255148956,3.623313023336562,3.6386338150410973,3.6691340529731611,3.7726428253132771,3.4916835370830759,3.7231937181060339,3.6037138607410877,3.1756581713512597,3.4993674862613604,3.8197670742914895,3.6426453148810634,3.6745851752772758,3.6423094898965722,3.7516032480526791,3.5224746569512906,3.3533248193791731,3.625693306353583,3.7210348304880463,3.6086930347406505,3.7725916700789561,3.6857129540353393,3.7815500843758025,4.0134191789255107,3.8080457169393371,3.6075430021664601,3.7003092091906735,3.6841905058940854,3.7513269563513645,3.5632233121991277,3.7391628064477658,3.5676166801646083,3.5805650088466785,3.555126513529911,3.6460107140738161,3.5341443246746134,3.7833153853660977,3.6892677617644569,3.5091307383403114,3.6014272848590414,3.6640925385351499,3.6099684010872291,3.6525029505273272,3.6503344976004248,3.5710522061297452,3.8115815655856808,3.5237781079154979,3.8410058724459186,3.4848606202279284,3.6976898171886288,3.711616447018339,3.4994676934146782,3.5741136743309978,3.5119545382482116,3.6104261845353323,3.8258330836741776,3.6559023524005765,3.5169235321584695,3.783881693673786,3.7221556943271996,3.5217555690314044,3.495763311574553,3.5074875735893545,3.4952929787593936,3.4957125457681979,3.6961768102460946,3.5198867946870518,3.8447343844866366,3.6205690195586056,3.7671670832648552,3.7058939946256961,3.6326237366461798,3.6935815588379559,3.7291437489464472,3.6301862080785448,3.6356922147465975,3.6237656353295145,3.5388714346262664,3.8160731588208074,3.7508016273892326,3.501724080706563,3.4975145937342504,3.5015053973824659,3.5901743333735401,3.623813887538506,3.5146694205448901,3.6202783471537803,3.4322087602049494,3.7263541285309034,3.6102797330769514,3.6633890622637941,3.4994177845961691,3.613253150274284,3.6342518146548106,3.7196051693491543,3.6228656702136104,3.5499031303787589,3.7180414512972533,3.5086752577866047,3.5752200506594365,3.7685950557509704,3.6067502583327924,3.5433866614847065,3.4806893800577279,3.5002180670905703,3.6322605145971174,3.6757749358142786,3.9360008006566396,3.7275329757729843,3.6515501236208121,3.6947752903631379,3.7803315152843955,3.5305432069184874,3.4945374037861017,3.8881355275521043,3.5714444651679802,3.6499106525066365,3.9331996475887978,3.9162779341516698,3.6886491913429715,3.7267231160085705,3.6133397552160322,3.8714960578880273,3.4975112302842013,3.6596863573375451,3.5901073249620059,3.6310735321333478,3.4990893831719316,3.4936787956923028,3.6432131731388879,3.6831324494241722,3.7184471967580732,3.6346942421430377,3.5194532742995484,3.6717397067948014,3.4833591924186647,3.8275608467032516,3.4726412685998516,3.4964073961042192,3.482545441583468,3.6373574070824053,3.5553427194095879,3.5009019813203013,3.690834282153693,3.7519535256450092,3.7960388362148194,3.477463285526567,3.5898223288448192,3.4847396610464489,3.7340341070250047,3.6436933016603215,3.520370936614714,3.5846715929926898,3.6487348585566322,3.6370966212918634,3.6995854696829129,3.6824510017310041,4.0112657814954691,3.7003287830710319,3.511229864426364,3.6899385534051579,3.6651545214141859,3.520422022973634,3.9537100690310352,3.5896760351547328,3.5822814532373926,3.5304524285035739,3.6431991043038954,3.5397749300757089,3.5084027526545256,3.5854176438384364,3.7151443690280876,3.5279301355269719,3.9717590920483761,3.6948709026310858,3.6792240547525452,3.5012812307858119,3.6346597125185509,3.7297008646269574,3.5420845333205939,2.9982638673324389,3.5030594133480188,3.4819997363462822,3.5408881112233299,3.4960394317018668,3.6461638666592608,3.938195846828656,3.6282834361573784,3.685186134805341,3.5102933305555535,3.7084145147440166,3.6783863928185077,3.8755816324733949,3.6911864590317709,3.6393548357878864,3.6448333387064409,3.6420241303236818,3.5310494811500246,3.6447338613836306,3.6807160862312953,3.6366660847473637,3.5479493445652084,3.6504464405140098,3.7131786707023187,3.7309347531632984,3.6090252551905464,3.6649037631993329,3.7222640304644337,3.7526412288943161,4.0502081784690107,3.6711321408226527,3.7301941652443777,3.6017997963609054,3.4920227267625052,3.7334675191402207,3.700754744276427,3.3059640906046628,4.0176955475382474,3.8275842004635896,3.9435876703839954,3.7350006599795336,3.588388111119456,3.6991382022893031,3.5417309559619747,3.6350266948354286,3.6704401895252974,3.5850260900867741,3.6651040142482958,3.600943836011897,3.6605111945150637,3.5470116084703376,3.5308602683697674,3.1127091831602027,3.6856515743925384,3.656795653633313,3.4601231039484097,3.4922372440102443,3.7773908094890665,3.49401636477447,3.6448573586913788,3.4216604749204684,3.4818883911161329,3.4980258382555904,3.4760465896749397,3.7352360663516873,3.6280944028058628,3.4969576060594094,3.7925634144966645,3.5910554568930801,3.5051252928159538,3.5302898464157324,3.4873395608928983,3.685750447403739,3.4960517199603673,3.8492412348273137,3.7208903044662787,3.7065618367579676,3.7125954646403931,3.6409083075582189,3.5149582412170135,3.5005970286509283,3.546221224245421,3.5110951889341497,3.834165102930343,3.6673998017768223,3.6838796304748094,3.4876708329092363,3.7393141835590797,3.8226777986517151,3.7400918667009124,3.5377633678613929,3.5601777331583437,3.7273749054675354,3.5811675329539279,3.5771203989538791,3.589273291436184,3.8326075725630617,3.6216245661209272,3.6157862875818743,3.6755747929473408,3.4881693587165969,3.666242756339257,3.5302687186024175,3.489310665133825,3.7394218845645986,3.6645485817623817,3.7814667552936765,3.7093200216224655,3.5355236655161657,3.6729664142758285,3.5582835575048559,3.471917375673347,3.5412375014876729,3.6664968467403538,3.6325708315744194,3.5665115673207475,3.6520125650551107,3.5214661808856871,3.6214205181407153,3.6483198800205696,3.6564904247599248,3.8189238202374707,3.4702852146241963,3.6104081292252688,3.6019714550953825,3.5299436908003026,3.6915737465418275,3.5437426251840183,3.5675284733566919,3.4959978427464509,3.5791355418463384,3.4389832696601963,3.5437493314738111,3.4761167761296199,3.7148097393347856,3.7306041238680994,3.5992587393051125,3.5407134511327203,3.5472063578760173,3.861336268419358,3.6903981296815127,3.5891634006315876,3.6448108766138314,3.5395288940935323,3.6965225884155606,3.5826907384878752,3.8881910559334871,3.4915277102309568,3.79129330379505,3.5834027832361031,3.7022105431354593,3.6227727577394635,3.5034547958129494,3.6557918964086804,3.7775670081150419,3.7155254268940672,3.635207541911134,3.5555431286922659,3.7376810600569179,3.6960212310308336,3.49587666838389,3.6612557591904875,3.6021196065131673,3.4890688756877091,3.4962527181303606,3.7678252947668218,3.7160484295686107,3.4867343597263853,3.4975793205310728,3.8228455075745735,3.5004730724820989,3.9654833650363694,3.4936887218345034,3.6135991863540671,3.5831795765226913,3.5088275933252797,3.6314953321176069,3.5237025021199222,3.2674815129662242,3.946660735881057,3.9054549639708438,3.5295571319583408,3.5857851715760884,3.4845299232860762,3.5868200475118313,3.4821680571947806,3.5959830922671738,3.4991767737979966,3.5191500838710614,3.9996742691082603,3.6764926693319184,3.5043432290543994,3.6826123908499193,3.822213663126635,3.6764323159512289,3.7031527122971788,4.1315162972616619,3.7283328186233993,3.4962620898648216,3.8357902077416375,3.4927443919687695,3.4853623589528238,3.5090784458827677,3.8322846713172316,3.6009321431222743,3.7229106083198444,3.7590211881723525,3.7232155226296122,3.635875478176076,3.5275783467761799,3.6976513594927427,3.4842708506475724,3.7330880969097984,3.762979863197367,3.4843417484094936,3.6308357523001442,3.5671671912702392,3.7208238678817951,3.59221006018966,3.8112289352494741,3.4945536328216895,3.7618540120820523,3.4893228487637957,3.496191259171697,3.5958989699340766,3.6772551738156123,3.6307257154214945,3.4729253625163632,3.7060042227026071,3.4869323935491381,3.5003105721278631,3.9669243972540422,3.4536600517670282,3.53056825353619,3.6873504417637148,3.7695824923833681,3.70548392047088,3.6348024809220072,3.7651116872566504,3.5018784395514992,3.4963547693645851,3.5949212934568431,3.6862964314629885,3.6498707792310543,3.4436882472972026,3.6687530675548388,3.521618938542149,3.492446479251937,3.8315302825433823,3.5988645896279303,3.9602435373051899,3.7681111273760561,3.5200530872446265,3.5774190713648286,3.5479629802288506,3.5612056930376541,3.6817016219479282,3.6221120634690411,3.8009434436735825,3.5995157829802542,3.7425128224900575,3.6607242021486033,3.5063524726463711,3.4902427203694488,3.6514282035187211,3.6813170829528721,3.6291009694027379,3.7105301050510251,3.696145439565405,3.7151290319724053,3.5667740259846115,3.6420153447483479,3.6693721244032806,3.5730657102125454,3.6260212291662528,3.7603169583244926,3.7594575824985634,3.7473181200988335,3.5747657279326503,3.5767041669743667,3.6351098439566543,3.7676271107543791,4.0361513189000853,3.6200272020570727,3.6405718499800952,3.9153917471591382,3.8521020699569952,3.7965733754262372,3.6992241487699742,3.8131695586792307,3.7445647398566608,3.7906887317243489,3.6553890366055253,3.6158049919649033,3.7847514323678837,3.5752652650788272,3.4966903526977924,3.5157397724176844,3.4938420246302684,3.7693656510938789,3.4957200609561032,3.5338707548236727,3.864134124445227,3.8096993723682799,3.5547323655604219,3.7833280782624796,3.650797736348109,3.6021983694756248,3.6725268497276491,3.7172067125275827,3.569908493844514,3.5517346136247632,3.5155254196989327,3.7166537474129284,3.6160779018912654,3.6767193190102008,3.6356816032525758,3.4928740608893216,3.8495725313748164,3.5454080107405819,3.497870655468887,3.6108179154550832,3.668046125796975,3.5771560549842696,3.5652088655615253,3.4814950628338006,3.6862707735632201,3.5923578295865584,3.5887077036603752,3.5086183999195129,3.5793044468535231,3.586177368044928,3.5985628875166076,3.4959790106700512,3.5585867420483082,3.766086887921726,3.6634663414718087,3.5871524439149129,3.5847964084215418,3.9547589970181409,3.6656707301808371,3.7087180341986965,3.6835753594123073,3.5028635179784784,3.4941331672478606,3.5864953176189909,3.7793047971331561,3.6447631010723547,3.5568991406042878,4.128084064082902,3.7610776723741561,3.5321883484121921,3.7473725680134948,3.7119908707511051,3.4897035729686343,3.6653624090810002,3.7478842084325752,3.4305701123958956,3.5823252770577443,3.9626135566945511,3.6821887026976277,3.470836426276239,3.8212943788038793,3.6128784194968375,3.5732532924324474,3.7594191374211774,3.8255034276856765,3.4984861641413985,3.5445192464745556,3.6783995083794228,3.6696167093717031,3.7224810964784436,3.7040788375859406,3.5884587688070422,3.7703322874953731,3.4890960665795658,3.8377980308340591,3.8017603778727724,3.8787279415875426,3.6051132692406087,3.7008764768526028,3.5703652122769789,3.6860755560233054,3.6359511900277059,3.7853245061371172,3.971451735768845,3.5866637119600484,3.7465782264202176,3.9324146722846951,3.5834898415890342,3.4899978817791979,3.7911198713863441,3.4967464751562956,3.5133454033476941,3.9251127397736285,3.6634846875108602,3.7252806006224453,3.8915786805401802,3.5110052266149014,3.6553848928208459,3.4920462421932998,3.5696966777023893,3.5742251021963485,3.4972081695987227,3.6210132331473606,3.8367034769907882,3.8189171870775809,3.516289897559667,3.873677411010179,3.6000646082799981,3.5226014934381347,3.8915173528674658,3.4885976586629805,3.7522517983800996,3.6663101014995565,3.6662660377028677,3.5964222214035506,3.5563282796206228,3.4756213335580015,3.5013691268059124,3.6842691368114457,3.6332649207030157,3.4990872655256036,3.5392589607099354,3.829189573660547,3.4681612086205185,3.674855615861897,3.6983306584429343,3.8080275463385354,3.6473646733533625,3.5946944545798591,3.6721196523545374,3.6983309975430929,3.7951709961669748,3.7764671757395831,3.4891708809051849,3.8101972528728201,3.543229871109244,3.6278079752186114,3.5546131388725155,3.8039421316465534,3.5090200217589338,3.6473814755949228,3.6396145272810232,3.7063954254921985,3.6797944917314767,3.5005399210996058,3.5769619668549026,3.4990665372652914,3.599152844031098,3.5387276345105136,3.8119223335338659,3.7957900478843944,3.4902979788556929,3.4858649434508977,3.7479823439499369,3.544393092273125,3.5023628630991199,3.648861449991434,3.6896407746117781,3.7840447465298652,3.5362129365750579,3.6540477851676267,3.5085934519002473,3.628457107478245,3.6109906994847769,3.5749990184410856,3.8124917125337205,3.6043885604924877,3.5754010361412885,3.5286207806670369,3.5129457739019334,3.6480529588673241,3.5645617754217409,3.6071193410120008,3.7612331319050987,3.7131621311637573,3.8253799749068893,3.5117094675718183,3.4935985993840482,3.6169582232393092,3.5064030363020571,3.6413807701329777,3.9294858654180396,3.7718126311820619,3.7146709402347229,3.8381705156219401,3.4959833417286954,3.679257837363227,3.745947255759233,3.5139813398288737,3.5734807248146927,3.9835365216113718,3.6272831726055283,3.7760067601718812,3.7383917631589512,3.7689948797203021,3.8990221198227659,3.6083118578769424,3.7681415151089555,3.6674958982079642,3.7651343172255416,3.6019571024453207,3.6120045132801253,3.4898889026393376,3.5088905838057638,3.6670189592153446,3.4940623029159568,3.6672840706712875,3.6158589902585034,3.5189706643477483,3.4489026433301779,3.6383277539668271,3.4994801022461246,3.5346680104131005,3.6891623575022563,3.664989152041878,3.8694882433265967,3.4983772796608021,3.6779753963871351,3.5768870106537549,3.4687999482123666,3.6211630868861127,3.6431934643551513,3.7517331110374732,3.4807799713092575,3.6835879463761763,3.5590216344721597,3.8362647790169628,3.500366879747066,3.1448644816593143,3.5048187180887322,3.5009309058294873,3.5000938147075886,3.6830431033943536,3.7473593415148856,3.7274530101089018,3.6093485336511599,3.6498598462926193,3.4838396867297985,3.5436064535958622,3.5024346022941351,3.6871713800896235,3.4581247841736382,3.7928979433772421,3.56623217280602,3.7147589530177738,3.6660373575284453,3.6999629035894865,3.6188074307522715,3.7038110584515502,3.5497638545332455,3.6600488333206074,3.5612755132436851,3.6754983384307338,3.9620201685543801,3.6037288281428692,3.4839893724554121,3.6600317408990266,3.5514195442942498,3.6384468009830742,3.7582876589454473,3.7616947725468304,3.6977270817260042,3.4990285352988799,3.7357047147409204,3.8228020103155247,3.6362663766075034,3.5752934298618646,3.648514933049738,3.4976239063393608,3.7557567987875808,3.7567424105907086,3.6505281639713263,3.7609341080178131,3.48262647054477,3.5924262830620686,3.4932417531196114,3.7139340681140891,3.7263740483440513,3.4976510176021378,3.5908367125663561,3.5976924034372195,3.9252582178001623,3.6363941064532086,3.4987658049081531,3.8048424580857643,3.729851137875674,3.5129571782025955,3.9344138738021925,3.5025794148714513,3.5374469294558257,3.638007230814218,3.6032021952385289,3.9402818746694512,3.545687159485162,3.6482714229999109,3.479676611807613,3.7894559873768703,3.651471781502579,3.7903621709280708,3.4898616107227105,3.5031142663536752,3.9711098398801585,3.6467974663385014,3.481835614471315,3.7973279849860653,3.8982757944225566,3.7262140535438513,3.9821649937679671,3.6229172383859831,3.6202716364734107,3.6051181014844369,3.4542932270350395,3.6789803674401158,3.495508195826841,3.6981863436322637,3.5698543298626193,3.7875845743017136,3.519073774318112,3.6151945891125825,3.7194032728315265,3.6835460455615809,3.7286574549080229,3.7133015191995948,3.614422431099694,3.5044504556716127,3.6054727875221459,3.6171942529478187,3.8014579070680954,3.4877595093516152,3.8852177501197742,3.7476079576700383,3.6517846322653131,3.6257258745956507,3.5892643954100896,3.510646863112945,3.8857102315238401,3.6863668467224424,3.6946932331179441,3.648658246827678,3.7553567240530064,3.6915638357358742,3.7479650037894592,3.528899543304548,3.5488965690030874,3.6380118754444708,3.5035215385680782,3.5545823731542563,3.6515501512579265,3.5872577791674369,3.70876648608284,3.5844661333780028,3.7371362808892163,3.9570132585102615,3.9574529909143248,3.6467978038474524,3.6392153823277158,3.610329877665106,3.543424219601345,3.7588360562361145,3.5115260588465991,3.6563004278882891,3.6294011497414185,3.7535564186046995,3.6826907061367224,3.6608839955366967,3.8297242073080269,3.5434671109352172,3.4972115759163525,3.8525435903809981,3.7723545456881045,3.5653961218307386,3.7119790935142158,3.5899446601940896,3.4921828465351124,3.583302219232162,3.4864837330106928,3.6697568773574138,3.6063422179573186,3.883614571765011,3.1271573604709717,3.4564390663194984,3.656909515590729,3.5240648677545092,3.7887874379203943,2.938178997459326,3.6458859975097617,3.6273279414374668,3.4749093562895017,3.572501647911658,3.6612703201351149,3.7386447070413724,3.9545934161453231,3.495130666127455,3.7119618491507747,3.7811734997976223,3.8734009826550699,3.5677464722982228,3.6144787037911112,3.7056122363686907,3.6756844161186999,3.7533689871524039,3.4928720828664845,3.4850189673920071,3.7540886586777527,3.4921204438048568,3.5659551435933494,3.6723105166747585,3.6817790862223321,3.654472671979371,3.6438005928499222,3.5114393564006456,3.5204739003622403,3.7100061922574805,3.6969941178044161,3.6089800225731925,3.5978170680540638,3.6954446361670343,3.7359549010370308,3.7050794310834911,3.6743333126002748,3.0714680416633753,3.7772503365112526,3.657277526259858,3.5307315419377536,3.6666149818437854,3.5924791304006618,3.5140856567297742,3.5516979409820189,3.5555941634269157,3.559066566367433,3.5675981817731928,3.7301654901781744,3.6877480845958224,3.5886295131143684,3.6266151249528966,3.8616233195500773,3.6566638778103933,3.7146487759942488,3.5093891887718236,3.6153419406130229,3.570977055396177,3.5222117878322496,3.6777279309646413,3.6839859224118223,3.5080391636322994,3.4944936681847718,3.526041863770172,3.6333044689081997,3.4636489823753243,3.86413458635066,3.5369590356638185,3.6685025059309506,3.5951941254583506,3.5029113235064608,3.478334433496828,3.4990795133043,3.7721433217594349,3.5758916213867584,3.7389268164399025,3.5230064868366049,3.7021010162309236,3.8039739452420656,3.7598378134999328,3.4860254534278461,3.7839164630770661,3.694647095163655,3.5273580803577333,3.5064696719572619,3.7318258275685858,3.5906636451587248,3.671771373119137,3.7089655757944646,3.497665943555186,3.7556592727768514,3.5639298678481368,3.5716469002781959,3.8738188767877872,3.7890779173742497,3.487514859140965,3.6128541535957757,3.6736502940817783,3.489921442224488,3.641826600465166,3.5718723942601378],"type":"scatter3d","mode":"markers","marker":{"color":"black","size":3,"opacity":0.14999999999999999,"line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"frame":null},{"x":[0.0375997],"y":[0.29339290000000001],"z":[3.7802642999999998],"type":"scatter3d","mode":"markers","marker":{"color":"red","size":5,"symbol":"cross","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```














