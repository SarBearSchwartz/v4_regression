# Example: Ventricular shortening velocity

## Packages


```r
library(tidyverse)
library(magrittr)
library(furniture)
library(texreg)
library(stargazer)
library(psych)
library(car)
library(effects)
library(ISwR)  # Introduction to Statistics with R (datasets)
```


load dataset that is included in the `ISwR` package

```r
data(thuesen)
```

The `thuesen` data frame has 24 rows and 2 columns. It contains ventricular shortening velocity and blood glucose for type 1 diabetic patients.

* `blood.glucose` a numeric vector, fasting blood glucose (mmol/l).
* `short.velocity` a numeric vector, mean circumferential shortening velocity (%/s).


### Get to know the data with `?thuesen`



```r
dim(thuesen)      # number of rows (subjects) & columns (variables)
```

```
## [1] 24  2
```

```r
names(thuesen)    # names of the variables
```

```
## [1] "blood.glucose"  "short.velocity"
```

```r
glimpse(thuesen)  # view the class and 1st few values of each variable
```

```
## Observations: 24
## Variables: 2
## $ blood.glucose  <dbl> 15.3, 10.8, 8.1, 19.5, 7.2, 5.3, 9.3, 11.1, 7.5...
## $ short.velocity <dbl> 1.76, 1.34, 1.27, 1.47, 1.27, 1.49, 1.31, 1.09,...
```

```r
summary(thuesen)  # notice a missing value (NA) on velocity
```

```
##  blood.glucose    short.velocity 
##  Min.   : 4.200   Min.   :1.030  
##  1st Qu.: 7.075   1st Qu.:1.185  
##  Median : 9.400   Median :1.270  
##  Mean   :10.300   Mean   :1.326  
##  3rd Qu.:12.700   3rd Qu.:1.420  
##  Max.   :19.500   Max.   :1.950  
##                   NA's   :1
```


```r
stargazer(thuesen, 
          type   = "html", 
          digits = 4, 
          flip   = TRUE,                    
          summary.stat   = c("n", "mean", "sd", "min", "median", "max"),
          title  = "Descriptives")
```


<table style="text-align:center"><caption><strong>Descriptives</strong></caption>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Statistic</td><td>blood.glucose</td><td>short.velocity</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">N</td><td>24</td><td>23</td></tr>
<tr><td style="text-align:left">Mean</td><td>10.3000</td><td>1.3257</td></tr>
<tr><td style="text-align:left">St. Dev.</td><td>4.3375</td><td>0.2329</td></tr>
<tr><td style="text-align:left">Min</td><td>4.2000</td><td>1.0300</td></tr>
<tr><td style="text-align:left">Median</td><td>9.4000</td><td>1.2700</td></tr>
<tr><td style="text-align:left">Max</td><td>19.5000</td><td>1.9500</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr></table>

## VISUALIZATION: Raw Data 

Base Graphics: let it determine the type of plot

```r
ggplot(thuesen, 
       aes(x = blood.glucose, 
           y = short.velocity)) + 
  geom_point()
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

<img src="72-example_thuesen_files/figure-html/unnamed-chunk-5-1.png" width="672" />


`ggplot2`: specify plot type

```r
thuesen %>% 
  ggplot(aes(x = blood.glucose,       # x-axis variable name
             y = short.velocity)) +   # y-axis variable name
  geom_point() +                      # scatterplot
  theme_bw()                          # black-and-white theme 
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

<img src="72-example_thuesen_files/figure-html/unnamed-chunk-6-1.png" width="672" />

### CORRELATION: un-adjusted 

cor doesn't like NA values

```r
thuesen %>% cor           
```

```
##                blood.glucose short.velocity
## blood.glucose              1             NA
## short.velocity            NA              1
```

specify to do listwise deletion

```r
thuesen %>% cor(use = "complete.obs") 
```

```
##                blood.glucose short.velocity
## blood.glucose      1.0000000      0.4167546
## short.velocity     0.4167546      1.0000000
```

you can abbreviate

```r
thuesen %>% cor(use = "complete")     # 
```

```
##                blood.glucose short.velocity
## blood.glucose      1.0000000      0.4167546
## short.velocity     0.4167546      1.0000000
```

same as above, but without the pipe

```r
cor(thuesen, use = "complete")     
```

```
##                blood.glucose short.velocity
## blood.glucose      1.0000000      0.4167546
## short.velocity     0.4167546      1.0000000
```

you can choose the number of decimal places

```r
thuesen %>% 
  cor(use="complete") %>%   
  round(2)                   
```

```
##                blood.glucose short.velocity
## blood.glucose           1.00           0.42
## short.velocity          0.42           1.00
```


this version give a single value instead of a matrix

```r
thuesen %$% 
  cor(blood.glucose,                
      short.velocity,               
      use="complete")
```

```
## [1] 0.4167546
```

This TESTS if the cor == 0

```r
thuesen %$% 
  cor.test(blood.glucose,           
           short.velocity, 
           use = 'complete')
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  blood.glucose and short.velocity
## t = 2.101, df = 21, p-value = 0.0479
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.005496682 0.707429479
## sample estimates:
##       cor 
## 0.4167546
```

The default is Pearson's R, which assesses linear relationships.  Spearman's correlation assesses monotonic relationships.


```r
thuesen %$% 
  cor.test(blood.glucose, 
           short.velocity, 
           use    = 'complete',
           method = 'spearman')     # spearman's (rho) 
```

```
## Warning in cor.test.default(blood.glucose, short.velocity, use =
## "complete", : Cannot compute exact p-value with ties
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  blood.glucose and short.velocity
## S = 1380.4, p-value = 0.1392
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##      rho 
## 0.318002
```

## FIT REGRESSION: simple linear 


```r
fit_vel_glu <- lm(short.velocity ~ blood.glucose, data = thuesen)

fit_vel_glu
```

```
## 
## Call:
## lm(formula = short.velocity ~ blood.glucose, data = thuesen)
## 
## Coefficients:
##   (Intercept)  blood.glucose  
##       1.09781        0.02196
```


```r
summary(fit_vel_glu)
```

```
## 
## Call:
## lm(formula = short.velocity ~ blood.glucose, data = thuesen)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.40141 -0.14760 -0.02202  0.03001  0.43490 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.09781    0.11748   9.345 6.26e-09 ***
## blood.glucose  0.02196    0.01045   2.101   0.0479 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2167 on 21 degrees of freedom
##   (1 observation deleted due to missingness)
## Multiple R-squared:  0.1737,	Adjusted R-squared:  0.1343 
## F-statistic: 4.414 on 1 and 21 DF,  p-value: 0.0479
```



```r
coef(fit_vel_glu)
```

```
##   (Intercept) blood.glucose 
##    1.09781488    0.02196252
```



```r
confint(fit_vel_glu)
```

```
##                      2.5 %     97.5 %
## (Intercept)   0.8534993816 1.34213037
## blood.glucose 0.0002231077 0.04370194
```



```r
anova(fit_vel_glu)
```

```
## Analysis of Variance Table
## 
## Response: short.velocity
##               Df  Sum Sq  Mean Sq F value Pr(>F)  
## blood.glucose  1 0.20727 0.207269   4.414 0.0479 *
## Residuals     21 0.98610 0.046957                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


model fit indicies

```r
logLik(fit_vel_glu)     
```

```
## 'log Lik.' 3.583612 (df=3)
```

```r
AIC(fit_vel_glu)
```

```
## [1] -1.167223
```

```r
BIC(fit_vel_glu)
```

```
## [1] 2.239259
```


```r
stargazer(fit_vel_glu, type = "html")
```


<table style="text-align:center"><tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="1" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>short.velocity</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">blood.glucose</td><td>0.022<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.010)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>1.098<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.117)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>23</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.174</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.134</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.217 (df = 21)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>4.414<sup>**</sup> (df = 1; 21)</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>



```r
texreg::htmlreg(fit_vel_glu)
```


<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
<caption align="bottom" style="margin-top:0.3em;">Statistical models</caption>
<tr>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b></b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 1</b></th>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">(Intercept)</td>
<td style="padding-right: 12px; border: none;">1.10<sup style="vertical-align: 0px;">***</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.12)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">blood.glucose</td>
<td style="padding-right: 12px; border: none;">0.02<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.01)</td>
</tr>
<tr>
<td style="border-top: 1px solid black;">R<sup style="vertical-align: 0px;">2</sup></td>
<td style="border-top: 1px solid black;">0.17</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Adj. R<sup style="vertical-align: 0px;">2</sup></td>
<td style="padding-right: 12px; border: none;">0.13</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. obs.</td>
<td style="padding-right: 12px; border: none;">23</td>
</tr>
<tr>
<td style="border-bottom: 2px solid black;">RMSE</td>
<td style="border-bottom: 2px solid black;">0.22</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;" colspan="3"><span style="font-size:0.8em"><sup style="vertical-align: 0px;">***</sup>p &lt; 0.001, <sup style="vertical-align: 0px;">**</sup>p &lt; 0.01, <sup style="vertical-align: 0px;">*</sup>p &lt; 0.05</span></td>
</tr>
</table>


## VISUALIZATION: Model Fit 

Base Graphics: let it determine the type of plot `?plot.lm`


show all plots at once

```r
par(mfrow = c(2, 3))
plot(fit_vel_glu, which = 1:6)
```

<img src="72-example_thuesen_files/figure-html/unnamed-chunk-23-1.png" width="672" />

```r
par(mfrow = c(1, 1))
```

potentially influencial or outlier points

```r
thuesen %>% 
  dplyr::mutate(id = row_number()) %>% 
  dplyr::filter(id == c(13, 20, 24))
```

```
##   blood.glucose short.velocity id
## 1          19.0           1.95 13
## 2          16.1           1.05 20
## 3           9.5           1.70 24
```



`ggplot2`: specify plot type

```r
thuesen %>% 
  dplyr::filter(complete.cases(.)) %>%            # get ride fo the incomplete cases
  ggplot(data = ,                        # name the dataset 1st
         mapping = aes(x = blood.glucose,       # x-axis variable name
                       y = short.velocity)) +   # y-axis variable name
  geom_point() +                              # do a scatterplot
  stat_smooth(method = "lm") +                # smooth: linear model
  theme_bw()  +                               # black-and-while theme
  geom_point(data = thuesen %>% 
               filter(row_number() == c(13, 20, 24)), #  
             pch = 19,
             size = 4,
             color = "red")
```

<img src="72-example_thuesen_files/figure-html/unnamed-chunk-25-1.png" width="672" />


## CHECK VALIDITY of the  ASSUMPTIONS w/ residual diagnostics

store values from the model (into the dataset)


```r
thuesen %>% 
  dplyr::filter(complete.cases(.)) %>%            # get ride fo the incomplete cases
  dplyr::mutate(pred = fitted(fit_vel_glu)) %>%   # fitted/prediction values
  dplyr::mutate(resid = residuals(fit_vel_glu))   # residual values
```

```
##    blood.glucose short.velocity     pred        resid
## 1           15.3           1.76 1.433841  0.326158532
## 2           10.8           1.34 1.335010  0.004989882
## 3            8.1           1.27 1.275711 -0.005711308
## 4           19.5           1.47 1.526084 -0.056084062
## 5            7.2           1.27 1.255945  0.014054962
## 6            5.3           1.49 1.214216  0.275783754
## 7            9.3           1.31 1.302066  0.007933665
## 8           11.1           1.09 1.341599 -0.251598875
## 9            7.5           1.18 1.262534 -0.082533795
## 10          12.2           1.22 1.365758 -0.145757649
## 11           6.7           1.25 1.244964  0.005036223
## 12           5.2           1.19 1.212020 -0.022019994
## 13          19.0           1.95 1.515103  0.434897199
## 14          15.1           1.28 1.429449 -0.149448964
## 15           6.7           1.52 1.244964  0.275036223
## 16           4.2           1.12 1.190057 -0.070057471
## 17          10.3           1.37 1.324029  0.045971143
## 18          12.5           1.19 1.372346 -0.182346406
## 19          16.1           1.05 1.451411 -0.401411486
## 20          13.3           1.32 1.389916 -0.069916424
## 21           4.9           1.03 1.205431 -0.175431237
## 22           8.8           1.12 1.291085 -0.171085074
## 23           9.5           1.70 1.306459  0.393541161
```


```r
thuesen %>% 
  dplyr::mutate(id = row_number()) %>% 
  dplyr::filter(complete.cases(.)) %>%                # get ride fo the incomplete cases
  dplyr::mutate(pred = fitted(fit_vel_glu)) %>%       # fitted/prediction values
  dplyr::mutate(resid = residuals(fit_vel_glu)) %>%   # residual values
  ggplot(aes(x = id,
             y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0,
             color = "red",
             size = 1,
             linetype = "dashed") +
  theme_classic() +
  labs(title = "Looking for homogeneity of residuals",
       subtitle = "want to see equal spread all across")
```

<img src="72-example_thuesen_files/figure-html/unnamed-chunk-27-1.png" width="672" />


```r
thuesen %>% 
  dplyr::filter(complete.cases(.)) %>%                # get ride fo the incomplete cases
  dplyr::mutate(pred = fitted(fit_vel_glu)) %>%       # fitted/prediction values
  dplyr::mutate(resid = residuals(fit_vel_glu)) %>%   # residual values
  ggplot(aes(x = resid)) +
  geom_histogram(bins = 12,
                 color = "blue",
                 fill = "blue",
                 alpha = 0.3) +
  geom_vline(xintercept = 0,
             size = 1,
             color = "red",
             linetype = "dashed") +
  theme_classic() +
  labs(title = "Looking for normality of residuals",
       subtitle = "want to see roughly a bell curve")
```

<img src="72-example_thuesen_files/figure-html/unnamed-chunk-28-1.png" width="672" />

the residual plots 

```r
residualPlots(fit_vel_glu)
```

<img src="72-example_thuesen_files/figure-html/unnamed-chunk-29-1.png" width="672" />

```
##               Test stat Pr(>|Test stat|)
## blood.glucose    0.9289           0.3640
## Tukey test       0.9289           0.3529
```


`ggplot2`: plotting confidence intervals for the mean outcome

```r
ggplot(thuesen,
       aes(x = blood.glucose,
           y = short.velocity)) +
  stat_smooth(method = "lm", 
              color  = "blue") +
  geom_point(shape = 10,
             size  = 2) +                    
  theme_bw()
```

```
## Warning: Removed 1 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

<img src="72-example_thuesen_files/figure-html/unnamed-chunk-30-1.png" width="672" />


## Plot the model


```r
effects::Effect(focal.predictors = c("blood.glucose"),
                mod = fit_vel_glu)
```

```
## 
##  blood.glucose effect
## blood.glucose
##      4.2        8       12       16       20 
## 1.190057 1.273515 1.361365 1.449215 1.537065
```


```r
effects::Effect(focal.predictors = c("blood.glucose"),
                mod = fit_vel_glu) %>% 
  data.frame() 
```

```
##   blood.glucose      fit         se    lower    upper
## 1           4.2 1.190057 0.07878423 1.026217 1.353898
## 2           8.0 1.273515 0.05155032 1.166310 1.380720
## 3          12.0 1.361365 0.04827581 1.260970 1.461760
## 4          16.0 1.449215 0.07416568 1.294979 1.603451
## 5          20.0 1.537065 0.11030602 1.307671 1.766459
```


```r
effects::Effect(focal.predictors = c("blood.glucose"),
                mod = fit_vel_glu,
                xlevels = list(blood.glucose = c(5, 10, 15, 20))) %>% 
  data.frame()
```

```
##   blood.glucose      fit         se    lower    upper
## 1             5 1.207627 0.07209315 1.057702 1.357553
## 2            10 1.317440 0.04535290 1.223124 1.411757
## 3            15 1.427253 0.06618321 1.289617 1.564888
## 4            20 1.537065 0.11030602 1.307671 1.766459
```



```r
effects::Effect(focal.predictors = c("blood.glucose"),
                mod = fit_vel_glu,
                xlevels = list(blood.glucose = seq(from = 4, to = 20, by = 1))) %>% 
  data.frame() %>% 
  ggplot(aes(x = blood.glucose,
             y = fit)) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              alpha = .5) +
  geom_line()
```

<img src="72-example_thuesen_files/figure-html/unnamed-chunk-34-1.png" width="672" />



