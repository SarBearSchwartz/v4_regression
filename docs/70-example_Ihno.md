# Example: Ihno's Experiment


## Packages


```r
library(tidyverse)       # super helpful everything!
library(haven)           # inporting SPSS data files
library(furniture)       # nice tables of descriptives
library(texreg)          # nice regression summary tables
library(stargazer)       # nice tables of descrip and regression
library(car)             # companion for applied regression
```


## Research Question

> Does math phobia moderate the relationship between math and statistics performance?


## Data: Sample & Measures


<div class="rmdlink">
<p>Inho's dataset is included in &quot;Explaining Psychological Statistics&quot; <span class="citation">[@epse4]</span> and is presented indetail previously in this Encyclopedia's <a href="https://cehs-research.github.io/eBook_explore/example-ihnos-dataset.html">Vol. 2 - Ihno's Example</a>.</p>
</div>


```r
data_ihno <- haven::read_spss("http://www.psych.nyu.edu/cohen/Ihno_dataset.sav") %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::mutate(gender = factor(gender, 
                               levels = c(1, 2),
                               labels = c("Female", 
                                          "Male"))) %>% 
  dplyr::mutate(major = factor(major, 
                              levels = c(1, 2, 3, 4,5),
                              labels = c("Psychology",
                                         "Premed",
                                         "Biology",
                                         "Sociology",
                                         "Economics"))) %>% 
  dplyr::mutate(reason = factor(reason,
                                levels = c(1, 2, 3),
                                labels = c("Program requirement",
                                           "Personal interest",
                                           "Advisor recommendation"))) %>% 
  dplyr::mutate(exp_cond = factor(exp_cond,
                                  levels = c(1, 2, 3, 4),
                                  labels = c("Easy",
                                             "Moderate",
                                             "Difficult",
                                             "Impossible"))) %>% 
  dplyr::mutate(coffee = factor(coffee,
                                levels = c(0, 1),
                                labels = c("Not a regular coffee drinker",
                                           "Regularly drinks coffee"))) 
```


## Exploratory Data Analysis

Before enbarking on any inferencial anlaysis or modeling, always get familiar with your variables one at a time (univariate), as well as pairwise (bivariate).


```r
data_ihno %>% 
  dplyr::select(phobia, mathquiz, statquiz) %>% 
  data.frame() %>% 
  stargazer::stargazer(type = "html")
```


<table style="text-align:center"><tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Statistic</td><td>N</td><td>Mean</td><td>St. Dev.</td><td>Min</td><td>Pctl(25)</td><td>Pctl(75)</td><td>Max</td></tr>
<tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">phobia</td><td>100</td><td>3.310</td><td>2.444</td><td>0</td><td>1</td><td>4</td><td>10</td></tr>
<tr><td style="text-align:left">mathquiz</td><td>85</td><td>29.071</td><td>9.480</td><td>9.000</td><td>22.000</td><td>35.000</td><td>49.000</td></tr>
<tr><td style="text-align:left">statquiz</td><td>100</td><td>6.860</td><td>1.700</td><td>1</td><td>6</td><td>8</td><td>10</td></tr>
<tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr></table>



```r
data_ihno %>% 
  dplyr::mutate(phobia_cut3 = cut(phobia,
                                 breaks = c(0, 2, 4, 10),
                                 include.lowest = TRUE)) %>% 
  furniture::table1(mathquiz, statquiz,
                    splitby = ~ phobia_cut3,
                    test = TRUE,
                    output = "markdown")
```



|          |   [0,2]    |   (2,4]    |   (4,10]   | P-Value |
|----------|------------|------------|------------|---------|
|          |   n = 39   |   n = 37   |   n = 24   |         |
| mathquiz |            |            |            |  0.014  |
|          | 32.6 (8.5) | 26.5 (9.8) | 26.8 (8.9) |         |
| statquiz |            |            |            |  0.001  |
|          | 7.6 (1.3)  | 6.6 (1.6)  | 6.1 (2.0)  |         |


```r
data_ihno %>% 
  dplyr::select(phobia, mathquiz, statquiz) %>% 
  cor(use = "complete.obs") %>% 
  corrplot::corrplot.mixed(lower  = "ellipse",
                           upper  = "number",
                           tl.col = "black")
```

<img src="70-example_Ihno_files/figure-html/unnamed-chunk-6-1.png" width="672" />



## Fitting Nested Models

The **bottom-up** approach consists of starting with an initial `NULL` model with only an intercept term and them building additional models that are nested.  

Two models are considered **nested** if one is conains a subset of the terms (predictors or IV) compared to the other. 



```r
fit_ihno_lm_0 <- lm(statquiz ~ 1,
                    data = data_ihno %>% 
                      dplyr::filter(complete.cases(mathquiz, statquiz, phobia)))

fit_ihno_lm_1 <- lm(statquiz ~ mathquiz,
                    data = data_ihno %>% 
                      dplyr::filter(complete.cases(mathquiz, statquiz, phobia)))

fit_ihno_lm_2 <- lm(statquiz ~ phobia,
                    data = data_ihno %>% 
                      dplyr::filter(complete.cases(mathquiz, statquiz, phobia)))

fit_ihno_lm_3 <- lm(statquiz ~ mathquiz + phobia,
                    data = data_ihno %>% 
                      dplyr::filter(complete.cases(mathquiz, statquiz, phobia)))

fit_ihno_lm_4 <- lm(statquiz ~ mathquiz*phobia,
                    data = data_ihno %>% 
                      dplyr::filter(complete.cases(mathquiz, statquiz, phobia)))
```



## Comparing Nested Models


### Model Comparison Table

In single level, multiple linear regression significance of predictors (independent variables, IV) is usually based on both the Wald tests of significance for each beta estimate (shown with stars here) and comparisons in the model fit via the $R^2$ values.

> There is evidence both `mathquiz` and `phobia` are associated with `statquiz` and that the relationship is addative (i.e. no interaction, which would be multaplicative or suppressory).  


```r
texreg::htmlreg(list(fit_ihno_lm_0, 
                     fit_ihno_lm_1, 
                     fit_ihno_lm_2, 
                     fit_ihno_lm_3, 
                     fit_ihno_lm_4),
                custom.model.names = c("No Predictors", 
                                       "Only Math Quiz", 
                                       "Only Phobia", 
                                       "Both IVs", 
                                       "Add Interaction"))
```


<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
<caption align="bottom" style="margin-top:0.3em;">Statistical models</caption>
<tr>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b></b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>No Predictors</b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Only Math Quiz</b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Only Phobia</b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Both IVs</b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Add Interaction</b></th>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">(Intercept)</td>
<td style="padding-right: 12px; border: none;">6.85<sup style="vertical-align: 0px;">***</sup></td>
<td style="padding-right: 12px; border: none;">4.14<sup style="vertical-align: 0px;">***</sup></td>
<td style="padding-right: 12px; border: none;">7.65<sup style="vertical-align: 0px;">***</sup></td>
<td style="padding-right: 12px; border: none;">5.02<sup style="vertical-align: 0px;">***</sup></td>
<td style="padding-right: 12px; border: none;">5.60<sup style="vertical-align: 0px;">***</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.19)</td>
<td style="padding-right: 12px; border: none;">(0.53)</td>
<td style="padding-right: 12px; border: none;">(0.29)</td>
<td style="padding-right: 12px; border: none;">(0.63)</td>
<td style="padding-right: 12px; border: none;">(0.91)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">mathquiz</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">0.09<sup style="vertical-align: 0px;">***</sup></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">0.08<sup style="vertical-align: 0px;">***</sup></td>
<td style="padding-right: 12px; border: none;">0.06<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.02)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.02)</td>
<td style="padding-right: 12px; border: none;">(0.03)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">phobia</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-0.25<sup style="vertical-align: 0px;">***</sup></td>
<td style="padding-right: 12px; border: none;">-0.16<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">-0.34</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.07)</td>
<td style="padding-right: 12px; border: none;">(0.07)</td>
<td style="padding-right: 12px; border: none;">(0.21)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">mathquiz:phobia</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">0.01</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.01)</td>
</tr>
<tr>
<td style="border-top: 1px solid black;">R<sup style="vertical-align: 0px;">2</sup></td>
<td style="border-top: 1px solid black;">0.00</td>
<td style="border-top: 1px solid black;">0.26</td>
<td style="border-top: 1px solid black;">0.13</td>
<td style="border-top: 1px solid black;">0.31</td>
<td style="border-top: 1px solid black;">0.31</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Adj. R<sup style="vertical-align: 0px;">2</sup></td>
<td style="padding-right: 12px; border: none;">0.00</td>
<td style="padding-right: 12px; border: none;">0.25</td>
<td style="padding-right: 12px; border: none;">0.12</td>
<td style="padding-right: 12px; border: none;">0.29</td>
<td style="padding-right: 12px; border: none;">0.29</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. obs.</td>
<td style="padding-right: 12px; border: none;">85</td>
<td style="padding-right: 12px; border: none;">85</td>
<td style="padding-right: 12px; border: none;">85</td>
<td style="padding-right: 12px; border: none;">85</td>
<td style="padding-right: 12px; border: none;">85</td>
</tr>
<tr>
<td style="border-bottom: 2px solid black;">RMSE</td>
<td style="border-bottom: 2px solid black;">1.74</td>
<td style="border-bottom: 2px solid black;">1.50</td>
<td style="border-bottom: 2px solid black;">1.63</td>
<td style="border-bottom: 2px solid black;">1.46</td>
<td style="border-bottom: 2px solid black;">1.46</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;" colspan="7"><span style="font-size:0.8em"><sup style="vertical-align: 0px;">***</sup>p &lt; 0.001, <sup style="vertical-align: 0px;">**</sup>p &lt; 0.01, <sup style="vertical-align: 0px;">*</sup>p &lt; 0.05</span></td>
</tr>
</table>



### Likelihood Ratio Test of Nested Models

An alternative method for determing model fit and variable importance is the likelihood ratio test.  This involved comparing the $-2LL$ or inverse of twice the log of the likelihood value for the model.  The difference in these values follows a Chi Squared distribution with degrees of freedom equal to the difference in the number of parameters estimated *(number of betas)*.

Test the main effect of math quiz:

```r
anova(fit_ihno_lm_0, fit_ihno_lm_1)
```

```
## # A tibble: 2 x 6
##   Res.Df   RSS    Df `Sum of Sq`     F     `Pr(>F)`
## *  <dbl> <dbl> <dbl>       <dbl> <dbl>        <dbl>
## 1     84  253.    NA        NA    NA   NA          
## 2     83  188.     1        65.3  28.8  0.000000700
```

Test the main effect of math phobia

```r
anova(fit_ihno_lm_0, fit_ihno_lm_2)
```

```
## # A tibble: 2 x 6
##   Res.Df   RSS    Df `Sum of Sq`     F  `Pr(>F)`
## *  <dbl> <dbl> <dbl>       <dbl> <dbl>     <dbl>
## 1     84  253.    NA        NA    NA   NA       
## 2     83  221.     1        32.3  12.1  0.000791
```


Test the main effect of math phobia,  after controlling for math test

```r
anova(fit_ihno_lm_1, fit_ihno_lm_3) 
```

```
## # A tibble: 2 x 6
##   Res.Df   RSS    Df `Sum of Sq`     F `Pr(>F)`
## *  <dbl> <dbl> <dbl>       <dbl> <dbl>    <dbl>
## 1     83  188.    NA        NA   NA     NA     
## 2     82  175.     1        12.6  5.88   0.0175
```

Test the interaction between math test and math phobia (i.e. moderation)

```r
anova(fit_ihno_lm_3, fit_ihno_lm_4)
```

```
## # A tibble: 2 x 6
##   Res.Df   RSS    Df `Sum of Sq`      F `Pr(>F)`
## *  <dbl> <dbl> <dbl>       <dbl>  <dbl>    <dbl>
## 1     82  175.    NA       NA    NA       NA    
## 2     81  173.     1        1.69  0.789    0.377
```



## Checking Assumptions via Residual Diagnostics

Before reporting a model, make sure to check the residules to ensure that the model assumptions are not violated.



```r
plot(fit_ihno_lm_3, which = 1)
```

<img src="70-example_Ihno_files/figure-html/unnamed-chunk-13-1.png" width="672" />


```r
plot(fit_ihno_lm_3, which = 2)
```

<img src="70-example_Ihno_files/figure-html/unnamed-chunk-14-1.png" width="672" />



```r
car::residualPlots(fit_ihno_lm_3)
```

<img src="70-example_Ihno_files/figure-html/unnamed-chunk-15-1.png" width="672" />

```
##            Test stat Pr(>|Test stat|)  
## mathquiz     -1.7778          0.07918 .
## phobia        0.5004          0.61813  
## Tukey test   -1.5749          0.11527  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## Conclusion

### Tabulate the Final Model Summary

Many journals prefer that regression tables include 95% confidence intervals, rater than standard errors for the beta estimates.

<div class="rmdlightbulb">
<p>The <code>texreg</code> package contains three version of the regression table function.</p>
<ul>
<li><code>screenreg()</code> Use when working on a project and viewing tables on your computer screen</li>
<li><code>htmlreg()</code> Use when knitting your <code>.Rmd</code> file to a <code>.html</code> document</li>
<li><code>texreg()</code> Use when knitting your <code>.Rmd</code> file to a <code>.pdf</code> via LaTeX</li>
</ul>
</div>


```r
texreg::htmlreg(fit_ihno_lm_3,
               custom.model.names = "Main Effects Model",
               ci.force = TRUE,                              # request 95% conf interv
               caption = "Final Model for Stat's Quiz",
               single.row = TRUE)
```


<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
<caption align="bottom" style="margin-top:0.3em;">Final Model for Stat's Quiz</caption>
<tr>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b></b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Main Effects Model</b></th>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">(Intercept)</td>
<td style="padding-right: 12px; border: none;">5.02 [3.79; 6.25]<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">mathquiz</td>
<td style="padding-right: 12px; border: none;">0.08 [0.05; 0.12]<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">phobia</td>
<td style="padding-right: 12px; border: none;">-0.16 [-0.29; -0.03]<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="border-top: 1px solid black;">R<sup style="vertical-align: 0px;">2</sup></td>
<td style="border-top: 1px solid black;">0.31</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Adj. R<sup style="vertical-align: 0px;">2</sup></td>
<td style="padding-right: 12px; border: none;">0.29</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. obs.</td>
<td style="padding-right: 12px; border: none;">85</td>
</tr>
<tr>
<td style="border-bottom: 2px solid black;">RMSE</td>
<td style="border-bottom: 2px solid black;">1.46</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;" colspan="3"><span style="font-size:0.8em"><sup>*</sup> 0 outside the confidence interval</span></td>
</tr>
</table>


### Plot the Model

When a model only contains main effects, a plot is not important for interpretation, but can help understand the relationship between multiple predictors.

<div class="rmdlightbulb">
<p>When plotting a regression model the outcome (dependent variable) is always on the y-axis (<code>fit</code>) and only one predictor (independent variable) may be used on the x-axis. You may incorporate additional predictor using colors, shapes, linetypes, or facets. For these predictors, you will want to specify only 2-4 values for illustration and then declare them as factors prior to plotting.</p>
</div>


```r
effects::Effect(focal.predictors = c("mathquiz", "phobia"),
                mod = fit_ihno_lm_3,
                xlevels = list(phobia = c(0, 5, 10))) %>%   # values for illustration
  data.frame %>% 
  dplyr::mutate(phobia = factor(phobia)) %>%               # factor for illustration
  ggplot() +
  aes(x = mathquiz,
      y = fit,
      fill = phobia,
      color = phobia) +
  geom_ribbon(aes(ymin = fit - se, 
                  ymax = fit + se),
              alpha = .3) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Score on Math Quiz",
       y = "Estimated Marginal Mean\nScore on Stat Quiz",
       fill  = "Self Rated\nMath Phobia",
       color = "Self Rated\nMath Phobia") +
  theme(legend.background = element_rect(color = "black"),
        legend.position = c(0, 1),
        legend.justification = c(0, 1))
```

<img src="70-example_Ihno_files/figure-html/unnamed-chunk-19-1.png" width="672" />



