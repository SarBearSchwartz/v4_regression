# Example: Ihno's Experiment


## Packages


```r
library(tidyverse)
library(haven)
library(furniture)
library(texreg)
library(stargazer)
```



## The Data


Author's website for the textbook: http://www.psych.nyu.edu/cohen/EPS4e.html 


```r
data_ihno <- read_spss("http://www.psych.nyu.edu/cohen/Ihno_dataset.sav") %>% 
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

data_ihno
```

```
## # A tibble: 100 x 18
##    sub_num gender major reason exp_cond coffee num_cups phobia prevmath
##      <dbl> <fct>  <fct> <fct>  <fct>    <fct>     <dbl>  <dbl>    <dbl>
##  1       1 Female Psyc~ Advis~ Easy     Regul~        0      1        3
##  2       2 Female Psyc~ Perso~ Easy     Not a~        0      1        4
##  3       3 Female Psyc~ Progr~ Easy     Not a~        0      4        1
##  4       4 Female Psyc~ Progr~ Easy     Not a~        0      4        0
##  5       5 Female Psyc~ Progr~ Easy     Not a~        1     10        1
##  6       6 Female Psyc~ Progr~ Moderate Regul~        1      4        1
##  7       7 Female Psyc~ Progr~ Moderate Not a~        0      4        2
##  8       8 Female Psyc~ Advis~ Moderate Regul~        2      4        1
##  9       9 Female Psyc~ Progr~ Moderate Not a~        0      4        1
## 10      10 Female Psyc~ Progr~ Moderate Regul~        2      5        0
## # ... with 90 more rows, and 9 more variables: mathquiz <dbl>,
## #   statquiz <dbl>, exp_sqz <dbl>, hr_base <dbl>, hr_pre <dbl>,
## #   hr_post <dbl>, anx_base <dbl>, anx_pre <dbl>, anx_post <dbl>
```

### Describe the Raw Data

#### Table of Summary Statistics


```r
data_ihno %>% 
  furniture::table1(phobia, statquiz, mathquiz,
                    test = TRUE,
                    output = "markdown")
```



|          | Mean/Count (SD/%) |
|----------|-------------------|
|          |      n = 100      |
|  phobia  |                   |
|          |     3.3 (2.4)     |
| statquiz |                   |
|          |     6.9 (1.7)     |
| mathquiz |                   |
|          |    29.1 (9.5)     |




```r
data_ihno %>% 
  dplyr::mutate(phobia_cut3 = cut(phobia, 
                                  breaks = c(0, 2, 4, 10),
                                  include.lowest = TRUE)) %>% 
  furniture::table1(phobia, statquiz, mathquiz,
                    splitby = ~ phobia_cut3,
                    test = TRUE,
                    output = "markdown")
```



|          |   [0,2]    |   (2,4]    |   (4,10]   | P-Value |
|----------|------------|------------|------------|---------|
|          |   n = 39   |   n = 37   |   n = 24   |         |
|  phobia  |            |            |            |  <.001  |
|          | 1.0 (0.8)  | 3.6 (0.5)  | 6.7 (1.8)  |         |
| statquiz |            |            |            |  0.001  |
|          | 7.6 (1.3)  | 6.6 (1.6)  | 6.1 (2.0)  |         |
| mathquiz |            |            |            |  0.014  |
|          | 32.6 (8.5) | 26.5 (9.8) | 26.8 (8.9) |         |




#### Plot of Raw Data


```r
data_ihno %>% 
  ggplot() +
  aes(phobia) +
  geom_histogram(binwidth = 1)
```

<img src="70-example_Ihno_files/figure-html/unnamed-chunk-5-1.png" width="672" />



```r
data_ihno %>% 
  dplyr::mutate(phobia_cut3 = cut(phobia, 
                                  breaks = c(0, 2, 4, 10),
                                  include.lowest = TRUE)) %>% 
  ggplot() +
  aes(phobia,
      fill = phobia_cut3) +
  geom_histogram(binwidth = 1)
```

<img src="70-example_Ihno_files/figure-html/unnamed-chunk-6-1.png" width="672" />




```r
data_ihno %>% 
  ggplot() +
  aes(x = mathquiz,
      y = statquiz) +
  geom_point()
```

```
## Warning: Removed 15 rows containing missing values (geom_point).
```

<img src="70-example_Ihno_files/figure-html/unnamed-chunk-7-1.png" width="672" />


```r
data_ihno %>% 
  dplyr::mutate(phobia_cut3 = cut(phobia, 
                                  breaks = c(0, 2, 4, 10),
                                  include.lowest = TRUE)) %>% 
  ggplot() +
  aes(x = mathquiz,
      y = statquiz,
      color = phobia_cut3) +
  geom_point()
```

```
## Warning: Removed 15 rows containing missing values (geom_point).
```

<img src="70-example_Ihno_files/figure-html/unnamed-chunk-8-1.png" width="672" />



```r
data_ihno %>% 
  dplyr::mutate(phobia_cut3 = cut(phobia, 
                                  breaks = c(0, 2, 4, 10),
                                  include.lowest = TRUE)) %>% 
  ggplot() +
  aes(x = mathquiz,
      y = statquiz) +
  geom_count() +
  facet_grid(. ~ phobia_cut3)
```

```
## Warning: Removed 15 rows containing non-finite values (stat_sum).
```

<img src="70-example_Ihno_files/figure-html/unnamed-chunk-9-1.png" width="672" />


## Regression

### Fit Nested Models (bottom up)



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

texreg::htmlreg(list(fit_ihno_lm_0, 
                     fit_ihno_lm_1, fit_ihno_lm_2, 
                     fit_ihno_lm_3, fit_ihno_lm_4),
                  custom.model.names = c("No Predictors", "Only Math Quiz", 
                                         "Only Phobia", "Both IVs", 
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

### Compare Models

Likelihood Ratio Test of Nested Models


```r
anova(fit_ihno_lm_0, fit_ihno_lm_1) %>% 
  pander::pander(caption = "LRT: Main Effect of Math Quiz")
```


-----------------------------------------------------
 Res.Df    RSS    Df   Sum of Sq     F      Pr(>F)   
-------- ------- ---- ----------- ------- -----------
   84      253    NA      NA        NA        NA     

   83     187.8   1      65.26     28.85   6.999e-07 
-----------------------------------------------------

Table: LRT: Main Effect of Math Quiz



```r
anova(fit_ihno_lm_0, fit_ihno_lm_2) %>% 
  pander::pander(caption = "LRT: Main Effect of Math Phobia")
```


-----------------------------------------------------
 Res.Df    RSS    Df   Sum of Sq     F      Pr(>F)   
-------- ------- ---- ----------- ------- -----------
   84      253    NA      NA        NA        NA     

   83     220.7   1      32.28     12.14   0.0007912 
-----------------------------------------------------

Table: LRT: Main Effect of Math Phobia




```r
anova(fit_ihno_lm_1, fit_ihno_lm_3) %>% 
  pander::pander(caption = "LRT: Main Effect of Math Phobia, 
                            after controlling for Math Test")
```


--------------------------------------------------
 Res.Df    RSS    Df   Sum of Sq     F     Pr(>F) 
-------- ------- ---- ----------- ------- --------
   83     187.8   NA      NA        NA       NA   

   82     175.2   1      12.57     5.881   0.0175 
--------------------------------------------------

Table: LRT: Main Effect of Math Phobia, 
                            after controlling for Math Test



```r
anova(fit_ihno_lm_3, fit_ihno_lm_4) %>% 
  pander::pander(caption = "LRT: Interaction between Math Test and Math Phobia")
```


---------------------------------------------------
 Res.Df    RSS    Df   Sum of Sq     F      Pr(>F) 
-------- ------- ---- ----------- -------- --------
   82     175.2   NA      NA         NA       NA   

   81     173.5   1      1.689     0.7887   0.3771 
---------------------------------------------------

Table: LRT: Interaction between Math Test and Math Phobia



### Residual Diagnostics



```r
par(mfrow = c(2, 2))
plot(fit_ihno_lm_3)
```

<img src="70-example_Ihno_files/figure-html/unnamed-chunk-15-1.png" width="672" />

```r
par(mfrow = c(1, 1))
```


### Final Model


```r
texreg::htmlreg(fit_ihno_lm_3,
               custom.model.names = "Main Effects Model",
               ci.force = TRUE,
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


### Plot Model


```r
effects::Effect(focal.predictors = c("mathquiz", "phobia"),
                mod = fit_ihno_lm_3,
                xlevels = list(phobia = c(0, 5, 10))) %>% 
  data.frame %>% 
  dplyr::mutate(phobia = factor(phobia)) %>% 
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

<div class="figure">
<img src="70-example_Ihno_files/figure-html/unnamed-chunk-17-1.png" alt="Illustration of the effects of the background math quiz score and self-rated math phobia on the baseline statistics quiz. Confidence bands represent one standard error from the mean." width="672" />
<p class="caption">(\#fig:unnamed-chunk-17)Illustration of the effects of the background math quiz score and self-rated math phobia on the baseline statistics quiz. Confidence bands represent one standard error from the mean.</p>
</div>






















