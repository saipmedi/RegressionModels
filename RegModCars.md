---
title: "RegressionModels_CourseProject"
author: "Sai Preteek Medi"
date: "Saturday, June 20, 2015"
output: pdf_document
---
# Regression Models Course Project: 

## MPG Differential based on Transmission Type.

### This Analysis satisfies the Course Project requirements for the Regression Models Course offered by Johns Hopkins University and Coursera. The principal focus of the research is to investigate trends in vehicle miles per gallon as a function of transmission type. This research will quantify the relationship between transmission type and MPG.Our best model including Number of Cylinders, VS, and Transmission type has estimated an MPG differential of 3.164892 between cars with Automatic (22.809 MPG) and Manual (25.974 MPG) Transmissions.

## Data Processing:
### This section is to load and transform the Motor Trend's Car Data. We also perform variable conversion as factors for proper analysis.

```r
data(mtcars)
str(mtcars) #producing factor variables allow plots to show the levels for certain variables.
```

```
## 'data.frame':	32 obs. of  11 variables:
##  $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
##  $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
##  $ disp: num  160 160 108 258 360 ...
##  $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
##  $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
##  $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
##  $ qsec: num  16.5 17 18.6 19.4 17 ...
##  $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
##  $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
##  $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
##  $ carb: num  4 4 1 1 2 1 4 2 2 4 ...
```

```r
mtcars$vs<-factor(mtcars$vs)
mtcars$cyl<-factor(mtcars$cyl)
mtcars$am<-factor(mtcars$am)
mtcars$carb<-factor(mtcars$carb)
mtcars$gear<-factor(mtcars$gear)
```

## EDA:
### Exploratory Data Analysis is the process of investigating the relationships between the variables in our data. We will use plotting and linear models to explore trends.

```r
pairs(mtcars)   #This plot allows preliminary Exploratory Data Analysis.
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 
### It seems apparent from the top row of the pairs plot that the variables "cyl", "disp", "hp", "wt", "vs", "am", "carb", and "gear" strongly correlate with mpg. 


```r
library(ggplot2);library(gridExtra)
par(mfrow=c(2,2))
cyl<-qplot(cyl, mpg, data=mtcars,colour=cyl,fill=cyl, geom=c("boxplot","jitter"), main="Number of Cylinders")
vs<-qplot(vs, mpg, data=mtcars,colour=vs,fill=vs, geom=c("boxplot","jitter"), main="Type of VS")
gear<-qplot(gear, mpg, data=mtcars,colour=gear,fill=gear, geom=c("boxplot","jitter"), main="Number of Gears")
am<-qplot(am, mpg, data=mtcars,colour=am,fill=am, geom=c("boxplot","jitter"),main="Transmission Type")
grid.arrange(cyl,vs,gear,am,ncol=2,nrow=2)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

## Analysis: 
### Using the findings of the previous section's EDA, we will perform Simple Linear Regression - lm(mpg~am,data=mtcars)

```r
fitBasic<-lm(mpg~am,data=mtcars)
coef(summary(fitBasic))#using only transmission type 
```

```
##              Estimate Std. Error   t value     Pr(>|t|)
## (Intercept) 17.147368   1.124603 15.247492 1.133983e-15
## am1          7.244939   1.764422  4.106127 2.850207e-04
```
#### Our beta0 coefficient for the simple model is ~17.147 which is the mean MPG for cars with automatic transmission. The beta1 coefficient is the mean increase in MPG for cars with manual transmission. Summing beta0+beta1 will give the mean MPG for cars with manual transmission. 

#### We can also calculate the 95% confidence interval for the mean MPG difference:


```r
pe <- coef(summary(fitBasic))[2, 1]
se <- coef(summary(fitBasic))[2, 2]
tstat <- qt(1 - 0.05/2, length(mtcars$mpg) - 2) 
pe + c(-1, 1) * (se * tstat)
```

```
## [1]  3.64151 10.84837
```
#### The P value being 2.85e-4 is small enough for the estimation to remain significant. The Confidence Intervals do not include zero, using these two parameters, we can reject the null hypothesis and accept the significant difference in MPG between these two groups.


### We can now explore other Linear Models by including additional regressors:

```r
fitAll<-lm(mpg~.,data=mtcars)
fit2<-lm(mpg~am+cyl,data=mtcars)
fit3<-lm(mpg~am+cyl+vs,data=mtcars)
coef(summary(fitAll)) #using all variables as regressors
```

```
##                Estimate  Std. Error     t value   Pr(>|t|)
## (Intercept) 23.87913244 20.06582026  1.19004018 0.25252548
## cyl6        -2.64869528  3.04089041 -0.87102622 0.39746642
## cyl8        -0.33616298  7.15953951 -0.04695316 0.96317000
## disp         0.03554632  0.03189920  1.11433290 0.28267339
## hp          -0.07050683  0.03942556 -1.78835344 0.09393155
## drat         1.18283018  2.48348458  0.47627845 0.64073922
## wt          -4.52977584  2.53874584 -1.78425732 0.09461859
## qsec         0.36784482  0.93539569  0.39325050 0.69966720
## vs1          1.93085054  2.87125777  0.67247551 0.51150791
## am1          1.21211570  3.21354514  0.37718957 0.71131573
## gear4        1.11435494  3.79951726  0.29328856 0.77332027
## gear5        2.52839599  3.73635801  0.67670068 0.50889747
## carb2       -0.97935432  2.31797446 -0.42250436 0.67865093
## carb3        2.99963875  4.29354611  0.69863900 0.49546781
## carb4        1.09142288  4.44961992  0.24528452 0.80956031
## carb6        4.47756921  6.38406242  0.70136677 0.49381268
## carb8        7.25041126  8.36056638  0.86721532 0.39948495
```

```r
pe <- coef(summary(fitAll))[10, 1]
se <- coef(summary(fitAll))[10, 2]
tstat <- qt(1 - 0.05/2, length(mtcars$mpg) - 2) 
pe + c(-1, 1) * (se * tstat) 
```

```
## [1] -5.350819  7.775050
```
#### The Confidence Interval for fitAlll includes 0 and the P value is large 0.711.

```r
coef(summary(fit2))  # using regressors: am,cyl
```

```
##               Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)  24.801852   1.322615 18.752135 2.182425e-17
## am1           2.559954   1.297579  1.972869 5.845717e-02
## cyl6         -6.156118   1.535723 -4.008612 4.106131e-04
## cyl8        -10.067560   1.452082 -6.933187 1.546574e-07
```

```r
pe <- coef(summary(fit2))[2, 1]
se <- coef(summary(fit2))[2, 2]
tstat <- qt(1 - 0.05/2, length(mtcars$mpg) - 2) 
pe + c(-1, 1) * (se * tstat)
```

```
## [1] -0.09005587  5.20996328
```
#### The Confidence interval for fit2 barely includes 0 and the p value is greater than 0.05 at 0.05845

```r
coef(summary(fit3))  # using regressors: am,cyl,vs
```

```
##              Estimate Std. Error   t value         Pr(>|t|)
## (Intercept) 22.809113   2.928225  7.789398 0.00000002240449
## am1          3.164892   1.528283  2.070882 0.04805371408107
## cyl6        -5.398674   1.837466 -2.938107 0.00668336372203
## cyl8        -8.161240   2.891755 -2.822245 0.00884142859394
## vs1          1.708062   2.234961  0.764247 0.45134784638495
```

```r
pe <- coef(summary(fit3))[2, 1]
se <- coef(summary(fit3))[2, 2]
tstat <- qt(1 - 0.05/2, length(mtcars$mpg) - 2) 
pe + c(-1, 1) * (se * tstat)
```

```
## [1] 0.04372278 6.28606189
```
#### The Linear Model including am, vs, and cyl has a barely acceptable p value at 0.048 and the confidence intervals barely avoid 0. We can accept this model as the best fitting model to estimate MPG differentials between cars with Automatic (22.809 MPG) and Manual (25.974 MPG) Transmissions.


### Residuals plots:

```r
residFinal<-qplot(fit3$fitted,fit3$residuals, data=fit3)+geom_line(method="lm")
residBasic<-qplot(fitBasic$fitted,fitBasic$residuals,data=fitBasic)+geom_smooth(method="lm")
grid.arrange(residBasic,residFinal, ncol=2)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 
### The Residuals plot of the basic model is much more clean than the model including cyl, am and vs as regressors. There seem to be significant outliers at the min and max values of the fitted models.

