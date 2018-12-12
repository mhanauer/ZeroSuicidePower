# ZeroSuicidePower
---
---
title: "ITS"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library packages

Good time series information: https://www.itl.nist.gov/div898/handbook/pmc/section4/pmc4431.htm

More good time series information: https://otexts.org/fpp2/stationarity.html

Good time series: https://datascienceplus.com/time-series-analysis-in-r-part-1-the-time-series-object/

Good information on time series in R: https://datascienceplus.com/time-series-analysis-in-r-part-2-time-series-transformations/
```{r}
library(lavaan)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(paran)
library(caret)
library(ggplot2)
library(pracma)
library(AER)
library(pscl)
library(TSA)
library(TTR)
library(smooth)
library(greybox)
library(tseries)
library(descr)
library(urca)
library(forecast)
```
Ok start with a linear model and estimate the power
Variables to manipulate
n total
slope
n for intervention

```{r}
matt_power = function(){
n = 12*14
slope = .25
intervention = c(rep(0,n*.75), rep(1,n*.25))
y = 0 + slope*intervention+rnorm(n, mean = 0, sd= .5)
dat = data.frame(y, intervention)
model_lm = lm(y ~ intervention, data = dat)
model_lm_sum = (summary(model_lm))
power = ifelse(model_lm_sum$coefficients[,4][2] < .05, 1, 0)
}

reps = 10000
power = data.frame(replicate(reps, matt_power()))
power = apply(power, 2, sum)/reps
power

```
Now try with count data

We want about a 25% decrease (keep it as increase for simplicity) in the odds of success, which means that we need a parameter estimate of .22, because we get the exp later on
```{r}
matt_power_p = function(){
n = 12*14
b0 = 0
b1 = .22
intervention = c(rep(0,n*.75), rep(1,n*25))
y = rpois(n, exp(b0+b1*intervention))
dat = data.frame(y, intervention)
dat
model_p = glm(y ~ intervention, family = "poisson", data = dat)
model_p = summary(model_p)
power = ifelse(model_lm_sum$coefficients[,4][2] < .05, 1, 0)
}

reps = 10000
power = data.frame(replicate(reps, matt_power()))
power = apply(power, 2, sum)/reps
power
```
