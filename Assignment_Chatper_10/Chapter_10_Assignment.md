# Chapter_10_Assignment




```r
library(rethinking)
```

```
## Loading required package: rstan
```

```
## Warning: package 'rstan' was built under R version 3.2.5
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

```
## Loading required package: StanHeaders
```

```
## Warning: package 'StanHeaders' was built under R version 3.2.5
```

```
## rstan (Version 2.11.1, packaged: 2016-07-28 18:19:31 UTC, GitRev: 85f7a56811da)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## rstan_options(auto_write = TRUE)
## options(mc.cores = parallel::detectCores())
```

```
## Loading required package: parallel
```

```
## rethinking (Version 1.59)
```

```r
library(ggplot2)
```


## 10E1
_If an event has probability of 0.35 what are the log-odds of this event?_

The log odds are 

```r
log(.35/(1-.35))
```

```
## [1] -0.6190392
```

## 10E2
_If an event has log-odds of 3.2, what is the probability of this event?_

The probability is

```r
logistic(3.2) # this is p/(1-p)
```

```
## [1] 0.9608343
```

## 10E3
_Suppose that a coefficient in a logistic regression has a value of 1.7.  What does this imply about the proportional change in odds ofthe outcome_

There is a proportional increase of

```r
exp(1.7)
```

```
## [1] 5.473947
```
in the odds of the event

## 10M1
_Bionomial data can be organized as aggregated or disaggregated without any impact on inference.  The likelihood of the data does change.  Why?_


```r
dbinom(5,10,.5)
```

```
## [1] 0.2460938
```

```r
dbinom(rep(c(0,1),each=5),1,.5)
```

```
##  [1] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
```

The latter does not take into account all of the different ways that the observations could have occured.

## 10M3
_Explain why the logit link is appropriate for a binomial model_

Because it allows modeling of p, the important parameter for the binomial, as a linear function of the predictors, and limits p to between 0 and 1.

## 10H1

