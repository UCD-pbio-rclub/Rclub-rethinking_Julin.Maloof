# Chapter 6 part 1

_Julin Maloof_

## 6E1

The three motivating criteria for information entropy are:

1. The scale should be continuous such that small changes in probability don't result in jumps in our measure of uncertainty.
2. Uncertainty should increase as the number of possible outcome increases.  This makes sense because if there are more choices it is of course less certain which one will happen.
3. Uncertainty should be additive.  If there are different combinations of outcomes the uncertainty associated with their combinations should be the sum of the uncertainty of each outcome.

## 6E2

_What is the entropy of a coin that is 70% Heads?_


```r
probs <- c(.7,.3)
-sum(probs*log(probs))
```

```
## [1] 0.6108643
```

## 6E3

_A four sided die with probs 20%, 25%, 25%, 30%.  What is the entropy?_


```r
probs <- c(.2,.25,.25,.3)
-sum(probs*log(probs))
```

```
## [1] 1.376227
```

## 6E4

_What is the enrtopy of a four-sided die where the fourth side never shows and the other three are equal_


```r
probs <- rep(1/3,3)
-sum(probs*log(probs))
```

```
## [1] 1.098612
```

## 6M1

_Write down and compare the defintions of AIC, DIC, and WAIC.  Which one is most general?  Which assumptions are required to transform a more general criteria into a less general one?_

AIC is the training training deviance + 2 * the number of parameters in the model

DIC is average training Deviance across the posterior + the difference between the average training deviance and the deviance calculate using the posterior mean.

WAIC is negative 2 times (the sum of the log average posterior likelihood of each observation minus the sum of the variance of average posterior likelihoods).

WAIC is most general, then DIC, then AIC.  DIC assumes that the posterior likelihood is multivariate Gaussian; AIC assumes this and the priors are uninformative.

## 6M3

_When comparing models with IC why must the same number of samples be used?_

Because these criteria all in some way sum likelihoods across samples.  So more samples means more deviance even for the same model.

### test it


```r
library(rethinking)
```

```
## Loading required package: rstan
```

```
## Loading required package: ggplot2
```

```
## rstan (Version 2.9.0-3, packaged: 2016-02-11 15:54:41 UTC, GitRev: 05c3d0058b6a)
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
## rethinking (Version 1.58)
```

```r
data(cars)
m <- map(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,30)
  ) , data=cars )
DIC(m)
```

```
## [1] 419.1422
## attr(,"pD")
## [1] 2.992301
```

```r
WAIC(m)
```

```
## Constructing posterior predictions
```

```
## [ 100 / 1000 ]
[ 200 / 1000 ]
[ 300 / 1000 ]
[ 400 / 1000 ]
[ 500 / 1000 ]
[ 600 / 1000 ]
[ 700 / 1000 ]
[ 800 / 1000 ]
[ 900 / 1000 ]
[ 1000 / 1000 ]
```

```
## [1] 421.047
## attr(,"lppd")
## [1] -206.5114
## attr(,"pWAIC")
## [1] 4.012106
## attr(,"se")
## [1] 14.21131
```

```r
cars.small <- cars[sample(1:nrow(cars),25,replace=FALSE),]

m.small <- map(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,30)
  ) , data=cars.small )
DIC(m.small)
```

```
## [1] 208.2109
## attr(,"pD")
## [1] 3.244942
```

```r
WAIC(m.small)
```

```
## Constructing posterior predictions
```

```
## [ 100 / 1000 ]
[ 200 / 1000 ]
[ 300 / 1000 ]
[ 400 / 1000 ]
[ 500 / 1000 ]
[ 600 / 1000 ]
[ 700 / 1000 ]
[ 800 / 1000 ]
[ 900 / 1000 ]
[ 1000 / 1000 ]
```

```
## [1] 210.8831
## attr(,"lppd")
## [1] -100.7084
## attr(,"pWAIC")
## [1] 4.733119
## attr(,"se")
## [1] 12.36653
```

Yes, reducing sample size reduced DIC and WAIC

## 6M4 

_What happens to the effective number of parameters in WAIC and DIC as the prior becomes more concentrated?_ 

The effective number of parameters will decrease

test it

```r
m.narrow <- map(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,10),
    b ~ dnorm(0,1),
    sigma ~ dunif(0,30)
  ) , data=cars )
attr(DIC(m),"pD")
```

```
## [1] 3.139742
```

```r
attr(DIC(m.narrow),"pD")
```

```
## [1] 2.851726
```

```r
attr(WAIC(m),"pWAIC")
```

```
## Constructing posterior predictions
```

```
## [ 100 / 1000 ]
[ 200 / 1000 ]
[ 300 / 1000 ]
[ 400 / 1000 ]
[ 500 / 1000 ]
[ 600 / 1000 ]
[ 700 / 1000 ]
[ 800 / 1000 ]
[ 900 / 1000 ]
[ 1000 / 1000 ]
```

```
## [1] 4.101428
```

```r
attr(WAIC(m.narrow),"pWAIC")
```

```
## Constructing posterior predictions
```

```
## [ 100 / 1000 ]
[ 200 / 1000 ]
[ 300 / 1000 ]
[ 400 / 1000 ]
[ 500 / 1000 ]
[ 600 / 1000 ]
[ 700 / 1000 ]
[ 800 / 1000 ]
[ 900 / 1000 ]
[ 1000 / 1000 ]
```

```
## [1] 3.599008
```

does it matter what the mean of the priors is?


```r
m.narrow1 <- map(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(-17,10),
    b ~ dnorm(4,1),
    sigma ~ dunif(0,30)
  ) , data=cars )
attr(DIC(m),"pD")
```

```
## [1] 3.12502
```

```r
attr(DIC(m.narrow),"pD")
```

```
## [1] 2.8491
```

```r
attr(DIC(m.narrow1),"pD")
```

```
## [1] 2.613259
```

```r
attr(WAIC(m),"pWAIC")
```

```
## Constructing posterior predictions
```

```
## [ 100 / 1000 ]
[ 200 / 1000 ]
[ 300 / 1000 ]
[ 400 / 1000 ]
[ 500 / 1000 ]
[ 600 / 1000 ]
[ 700 / 1000 ]
[ 800 / 1000 ]
[ 900 / 1000 ]
[ 1000 / 1000 ]
```

```
## [1] 4.133052
```

```r
attr(WAIC(m.narrow),"pWAIC")
```

```
## Constructing posterior predictions
```

```
## [ 100 / 1000 ]
[ 200 / 1000 ]
[ 300 / 1000 ]
[ 400 / 1000 ]
[ 500 / 1000 ]
[ 600 / 1000 ]
[ 700 / 1000 ]
[ 800 / 1000 ]
[ 900 / 1000 ]
[ 1000 / 1000 ]
```

```
## [1] 3.745652
```

```r
attr(WAIC(m.narrow1),"pWAIC")
```

```
## Constructing posterior predictions
```

```
## [ 100 / 1000 ]
[ 200 / 1000 ]
[ 300 / 1000 ]
[ 400 / 1000 ]
[ 500 / 1000 ]
[ 600 / 1000 ]
[ 700 / 1000 ]
[ 800 / 1000 ]
[ 900 / 1000 ]
[ 1000 / 1000 ]
```

```
## [1] 3.682299
```

not so much

## 6M5

_provide an informal explanation of why informative priors reduce overfitting_

Because they are less likely to be influenced by the particulars of the training set.

## 6M6

_provide an informal explanation of why overly informative priors may cause underfitting_

Because they coefficients are too constrained and may not adequately fit the data.


