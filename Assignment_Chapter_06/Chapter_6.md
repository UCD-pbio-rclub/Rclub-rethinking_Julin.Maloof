# Chapter 6 part 1

_Julin Maloof_


```r
knitr::opts_chunk$set(cache=TRUE)
```


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

## 6M2

Model selection is where you try to use information criteria to pick the one best model.  In model averaging you take the information across all the models, weighting it by the model's probability.  Model selection loses information about other possilbe outcomes.  I am not sure what is loss under model averaging.


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
## Warning: package 'ggplot2' was built under R version 3.2.4
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
## [1] 419.4048
## attr(,"pD")
## [1] 3.123728
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
## [1] 420.9763
## attr(,"lppd")
## [1] -206.4806
## attr(,"pWAIC")
## [1] 4.007587
## attr(,"se")
## [1] 14.26299
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
## [1] 216.3693
## attr(,"pD")
## [1] 3.089107
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
## [1] 218.3087
## attr(,"lppd")
## [1] -105.132
## attr(,"pWAIC")
## [1] 4.022302
## attr(,"se")
## [1] 10.64524
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
## [1] 3.034697
```

```r
attr(DIC(m.narrow),"pD")
```

```
## [1] 2.598366
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
## [1] 3.927035
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
## [1] 3.547529
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
## [1] 3.155912
```

```r
attr(DIC(m.narrow),"pD")
```

```
## [1] 2.847621
```

```r
attr(DIC(m.narrow1),"pD")
```

```
## [1] 2.662935
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
## [1] 3.745118
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
## [1] 3.592136
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
## [1] 3.442198
```

not so much

## 6M5

_provide an informal explanation of why informative priors reduce overfitting_

Because they are less likely to be influenced by the particulars of the training set.

## 6M6

_provide an informal explanation of why overly informative priors may cause underfitting_

Because they coefficients are too constrained and may not adequately fit the data.


## 6H1


```r
library(rethinking)
data(Howell1)
d <- Howell1
d$age <- (d$age - mean(d$age))/sd(d$age)
head(d)
```

```
##    height   weight       age male
## 1 151.765 47.82561 1.6222002    1
## 2 139.700 36.48581 1.6222002    0
## 3 136.525 31.86484 1.7186002    0
## 4 156.845 53.04191 0.5618002    1
## 5 145.415 41.27687 1.0438002    0
## 6 163.830 62.99259 0.2726002    1
```

```r
set.seed( 1000 )
i <- sample(1:nrow(d),size=nrow(d)/2)
d1 <- d[ i , ]
d2 <- d[ -i , ]

M1 <- map(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age,
  a ~ dnorm(mean(height),50),
  b1 ~ dnorm(0,10),
  sigma ~ dunif(0,50)),
  data=d1)
precis(M1)
```

```
##         Mean StdDev   5.5%  94.5%
## a     138.44   1.18 136.55 140.34
## b1     18.39   1.19  16.49  20.29
## sigma  19.54   0.84  18.20  20.88
```

```r
M2 <- map(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age +b2*age^2,
  a ~ dnorm(mean(height),50),
  c(b1, b2) ~ dnorm(0,10),
  sigma ~ dunif(0,50)),
  data=d1)
precis(M2)
```

```
##         Mean StdDev   5.5%  94.5%
## a     152.25   1.02 150.62 153.88
## b1     25.58   0.83  24.25  26.91
## b2    -13.98   0.70 -15.09 -12.86
## sigma  12.38   0.53  11.53  13.23
```

```r
M3 <- map(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age^2 +b3*age^3,
  a ~ dnorm(mean(height),50),
  c(b1,b2,b3) ~ dnorm(0,10),
  sigma ~ dunif(0,50)),
  data=d1)
precis(M3)
```

```
##         Mean StdDev   5.5%  94.5%
## a     158.01   0.78 156.76 159.26
## b1     11.36   1.02   9.74  12.99
## b2    -23.99   0.76 -25.20 -22.78
## b3      8.04   0.47   7.29   8.79
## sigma   8.59   0.37   8.00   9.18
```

```r
M4 <- map(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4,
  a ~ dnorm(mean(height),50),
  c(b1,b2,b3,b4) ~ dnorm(0,10),
  sigma ~ dunif(0,50)),
  data=d1)
precis(M4)
```

```
##         Mean StdDev   5.5%  94.5%
## a     156.58   0.79 155.32 157.84
## b1      5.90   1.38   3.69   8.10
## b2    -19.06   1.14 -20.89 -17.24
## b3     12.39   0.90  10.95  13.83
## b4     -2.36   0.42  -3.04  -1.68
## sigma   8.17   0.35   7.61   8.73
```

```r
M5 <- map(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4 + b5*age^5,
  a ~ dnorm(mean(height),50),
  c(b1,b2,b3,b4,b5) ~ dnorm(0,10),
  sigma ~ dunif(0,50)),
  data=d1)
precis(M5)
```

```
##         Mean StdDev   5.5%  94.5%
## a     156.65   0.91 155.20 158.11
## b1      5.82   1.45   3.50   8.14
## b2    -19.39   2.29 -23.06 -15.73
## b3     12.53   1.23  10.57  14.49
## b4     -2.20   1.08  -3.92  -0.47
## b5     -0.07   0.41  -0.71   0.58
## sigma   8.16   0.35   7.60   8.72
```

```r
M6 <- map(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4 + b5*age^5 + b6*age^6,
  a ~ dnorm(mean(height),50),
  c(b1,b2,b3,b4,b5,b6) ~ dnorm(0,10),
  sigma ~ dunif(0,50)),
  data=d1)
precis(M6)
```

```
##         Mean StdDev   5.5%  94.5%
## a     156.64   0.91 155.19 158.09
## b1      4.41   2.13   1.00   7.81
## b2    -19.16   2.30 -22.84 -15.48
## b3     15.28   3.28  10.03  20.54
## b4     -2.82   1.28  -4.86  -0.78
## b5     -1.11   1.22  -3.07   0.84
## b6      0.35   0.39  -0.27   0.97
## sigma   8.14   0.35   7.58   8.70
```

```r
(model.comp <- compare(M1,M2,M3,M4,M5,M6))
```

```
##      WAIC pWAIC dWAIC weight    SE   dSE
## M4 1926.0   5.6   0.0   0.57 25.44    NA
## M5 1927.5   6.3   1.5   0.27 25.38  0.37
## M6 1928.5   7.4   2.5   0.16 25.19  1.67
## M3 1952.3   5.4  26.3   0.00 24.20 11.00
## M2 2150.1   5.2 224.1   0.00 22.76 26.74
## M1 2395.4   3.4 469.4   0.00 23.14 31.04
```

```r
plot(model.comp)
```

![](Chapter_6_files/figure-html/unnamed-chunk-8-1.png)
  
The 4, 5, and 6 factor models perform similarly by WAIC and each carry a substatial proportion of the weight.  The simpler models perform much less well.

## 6H2


```r
range(d1$age)

pred.df <- data.frame(age=seq(-2,3.5,length.out=50))

for(model in ls(pattern="^M[1-6]$")) {
  pred.M <- link(get(model),data=pred.df)
  mu <- apply(pred.M,2,mean)
  mu.PI <- apply(pred.M,2,PI,prob=0.97)
  print(plot(d1$height~d1$age,xlim=range(pred.df$age),ylim=range(c(mu,d1$height)),main=model,col=rangi2))
  print(lines(mu ~ pred.df$age))
  print(lines(mu.PI[1,] ~ pred.df$age,lty=2))
  print(lines(mu.PI[2,] ~ pred.df$age,lty=2))
}
```

![](Chapter_6_files/figure-html/unnamed-chunk-9-1.png)![](Chapter_6_files/figure-html/unnamed-chunk-9-2.png)![](Chapter_6_files/figure-html/unnamed-chunk-9-3.png)![](Chapter_6_files/figure-html/unnamed-chunk-9-4.png)![](Chapter_6_files/figure-html/unnamed-chunk-9-5.png)![](Chapter_6_files/figure-html/unnamed-chunk-9-6.png)

M1 does a poor job fitting the actual data throughout its range.  
M2 is better through much of the data but does poorly at both ends.  
M3 does well, but perhaps a bit too much of a dip at older ages and quickly goes off range as soon as age increases beyond the observations
M4 - M6 all fit the observations nicely.  M4 has the added bonus of being stable for a bit longer after age extends beyond our observations.

## 6H3


```r
height.ensemble <- ensemble(M1,M2,M3,M4,M5,M6,data = pred.df)
```

```
## Constructing posterior predictions
## Constructing posterior predictions
## Constructing posterior predictions
## Constructing posterior predictions
## Constructing posterior predictions
## Constructing posterior predictions
```

```r
mu <- apply(height.ensemble$link,2,mean)    
mu.PI <- apply(height.ensemble$link,2,PI)
plot(d1$height~d1$age,xlim=range(pred.df$age),ylim=range(c(d1$height,mu)),col=rangi2)
lines(mu ~ pred.df$age)
shade(mu.PI,pred.df$age)
```

![](Chapter_6_files/figure-html/unnamed-chunk-10-1.png)

So the nice thing about this is that the predictions do not go haywire once we are out of the observed range.

## 6H4


```r
models <- ls(pattern="^M[1-6]$")
test.dev <- sapply(models,function(m) {
  model <- get(m)
  input <- as.list(coef(model))
  input$age <- d2$age
  equation <- model@links[[1]][[2]]
  mu <- eval(parse(text=equation),envir = input)
  dev <- -2*sum(dnorm(d2$height,mu,input$sigma,log=T))
  dev
})

test.dev
```

```
##       M1       M2       M3       M4       M5       M6 
## 2422.307 2138.064 1932.348 1876.231 1876.587 1875.823
```

## 6H5

```r
model.WAIC <- sapply(models,function(m) {
  WAIC(get(m))
})
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

```r
model.WAIC
```

```
##       M1       M2       M3       M4       M5       M6 
## 2395.567 2150.110 1952.286 1925.984 1927.639 1927.898
```

```r
test.dev-min(test.dev)
```

```
##          M1          M2          M3          M4          M5          M6 
## 546.4837386 262.2402828  56.5246997   0.4081016   0.7634164   0.0000000
```

```r
model.WAIC-min(model.WAIC)
```

```
##         M1         M2         M3         M4         M5         M6 
## 469.582510 224.125152  26.301570   0.000000   1.654824   1.913563
```
Overall the WAIC does a good job of estimating the test deviance, especially for the purpose of model comparisoins.  In both cases M4,5,and 6 have similar scores; WAIC penalizes the more complex models (M5 and M6) whereas this does not show up in the test deviance, but the differences are trivial.  Test deviance is also relatively greater for the simpler models M1, M2 and M3, but again in terms of choosing a model we would get similar results from these two methods.


