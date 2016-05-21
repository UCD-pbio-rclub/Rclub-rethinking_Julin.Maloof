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
## [1] 419.0382
## attr(,"pD")
## [1] 2.940279
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
## [1] 421.0564
## attr(,"lppd")
## [1] -206.48
## attr(,"pWAIC")
## [1] 4.048152
## attr(,"se")
## [1] 14.29656
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
## [1] 194.5674
## attr(,"pD")
## [1] 3.377029
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
## [1] 195.2194
## attr(,"lppd")
## [1] -93.99989
## attr(,"pWAIC")
## [1] 3.609833
## attr(,"se")
## [1] 8.597101
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
## [1] 3.272274
```

```r
attr(DIC(m.narrow),"pD")
```

```
## [1] 2.743143
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
## [1] 3.843336
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
## [1] 3.95098
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
## [1] 3.138907
```

```r
attr(DIC(m.narrow),"pD")
```

```
## [1] 2.838966
```

```r
attr(DIC(m.narrow1),"pD")
```

```
## [1] 2.715964
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
## [1] 4.06807
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
## [1] 3.945525
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
## [1] 3.659229
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

![](Chapter_6_files/figure-html/unnamed-chunk-7-1.png)
  
The 4, 5, and 6 factor models perform similarly by WAIC and each carry a substatial proportion of the weight.  The simpler models perform much less well.

## 6H2


```r
range(d1$age)
```

```
## [1] -1.41440  2.60548
```

```r
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

![](Chapter_6_files/figure-html/unnamed-chunk-8-1.png)

```
## NULL
## NULL
## NULL
## NULL
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

![](Chapter_6_files/figure-html/unnamed-chunk-8-2.png)

```
## NULL
## NULL
## NULL
## NULL
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

![](Chapter_6_files/figure-html/unnamed-chunk-8-3.png)

```
## NULL
## NULL
## NULL
## NULL
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

![](Chapter_6_files/figure-html/unnamed-chunk-8-4.png)

```
## NULL
## NULL
## NULL
## NULL
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

![](Chapter_6_files/figure-html/unnamed-chunk-8-5.png)

```
## NULL
## NULL
## NULL
## NULL
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

![](Chapter_6_files/figure-html/unnamed-chunk-8-6.png)

```
## NULL
## NULL
## NULL
## NULL
```

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
str(height.ensemble)
```

```
## List of 2
##  $ link: num [1:1000, 1:50] -44.2 -79.5 -69.9 -68.1 -69.9 ...
##  $ sim : num [1:1000, 1:50] -65.7 -59.7 -64.3 -37.3 -97.8 ...
##  - attr(*, "weights")= Named num [1:6] 6.90e-103 1.08e-49 1.14e-06 5.00e-01 2.78e-01 ...
##   ..- attr(*, "names")= chr [1:6] "M1" "M2" "M3" "M4" ...
##  - attr(*, "indices")= num [1:6, 1:2] 1 1 1 1 501 779 0 0 0 500 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:6] "M1" "M2" "M3" "M4" ...
##   .. ..$ : chr [1:2] "idx_start" "idx_end"
##  - attr(*, "ictab")=Formal class 'compareIC' [package "rethinking"] with 2 slots
##   .. ..@ output:'data.frame':	6 obs. of  3 variables:
##   .. .. ..$ IC    : num [1:6] 2395 2150 1952 1926 1927 ...
##   .. .. ..$ dIC   : num [1:6] 469.08 224.11 25.98 0 1.17 ...
##   .. .. ..$ weight: num [1:6] 6.90e-103 1.08e-49 1.14e-06 5.00e-01 2.78e-01 ...
##   .. ..@ dSE   : logi [1:6, 1:6] NA NA NA NA NA NA ...
```

```r
mu <- apply(height.ensemble$link,2,mean)    
mu.PI <- apply(height.ensemble$link,2,PI)
plot(d1$height~d1$age,xlim=range(pred.df$age),ylim=range(c(d1$height,mu)),col=rangi2)
lines(mu ~ pred.df$age)
shade(mu.PI,pred.df$age)
```

![](Chapter_6_files/figure-html/unnamed-chunk-9-1.png)

So the nice thing about this is that the predictions do not go haywire once we are out of the observed range.
