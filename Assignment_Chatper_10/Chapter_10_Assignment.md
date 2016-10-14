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

## 10E4
_Why do Poisson regressons sometimes require an offset?_

An offset is required of the measurement interval is different for different observations.  Perhaps you are comparing transposon insertion rates in two different species and in one the reported rate is per 100kb and the other is per 10kb.

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

## 10M2
_If a coefficint in a Poisson regression has a value of 1.7, what does this imply about the change in outcome?_

For this we need to use the inverse function

```r
exp(1.7)
```

```
## [1] 5.473947
```
each unit of change in the predictor increases the lambda (aka the mean and variance) by 5.47.

## 10M3
_Explain why the logit link is appropriate for a binomial model_

Because it allows modeling of p, the important parameter for the binomial, as a linear function of the predictors, and limits p to between 0 and 1.

## 10M4
_Explain why the log link is appropriate for Poisson GLM_
The log link ensures that the outcome is positive.

## 10M5
_What would it impoly to use a logit link for a Poisson GLM?  Why might you want to_
IT implies that the mean is between 0 and 1.  This makes sense if it is an impossibilty for the event to occur more than once per unit of measurement.  So if you are measureing the number of balls per widget and each widget can only hold a single ball and you measure every widget.

## 10H1

_Use `map` to construct a quadractic approximation of the posterior distribution for m10.4.  Compare to MCMC and explain the similarities and differences_

### First set up the data and run the MCMC model

```r
data("chimpanzees")
d <- chimpanzees
d2 <- d
d2$recipient <- NULL
m10.4.stan <- map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left ,
    a[actor] ~ dnorm(0,10),
    bp ~ dnorm(0,10),
    bpC ~ dnorm(0,10)
  ),
  data=d2 , chains=2 , iter=2500 , warmup=500 )
```

```
## 
## SAMPLING FOR MODEL 'pulled_left ~ dbinom(1, p)' NOW (CHAIN 1).
## 
## Chain 1, Iteration:    1 / 2500 [  0%]  (Warmup)
## Chain 1, Iteration:  250 / 2500 [ 10%]  (Warmup)
## Chain 1, Iteration:  500 / 2500 [ 20%]  (Warmup)
## Chain 1, Iteration:  501 / 2500 [ 20%]  (Sampling)
## Chain 1, Iteration:  750 / 2500 [ 30%]  (Sampling)
## Chain 1, Iteration: 1000 / 2500 [ 40%]  (Sampling)
## Chain 1, Iteration: 1250 / 2500 [ 50%]  (Sampling)
## Chain 1, Iteration: 1500 / 2500 [ 60%]  (Sampling)
## Chain 1, Iteration: 1750 / 2500 [ 70%]  (Sampling)
## Chain 1, Iteration: 2000 / 2500 [ 80%]  (Sampling)
## Chain 1, Iteration: 2250 / 2500 [ 90%]  (Sampling)
## Chain 1, Iteration: 2500 / 2500 [100%]  (Sampling)
##  Elapsed Time: 0.647754 seconds (Warm-up)
##                1.95672 seconds (Sampling)
##                2.60448 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'pulled_left ~ dbinom(1, p)' NOW (CHAIN 2).
## 
## Chain 2, Iteration:    1 / 2500 [  0%]  (Warmup)
## Chain 2, Iteration:  250 / 2500 [ 10%]  (Warmup)
## Chain 2, Iteration:  500 / 2500 [ 20%]  (Warmup)
## Chain 2, Iteration:  501 / 2500 [ 20%]  (Sampling)
## Chain 2, Iteration:  750 / 2500 [ 30%]  (Sampling)
## Chain 2, Iteration: 1000 / 2500 [ 40%]  (Sampling)
## Chain 2, Iteration: 1250 / 2500 [ 50%]  (Sampling)
## Chain 2, Iteration: 1500 / 2500 [ 60%]  (Sampling)
## Chain 2, Iteration: 1750 / 2500 [ 70%]  (Sampling)
## Chain 2, Iteration: 2000 / 2500 [ 80%]  (Sampling)
## Chain 2, Iteration: 2250 / 2500 [ 90%]  (Sampling)
## Chain 2, Iteration: 2500 / 2500 [100%]  (Sampling)
##  Elapsed Time: 0.621152 seconds (Warm-up)
##                2.36161 seconds (Sampling)
##                2.98276 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'pulled_left ~ dbinom(1, p)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 4e-06 seconds (Warm-up)
##                0.000277 seconds (Sampling)
##                0.000281 seconds (Total)
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## [ 400 / 4000 ]
[ 800 / 4000 ]
[ 1200 / 4000 ]
[ 1600 / 4000 ]
[ 2000 / 4000 ]
[ 2400 / 4000 ]
[ 2800 / 4000 ]
[ 3200 / 4000 ]
[ 3600 / 4000 ]
[ 4000 / 4000 ]
```

```
## Warning in map2stan(alist(pulled_left ~ dbinom(1, p), logit(p) <- a[actor] + : There were 7 divergent iterations during sampling.
## Check the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.
```

### Now run the quadratic approximation

```r
m10.4.map <- map(  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left ,
    a[actor] ~ dnorm(0,10),
    bp ~ dnorm(0,10),
    bpC ~ dnorm(0,10)
  ),
  data=d2)
```

### compare

```r
precis(m10.4.stan,depth=2)
```

```
## Warning in precis(m10.4.stan, depth = 2): There were 7 divergent iterations during sampling.
## Check the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.
```

```
##       Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## a[1] -0.75   0.26      -1.15      -0.31  3125    1
## a[2] 10.91   5.17       3.83      18.26  1312    1
## a[3] -1.05   0.27      -1.47      -0.60  2438    1
## a[4] -1.06   0.27      -1.49      -0.62  2816    1
## a[5] -0.75   0.26      -1.15      -0.31  2843    1
## a[6]  0.20   0.26      -0.24       0.60  2729    1
## a[7]  1.81   0.39       1.20       2.42  2921    1
## bp    0.85   0.25       0.43       1.23  1780    1
## bpC  -0.13   0.29      -0.61       0.31  2661    1
```

```r
precis(m10.4.map,depth=2)
```

```
##       Mean StdDev  5.5% 94.5%
## a[1] -0.73   0.27 -1.16 -0.30
## a[2]  6.68   3.61  0.90 12.45
## a[3] -1.03   0.28 -1.48 -0.59
## a[4] -1.03   0.28 -1.48 -0.59
## a[5] -0.73   0.27 -1.16 -0.30
## a[6]  0.21   0.27 -0.21  0.64
## a[7]  1.75   0.38  1.14  2.37
## bp    0.82   0.26  0.40  1.24
## bpC  -0.13   0.30 -0.61  0.34
```

```r
compare(m10.4.stan,m10.4.map) # not a good idea?
```

```
## Warning in compare(m10.4.stan, m10.4.map): Not all model fits of same class.
## This is usually a bad idea, because it implies they were fit by different algorithms.
## Check yourself, before you wreck yourself.
```

```
##            WAIC pWAIC dWAIC weight    SE  dSE
## m10.4.stan  529   7.9     0      1 19.95   NA
## m10.4.map   551  15.8    22      0 18.58 2.47
```

```r
coeftab(m10.4.map,m10.4.stan)
```

```
##      m10.4.map m10.4.stan
## a[1]   -0.73     -0.75   
## a[2]    6.68     10.91   
## a[3]   -1.03     -1.05   
## a[4]   -1.03     -1.06   
## a[5]   -0.73     -0.75   
## a[6]    0.21      0.20   
## a[7]    1.75      1.81   
## bp      0.82      0.85   
## bpC    -0.13     -0.13   
## nobs     504       504
```

```r
plot(coeftab(m10.4.map,m10.4.stan))
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
pairs(m10.4.map)
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

```r
pairs(m10.4.stan)
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-9-3.png)<!-- -->

The models differ the most in the posterior for a2 and a7.  These posteriors have the most non--Guassian distribution.

## 10H2
_Use WAIC to compare m10.4 to the simpler models fit to the same data_

So I guess I shoud fit the all with stan

```r
m10.1.stan <- map2stan(
  alist(
    pulled_left ~ dbinom(1,p),
    logit(p) <- a,
    a ~ dnorm(0,10)
  ),
  data=d2)
```

```
## 
## SAMPLING FOR MODEL 'pulled_left ~ dbinom(1, p)' NOW (CHAIN 1).
## 
## Chain 1, Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 1, Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 1, Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 1, Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 1, Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 1, Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 1, Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 1, Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 1, Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 1, Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 1, Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 1, Iteration: 2000 / 2000 [100%]  (Sampling)
##  Elapsed Time: 0.272146 seconds (Warm-up)
##                0.282721 seconds (Sampling)
##                0.554867 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'pulled_left ~ dbinom(1, p)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 4e-06 seconds (Warm-up)
##                0.00021 seconds (Sampling)
##                0.000214 seconds (Total)
```

```
## Computing WAIC
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
m10.2.stan <- map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a + bp*prosoc_left ,
    a ~ dnorm(0,10) ,
    bp ~ dnorm(0,10)
  ),
  data=d2 )
```

```
## 
## SAMPLING FOR MODEL 'pulled_left ~ dbinom(1, p)' NOW (CHAIN 1).
## 
## Chain 1, Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 1, Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 1, Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 1, Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 1, Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 1, Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 1, Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 1, Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 1, Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 1, Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 1, Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 1, Iteration: 2000 / 2000 [100%]  (Sampling)
##  Elapsed Time: 0.44034 seconds (Warm-up)
##                0.471916 seconds (Sampling)
##                0.912256 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'pulled_left ~ dbinom(1, p)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 4e-06 seconds (Warm-up)
##                0.000299 seconds (Sampling)
##                0.000303 seconds (Total)
```

```
## Computing WAIC
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
m10.3.stan <- map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a + (bp + bpC*condition)*prosoc_left ,
    a ~ dnorm(0,10) ,
    bp ~ dnorm(0,10) ,
    bpC ~ dnorm(0,10)
  ), data=d2 )
```

```
## 
## SAMPLING FOR MODEL 'pulled_left ~ dbinom(1, p)' NOW (CHAIN 1).
## 
## Chain 1, Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 1, Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 1, Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 1, Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 1, Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 1, Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 1, Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 1, Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 1, Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 1, Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 1, Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 1, Iteration: 2000 / 2000 [100%]  (Sampling)
##  Elapsed Time: 0.700338 seconds (Warm-up)
##                0.65195 seconds (Sampling)
##                1.35229 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'pulled_left ~ dbinom(1, p)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 4e-06 seconds (Warm-up)
##                0.000272 seconds (Sampling)
##                0.000276 seconds (Total)
```

```
## Computing WAIC
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
compare(m10.1.stan,m10.2.stan,m10.3.stan,m10.4.stan)
```

```
##             WAIC pWAIC dWAIC weight    SE   dSE
## m10.4.stan 529.0   7.9   0.0      1 19.95    NA
## m10.2.stan 680.2   1.8 151.1      0  9.47 19.25
## m10.3.stan 682.0   2.8 153.0      0  9.46 19.17
## m10.1.stan 687.9   1.0 158.9      0  7.11 19.96
```

Conclusion: the model with an individual intercept for each actor is strongly favored.

## 10H3

### A

```r
library(MASS)
data("eagles")
eagles
```

```
##    y  n P A V
## 1 17 24 L A L
## 2 29 29 L A S
## 3 17 27 L I L
## 4 20 20 L I S
## 5  1 12 S A L
## 6 15 16 S A S
## 7  0 28 S I L
## 8  1  4 S I S
```

```r
eagles$PSize <- abs(as.numeric(eagles$P)-2) # 0 is small, 1 is large
eagles$PAge <- abs(as.numeric(eagles$A)-2) #0 is immature, 1 is adult
eagles$VSize <- abs(as.numeric(eagles$V)-2) # 0 is small, 1 is large
M10h3.1 <- map(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- a + b_PS*PSize + b_PA*PAge + b_VS*VSize,
    a ~ dnorm(0,10),
    c(b_PS, b_PA, b_VS) ~ dnorm(0,5)),
  data=eagles)
precis(M10h3.1)
```

```
##       Mean StdDev  5.5% 94.5%
## a     0.59   0.66 -0.47  1.65
## b_PS  4.24   0.90  2.81  5.67
## b_PA  1.08   0.53  0.23  1.93
## b_VS -4.59   0.96 -6.13 -3.06
```

```r
M10h3.1.stan <- map2stan(M10h3.1,chains=4,cores=2)
```

```
## Warning in FUN(X[[i]], ...): data with name P is not numeric and not used
```

```
## Warning in FUN(X[[i]], ...): data with name A is not numeric and not used
```

```
## Warning in FUN(X[[i]], ...): data with name V is not numeric and not used
```

```
## Warning in FUN(X[[i]], ...): data with name P is not numeric and not used
```

```
## Warning in FUN(X[[i]], ...): data with name A is not numeric and not used
```

```
## Warning in FUN(X[[i]], ...): data with name V is not numeric and not used
```

```
## 
## SAMPLING FOR MODEL 'y ~ dbinom(n, p)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 4e-06 seconds (Warm-up)
##                4.4e-05 seconds (Sampling)
##                4.8e-05 seconds (Total)
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## [ 400 / 4000 ]
[ 800 / 4000 ]
[ 1200 / 4000 ]
[ 1600 / 4000 ]
[ 2000 / 4000 ]
[ 2400 / 4000 ]
[ 2800 / 4000 ]
[ 3200 / 4000 ]
[ 3600 / 4000 ]
[ 4000 / 4000 ]
```

```
## Aggregated binomial counts detected. Splitting to 0/1 outcome for WAIC calculation.
```

```r
precis(M10h3.1.stan)
```

```
##       Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## a     0.67   0.67      -0.41       1.70  1572    1
## b_PS  4.69   0.96       3.22       6.21  1370    1
## b_PA  1.14   0.53       0.30       1.97  2106    1
## b_VS -5.11   1.01      -6.77      -3.60  1156    1
```

```r
plot(M10h3.1.stan)
pairs(M10h3.1.stan)
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-11-1.png)<!-- -->![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

b\_PS and b\_VS show some skew, so better to use STAN

### B

Inerpret the model


```r
precis(M10h3.1.stan)
```

```
##       Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## a     0.67   0.67      -0.41       1.70  1572    1
## b_PS  4.69   0.96       3.22       6.21  1370    1
## b_PA  1.14   0.53       0.30       1.97  2106    1
## b_VS -5.11   1.01      -6.77      -3.60  1156    1
```

The means are the log odds associated with the various predictors.  To get the odds change we need to exponentiate these.  For all but alpha the 89% confidence interval does not include 0, indicating that the predictors are important


```r
# Pirate Size
exp(4.65) #more than 100-fold increase in success if large pirate
```

```
## [1] 104.585
```

```r
# Pirate Age
exp(1.13) # 3-fold increase in success if adult pirate
```

```
## [1] 3.095657
```

```r
# Victim Size
exp(-5.06) # much less chance of success if large victim
```

```
## [1] 0.00634556
```

We can look at the probabilty of success by using this logistic function


```r
#baseline (small victim, small pirate, immature pirate)
logistic(0.66)
```

```
## [1] 0.6592604
```

```r
#probability with large pirate
logistic(0.66+4.65)
```

```
## [1] 0.9950824
```

```r
#probability with adult pirate
logistic(0.66+1.13)
```

```
## [1] 0.8569273
```

```r
#probability with large, adult pirate
logistic(0.66 + 4.65 + 1.13)
```

```
## [1] 0.9984061
```

```r
#probability with large victim (and small, immature pirate)
logistic(0.66 -5.06)
```

```
## [1] 0.01212843
```

```r
#probability with large victim and large, adult pirate)
logistic(0.66 + 4.65 + 1.13 -5.06)
```

```
## [1] 0.798991
```

Some predictions...

First get the probability predictions

```r
pred.df <- eagles[,-1]

eagle.link <- link(fit = M10h3.1.stan, data = pred.df)
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
dim(eagle.link)
```

```
## [1] 1000    8
```

```r
head(eagle.link) #these are probabilities
```

```
##           [,1]      [,2]      [,3]      [,4]        [,5]      [,6]
## [1,] 0.7674346 0.9965279 0.4953460 0.9884222 0.064872374 0.8578289
## [2,] 0.8585640 0.9996581 0.5731105 0.9984560 0.026524299 0.9292021
## [3,] 0.7248013 0.9996421 0.4512791 0.9988547 0.007687156 0.8914837
## [4,] 0.8112001 0.9954604 0.6081797 0.9875340 0.076044151 0.8077080
## [5,] 0.7175205 0.9980269 0.6279278 0.9970332 0.021554601 0.8143602
## [6,] 0.8298059 0.9987392 0.6699632 0.9969770 0.037614676 0.8639455
##             [,7]      [,8]
## [1,] 0.020217944 0.6421883
## [2,] 0.005989902 0.7437661
## [3,] 0.002413175 0.7195194
## [4,] 0.028874128 0.6027713
## [5,] 0.014425404 0.7445468
## [6,] 0.016012329 0.7255618
```

```r
pred.df$p.mean <- apply(eagle.link,2,mean)
pred.df$p.PI.low <- apply(eagle.link,2,PI)[1,]
pred.df$p.PI.high <- apply(eagle.link,2,PI)[2,]
```

Calculate count predictions


```r
pred.df$counts.mean <- pred.df$n * pred.df$p.mean
pred.df$counts.PI.low <- pred.df$n * pred.df$p.PI.low
pred.df$counts.PI.high <- pred.df$n * pred.df$p.PI.high
```

Plot the predictions


```r
#make some intellegent names for plotting
pred.df$pirate_size <- factor(pred.df$P,levels=c("S","L"),labels = c("small","large"))
pred.df$pirate_age <- factor(pred.df$A,levels=c("I","A"),labels = c("immature","adult"))
pred.df$victim_size <- factor(pred.df$V,levels=c("S","L"),labels = c("small_victim","large_victim"))

pl <- ggplot(pred.df,aes(x=pirate_size,fill=pirate_age))
pl <- pl + geom_bar(stat="identity",position="dodge")
pl <- pl + geom_errorbar(position=position_dodge(width=0.9),width=0.5)
pl <- pl + facet_grid(. ~ victim_size)
pl + aes(y=p.mean,ymax=p.PI.high,ymin=p.PI.low) + ggtitle("probability of pirating success")
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
pl + aes(y=counts.mean,ymax=counts.PI.high,ymin=counts.PI.low) + ggtitle("count of successful pirating attempts")
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-17-2.png)<!-- -->

### C


```r
M10h3.2.stan <- map2stan(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- a + b_PS*PSize + b_PA*PAge + b_VS*VSize + b_PA_PS*PSize*PAge,
    a ~ dnorm(0,10),
    c(b_PS, b_PA, b_VS, b_PA_PS) ~ dnorm(0,5)),
  data=eagles,chains = 4, cores=2)
```

```
## Warning in FUN(X[[i]], ...): data with name P is not numeric and not used
```

```
## Warning in FUN(X[[i]], ...): data with name A is not numeric and not used
```

```
## Warning in FUN(X[[i]], ...): data with name V is not numeric and not used
```

```
## Warning in FUN(X[[i]], ...): data with name P is not numeric and not used
```

```
## Warning in FUN(X[[i]], ...): data with name A is not numeric and not used
```

```
## Warning in FUN(X[[i]], ...): data with name V is not numeric and not used
```

```
## 
## SAMPLING FOR MODEL 'y ~ dbinom(n, p)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 4e-06 seconds (Warm-up)
##                7.2e-05 seconds (Sampling)
##                7.6e-05 seconds (Total)
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## [ 400 / 4000 ]
[ 800 / 4000 ]
[ 1200 / 4000 ]
[ 1600 / 4000 ]
[ 2000 / 4000 ]
[ 2400 / 4000 ]
[ 2800 / 4000 ]
[ 3200 / 4000 ]
[ 3600 / 4000 ]
[ 4000 / 4000 ]
```

```
## Aggregated binomial counts detected. Splitting to 0/1 outcome for WAIC calculation.
```

```r
plot(M10h3.2.stan)
pairs(M10h3.2.stan)
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-18-1.png)<!-- -->![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

```r
precis(M10h3.2.stan)
```

```
##          Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## a       -0.78   1.01      -2.37       0.74  1367 1.00
## b_PS     6.56   1.36       4.23       8.51  1092 1.00
## b_PA     3.44   1.21       1.55       5.38  1178 1.00
## b_VS    -5.27   1.11      -7.00      -3.50  1311 1.01
## b_PA_PS -2.98   1.31      -4.97      -0.80  1211 1.00
```

```r
compare(M10h3.1.stan,M10h3.2.stan)
```

```
##              WAIC pWAIC dWAIC weight    SE  dSE
## M10h3.2.stan 93.1   4.4   0.0   0.93 12.60   NA
## M10h3.1.stan 98.2   3.9   5.1   0.07 13.34 4.69
```

```r
coeftab(M10h3.1.stan,M10h3.2.stan)
```

```
##         M10h3.1.stan M10h3.2.stan
## a          0.67        -0.78     
## b_PS       4.69         6.56     
## b_PA       1.14         3.44     
## b_VS      -5.11        -5.27     
## b_PA_PS      NA        -2.98     
## nobs          8            8
```

```r
par(mfrow=c(1,1))
plot(coeftab(M10h3.1.stan,M10h3.2.stan))
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-18-3.png)<!-- -->

Redo plots

First get the probability predictions

```r
pred.df.int <- eagles[,-1]

eagle.link.int <- link(fit = M10h3.2.stan, data = pred.df.int)
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
dim(eagle.link.int)
```

```
## [1] 1000    8
```

```r
head(eagle.link.int) #these are probabilities
```

```
##           [,1]      [,2]      [,3]      [,4]       [,5]      [,6]
## [1,] 0.6412921 0.9986797 0.5863041 0.9983350 0.08587581 0.9754577
## [2,] 0.8849807 0.9986316 0.4648747 0.9880091 0.17940530 0.9539944
## [3,] 0.6687391 0.9928529 0.6579161 0.9925006 0.12171007 0.9050853
## [4,] 0.7404338 0.9971573 0.6294337 0.9952352 0.04273685 0.8459145
## [5,] 0.7739649 0.9994285 0.6083507 0.9987411 0.15781245 0.9896590
## [6,] 0.8193010 0.9984295 0.5714308 0.9946796 0.07067999 0.9142670
##             [,7]      [,8]
## [1,] 0.001425169 0.3764916
## [2,] 0.010722662 0.5069150
## [3,] 0.006371255 0.3061499
## [4,] 0.001157946 0.1247693
## [5,] 0.001523745 0.4380154
## [6,] 0.005800436 0.4499607
```

```r
pred.df.int$p.mean <- apply(eagle.link.int,2,mean)
pred.df.int$p.PI.low <- apply(eagle.link.int,2,PI)[1,]
pred.df.int$p.PI.high <- apply(eagle.link.int,2,PI)[2,]
```

Calculate count predictions


```r
pred.df.int$counts.mean <- pred.df.int$n * pred.df.int$p.mean
pred.df.int$counts.PI.low <- pred.df.int$n * pred.df.int$p.PI.low
pred.df.int$counts.PI.high <- pred.df.int$n * pred.df.int$p.PI.high
```

Plot the predictions


```r
#make some intellegent names for plotting
pred.df.int$pirate_size <- factor(pred.df.int$P,levels=c("S","L"),labels = c("small","large"))
pred.df.int$pirate_age <- factor(pred.df.int$A,levels=c("I","A"),labels = c("immature","adult"))
pred.df.int$victim_size <- factor(pred.df.int$V,levels=c("S","L"),labels = c("small_victim","large_victim"))

pl <- ggplot(pred.df.int,aes(x=pirate_size,fill=pirate_age))
pl <- pl + geom_bar(stat="identity",position="dodge")
pl <- pl + geom_errorbar(position=position_dodge(width=0.9),width=0.5)
pl <- pl + facet_grid(. ~ victim_size)
pl + aes(y=p.mean,ymax=p.PI.high,ymin=p.PI.low) + ggtitle("probability of pirating success")
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

```r
pl + aes(y=counts.mean,ymax=counts.PI.high,ymin=counts.PI.low) + ggtitle("count of successful pirating attempts")
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-21-2.png)<!-- -->


## 10H4

__(a)__ _Model the relationship between density and percent cover.  In what way does the model do a good and bad job?_


```r
library(rethinking)
library(ggplot2)
data(salamanders)
head(salamanders)
```

```
##   SITE SALAMAN PCTCOVER FORESTAGE
## 1    1      13       85       316
## 2    2      11       86        88
## 3    3      11       90       548
## 4    4       9       88        64
## 5    5       8       89        43
## 6    6       7       83       368
```

```r
qplot(x=PCTCOVER,y=SALAMAN,geom="point",data=salamanders)
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

```r
msal1 <- map(alist( SALAMAN ~ dpois(lambda),
                    log(lambda) <- a + b_c*PCTCOVER,
                    a ~ dnorm(0,2),
                    b_c ~ dnorm(0,1)),
             data=salamanders)
precis(msal1,corr = TRUE)
```

```
##      Mean StdDev  5.5% 94.5%     a   b_c
## a   -1.38   0.43 -2.07 -0.70  1.00 -0.98
## b_c  0.03   0.01  0.02  0.04 -0.98  1.00
```

```r
plot(precis(msal1))                    
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-22-2.png)<!-- -->


```r
pred.df <- data.frame(PCTCOVER=seq(0,100,1))
lambda.link <- link(msal1,pred.df)
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
pred.df$lambda.med <- apply(lambda.link,2,median)
pred.df <- cbind(pred.df,t(apply(lambda.link,2,PI)))
colnames(pred.df)[c(3,4)] <- c("PI.low","PI.high")
pl <- ggplot(pred.df,aes(x=PCTCOVER,y=lambda.med))
pl <- pl + geom_line()
pl <- pl + geom_ribbon(aes_string(ymin="PI.low",ymax="PI.high"),alpha=.1)
pl <- pl + geom_point(data=salamanders,aes(y=SALAMAN)) 
pl + ylab("Salamander density")
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-23-1.png)<!-- -->



```r
msal1.stan <- map2stan(msal1,chains=4,iter=4000)
```

```
## 
## SAMPLING FOR MODEL 'SALAMAN ~ dpois(lambda)' NOW (CHAIN 1).
## 
## Chain 1, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 1, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 1, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 1, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 1, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 1, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 1, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 1, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 1, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 1, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 1, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 1, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 0.286047 seconds (Warm-up)
##                0.112102 seconds (Sampling)
##                0.398149 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'SALAMAN ~ dpois(lambda)' NOW (CHAIN 2).
## 
## Chain 2, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 2, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 2, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 2, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 2, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 2, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 2, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 2, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 2, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 2, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 2, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 2, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 0.364529 seconds (Warm-up)
##                0.131949 seconds (Sampling)
##                0.496478 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'SALAMAN ~ dpois(lambda)' NOW (CHAIN 3).
## 
## Chain 3, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 3, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 3, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 3, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 3, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 3, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 3, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 3, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 3, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 3, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 3, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 3, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 0.181168 seconds (Warm-up)
##                0.119699 seconds (Sampling)
##                0.300867 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'SALAMAN ~ dpois(lambda)' NOW (CHAIN 4).
## 
## Chain 4, Iteration:    1 / 4000 [  0%]  (Warmup)
## Chain 4, Iteration:  400 / 4000 [ 10%]  (Warmup)
## Chain 4, Iteration:  800 / 4000 [ 20%]  (Warmup)
## Chain 4, Iteration: 1200 / 4000 [ 30%]  (Warmup)
## Chain 4, Iteration: 1600 / 4000 [ 40%]  (Warmup)
## Chain 4, Iteration: 2000 / 4000 [ 50%]  (Warmup)
## Chain 4, Iteration: 2001 / 4000 [ 50%]  (Sampling)
## Chain 4, Iteration: 2400 / 4000 [ 60%]  (Sampling)
## Chain 4, Iteration: 2800 / 4000 [ 70%]  (Sampling)
## Chain 4, Iteration: 3200 / 4000 [ 80%]  (Sampling)
## Chain 4, Iteration: 3600 / 4000 [ 90%]  (Sampling)
## Chain 4, Iteration: 4000 / 4000 [100%]  (Sampling)
##  Elapsed Time: 0.778915 seconds (Warm-up)
##                0.132007 seconds (Sampling)
##                0.910922 seconds (Total)
## 
## 
## SAMPLING FOR MODEL 'SALAMAN ~ dpois(lambda)' NOW (CHAIN 1).
## WARNING: No variance estimation is
##          performed for num_warmup < 20
## 
## 
## Chain 1, Iteration: 1 / 1 [100%]  (Sampling)
##  Elapsed Time: 4e-06 seconds (Warm-up)
##                3.2e-05 seconds (Sampling)
##                3.6e-05 seconds (Total)
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## [ 800 / 8000 ]
[ 1600 / 8000 ]
[ 2400 / 8000 ]
[ 3200 / 8000 ]
[ 4000 / 8000 ]
[ 4800 / 8000 ]
[ 5600 / 8000 ]
[ 6400 / 8000 ]
[ 7200 / 8000 ]
[ 8000 / 8000 ]
```

```r
precis(msal1.stan) #pretty poor sampling, really.  Due to correlation I assume.
```

```
##      Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## a   -1.46   0.44      -2.14      -0.78  1436    1
## b_c  0.03   0.01       0.02       0.04  1436    1
```

```r
pairs(msal1.stan)
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-24-1.png)<!-- -->


The model does a good job at low PCTCOVER and captures the average increase but does not account for the variability at high PCTCOVER.


__(b)__ _Can you improve the model by using the FORESTAGE predictor?  Try any models that may be useful.  Explain why FORESTAGE helps or does not help_

Additive model 


```r
qplot(x=FORESTAGE,y=SALAMAN,geom="point",data=salamanders)
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

```r
qplot(x=FORESTAGE,y=PCTCOVER,geom="point",data=salamanders)
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-25-2.png)<!-- -->

```r
msal.add <- map(alist( SALAMAN ~ dpois(lambda),
                    log(lambda) <- a + b_c*PCTCOVER + b_age*FORESTAGE,
                    a ~ dnorm(0,2),
                    c(b_c,b_age) ~ dnorm(0,1)),
                method="Nelder-Mead",
                start = list(a=0,b_c=0,b_age=0),
             data=salamanders)
precis(msal.add,corr = TRUE) #note : this can be really variable!
```

```
##        Mean StdDev  5.5% 94.5%     a   b_c b_age
## a     -1.41   0.43 -2.11 -0.72  1.00 -0.94  0.08
## b_c    0.03   0.01  0.02  0.04 -0.94  1.00 -0.34
## b_age  0.00   0.00  0.00  0.00  0.08 -0.34  1.00
```

```r
plot(precis(msal.add))  
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-25-3.png)<!-- -->

```r
(compare.out <- compare(msal1,msal.add)) #model with FORESTAGE is a bit worse
```

```
##           WAIC pWAIC dWAIC weight    SE  dSE
## msal1    213.2   4.6   0.0   0.87 26.22   NA
## msal.add 217.0   7.2   3.7   0.13 26.70 1.02
```

```r
plot(compare.out)
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-25-4.png)<!-- -->

Model that includes FORESTAGE is a bit worse than only with PCTCOVER and FORESTAGE is not predicting anything.  (note that the coefficient estimates can vary a lot with different runs; this also makes me think that we are over-paramterized here).  Bottom line, this additive model does not help.

What about FORESTAGE on its own?


```r
msal.age <- map(alist( SALAMAN ~ dpois(lambda),
                    log(lambda) <- a +  b_age*FORESTAGE,
                    a ~ dnorm(0,2),
                    c(b_age) ~ dnorm(0,1)),
                method="Nelder-Mead",
             data=salamanders)
precis(msal.age,corr = TRUE)
```

```
##       Mean StdDev 5.5% 94.5%     a b_age
## a      0.5   0.14 0.28  0.72  1.00 -0.74
## b_age  0.0   0.00 0.00  0.00 -0.74  1.00
```

```r
plot(precis(msal.age))  
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

```r
(compare.out <- compare(msal1,msal.add,msal.age)) 
```

```
##           WAIC pWAIC dWAIC weight    SE   dSE
## msal1    213.0   4.6   0.0   0.88 26.10    NA
## msal.add 217.0   7.2   3.9   0.12 26.64  1.03
## msal.age 264.8   6.9  51.7   0.00 35.25 22.22
```

```r
plot(compare.out)
```

![](Chapter_10_Assignment_files/figure-html/unnamed-chunk-26-2.png)<!-- -->

Model with FORESTAGE alone is much worse.

Model with interaction


```r
msal.int <- map(alist( SALAMAN ~ dpois(lambda),
                    log(lambda) <- a + b_c*PCTCOVER + b_age*FORESTAGE + b_c_age*PCTCOVER * FORESTAGE,
                    a ~ dnorm(0,2),
                    c(b_c,b_age,b_c_age) ~ dnorm(0,1)),
             data=salamanders,
             method="Nelder-Mead",
             start=list(a=0,b_c=0,b_age=0,b_c_age=0))
precis(msal1,corr = TRUE)
plot(precis(msal1)) 
```

I cannot get this to work.
