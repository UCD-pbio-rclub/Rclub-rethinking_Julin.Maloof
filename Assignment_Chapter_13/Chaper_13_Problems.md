# Chapter 13 Problems


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
## Warning: package 'ggplot2' was built under R version 3.2.5
```

```
## Loading required package: StanHeaders
```

```
## Warning: package 'StanHeaders' was built under R version 3.2.5
```

```
## rstan (Version 2.14.1, packaged: 2016-12-28 14:55:41 UTC, GitRev: 5fa1e80eb817)
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
library(brms)
```

```
## Loading required package: Rcpp
```

```
## Warning: package 'Rcpp' was built under R version 3.2.5
```

```
## Warning: replacing previous import by 'rstan::cpp_object_initializer' when
## loading 'brms'
```

```
## Loading 'brms' package (version 1.3.1.9000). Useful instructions 
## can be found by typing help('brms'). A more detailed introduction 
## to the package is available through vignette('brms_overview').
```

```
## 
## Attaching package: 'brms'
```

```
## The following objects are masked from 'package:rethinking':
## 
##     LOO, stancode, WAIC
```

```r
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
knitr::opts_chunk$set(cache=TRUE,autodep=TRUE)
```

## 13E1

_Add to the following model varying slopes on the predictor x_

Too hard to figure out the LaTex code...

Basically we need a multivariate distrubtion on A and X,
A covariance matrix,
An adaptive prior on B
An a prior for the correlation matrix

## 13E2

_Think up a context inwhich varyign intervepts will b positively correlated with varying slopes.  Provide a mechanistic explanation for the correlation_

Automobile gas consumption at idle and at 60mph.  Larger engines consume more at idle and are generally less efficient.

## 13M1


```r
a <- 3.5 # average morning wait time
b <- (-1) # average difference afternoon wait time
sigma_a <- 1 # std dev in intercepts
sigma_b <- 0.5 # std dev in slopes
rho <- 0 # correlation between intercepts and slopes

Mu <- c( a , b )

cov_ab <- sigma_a*sigma_b*rho
Sigma <- matrix( c(sigma_a^2,cov_ab,cov_ab,sigma_b^2) , ncol=2 )

sigmas <- c(sigma_a,sigma_b) # standard deviations

Rho <- matrix( c(1,rho,rho,1) , nrow=2 ) # correlation matrix
# now matrix multiply to get covariance matrix
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)
```


```r
N_cafes <- 20
library(MASS)
set.seed(5) # used to replicate example
vary_effects <- mvrnorm( N_cafes , Mu , Sigma )
```


```r
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]

plot( a_cafe , b_cafe , col=rangi2 ,
      xlab="intercepts (a_cafe)" , ylab="slopes (b_cafe)" )
# overlay population distribution
library(ellipse)
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma,centre=Mu,level=l),col=col.alpha("black",0.2))
```

![](Chaper_13_Problems_files/figure-html/13.8_9-1.png)<!-- -->

```r
#not quite the same: contours are based on actual data instead of Sigma matrix
pl <- qplot(x=a_cafe,y=b_cafe,color=I("skyblue"),geom="point")
pl + geom_density_2d(bins=5)
```

![](Chaper_13_Problems_files/figure-html/13.8_9-2.png)<!-- -->


```r
N_visits <- 10
afternoon <- rep(0:1,N_visits*N_cafes/2)
cafe_id <- rep( 1:N_cafes , each=N_visits )
mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
sigma <- 0.5 # std dev within cafes
wait <- rnorm( N_visits*N_cafes , mu , sigma )
d <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )
head(d)
```

```
##   cafe afternoon     wait
## 1    1         0 5.115886
## 2    1         1 2.489388
## 3    1         0 4.303566
## 4    1         1 3.838433
## 5    1         0 4.112571
## 6    1         1 3.171711
```

```r
summary(d)
```

```
##       cafe         afternoon        wait        
##  Min.   : 1.00   Min.   :0.0   Min.   :-0.4921  
##  1st Qu.: 5.75   1st Qu.:0.0   1st Qu.: 2.4084  
##  Median :10.50   Median :0.5   Median : 3.2697  
##  Mean   :10.50   Mean   :0.5   Mean   : 3.1889  
##  3rd Qu.:15.25   3rd Qu.:1.0   3rd Qu.: 3.9826  
##  Max.   :20.00   Max.   :1.0   Max.   : 6.3936
```



```r
m13.1.no.cor <- map2stan(
  alist(
    wait ~ dnorm( mu , sigma ),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    c(a_cafe,b_cafe)[cafe] ~ dmvnorm2(c(a,b),sigma_cafe,Rho),
    a ~ dnorm(0,10),
    b ~ dnorm(0,10),
    sigma_cafe ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ),
  data=d ,
  iter=5000 , warmup=2000 , chains=2 )
```

```
## Warning: There were 2 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See
## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## Warning in map2stan(alist(wait ~ dnorm(mu, sigma), mu <- a_cafe[cafe] + : There were 2 divergent iterations during sampling.
## Check the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.
```


```r
precis(m13.1.no.cor,depth = 2)
```

```
## Warning in precis(m13.1.no.cor, depth = 2): There were 2 divergent iterations during sampling.
## Check the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.
```

```
##                Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## b_cafe[1]     -1.19   0.24      -1.56      -0.79  6000    1
## b_cafe[2]     -1.83   0.25      -2.25      -1.44  6000    1
## b_cafe[3]     -1.09   0.24      -1.45      -0.69  6000    1
## b_cafe[4]     -1.49   0.24      -1.86      -1.09  6000    1
## b_cafe[5]     -2.05   0.26      -2.47      -1.64  6000    1
## b_cafe[6]     -1.07   0.24      -1.43      -0.68  6000    1
## b_cafe[7]     -1.28   0.24      -1.65      -0.90  6000    1
## b_cafe[8]     -1.27   0.24      -1.68      -0.90  6000    1
## b_cafe[9]     -0.86   0.25      -1.24      -0.43  6000    1
## b_cafe[10]    -1.10   0.24      -1.49      -0.73  6000    1
## b_cafe[11]    -1.36   0.26      -1.78      -0.95  6000    1
## b_cafe[12]    -1.05   0.24      -1.43      -0.66  6000    1
## b_cafe[13]    -1.36   0.25      -1.76      -0.96  6000    1
## b_cafe[14]    -1.67   0.25      -2.05      -1.25  6000    1
## b_cafe[15]    -1.67   0.29      -2.11      -1.20  3681    1
## b_cafe[16]    -1.22   0.24      -1.62      -0.85  6000    1
## b_cafe[17]    -0.71   0.25      -1.11      -0.31  6000    1
## b_cafe[18]    -0.22   0.27      -0.68       0.19  6000    1
## b_cafe[19]    -0.75   0.27      -1.17      -0.33  6000    1
## b_cafe[20]    -0.83   0.25      -1.23      -0.44  6000    1
## a_cafe[1]      4.22   0.18       3.92       4.50  6000    1
## a_cafe[2]      2.42   0.19       2.11       2.73  6000    1
## a_cafe[3]      4.15   0.18       3.85       4.44  6000    1
## a_cafe[4]      3.54   0.18       3.24       3.84  6000    1
## a_cafe[5]      2.20   0.19       1.90       2.51  6000    1
## a_cafe[6]      4.27   0.19       3.96       4.55  6000    1
## a_cafe[7]      3.72   0.18       3.44       4.02  6000    1
## a_cafe[8]      3.96   0.19       3.66       4.25  6000    1
## a_cafe[9]      3.81   0.19       3.50       4.11  6000    1
## a_cafe[10]     3.61   0.19       3.29       3.90  6000    1
## a_cafe[11]     2.41   0.20       2.09       2.71  6000    1
## a_cafe[12]     4.20   0.19       3.89       4.50  6000    1
## a_cafe[13]     4.16   0.19       3.86       4.47  6000    1
## a_cafe[14]     3.53   0.19       3.22       3.82  6000    1
## a_cafe[15]     4.52   0.21       4.20       4.86  6000    1
## a_cafe[16]     3.67   0.19       3.37       3.97  6000    1
## a_cafe[17]     4.32   0.19       4.02       4.61  6000    1
## a_cafe[18]     5.97   0.20       5.65       6.28  6000    1
## a_cafe[19]     3.28   0.20       2.96       3.58  6000    1
## a_cafe[20]     3.84   0.19       3.56       4.17  6000    1
## a              3.79   0.21       3.47       4.13  6000    1
## b             -1.20   0.14      -1.42      -0.99  6000    1
## sigma_cafe[1]  0.90   0.16       0.64       1.14  6000    1
## sigma_cafe[2]  0.52   0.13       0.33       0.72  3981    1
## sigma          0.48   0.03       0.44       0.52  6000    1
## Rho[1,1]       1.00   0.00       1.00       1.00  6000  NaN
## Rho[1,2]       0.44   0.23       0.09       0.80  2794    1
## Rho[2,1]       0.44   0.23       0.09       0.80  2794    1
## Rho[2,2]       1.00   0.00       1.00       1.00  5820    1
```

There is now a positive correlation, for reasons that I don't exactly understand.  I could go back and try to plot some of the posteriors out and see if it makes sense.

## 13M2

Fit a model without the multivariate gaussian prior.  Compare to model from chapter.

First, resimulate the data with rho -0.7 and run model from the chapter:


```r
a <- 3.5 # average morning wait time
b <- (-1) # average difference afternoon wait time
sigma_a <- 1 # std dev in intercepts
sigma_b <- 0.5 # std dev in slopes
rho <- (-0.7) # correlation between intercepts and slopes

Mu <- c( a , b )

cov_ab <- sigma_a*sigma_b*rho
Sigma <- matrix( c(sigma_a^2,cov_ab,cov_ab,sigma_b^2) , ncol=2 )

sigmas <- c(sigma_a,sigma_b) # standard deviations

Rho <- matrix( c(1,rho,rho,1) , nrow=2 ) # correlation matrix
# now matrix multiply to get covariance matrix
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)
N_cafes <- 20
library(MASS)
set.seed(5) # used to replicate example
vary_effects <- mvrnorm( N_cafes , Mu , Sigma )

a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]

N_visits <- 10
afternoon <- rep(0:1,N_visits*N_cafes/2)
cafe_id <- rep( 1:N_cafes , each=N_visits )
mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
sigma <- 0.5 # std dev within cafes
wait <- rnorm( N_visits*N_cafes , mu , sigma )
d <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )
head(d)
```

```
##   cafe afternoon     wait
## 1    1         0 4.998993
## 2    1         1 2.213394
## 3    1         0 4.186673
## 4    1         1 3.562440
## 5    1         0 3.995678
## 6    1         1 2.895718
```

```r
summary(d)
```

```
##       cafe         afternoon        wait       
##  Min.   : 1.00   Min.   :0.0   Min.   :0.2443  
##  1st Qu.: 5.75   1st Qu.:0.0   1st Qu.:2.2991  
##  Median :10.50   Median :0.5   Median :3.0790  
##  Mean   :10.50   Mean   :0.5   Mean   :3.1200  
##  3rd Qu.:15.25   3rd Qu.:1.0   3rd Qu.:3.8716  
##  Max.   :20.00   Max.   :1.0   Max.   :6.5223
```

```r
save.image("chap13.Rdata")
```



```r
load("chap13.Rdata")
m13.1 <- map2stan(
  alist(
    wait ~ dnorm( mu , sigma ),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    c(a_cafe,b_cafe)[cafe] ~ dmvnorm2(c(a,b),sigma_cafe,Rho),
    a ~ dnorm(0,10),
    b ~ dnorm(0,10),
    sigma_cafe ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ),
  data=d ,
  iter=5000 , warmup=2000 , chains=2 )
```

```
## Warning: There were 46 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See
## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## Warning in map2stan(alist(wait ~ dnorm(mu, sigma), mu <- a_cafe[cafe] + : There were 46 divergent iterations during sampling.
## Check the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.
```

```r
save.image("chap13.Rdata")
```


```r
m13.1.nomv <- map2stan(
  alist(
    wait ~ dnorm( mu , sigma ),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    a_cafe[cafe] ~ dnorm(a,sigma_a),
    b_cafe[cafe] ~ dnorm(b,sigma_b),
    a ~ dnorm(0,10),
    b ~ dnorm(0,10),
    sigma_a ~ dcauchy(0,1),
    sigma_b ~ dcauchy(0,1),
    sigma ~ dcauchy(0,1)
  ),
  data=d ,
  iter=5000 , warmup=2000 , chains=2 )
```

```
## Warning: There were 16 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See
## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## Warning in map2stan(alist(wait ~ dnorm(mu, sigma), mu <- a_cafe[cafe] + : There were 16 divergent iterations during sampling.
## Check the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.
```


```r
precis(m13.1,depth = 2)
```

```
## Warning in precis(m13.1, depth = 2): There were 46 divergent iterations during sampling.
## Check the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.
```

```
##                Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## b_cafe[1]     -1.30   0.19      -1.60      -1.02  3585    1
## b_cafe[2]     -1.19   0.20      -1.52      -0.89  4275    1
## b_cafe[3]     -1.26   0.19      -1.54      -0.94  4389    1
## b_cafe[4]     -1.29   0.19      -1.61      -1.01  3398    1
## b_cafe[5]     -1.25   0.20      -1.59      -0.95  3698    1
## b_cafe[6]     -1.29   0.18      -1.57      -0.99  3499    1
## b_cafe[7]     -1.23   0.18      -1.50      -0.93  6000    1
## b_cafe[8]     -1.26   0.18      -1.54      -0.96  6000    1
## b_cafe[9]     -1.12   0.19      -1.41      -0.81  3470    1
## b_cafe[10]    -1.19   0.18      -1.46      -0.89  6000    1
## b_cafe[11]    -1.02   0.22      -1.37      -0.68  1638    1
## b_cafe[12]    -1.21   0.18      -1.50      -0.93  4219    1
## b_cafe[13]    -1.33   0.20      -1.64      -1.03  2268    1
## b_cafe[14]    -1.37   0.20      -1.69      -1.07  1436    1
## b_cafe[15]    -1.55   0.26      -1.94      -1.15   751    1
## b_cafe[16]    -1.18   0.18      -1.46      -0.89  6000    1
## b_cafe[17]    -1.15   0.19      -1.47      -0.85  3576    1
## b_cafe[18]    -1.29   0.22      -1.64      -0.93  3596    1
## b_cafe[19]    -1.03   0.21      -1.37      -0.71  1206    1
## b_cafe[20]    -1.08   0.20      -1.37      -0.75  2306    1
## a_cafe[1]      4.09   0.18       3.80       4.36  6000    1
## a_cafe[2]      2.37   0.18       2.09       2.66  6000    1
## a_cafe[3]      3.94   0.18       3.66       4.23  6000    1
## a_cafe[4]      3.45   0.18       3.16       3.73  6000    1
## a_cafe[5]      2.14   0.19       1.86       2.45  6000    1
## a_cafe[6]      4.27   0.18       3.99       4.57  6000    1
## a_cafe[7]      3.56   0.18       3.28       3.85  6000    1
## a_cafe[8]      3.79   0.18       3.51       4.08  6000    1
## a_cafe[9]      3.89   0.18       3.59       4.17  6000    1
## a_cafe[10]     3.69   0.18       3.41       3.97  6000    1
## a_cafe[11]     2.46   0.19       2.17       2.76  2675    1
## a_cafe[12]     4.08   0.18       3.81       4.37  6000    1
## a_cafe[13]     3.88   0.19       3.57       4.16  6000    1
## a_cafe[14]     3.33   0.19       3.05       3.65  6000    1
## a_cafe[15]     4.24   0.20       3.92       4.56  1806    1
## a_cafe[16]     3.60   0.18       3.31       3.87  6000    1
## a_cafe[17]     4.43   0.18       4.13       4.71  6000    1
## a_cafe[18]     6.10   0.19       5.78       6.39  4193    1
## a_cafe[19]     3.49   0.19       3.19       3.79  2319    1
## a_cafe[20]     3.90   0.18       3.60       4.19  3384    1
## a              3.73   0.21       3.39       4.06  6000    1
## b             -1.23   0.09      -1.37      -1.09  2605    1
## sigma_cafe[1]  0.92   0.17       0.65       1.16  6000    1
## sigma_cafe[2]  0.23   0.11       0.04       0.36   499    1
## sigma          0.49   0.03       0.44       0.53  6000    1
## Rho[1,1]       1.00   0.00       1.00       1.00  6000  NaN
## Rho[1,2]      -0.19   0.31      -0.68       0.30  1932    1
## Rho[2,1]      -0.19   0.31      -0.68       0.30  1932    1
## Rho[2,2]       1.00   0.00       1.00       1.00  5614    1
```

```r
precis(m13.1.nomv,depth = 2)
```

```
## Warning in precis(m13.1.nomv, depth = 2): There were 16 divergent iterations during sampling.
## Check the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.
```

```
##             Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## a_cafe[1]   4.08   0.18       3.81       4.37  6000    1
## a_cafe[2]   2.40   0.18       2.12       2.68  6000    1
## a_cafe[3]   3.95   0.18       3.65       4.23  6000    1
## a_cafe[4]   3.46   0.18       3.17       3.73  6000    1
## a_cafe[5]   2.17   0.18       1.89       2.45  6000    1
## a_cafe[6]   4.26   0.18       3.97       4.54  6000    1
## a_cafe[7]   3.56   0.17       3.29       3.84  6000    1
## a_cafe[8]   3.80   0.18       3.52       4.08  6000    1
## a_cafe[9]   3.89   0.18       3.60       4.17  3767    1
## a_cafe[10]  3.70   0.18       3.42       3.97  4749    1
## a_cafe[11]  2.49   0.18       2.19       2.77  3020    1
## a_cafe[12]  4.08   0.18       3.80       4.36  6000    1
## a_cafe[13]  3.88   0.18       3.60       4.16  6000    1
## a_cafe[14]  3.35   0.18       3.06       3.62  6000    1
## a_cafe[15]  4.24   0.20       3.92       4.54  1752    1
## a_cafe[16]  3.59   0.18       3.30       3.86  4200    1
## a_cafe[17]  4.42   0.18       4.14       4.70  3873    1
## a_cafe[18]  6.07   0.18       5.79       6.35  6000    1
## a_cafe[19]  3.50   0.18       3.22       3.81  6000    1
## a_cafe[20]  3.89   0.18       3.59       4.16  2983    1
## b_cafe[1]  -1.29   0.19      -1.59      -1.00  3936    1
## b_cafe[2]  -1.25   0.18      -1.55      -0.97  4857    1
## b_cafe[3]  -1.26   0.18      -1.58      -0.99  4716    1
## b_cafe[4]  -1.31   0.18      -1.59      -1.00  4410    1
## b_cafe[5]  -1.32   0.18      -1.61      -1.03  3440    1
## b_cafe[6]  -1.27   0.18      -1.57      -0.98  4023    1
## b_cafe[7]  -1.24   0.18      -1.53      -0.95  4275    1
## b_cafe[8]  -1.26   0.18      -1.57      -0.98  3496    1
## b_cafe[9]  -1.12   0.19      -1.43      -0.82  2298    1
## b_cafe[10] -1.19   0.18      -1.47      -0.89  4285    1
## b_cafe[11] -1.06   0.21      -1.38      -0.74  1499    1
## b_cafe[12] -1.20   0.18      -1.49      -0.90  3805    1
## b_cafe[13] -1.33   0.19      -1.62      -1.03  3235    1
## b_cafe[14] -1.40   0.20      -1.73      -1.08  1776    1
## b_cafe[15] -1.56   0.25      -1.95      -1.16   973    1
## b_cafe[16] -1.18   0.19      -1.48      -0.89  3284    1
## b_cafe[17] -1.13   0.19      -1.42      -0.82  2619    1
## b_cafe[18] -1.22   0.18      -1.52      -0.94  4667    1
## b_cafe[19] -1.03   0.22      -1.37      -0.71  1327    1
## b_cafe[20] -1.07   0.20      -1.40      -0.77  1509    1
## a           3.74   0.20       3.43       4.06  6000    1
## b          -1.23   0.09      -1.37      -1.09  2096    1
## sigma_a     0.88   0.16       0.64       1.11  6000    1
## sigma_b     0.22   0.10       0.05       0.36   552    1
## sigma       0.49   0.03       0.44       0.53  6000    1
```

```r
compare(m13.1,m13.1.nomv)
```

```
##             WAIC pWAIC dWAIC weight    SE  dSE
## m13.1.nomv 308.2  26.5   0.0   0.66 20.07   NA
## m13.1      309.5  27.0   1.3   0.34 20.09 0.85
```

So no real difference...note that the 95%PI for Rho crosses over 0.

## 13M3

Re-estimate varying slopes model for UCBadmit, using non-centered paramterization.

Wait: it *is* non-centered already.  Does he mean using a centered one?


```r
data(UCBadmit)
d <- UCBadmit
d$male <- ifelse( d$applicant.gender=="male" , 1 , 0 )
d$dept_id <- coerce_index( d$dept )
m13.3 <- map2stan(
    alist(
        admit ~ dbinom( applications , p ),
        logit(p) <- a_dept[dept_id] +
                    bm_dept[dept_id]*male,
        c(a_dept,bm_dept)[dept_id] ~ dmvnorm2( c(a,bm) , sigma_dept , Rho ),
        a ~ dnorm(0,10),
        bm ~ dnorm(0,1),
        sigma_dept ~ dcauchy(0,2),
        Rho ~ dlkjcorr(2)
),
data=d , warmup=1000 , iter=5000 , chains=4 , cores=3 )
```

```
## Warning: Variable 'applicant.gender' contains dots '.'.
## Will attempt to remove dots internally.
```

```
## Warning: There were 39 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See
## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## Aggregated binomial counts detected. Splitting to 0/1 outcome for WAIC calculation.
```

```
## Warning in map2stan(alist(admit ~ dbinom(applications, p), logit(p) <- a_dept[dept_id] + : There were 39 divergent iterations during sampling.
## Check the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.
```


```r
m13.3.alt <- map2stan(
    alist(
        admit ~ dbinom( applications , p ),
        logit(p) <- a + bm_male*male + a_dept[dept_id] +
                    bm_dept[dept_id]*male,
        c(a_dept,bm_dept)[dept_id] ~ dmvnorm2( c(0,0) , sigma_dept , Rho ),
        a ~ dnorm(0,10),
        bm_male ~ dnorm(0,1),
        sigma_dept ~ dcauchy(0,2),
        Rho ~ dlkjcorr(2)
),
data=d , warmup=1000 , iter=5000 , chains=4 , cores=3 )
```

```
## Warning: Variable 'applicant.gender' contains dots '.'.
## Will attempt to remove dots internally.
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## Aggregated binomial counts detected. Splitting to 0/1 outcome for WAIC calculation.
```


```r
precis(m13.3,depth = 2)
```

```
## Warning in precis(m13.3, depth = 2): There were 39 divergent iterations during sampling.
## Check the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid.
```

```
##                Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## bm_dept[1]    -0.80   0.27      -1.22      -0.36  5815    1
## bm_dept[2]    -0.22   0.33      -0.75       0.29  7993    1
## bm_dept[3]     0.08   0.14      -0.14       0.31 11692    1
## bm_dept[4]    -0.09   0.14      -0.32       0.13 11677    1
## bm_dept[5]     0.12   0.19      -0.18       0.42 13401    1
## bm_dept[6]    -0.12   0.27      -0.56       0.29 10562    1
## a_dept[1]      1.31   0.26       0.89       1.71  5904    1
## a_dept[2]      0.75   0.32       0.21       1.24  8137    1
## a_dept[3]     -0.65   0.09      -0.78      -0.51 12391    1
## a_dept[4]     -0.62   0.11      -0.78      -0.45 12183    1
## a_dept[5]     -1.13   0.12      -1.32      -0.96 16000    1
## a_dept[6]     -2.60   0.20      -2.92      -2.28 11495    1
## a             -0.50   0.73      -1.61       0.58  8185    1
## bm            -0.16   0.23      -0.54       0.17 10129    1
## sigma_dept[1]  1.68   0.65       0.85       2.47  4164    1
## sigma_dept[2]  0.50   0.25       0.16       0.83  6922    1
## Rho[1,1]       1.00   0.00       1.00       1.00 16000  NaN
## Rho[1,2]      -0.32   0.35      -0.89       0.19  7391    1
## Rho[2,1]      -0.32   0.35      -0.89       0.19  7391    1
## Rho[2,2]       1.00   0.00       1.00       1.00 16000    1
```

```r
precis(m13.3.alt, depth = 2)
```

```
##                Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## bm_dept[1]    -0.63   0.32      -1.10      -0.10  5245    1
## bm_dept[2]    -0.04   0.35      -0.61       0.51  6473    1
## bm_dept[3]     0.25   0.26      -0.14       0.66  4376    1
## bm_dept[4]     0.07   0.26      -0.33       0.48  4298    1
## bm_dept[5]     0.29   0.28      -0.14       0.72  4858    1
## bm_dept[6]     0.04   0.32      -0.47       0.54  5796    1
## a_dept[1]      1.79   0.75       0.63       2.95  2693    1
## a_dept[2]      1.22   0.77       0.05       2.41  2792    1
## a_dept[3]     -0.17   0.73      -1.21       1.03  2604    1
## a_dept[4]     -0.14   0.73      -1.22       1.01  2603    1
## a_dept[5]     -0.65   0.73      -1.74       0.51  2624    1
## a_dept[6]     -2.12   0.74      -3.22      -0.94  2699    1
## a             -0.48   0.73      -1.59       0.62  2569    1
## bm_male       -0.17   0.24      -0.53       0.20  3767    1
## sigma_dept[1]  1.68   0.64       0.81       2.47  5876    1
## sigma_dept[2]  0.50   0.26       0.15       0.83  4824    1
## Rho[1,1]       1.00   0.00       1.00       1.00 16000  NaN
## Rho[1,2]      -0.31   0.35      -0.87       0.21  8177    1
## Rho[2,1]      -0.31   0.35      -0.87       0.21  8177    1
## Rho[2,2]       1.00   0.00       1.00       1.00 15961    1
```

```r
compare(m13.3, m13.3.alt)
```

```
##             WAIC pWAIC dWAIC weight    SE  dSE
## m13.3.alt 5190.9  11.1   0.0   0.51 57.25   NA
## m13.3     5191.0  11.1   0.1   0.49 57.28 0.09
```

# 13H1

_Revisit the `bangladesh` data from chatper 12.  Fit a model with both varying intercepts by `district_id` and vary slopes of `urban` by `district_id`.  Inspect the correlation between the slopes and intercepts.  Can you interpret this correlation, in terms of what it tells you about the pattern of contraceptive use in the sampe?  It might help to plot the mean (or median) varying effects estiamte for both the intercepts and slopes, by district.  Plotting predicted proportion of women using contraception, with urban women on one axis and rural on the other, might also help._


```r
data("bangladesh")
colnames(bangladesh) <- sub(".","_",colnames(bangladesh),fixed=TRUE)
bangladesh$district_id <- coerce_index(bangladesh$district)
#bangladesh$district_id <- as.factor(as.numeric(bangladesh$district))
summary(bangladesh)
```

```
##      woman           district     use_contraception living_children
##  Min.   :   1.0   Min.   : 1.00   Min.   :0.0000    Min.   :1.000  
##  1st Qu.: 484.2   1st Qu.:14.00   1st Qu.:0.0000    1st Qu.:1.000  
##  Median : 967.5   Median :29.00   Median :0.0000    Median :3.000  
##  Mean   : 967.5   Mean   :29.35   Mean   :0.3925    Mean   :2.652  
##  3rd Qu.:1450.8   3rd Qu.:45.00   3rd Qu.:1.0000    3rd Qu.:4.000  
##  Max.   :1934.0   Max.   :61.00   Max.   :1.0000    Max.   :4.000  
##   age_centered            urban         district_id   
##  Min.   :-13.560000   Min.   :0.0000   Min.   : 1.00  
##  1st Qu.: -7.559900   1st Qu.:0.0000   1st Qu.:13.00  
##  Median : -1.559900   Median :0.0000   Median :29.00  
##  Mean   :  0.002198   Mean   :0.2906   Mean   :29.49  
##  3rd Qu.:  6.440000   3rd Qu.:1.0000   3rd Qu.:45.00  
##  Max.   : 19.440000   Max.   :1.0000   Max.   :60.00
```

```r
head(bangladesh)
```

```
##   woman district use_contraception living_children age_centered urban
## 1     1        1                 0               4      18.4400     1
## 2     2        1                 0               1      -5.5599     1
## 3     3        1                 0               3       1.4400     1
## 4     4        1                 0               4       8.4400     1
## 5     5        1                 0               1     -13.5590     1
## 6     6        1                 0               1     -11.5600     1
##   district_id
## 1           1
## 2           1
## 3           1
## 4           1
## 5           1
## 6           1
```

varying intercepts and slope model

```r
m13h1.1 <- map2stan(alist(
  use_contraception ~ dbinom(1,p),
  logit(p) <- a_district[district_id] + b_urban_dept[district_id]*urban,
  c(a_district,b_urban_dept)[district_id] ~ dmvnorm2(c(a,b), sigma_district, Rho),
  a ~ dnorm(0,5),
  b ~ dnorm(0,5),
  sigma_district ~ dcauchy(0,1),
  Rho ~ dlkjcorr(2)),
  data=bangladesh,
  chains = 4)
```

```
## The following numerical problems occured the indicated number of times on chain 1
```

```
##                                                                        count
## Exception thrown at line 32: lkj_corr_log: y is not positive definite.     1
```

```
## When a numerical problem occurs, the Hamiltonian proposal gets rejected.
```

```
## See http://mc-stan.org/misc/warnings.html#exception-hamiltonian-proposal-rejected
```

```
## If the number in the 'count' column is small, do not ask about this message on stan-users.
```

```
## The following numerical problems occured the indicated number of times on chain 2
```

```
##                                                                        count
## Exception thrown at line 32: lkj_corr_log: y is not positive definite.     3
```

```
## When a numerical problem occurs, the Hamiltonian proposal gets rejected.
```

```
## See http://mc-stan.org/misc/warnings.html#exception-hamiltonian-proposal-rejected
```

```
## If the number in the 'count' column is small, do not ask about this message on stan-users.
```

```
## The following numerical problems occured the indicated number of times on chain 4
```

```
##                                                                        count
## Exception thrown at line 32: lkj_corr_log: y is not positive definite.     2
```

```
## When a numerical problem occurs, the Hamiltonian proposal gets rejected.
```

```
## See http://mc-stan.org/misc/warnings.html#exception-hamiltonian-proposal-rejected
```

```
## If the number in the 'count' column is small, do not ask about this message on stan-users.
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```


```r
plot(m13h1.1, ask=FALSE)
```

```
## Waiting to draw page 2 of 9
```

![](Chaper_13_Problems_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```
## Waiting to draw page 3 of 9
```

![](Chaper_13_Problems_files/figure-html/unnamed-chunk-13-2.png)<!-- -->

```
## Waiting to draw page 4 of 9
```

![](Chaper_13_Problems_files/figure-html/unnamed-chunk-13-3.png)<!-- -->

```
## Waiting to draw page 5 of 9
```

![](Chaper_13_Problems_files/figure-html/unnamed-chunk-13-4.png)<!-- -->

```
## Waiting to draw page 6 of 9
```

![](Chaper_13_Problems_files/figure-html/unnamed-chunk-13-5.png)<!-- -->

```
## Waiting to draw page 7 of 9
```

![](Chaper_13_Problems_files/figure-html/unnamed-chunk-13-6.png)<!-- -->

```
## Waiting to draw page 8 of 9
```

![](Chaper_13_Problems_files/figure-html/unnamed-chunk-13-7.png)<!-- -->

```
## Waiting to draw page 9 of 9
```

![](Chaper_13_Problems_files/figure-html/unnamed-chunk-13-8.png)<!-- -->

```r
precis(m13h1.1,depth=2)
```

```
##                    Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## b_urban_dept[1]    1.08   0.38       0.44       1.66  4000    1
## b_urban_dept[2]    1.28   0.79      -0.01       2.45  4000    1
## b_urban_dept[3]    1.70   0.85       0.33       3.01  1980    1
## b_urban_dept[4]    0.45   0.61      -0.53       1.41  4000    1
## b_urban_dept[5]    0.28   0.59      -0.62       1.22  4000    1
## b_urban_dept[6]    1.27   0.44       0.57       1.97  4000    1
## b_urban_dept[7]    0.48   0.60      -0.47       1.42  4000    1
## b_urban_dept[8]    0.61   0.65      -0.47       1.59  4000    1
## b_urban_dept[9]    0.82   0.70      -0.22       1.99  4000    1
## b_urban_dept[10]   0.91   0.51       0.12       1.74  4000    1
## b_urban_dept[11]   1.01   0.62       0.05       1.99  4000    1
## b_urban_dept[12]   0.68   0.70      -0.40       1.79  4000    1
## b_urban_dept[13]   0.57   0.74      -0.57       1.76  4000    1
## b_urban_dept[14]  -0.34   0.70      -1.39       0.77  1970    1
## b_urban_dept[15]   1.06   0.75      -0.01       2.34  4000    1
## b_urban_dept[16]   0.87   0.71      -0.26       1.96  4000    1
## b_urban_dept[17]   1.31   0.80       0.03       2.50  4000    1
## b_urban_dept[18]   0.20   0.46      -0.59       0.87  4000    1
## b_urban_dept[19]   0.61   0.71      -0.53       1.76  4000    1
## b_urban_dept[20]   1.21   0.62       0.24       2.20  4000    1
## b_urban_dept[21]   0.76   0.59      -0.26       1.61  4000    1
## b_urban_dept[22]   1.20   0.60       0.23       2.14  4000    1
## b_urban_dept[23]   1.02   0.82      -0.19       2.39  4000    1
## b_urban_dept[24]   1.06   0.49       0.22       1.78  4000    1
## b_urban_dept[25]   0.42   0.57      -0.49       1.31  4000    1
## b_urban_dept[26]   1.08   0.73      -0.08       2.21  4000    1
## b_urban_dept[27]   1.31   0.71       0.22       2.44  4000    1
## b_urban_dept[28]  -0.71   0.65      -1.72       0.32  1972    1
## b_urban_dept[29]   0.46   0.46      -0.27       1.22  4000    1
## b_urban_dept[30]   0.53   0.66      -0.48       1.58  4000    1
## b_urban_dept[31]   0.31   0.76      -0.89       1.50  4000    1
## b_urban_dept[32]   1.33   0.71       0.20       2.42  4000    1
## b_urban_dept[33]   0.28   0.67      -0.82       1.31  4000    1
## b_urban_dept[34]   1.67   0.65       0.58       2.63  1683    1
## b_urban_dept[35]   0.49   0.50      -0.27       1.32  4000    1
## b_urban_dept[36]  -0.27   0.69      -1.32       0.82  4000    1
## b_urban_dept[37]  -0.28   0.76      -1.48       0.89  4000    1
## b_urban_dept[38]   0.45   0.49      -0.42       1.14  4000    1
## b_urban_dept[39]   1.03   0.70      -0.12       2.09  4000    1
## b_urban_dept[40]   1.26   0.61       0.32       2.22  4000    1
## b_urban_dept[41]   0.48   0.48      -0.30       1.22  4000    1
## b_urban_dept[42]   0.55   0.65      -0.53       1.53  4000    1
## b_urban_dept[43]   0.39   0.49      -0.44       1.16  4000    1
## b_urban_dept[44]   1.06   0.82      -0.17       2.36  4000    1
## b_urban_dept[45]   0.65   0.65      -0.34       1.73  4000    1
## b_urban_dept[46]   1.23   0.69       0.17       2.32  4000    1
## b_urban_dept[47]   0.87   0.51       0.08       1.68  4000    1
## b_urban_dept[48]  -0.44   0.50      -1.21       0.34  4000    1
## b_urban_dept[49]   0.52   0.67      -0.49       1.63  4000    1
## b_urban_dept[50]   0.27   0.79      -0.98       1.51  4000    1
## b_urban_dept[51]   0.45   0.49      -0.31       1.24  4000    1
## b_urban_dept[52]   0.91   0.66      -0.12       1.95  4000    1
## b_urban_dept[53]  -0.20   0.57      -1.05       0.77  4000    1
## b_urban_dept[54]   1.16   0.78      -0.07       2.35  4000    1
## b_urban_dept[55]   1.35   0.58       0.37       2.19  4000    1
## b_urban_dept[56]   0.77   0.58      -0.12       1.68  4000    1
## b_urban_dept[57]   0.72   0.55      -0.09       1.63  4000    1
## b_urban_dept[58]   0.86   0.70      -0.28       1.94  4000    1
## b_urban_dept[59]   0.99   0.64      -0.07       1.97  4000    1
## b_urban_dept[60]   1.07   0.66       0.04       2.15  4000    1
## a_district[1]     -1.55   0.31      -2.04      -1.05  4000    1
## a_district[2]     -1.34   0.48      -2.07      -0.57  4000    1
## a_district[3]     -1.79   0.46      -2.54      -1.10  4000    1
## a_district[4]     -0.68   0.35      -1.25      -0.12  4000    1
## a_district[5]     -0.47   0.38      -1.11       0.11  4000    1
## a_district[6]     -0.64   0.40      -1.25      -0.01  4000    1
## a_district[7]     -0.68   0.40      -1.34      -0.06  4000    1
## a_district[8]     -0.28   0.37      -0.84       0.32  4000    1
## a_district[9]     -0.84   0.36      -1.43      -0.29  4000    1
## a_district[10]    -0.91   0.33      -1.44      -0.39  4000    1
## a_district[11]    -0.76   0.35      -1.31      -0.20  4000    1
## a_district[12]    -0.67   0.37      -1.22      -0.04  4000    1
## a_district[13]    -0.56   0.40      -1.17       0.09  4000    1
## a_district[14]    -0.25   0.45      -1.02       0.42  4000    1
## a_district[15]    -1.10   0.40      -1.70      -0.42  4000    1
## a_district[16]    -0.88   0.41      -1.48      -0.18  4000    1
## a_district[17]    -1.37   0.46      -2.05      -0.62  4000    1
## a_district[18]    -0.32   0.26      -0.74       0.09  4000    1
## a_district[19]    -0.61   0.41      -1.26       0.05  4000    1
## a_district[20]    -1.38   0.34      -1.93      -0.84  4000    1
## a_district[21]    -1.04   0.28      -1.49      -0.60  4000    1
## a_district[22]    -1.12   0.37      -1.70      -0.56  4000    1
## a_district[23]    -0.69   0.61      -1.69       0.24  4000    1
## a_district[24]    -0.46   0.28      -0.89      -0.02  4000    1
## a_district[25]    -0.39   0.33      -0.90       0.15  4000    1
## a_district[26]    -1.10   0.38      -1.71      -0.51  4000    1
## a_district[27]    -0.99   0.50      -1.72      -0.14  4000    1
## a_district[28]     0.50   0.37      -0.07       1.11  4000    1
## a_district[29]    -0.32   0.32      -0.86       0.14  4000    1
## a_district[30]    -0.67   0.41      -1.26       0.05  4000    1
## a_district[31]    -0.26   0.42      -0.95       0.36  4000    1
## a_district[32]    -1.24   0.52      -2.02      -0.39  4000    1
## a_district[33]    -0.25   0.34      -0.77       0.32  4000    1
## a_district[34]    -0.87   0.39      -1.49      -0.25  4000    1
## a_district[35]    -0.54   0.41      -1.18       0.10  4000    1
## a_district[36]    -0.10   0.35      -0.67       0.46  4000    1
## a_district[37]    -0.05   0.51      -0.85       0.77  4000    1
## a_district[38]    -0.23   0.32      -0.72       0.30  4000    1
## a_district[39]    -1.07   0.36      -1.65      -0.52  4000    1
## a_district[40]    -0.92   0.32      -1.43      -0.41  4000    1
## a_district[41]    -0.10   0.22      -0.44       0.24  4000    1
## a_district[42]    -0.52   0.44      -1.21       0.19  4000    1
## a_district[43]    -0.23   0.34      -0.74       0.32  4000    1
## a_district[44]    -1.08   0.55      -1.94      -0.21  4000    1
## a_district[45]    -0.65   0.31      -1.15      -0.16  4000    1
## a_district[46]    -0.69   0.41      -1.33      -0.07  4000    1
## a_district[47]    -0.66   0.37      -1.23      -0.06  4000    1
## a_district[48]    -0.11   0.28      -0.57       0.32  4000    1
## a_district[49]    -0.72   0.59      -1.64       0.21  4000    1
## a_district[50]    -0.76   0.62      -1.76       0.22  4000    1
## a_district[51]    -0.13   0.34      -0.67       0.43  4000    1
## a_district[52]    -1.22   0.39      -1.80      -0.57  4000    1
## a_district[53]    -0.17   0.37      -0.77       0.40  4000    1
## a_district[54]    -1.21   0.48      -1.90      -0.39  4000    1
## a_district[55]    -1.05   0.27      -1.47      -0.61  4000    1
## a_district[56]    -1.17   0.39      -1.81      -0.55  4000    1
## a_district[57]    -1.19   0.35      -1.74      -0.64  4000    1
## a_district[58]    -0.86   0.39      -1.48      -0.26  4000    1
## a_district[59]    -0.67   0.30      -1.13      -0.19  4000    1
## a_district[60]    -0.94   0.37      -1.53      -0.35  4000    1
## a                 -0.72   0.10      -0.88      -0.56  3210    1
## b                  0.72   0.18       0.44       0.99  2292    1
## sigma_district[1]  0.58   0.10       0.42       0.73  1518    1
## sigma_district[2]  0.80   0.20       0.47       1.10   604    1
## Rho[1,1]           1.00   0.00       1.00       1.00  4000  NaN
## Rho[1,2]          -0.66   0.16      -0.90      -0.42   848    1
## Rho[2,1]          -0.66   0.16      -0.90      -0.42   848    1
## Rho[2,2]           1.00   0.00       1.00       1.00  3725    1
```

![](Chaper_13_Problems_files/figure-html/unnamed-chunk-13-9.png)<!-- -->

The correlation between intercept and slope is negative, and on average the intercept (non-urban) is low and the slope is positive.  So urban areas tend to have more contraceptive use.  However if the district already has high use everywhere it can't get that much higher, so lower slope.


```r
newdata <- data.frame(
  district_id = rep(1:60,2),
  urban = rep(c(0,1),each=60)
)
post <- link(m13h1.1,newdata)
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
dim(post)
```

```
## [1] 1000  120
```

```r
mu <- apply(post,2,mean)
PI <- apply(post,2,PI)
newdata$mean <- mu
newdata$low <- PI[1,]
newdata$high <- PI[2,]
```


```r
pl <- ggplot(newdata,aes(x=as.factor(district_id),y=mean,fill=as.factor(urban)))
pl <- pl + geom_bar(position="dodge",stat="identity")
pl 
```

![](Chaper_13_Problems_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
with(newdata, qplot(x=mean[urban==0],y=mean[urban==1]))
```

![](Chaper_13_Problems_files/figure-html/unnamed-chunk-15-2.png)<!-- -->

```r
with(newdata, qplot(x=mean[urban==0],y=mean[urban==1]-mean[urban==0]) + 
       xlab("percent use rural") + 
       ylab ("percent increase in urban") + 
       ggtitle("contraceptive use"))
```

![](Chaper_13_Problems_files/figure-html/unnamed-chunk-15-3.png)<!-- -->

