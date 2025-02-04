# Chapter-05-part2-assignment
# Statistical Rethinking Chapter 4 problems

__Name: Julin Maloof__


# For <del>04/18/2016</del> 4/22/16

## 5M2

My example of a masked relationship is predicting energy usage (kW pulled from PG&E) for the Brady/Koch househould.  Because they have solar, this should be predictable by number pf sunny hours in the day.  However, long, sunny days also correlate with heat, and they have an air conditioner, which will affect energy use in the opposite way.

## 5H1

Are `area` or `groupsize` important predictors of body weight?

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
data(foxes)
data <- foxes
summary(data)
```

```
##      group          avgfood         groupsize          area      
##  Min.   : 1.00   Min.   :0.3700   Min.   :2.000   Min.   :1.090  
##  1st Qu.:11.75   1st Qu.:0.6600   1st Qu.:3.000   1st Qu.:2.590  
##  Median :18.00   Median :0.7350   Median :4.000   Median :3.130  
##  Mean   :17.21   Mean   :0.7517   Mean   :4.345   Mean   :3.169  
##  3rd Qu.:24.00   3rd Qu.:0.8000   3rd Qu.:5.000   3rd Qu.:3.772  
##  Max.   :30.00   Max.   :1.2100   Max.   :8.000   Max.   :5.070  
##      weight     
##  Min.   :1.920  
##  1st Qu.:3.720  
##  Median :4.420  
##  Mean   :4.530  
##  3rd Qu.:5.375  
##  Max.   :7.550
```

```r
head(data)
```

```
##   group avgfood groupsize area weight
## 1     1    0.37         2 1.09   5.02
## 2     1    0.37         2 1.09   2.84
## 3     2    0.53         2 2.05   5.33
## 4     2    0.53         2 2.05   6.07
## 5     3    0.49         2 2.12   5.85
## 6     3    0.49         2 2.12   3.25
```

First fit a model with area as a predictor

```r
m5h1.a <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b.area*area,
    a <- dnorm(4.5, 2),
    b.area <- dnorm(0,5),
    sigma <- dunif(0,10)),
    data=data)

precis(m5h1.a)
```

```
##        Mean StdDev  5.5% 94.5%
## a      4.45   0.38  3.84  5.07
## b.area 0.02   0.12 -0.16  0.21
## sigma  1.18   0.08  1.06  1.30
```

```r
plot(precis(m5h1.a))
```

![](Chapter-05-part2-assignment_files/figure-html/unnamed-chunk-2-1.png)

```r
pred.data.5h1.a <- data.frame(area=seq(0,10,length.out=100))

mu.5h1.a <- link(m5h1.a,pred.data.5h1.a,1e4)
```

```
## [ 1000 / 10000 ]
[ 2000 / 10000 ]
[ 3000 / 10000 ]
[ 4000 / 10000 ]
[ 5000 / 10000 ]
[ 6000 / 10000 ]
[ 7000 / 10000 ]
[ 8000 / 10000 ]
[ 9000 / 10000 ]
[ 10000 / 10000 ]
```

```r
mu.5h1.a.mean <- apply(mu.5h1.a,2,mean)
mu.5h1.a.PI <- apply(mu.5h1.a,2,PI)

plot(weight~area,data=data)
lines(pred.data.5h1.a$area,mu.5h1.a.mean)
lines(pred.data.5h1.a$area,mu.5h1.a.PI[1,],lty=2)
lines(pred.data.5h1.a$area,mu.5h1.a.PI[2,],lty=2)
```

![](Chapter-05-part2-assignment_files/figure-html/unnamed-chunk-2-2.png)
Not much of an effect

Now fit a model with groupsize as a predictor

```r
m5h1.b <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b.groupsize*groupsize,
    a <- dnorm(4.5, 2),
    b.groupsize <- dnorm(0,5),
    sigma <- dunif(0,10)),
    data=data)

precis(m5h1.b)
```

```
##              Mean StdDev  5.5% 94.5%
## a            5.05   0.32  4.54  5.57
## b.groupsize -0.12   0.07 -0.23 -0.01
## sigma        1.16   0.08  1.04  1.29
```

```r
plot(precis(m5h1.b))
```

![](Chapter-05-part2-assignment_files/figure-html/unnamed-chunk-3-1.png)

```r
pred.data.5h1.b <- data.frame(groupsize=seq(0,10,length.out=100))

mu.5h1.b <- link(m5h1.b,pred.data.5h1.b,1e4)
```

```
## [ 1000 / 10000 ]
[ 2000 / 10000 ]
[ 3000 / 10000 ]
[ 4000 / 10000 ]
[ 5000 / 10000 ]
[ 6000 / 10000 ]
[ 7000 / 10000 ]
[ 8000 / 10000 ]
[ 9000 / 10000 ]
[ 10000 / 10000 ]
```

```r
mu.5h1.b.mean <- apply(mu.5h1.b,2,mean)
mu.5h1.b.PI <- apply(mu.5h1.b,2,PI)

plot(weight~groupsize,data=data)
lines(pred.data.5h1.b$groupsize,mu.5h1.b.mean)
lines(pred.data.5h1.b$groupsize,mu.5h1.b.PI[1,],lty=2)
lines(pred.data.5h1.b$groupsize,mu.5h1.b.PI[2,],lty=2)
```

![](Chapter-05-part2-assignment_files/figure-html/unnamed-chunk-3-2.png)

Neither variable is particularly important for predicting body weight

## 5H2

What about using both area and groupsize?

```r
m5h2 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b.area*area + b.groupsize*groupsize,
    a <- dnorm(4.5, 2),
    b.groupsize <- dnorm(0,5),
    b.area <- dnorm(0,5),
    sigma <- dunif(0,10)),
    data=data)

precis(m5h2)
```

```
##              Mean StdDev  5.5% 94.5%
## a            4.45   0.36  3.87  5.04
## b.groupsize -0.43   0.12 -0.62 -0.24
## b.area       0.62   0.20  0.30  0.93
## sigma        1.12   0.07  1.00  1.24
```

```r
plot(precis(m5h2))
```

![](Chapter-05-part2-assignment_files/figure-html/unnamed-chunk-4-1.png)

```r
pred.data.5h2.gs <- data.frame(groupsize=seq(0,10,length.out=100),
                                      area=mean(data$area))

mu.5h2.gs <- link(m5h2,pred.data.5h2.gs,1e4)
```

```
## [ 1000 / 10000 ]
[ 2000 / 10000 ]
[ 3000 / 10000 ]
[ 4000 / 10000 ]
[ 5000 / 10000 ]
[ 6000 / 10000 ]
[ 7000 / 10000 ]
[ 8000 / 10000 ]
[ 9000 / 10000 ]
[ 10000 / 10000 ]
```

```r
mu.5h2.gs.mean <- apply(mu.5h2.gs,2,mean)
mu.5h2.gs.PI <- apply(mu.5h2.gs,2,PI)

plot(weight~groupsize,data=data,type="n")
lines(pred.data.5h2.gs$groupsize,mu.5h2.gs.mean)
lines(pred.data.5h2.gs$groupsize,mu.5h2.gs.PI[1,],lty=2)
lines(pred.data.5h2.gs$groupsize,mu.5h2.gs.PI[2,],lty=2)
```

![](Chapter-05-part2-assignment_files/figure-html/unnamed-chunk-4-2.png)

```r
pred.data.5h2.area <- data.frame(groupsize=mean(data$groupsize),
                                      area=seq(0,10,length.out=100))

mu.5h2.area <- link(m5h2,pred.data.5h2.area,1e4)
```

```
## [ 1000 / 10000 ]
[ 2000 / 10000 ]
[ 3000 / 10000 ]
[ 4000 / 10000 ]
[ 5000 / 10000 ]
[ 6000 / 10000 ]
[ 7000 / 10000 ]
[ 8000 / 10000 ]
[ 9000 / 10000 ]
[ 10000 / 10000 ]
```

```r
mu.5h2.area.mean <- apply(mu.5h2.area,2,mean)
mu.5h2.area.PI <- apply(mu.5h2.area,2,PI)

plot(weight~area,data=data,type="n")
lines(pred.data.5h2.area$area,mu.5h2.area.mean)
lines(pred.data.5h2.area$area,mu.5h2.area.PI[1,],lty=2)
lines(pred.data.5h2.area$area,mu.5h2.area.PI[2,],lty=2)
```

![](Chapter-05-part2-assignment_files/figure-html/unnamed-chunk-4-3.png)

```r
plot(groupsize~area,data=data)
```

![](Chapter-05-part2-assignment_files/figure-html/unnamed-chunk-4-4.png)

The fits much better.  Why? Because groupsize and area are both correlated but affect body weight in opposite ways.

## 5H3

What about average food available in the territory?

#### Avg Food and Group Size


```r
m5h3a <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b.avgfood*avgfood + b.groupsize*groupsize,
    a <- dnorm(4.5, 2),
    b.groupsize <- dnorm(0,5),
    b.avgfood <- dnorm(0,5),
    sigma <- dunif(0,10)),
    data=data)

precis(m5h3a)
```

```
##              Mean StdDev  5.5% 94.5%
## a            4.19   0.42  3.52  4.86
## b.groupsize -0.54   0.15 -0.79 -0.30
## b.avgfood    3.58   1.17  1.72  5.45
## sigma        1.12   0.07  1.00  1.23
```

```r
plot(precis(m5h3a))
```

![](Chapter-05-part2-assignment_files/figure-html/5H3_part1-1.png)

```r
pred.data.5h3.gs <- data.frame(groupsize=seq(0,10,length.out=100),
                                      avgfood=mean(data$avgfood))

mu.5h3.gs <- link(m5h3a,pred.data.5h3.gs,1e4)
```

```
## [ 1000 / 10000 ]
[ 2000 / 10000 ]
[ 3000 / 10000 ]
[ 4000 / 10000 ]
[ 5000 / 10000 ]
[ 6000 / 10000 ]
[ 7000 / 10000 ]
[ 8000 / 10000 ]
[ 9000 / 10000 ]
[ 10000 / 10000 ]
```

```r
mu.5h3.gs.mean <- apply(mu.5h3.gs,2,mean)
mu.5h3.gs.PI <- apply(mu.5h3.gs,2,PI)

plot(weight~groupsize,data=data,type="n")
lines(pred.data.5h3.gs$groupsize,mu.5h3.gs.mean)
lines(pred.data.5h3.gs$groupsize,mu.5h3.gs.PI[1,],lty=2)
lines(pred.data.5h3.gs$groupsize,mu.5h3.gs.PI[2,],lty=2)
```

![](Chapter-05-part2-assignment_files/figure-html/5H3_part1-2.png)

```r
pred.data.5h3.avgfood <- data.frame(groupsize=mean(data$groupsize),
                                      avgfood=seq(0,2,length.out=100))

mu.5h3.avgfood <- link(m5h3a,pred.data.5h3.avgfood,1e4)
```

```
## [ 1000 / 10000 ]
[ 2000 / 10000 ]
[ 3000 / 10000 ]
[ 4000 / 10000 ]
[ 5000 / 10000 ]
[ 6000 / 10000 ]
[ 7000 / 10000 ]
[ 8000 / 10000 ]
[ 9000 / 10000 ]
[ 10000 / 10000 ]
```

```r
mu.5h3.avgfood.mean <- apply(mu.5h3.avgfood,2,mean)
mu.5h3.avgfood.PI <- apply(mu.5h3.avgfood,2,PI)

plot(weight~avgfood,data=data,type="n")
lines(pred.data.5h3.avgfood$avgfood,mu.5h3.avgfood.mean)
lines(pred.data.5h3.avgfood$avgfood,mu.5h3.avgfood.PI[1,],lty=2)
lines(pred.data.5h3.avgfood$avgfood,mu.5h3.avgfood.PI[2,],lty=2)
```

![](Chapter-05-part2-assignment_files/figure-html/5H3_part1-3.png)

```r
plot(groupsize~avgfood,data=data)
```

![](Chapter-05-part2-assignment_files/figure-html/5H3_part1-4.png)

```r
cor(data$groupsize,data$avgfood)
```

```
## [1] 0.9014829
```
So Avgfood and GroupSize are correlated by have opposite effects on weight so they "mask" one another.  The model fits well when both are included.

#### Average Food, GroupSize, Area

```r
m5h3b <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b.avgfood*avgfood + b.groupsize*groupsize + b.area*area,
    a <- dnorm(4.5, 2),
    b.groupsize <- dnorm(0,5),
    b.avgfood <- dnorm(0,5),
    b.area <- dnorm(0.5),
    sigma <- dunif(0,10)),
    data=data)

precis(m5h3b)
```

```
##              Mean StdDev  5.5% 94.5%
## a            4.11   0.42  3.45  4.77
## b.groupsize -0.59   0.15 -0.84 -0.35
## b.avgfood    2.26   1.38  0.07  4.46
## b.area       0.41   0.23  0.04  0.77
## sigma        1.10   0.07  0.99  1.22
```

```r
plot(precis(m5h3b))
```

![](Chapter-05-part2-assignment_files/figure-html/5H3_part2-1.png)

Now the coefficients for avgfood and area are both lower and could be 0

Question: is average food or area a better predictor of weight?

The three-predictor model isn't very helpful because the two predictors are corelated

```r
plot(area ~ avgfood, data=data)
```

![](Chapter-05-part2-assignment_files/figure-html/unnamed-chunk-5-1.png)

```r
cor(data$area, data$avgfood)
```

```
## [1] 0.8831038
```

One possiblity is to look at the sigma in the two-way models.  Lowed sigma would indicate a better fit)

```r
#avgfood and groupsize
precis(m5h3a)
```

```
##              Mean StdDev  5.5% 94.5%
## a            4.19   0.42  3.52  4.86
## b.groupsize -0.54   0.15 -0.79 -0.30
## b.avgfood    3.58   1.17  1.72  5.45
## sigma        1.12   0.07  1.00  1.23
```

```r
#area and groupsize
precis(m5h2)
```

```
##              Mean StdDev  5.5% 94.5%
## a            4.45   0.36  3.87  5.04
## b.groupsize -0.43   0.12 -0.62 -0.24
## b.area       0.62   0.20  0.30  0.93
## sigma        1.12   0.07  1.00  1.24
```

They seem pretty similar to me; not sure how to ditinguish.

