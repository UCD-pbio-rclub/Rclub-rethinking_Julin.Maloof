#Chapter 6 Scratchpad

library(rethinking)

## Section 6.1

## Rcode 6.1

sppnames <- c( "afarensis","africanus","habilis","boisei",
               "rudolfensis","ergaster","sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )

## 6.2

m6.1 <- lm( brain ~ mass , data=d )

## 6.3

1 - var(resid(m6.1))/var(d$brain)

#or look at the summary

summary(m6.1)

## RCode 6.4 and 6.5

m6.2 <- lm( brain ~ mass + I(mass^2) , data=d )

m6.3 <- lm( brain ~ mass + I(mass^2) + I(mass^3) , data=d )
m6.4 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) ,
            data=d )
m6.5 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
              I(mass^5) , data=d )
m6.6 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
              I(mass^5) + I(mass^6) , data=d )

## Rcode 6.6

m6.7 <- lm( brain ~ 1 , data=d )

## Rcode 6.7 and 6.8

plot( brain ~ mass , d , col="slateblue" )
for ( i in 1:nrow(d) ) {
  d.new <- d[ -i , ]
  m0 <- lm( brain ~ mass, d.new )
  abline( m0 , col=col.alpha("black",0.5) )
}

# 6.11

# standardize the mass before fitting
d$mass.s <- (d$mass-mean(d$mass))/sd(d$mass)

m6.8 <- map(
  alist(
    brain ~ dnorm( mu , sigma ) ,
    mu <- a + b*mass.s
  ),
  data=d , start=list(a=mean(d$brain),b=0,sigma=sd(d$brain)) ,
  method="Nelder-Mead" )

# extract MAP estimates
theta <- coef(m6.8)
theta

# compute deviance
dev <- (-2)*sum( dnorm(
  d$brain ,
  mean=theta[1]+theta[2]*d$mass.s ,
  sd=theta[3] ,
  log=TRUE ) )

dev

# RCode 6.12-6.14
N <- 20
kseq <- 1:5
dev <- sapply( kseq , function(k) {
  print(k);
  r <- replicate( 1e4 , sim.train.test( N=N, k=k ) );
  c( mean(r[1,]) , mean(r[2,]) , sd(r[1,]) , sd(r[2,]) )
})

dev

plot( 1:5 , dev[1,] , ylim=c( min(dev[1:2,])-5 , max(dev[1:2,])+10 ) ,
      xlim=c(1,5.1) , xlab="number of parameters" , ylab="deviance" ,
      pch=16 , col=rangi2 )
mtext( concat( "N = ",N ) )
points( (1:5)+0.1 , dev[2,] )
for ( i in kseq ) {
  pts_in <- dev[1,i] + c(-1,+1)*dev[3,i]
  pts_out <- dev[2,i] + c(-1,+1)*dev[4,i]
  lines( c(i,i) , pts_in , col=rangi2 )
  lines( c(i,i)+0.1 , pts_out )
}

## Rcode 6.15 6.20

#fit the model
library(rethinking)
data(cars)

?cars
m <- map(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,30)
  ) , data=cars )

post <- extract.samples(m,n=1000)
head(post) # posterior distribution of model paramters

now compute log likelihood of each observation across posterior distribution

n_samples <- 1000
ll <- sapply( 1:n_samples ,
              function(s) {
                mu <- post$a[s] + post$b[s]*cars$speed
                dnorm( cars$dist , mu , post$sigma[s] , log=TRUE )
              })

dim(ll)
ll[1:10,1:10]

?log_sum_exp
log_sum_exp

test <- seq(0.0001,0.001,0.0001)
test
ltest <- log(test)
ltest
sum(test)
log(sum(test))
log(sum(exp(ltest)))
log_sum_exp(ltest)

#but if these were really small
ltest <- c(-1000,-800,-700,-600)
exp(ltest)
log_sum_exp(ltest)

log(sum(exp(ltest)))

n_cases <- nrow(cars)
lppd <- sapply( 1:n_cases , function(i) log_sum_exp(ll[i,]) - log(n_samples) )

#why not
lppd.jm <- apply(ll,1, function(x) log_sum_exp(x) - log(n_samples))

all(lppd.jm==lppd)

# in any case we now have the average likelihood of each sample (averaged across the posterior)

pWAIC <- sapply( 1:n_cases , function(i) var(ll[i,]) )

#or

pWAIC.jm <- apply(ll,1,var)

all(pWAIC.jm==pWAIC)

-2*(sum(lppd) - sum(pWAIC))

-2*sum(lppd-pWAIC)

WAIC(m)

waic_vec <- -2*( lppd - pWAIC )
sqrt( n_cases*var(waic_vec) )

# Rcode 6.21
library(rethinking)
data(milk)
d <- milk[ complete.cases(milk) , ]
d$neocortex <- d$neocortex.perc / 100
dim(d)

# Rcode 6.22
a.start <- mean(d$kcal.per.g)
sigma.start <- log(sd(d$kcal.per.g))
m6.11 <- map(
  alist(
    kcal.per.g ~ dnorm( a , exp(log.sigma) )
  ),
  data=d , start=list(a=a.start,log.sigma=sigma.start) )
m6.12 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
    mu <- a + bn*neocortex
  ),
  data=d , start=list(a=a.start,bn=0,log.sigma=sigma.start) )
m6.13 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
    mu <- a + bm*log(mass)
  ),
  data=d , start=list(a=a.start,bm=0,log.sigma=sigma.start) )
m6.14 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
    mu <- a + bn*neocortex + bm*log(mass)
  ),
  data=d , start=list(a=a.start,bn=0,bm=0,log.sigma=sigma.start) )

#Rcode 6.24
( milk.models <- compare( m6.11 , m6.12 , m6.13 , m6.14 ) )

plot( milk.models , SE=TRUE , dSE=TRUE )

coeftab(m6.11,m6.12,m6.13,m6.14)

plot(coeftab(m6.11,m6.12,m6.13,m6.14))

#6.29

# compute counterfactual predictions
# neocortex from 0.5 to 0.8
nc.seq <- seq(from=0.5,to=0.8,length.out=30)
d.predict <- list(
  kcal.per.g = rep(0,30), # empty outcome
  neocortex = nc.seq,     # sequence of neocortex
  mass = rep(4.5,30)      # average mass
)
pred.m6.14 <- link( m6.14 , data=d.predict )
mu <- apply( pred.m6.14 , 2 , mean )
mu.PI <- apply( pred.m6.14 , 2 , PI )
# plot it all
plot( kcal.per.g ~ neocortex , d , col=rangi2 )
lines( nc.seq , mu , lty=2 )
lines( nc.seq , mu.PI[1,] , lty=2 )
lines( nc.seq , mu.PI[2,] , lty=2 )

milk.ensemble <- ensemble( m6.11 , m6.12 , m6.13 , m6.14 , data=d.predict )
mu <- apply( milk.ensemble$link , 2 , mean )
mu.PI <- apply( milk.ensemble$link , 2 , PI )
lines( nc.seq , mu )
shade( mu.PI , nc.seq )
