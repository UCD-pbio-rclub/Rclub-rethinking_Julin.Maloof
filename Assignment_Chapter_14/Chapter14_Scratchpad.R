#Chapter 14 scratchpad


#14.2
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
head(WaffleDivorce)
# points
plot( d$Divorce ~ d$MedianAgeMarriage , ylim=c(4,15) ,
      xlab="Median age marriage" , ylab="Divorce rate" )
# standard errors
for ( i in 1:nrow(d) ) {
  ci <- d$Divorce[i] + c(-1,1)*d$Divorce.SE[i]
  x <- d$MedianAgeMarriage[i]
  lines( c(x,x) , ci )
}

## 14.3
dlist <- list(  div_obs=d$Divorce,
               div_sd=d$Divorce.SE,
               R=d$Marriage,
               A=d$MedianAgeMarriage
)

m14.1 <- map2stan(
  alist(
    div_est ~ dnorm(mu,sigma),
    mu <- a + bA*A + bR*R,
    div_obs ~ dnorm(div_est,div_sd),
    a ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2.5)
  ) ,
  data=dlist ,
  start=list(div_est=dlist$div_obs) ,
  WAIC=FALSE , iter=5000 , warmup=1000 , chains=2 , cores=2 ,
  control=list(adapt_delta=0.95) )

precis( m14.1 , depth=2 )

##14.5

dlist <- list(
  div_obs=d$Divorce,
  div_sd=d$Divorce.SE,
  mar_obs=d$Marriage,
  mar_sd=d$Marriage.SE,
  A=d$MedianAgeMarriage )

m14.2 <- map2stan(
  alist(
    div_est ~ dnorm(mu,sigma),
    mu <- a + bA*A + bR*mar_est[i],
    div_obs ~ dnorm(div_est,div_sd),
    mar_obs ~ dnorm(mar_est,mar_sd),
    a ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2.5)
  ) ,
  data=dlist ,
  start=list(div_est=dlist$div_obs,mar_est=dlist$mar_obs) ,
  WAIC=FALSE , iter=5000 , warmup=1000, chains=3 , cores=3 ,
  control=list(adapt_delta=0.95) )
precis(m14.2,depth = 2)

## 14.6

library(rethinking) 
data(milk)
d <- milk
d$neocortex.prop <- d$neocortex.perc / 100
d$logmass <- log(d$mass)

## 14.7
# prep data 14.7
data_list <- list(
  kcal = d$kcal.per.g,
  neocortex = d$neocortex.prop,
  logmass = d$logmass )
# fit model
m14.3 <- map2stan(
  alist(
    kcal ~ dnorm(mu,sigma),
    mu <- a + bN*neocortex + bM*logmass,
    neocortex ~ dnorm(nu,sigma_N),
    a ~ dnorm(0,100),
    c(bN,bM) ~ dnorm(0,10),
    nu ~ dnorm(0.5,1),
    sigma_N ~ dcauchy(0,1),
    sigma ~ dcauchy(0,1)
  ) ,
  data=data_list , iter=1e4 , chains=2 )