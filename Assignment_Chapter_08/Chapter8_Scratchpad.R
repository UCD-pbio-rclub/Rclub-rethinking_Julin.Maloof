#Chapter 8 scratchpad

#R Code 8.2 and 8.3

library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]

m8.1 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data=dd )
precis(m8.1)

# 8.4

dd.trim <- dd[ , c("log_gdp","rugged","cont_africa") ]
str(dd.trim)

# 8.5

m8.1stan <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2)
  ), data=dd.trim )

precis(m8.1stan)

#8.7
m8.1stan_4chains <- map2stan( m8.1stan , chains=4 , cores=4,iter=5000 )
precis(m8.1stan_4chains)

#8.8

post <- extract.samples( m8.1stan )
str(post)
pairs(post)
pairs(m8.1stan)
show(m8.1stan)
show(m8.1stan_4chains)
DIC(m8.1stan)
WAIC(m8.1stan)
plot(m8.1stan)
plot(m8.1stan_4chains)

#8.13
y <- c(-1,1)
m8.2 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- alpha
  ),
  data=list(y=y) , start=list(alpha=0,sigma=1) , chains=2 , iter=4000 , warmup=1000 )

#8.14
precis(m8.2)
plot(m8.2)

#8.16
m8.3 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- alpha ,
    alpha ~ dnorm( 1 , 10 ) ,
    sigma ~ dcauchy( 0 , 1 )
  ),
  data=list(y=y) , start=list(alpha=0,sigma=1) , chains=2 , iter=4000 , warmup=1000 )
precis(m8.3)


￼￼
y <- rcauchy(1e4,0,5)
mu <- sapply( 1:length(y) , function(i) sum(y[1:i])/i )
plot(mu,type="l")

stancode(m8.1stan)

#8.17, 8.18

y <- rnorm( 100 , mean=0 , sd=1 )

m8.4 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a1 + a2 ,
    sigma ~ dcauchy( 0 , 1 )
  ),
  data=list(y=y) , start=list(a1=0,a2=0,sigma=1) , chains=2 , iter=4000 , warmup=1000, cores=2 )
precis(m8.4)
plot(m8.4)

#8.19
m8.5 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a1 + a2 ,
    a1 ~ dnorm( 0 , 10 ) ,
    a2 ~ dnorm( 0 , 10 ) ,
    sigma ~ dcauchy( 0 , 1 )
  ),
  data=list(y=y) , start=list(a1=0,a2=0,sigma=1) , chains=2 , iter=4000 , warmup=1000,cores=2 )
precis(m8.5)
plot(m8.5)
