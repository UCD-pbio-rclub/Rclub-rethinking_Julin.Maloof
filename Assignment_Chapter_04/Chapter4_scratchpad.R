library(ggplot2)
library(rethinking)

plot_dist <- function(x) { # function to plot distribution of values and compare to normal
  pl <- qplot(x,geom="density")
  xnorm <- seq(min(x),max(x),length.out=1000)
  ynorm <- dnorm(xnorm,mean=mean(x),sd=sd(x))
  
  pl + geom_line(aes(x=xnorm,y=ynorm),lty=2,col="red")
}

pos <- replicate( 1000 , sum( runif(16,-1,1) ) )

plot_dist(pos)

# alternative: everyone takes the same size step

pos <- replicate( 1000 , sum( sample(c(-1,1),size = 16,replace = TRUE ) ))

plot_dist(pos)

# more replicates

pos <- replicate( 10000 , sum( sample(c(-1,1),size = 16,replace = TRUE ) ))

plot_dist(pos)

# more steps

pos <- replicate( 1000 , sum( sample(c(-1,1),size = 200,replace = TRUE ) ))

plot_dist(pos)

# more steps and more replicates

pos <- replicate( 10000 , sum( sample(c(-1,1),size = 200,replace = TRUE ) ))

plot_dist(pos)

# mutliplicative example

growth <- replicate( 10000 , prod( 1 + runif(12,0,0.1) ) )
plot_dist(growth)

# different effect sizes

big <- replicate( 10000 , prod( 1 + runif(12,0,0.5) ) )
small <- replicate( 10000 , prod( 1 + runif(12,0,0.01) ) )

plot_dist(big)
plot_dist(small)

log.big <- replicate( 10000 , log(prod(1 + runif(12,0,0.5))) )

plot_dist(log.big)

# Section 4.3

library(rethinking)
data(Howell1)
d <- Howell1

head(d)
summary(d)

d2 <- d[ d$age >= 18 , ]

summary(d2)

dens(d2$height)

plot_dist(d2$height)

curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )

curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )

sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

mu.list <- seq( from=150, to=160 , length.out=200 ) # I changed the lower min to get a smooth dist
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum( dnorm(
  d2$height ,
  mean=post$mu[i] ,
  sd=post$sigma[i] ,
  log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) ) #why is this max and not sum?  Because we are calculating relative log likelihoods.  And since we are on the log scale, sum wouldn't have worked to give us true probabilities anyway.

head(post)
tail(post)

contour_xyz( post$mu , post$sigma , post$prob )

image_xyz( post$mu , post$sigma , post$prob )

sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )

dens( sample.mu )
dens( sample.sigma )

HPDI( sample.mu )
HPDI( sample.sigma )


d3 <- sample( d2$height , size=20 )

mu.list <- seq( from=150, to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i)
  sum( dnorm( d3 , mean=post2$mu[i] , sd=post2$sigma[i] ,
              log=TRUE ) ) )
post2$prod <- post2$LL + dnorm( post2$mu , 178 , 20 , TRUE ) +
  dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
                        prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="mu" , ylab="sigma" , pch=16 )

dens( sample2.sigma , norm.comp=TRUE )

# Quadratic approximation

library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]


flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm( 178 , 20 ) ,
  sigma ~ dunif( 0 , 50 )
)

m4.1 <- map( flist , data=d2 )

str(m4.1)

precis( m4.1 )

m4.2 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu ~ dnorm( 178 , 0.1 ) ,
    sigma ~ dunif( 0 , 50 )
  ),
  data=d2 )
precis( m4.2 )

vcov(m4.1)

diag(vcov(m4.1))

cov2cor(vcov(m4.1))

post <- extract.samples(m4.1,n = 10e4)
head(post)
precis(post)
precis(m4.1)
plot(post,col=col.alpha(rangi2,.1))

plot(d2$height~d2$weight)

# load data again, since it's a long way back
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]
# fit model
m4.3 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 156 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ), data=d2 )

precis(m4.3)

precis(m4.3,corr=TRUE)

d2$weight.c <- d2$weight - mean(d2$weight)

m4.4 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight.c ,
    a ~ dnorm( 178 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 ) ),
  data=d2 )

precis( m4.4 , corr=TRUE )

plot(height~weight, data=d2)
abline( a=coef(m4.3)["a"] , b=coef(m4.3)["b"] )

post <- extract.samples( m4.3 )
post[1:5,]


N <- 10
dN <- d2[ 1:N , ]
mN <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 178 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=dN )

# extract 20 samples from the posterior
post <- extract.samples( mN , n=20 )
# display raw data and sample size
plot( dN$weight , dN$height ,
      xlim=range(d2$weight) , ylim=range(d2$height) ,
      col=rangi2 , xlab="weight" , ylab="height" )
mtext(concat("N = ",N))
# plot the lines, with transparency
for ( i in 1:20 )
  abline( a=post$a[i] , b=post$b[i] , col=col.alpha("black",0.3) )

N <- 100
dN <- d2[ 1:N , ]
mN <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 178 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=dN )

# extract 20 samples from the posterior
post <- extract.samples( mN , n=20 )
# display raw data and sample size
plot( dN$weight , dN$height ,
      xlim=range(d2$weight) , ylim=range(d2$height) ,
      col=rangi2 , xlab="weight" , ylab="height" )
mtext(concat("N = ",N))
# plot the lines, with transparency
for ( i in 1:20 )
  abline( a=post$a[i] , b=post$b[i] , col=col.alpha("black",0.3) )



# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq( from=25 , to=70 , by=1 )
# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link( m4.3 , data=data.frame(weight=weight.seq) )
str(mu)

# use type="n" to hide raw data
plot( height ~ weight , d2 , type="n" )


for ( i in 1:100 )
  points( weight.seq , mu[i,] , pch=16 , col=col.alpha(rangi2,0.1) )

# summarize the distribution of mu
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )

head(mu.mean)
head(mu.HPDI)

# plot raw data
# fading out points to make line and interval more visible
plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( weight.seq , mu.mean )
# plot a shaded region for 89% HPDI
shade( mu.HPDI , weight.seq )


sim.height <- sim( m4.3 , data=list(weight=weight.seq),n = 1e4 )
str(sim.height)

height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

# plot raw data
plot( height ~ weight , d2 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( weight.seq , mu.mean )
# draw HPDI region for line
shade( mu.HPDI , weight.seq )
# draw PI region for simulated heights
shade( height.PI , weight.seq )

## Section 4.5

library(rethinking)
data(Howell1)
d <- Howell1
str(d)
plot(d$weight,d$height)
#standardize weight:
d$weight.s <- (d$weight - mean(d$weight)) / sd(d$weight)

plot(d$weight.s,d$height)

d$weight.s2 <- d$weight.s^2
m4.5 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight.s + b2*weight.s2 ,
    a ~ dnorm( 178 , 100 ) ,
    b1 ~ dnorm( 0 , 10 ) ,
    b2 ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ), data=d )

precis( m4.5 )

weight.seq <- seq( from=-2.2 , to=2 , length.out=30 )
pred_dat <- list( weight.s=weight.seq , weight.s2=weight.seq^2 )
mu <- link( m4.5 , data=pred_dat )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.height <- sim( m4.5 , data=pred_dat )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

plot( height ~ weight.s , d , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq )

