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
