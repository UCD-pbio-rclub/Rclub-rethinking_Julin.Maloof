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