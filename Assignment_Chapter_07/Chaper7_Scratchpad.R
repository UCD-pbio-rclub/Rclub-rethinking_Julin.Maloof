          
library(rethinking)
data(rugged)
d <- rugged
# make log version of outcome
d$log_gdp <- log( d$rgdppc_2000 )
# extract countries with GDP data
dd <- d[ complete.cases(d$rgdppc_2000) , ]
# split countries into Africa and not-Africa
d.A1 <- dd[ dd$cont_africa==1 , ] # Africa
d.A0 <- dd[ dd$cont_africa==0 , ] # not Africa


# African nations
m7.1 <- map(
  alist(        log_gdp ~ dnorm( mu , sigma ) ,
                mu <- a + bR*rugged ,
                a ~ dnorm( 8 , 100 ) ,
                bR ~ dnorm( 0 , 1 ) ,
                sigma ~ dunif( 0 , 10 ) ),
  data=d.A1 )

# non-African nations
m7.2 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged ,
    a ~ dnorm( 8 , 100 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ), data=d.A0 )

precis(m7.1)

summary(d$rugged)

pred.df <- data.frame(rugged=seq(0,8,length.out = 50))

mu7.1 <- link(m7.1,pred.df)
                      
mean7.1 <- apply(mu7.1,2,mean)
PI.7.1 <- apply(mu7.1,2,PI)

plot(d.A1$log_gdp~d.A1$rugged,col=rangi2,main="Africa")
lines(pred.df$rugged,mean7.1)
shade(PI.7.1,pred.df$rugged)

precis(m7.2)

mu7.2 <- link(m7.2,pred.df)

mean7.2 <- apply(mu7.2,2,mean)
PI.7.2 <- apply(mu7.2,2,PI)

plot(d.A0$log_gdp~d.A0$rugged,col=rangi2,main="Not Africa")
lines(pred.df$rugged,mean7.2)
shade(PI.7.2,pred.df$rugged)

m7.3 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged ,
    a ~ dnorm( 8 , 100 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 ) ),
  data=dd )

m7.4 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa ,
    a ~ dnorm( 8 , 100 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ), data=dd )

rugged.seq <- seq(from=-1,to=8,by=0.25)
# compute mu over samples, fixing cont_africa=0
mu.NotAfrica <- link( m7.4 , data=data.frame(cont_africa=0,rugged=rugged.seq) )
# compute mu over samples, fixing cont_africa=1
mu.Africa <- link( m7.4 , data=data.frame(cont_africa=1,rugged=rugged.seq) )
# summarize to means and intervals
mu.NotAfrica.mean <- apply( mu.NotAfrica , 2 , mean )
mu.NotAfrica.PI <- apply( mu.NotAfrica , 2 , PI , prob=0.97 )
mu.Africa.mean <- apply( mu.Africa , 2 , mean )
mu.Africa.PI <- apply( mu.Africa , 2 , PI , prob=0.97 )

m7.5 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + gamma*rugged + bA*cont_africa ,
    gamma <- bR + bAR*cont_africa ,
    a ~ dnorm( 8 , 100 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bAR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ), data=dd )

compare( m7.3 , m7.4 , m7.5 )

rugged.seq <- seq(from=-1,to=8,by=0.25)
mu.Africa <- link( m7.5 , data=data.frame(cont_africa=1,rugged=rugged.seq) )
mu.Africa.mean <- apply( mu.Africa , 2 , mean )
mu.Africa.PI <- apply( mu.Africa , 2 , PI , prob=0.97 )
mu.NotAfrica <- link( m7.5 , data=data.frame(cont_africa=0,rugged=rugged.seq) )
mu.NotAfrica.mean <- apply( mu.NotAfrica , 2 , mean )
mu.NotAfrica.PI <- apply( mu.NotAfrica , 2 , PI , prob=0.97 )

# plot African nations with regression
d.A1 <- dd[dd$cont_africa==1,]
plot( log(rgdppc_2000) ~ rugged , data=d.A1 ,
      col=rangi2 , ylab="log GDP year 2000" ,
      xlab="Terrain Ruggedness Index" )
mtext( "African nations" , 3 )
lines( rugged.seq , mu.Africa.mean , col=rangi2 )
shade( mu.Africa.PI , rugged.seq , col=col.alpha(rangi2,0.3) )
# plot non-African nations with regression
d.A0 <- dd[dd$cont_africa==0,]
plot( log(rgdppc_2000) ~ rugged , data=d.A0 ,
      col="black" , ylab="log GDP year 2000" ,
      xlab="Terrain Ruggedness Index" )
mtext( "Non-African nations" , 3 )
lines( rugged.seq , mu.NotAfrica.mean )
shade( mu.NotAfrica.PI , rugged.seq )