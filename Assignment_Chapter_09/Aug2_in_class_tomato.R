# In class solving of the tomato problem
# Aug 2, 2016
# By: Julin, Nicole, Gina, Ruijuan, Xiaoyan, Jessica, Emily

setwd("~/git/r_club_members/Rclub-rethinking_Julin.Maloof/Assignment_Chapter_09")

# load the data and libraries
data <- read.csv("TomatoR2CSHL.csv")

library(rethinking)
library(ggplot2)
library(reshape2)

# take a look at the data
head(data)
summary(data)

# density plots
pl <- ggplot(data=data,aes(x=hyp,fill=trt))
pl <- pl + geom_density()
pl <- pl + facet_grid(species ~ trt)
pl

# box plots
pl <- ggplot(data=data,aes(y=hyp,x=species,fill=trt))
pl <- pl + geom_boxplot()
pl

# log2 transformation
pl <- ggplot(data=data,aes(x=log2(hyp),fill=trt))
pl <- pl + geom_density()
pl <- pl + facet_grid(species ~ trt)
pl + ggtitle("log2 transformed")

# Square root transformation
pl <- ggplot(data=data,aes(x=sqrt(hyp),fill=trt))
pl <- pl + geom_density()
pl <- pl + facet_grid(species ~ trt)
pl + ggtitle("sqrt transformed")

#normality test
by(data$hyp,list(data$trt,data$species),shapiro.test)
by(sqrt(data$hyp),list(data$trt,data$species),shapiro.test)
by(log2(data$hyp),list(data$trt,data$species),shapiro.test)
#sqrt transformed is best

# categorical variable for trt
data$trtL <- ifelse(data$trt=="L",1,0)

# alternative way
levels(data$trt)
data$trt2 <- as.numeric(data$trt)-1 # 0 = H, 1 = L

#categorical variables for species
data$index <- 1:nrow(data)
data2 <- dcast(data,index + hyp + trt2 ~ species, value.var="species",fun.aggregate=length)

head(data2)

#must subset the data frame to contain only the relevant columns
data2.trt <- data2[,c("hyp","trt2")]

head(data2.trt)

# trt model
hyp.stan <- map2stan(alist(
  hyp ~ dnorm(mu,sigma),
  mu <- a + bT * trt2,
  a ~ dnorm(0,100),
  bT ~ dnorm(0,10),
  sigma ~ dunif(0,20)),
  data2.trt,
  chains = 4)

plot(hyp.stan)
precis(hyp.stan)
par(mfrow=c(1,1),mfcol=c(1,1))
plot(precis(hyp.stan))

head(data2)

# fix species names to get rid of space
colnames(data2) <- sub(". ","_",colnames(data2))

# model where each species has its own intercept
data2.species.all <- data2[,c(2,4:8)]

head(data2.species.all)

species.stan <- map2stan(alist(
  hyp ~ dnorm(mu,sigma),
  mu <- bChil*S_chilense + bChmi*S_chmielewskii + bHab*S_habrochaites + bPen * S_pennellii + bPer*S_peruvianum,
  c(bChil,bChmi,bHab,bPen,bPer) ~ dnorm(33.35,20),
  sigma ~ dunif(0,20)),
  data2.species.all,
  chains = 4)

plot(species.stan)
precis(species.stan)
par(mfrow=c(1,1),mfcol=c(1,1))
plot(precis(species.stan))

# use S. chm as intercept

data2.species.intercept <- data2[,c(2,4,6:8)]

head(data2.species.intercept)
head(data2.species.all)

species.stan.intercept <- map2stan(alist(
  hyp ~ dnorm(mu,sigma),
  mu <- a + bChil*S_chilense + bHab*S_habrochaites + bPen * S_pennellii + bPer*S_peruvianum,
  a ~ dnorm(33.35,10),
  c(bChil,bHab,bPen,bPer) ~ dnorm(0,10),
  sigma ~ dunif(0,20)),
  data2.species.intercept,
  chains = 4)

plot(species.stan.intercept)
precis(species.stan.intercept)
par(mfrow=c(1,1),mfcol=c(1,1))
plot(precis(species.stan.intercept))

# model with species and treatment

data2.species.trt <- data2[,c(2:4,6:8)]

head(data2.species.trt)

species.trt.stan <- map2stan(alist(
  hyp ~ dnorm(mu,sigma),
  mu <- a + bT*trt2 + bChil*S_chilense + bHab*S_habrochaites + bPen * S_pennellii + bPer*S_peruvianum,
  a ~ dnorm(33.35,10),
  c(bT,bChil,bHab,bPen,bPer) ~ dnorm(0,10),
  sigma ~ dunif(0,20)),
  data2.species.trt,
  chains = 4)

plot(species.trt.stan)
precis(species.trt.stan)
par(mfrow=c(1,1),mfcol=c(1,1))
plot(precis(species.trt.stan))

#compare models

compare(hyp.stan,species.trt.stan,species.stan.intercept,species.stan)
plot(compare(hyp.stan,species.trt.stan,species.stan.intercept,species.stan))

compare(hyp.stan,species.trt.stan,species.stan.intercept,species.stan)

coeftab(hyp.stan,species.trt.stan,species.stan.intercept,species.stan)

plot(coeftab(hyp.stan,species.trt.stan,species.stan.intercept,species.stan))

# what can we say about the treatment effect? 

# get the posterior distrubution of bT, the treatment coefficient.

post.bT <- extract.samples(species.trt.stan)$bT

dens(post.bT,show.HPDI = 0.95) # the fact that the 95% HDPI intervals are far away from 0 is strong evidence that bT is positive

# what percent of the posterior distribution of bT is less than or equal to ?
sum(post.bT <= 0) / length(post.bT) # None of the posterior distribution for bT is less than or equal to 0.

data2.species.trt.sqrt <- within(data2.species.trt, hyp <- sqrt(hyp))
head(data2.species.trt.sqrt)

species.trt.stan.sqrt <- map2stan(alist(
  hyp ~ dnorm(mu,sigma),
  mu <- a + bT*trt2 + bChil*S_chilense + bHab*S_habrochaites + bPen * S_pennellii + bPer*S_peruvianum,
  a ~ dnorm(5,10),
  c(bT,bChil,bHab,bPen,bPer) ~ dnorm(0,10),
  sigma ~ dunif(0,20)),
  data2.species.trt.sqrt,
  chains = 4)

plot(species.trt.stan.sqrt)
precis(species.trt.stan.sqrt)
par(mfrow=c(1,1),mfcol=c(1,1))
plot(precis(species.trt.stan.sqrt))

compare(species.trt.stan,species.trt.stan.sqrt)

species.trt.stan.mu <- link(species.trt.stan)
dim(species.trt.stan.mu)
head(species.trt.stan.mu[,1:10])
tail(species.trt.stan.mu[,1:10])
species.trt.stan.mu.mean <- apply(species.trt.stan.mu,2,mean)
head(species.trt.stan.mu.mean)
plot(species.trt.stan@data$hyp,species.trt.stan.mu.mean)
cor(species.trt.stan@data$hyp,species.trt.stan.mu.mean)

species.trt.stan.sqrt.mu <- link(species.trt.stan.sqrt)
dim(species.trt.stan.sqrt.mu)
head(species.trt.stan.sqrt.mu[,1:10])
tail(species.trt.stan.sqrt.mu[,1:10])
species.trt.stan.sqrt.mu.mean <- apply(species.trt.stan.sqrt.mu,2,mean)
head(species.trt.stan.sqrt.mu.mean)
plot(species.trt.stan.sqrt@data$hyp^2,species.trt.stan.sqrt.mu.mean^2)
cor(species.trt.stan.sqrt@data$hyp^2,species.trt.stan.sqrt.mu.mean^2)

## interaction model (non-transformed)

species.trt.int.stan <- map2stan(alist(
  hyp ~ dnorm(mu,sigma),
  mu <- a + 
    bT*trt2 + 
    bChil*S_chilense + 
    bHab*S_habrochaites + 
    bPen*S_pennellii + 
    bPer*S_peruvianum +
    bChil_T*S_chilense*trt2 +
    bHab_T*S_habrochaites*trt2 + 
    bPen_T*S_pennellii*trt2 + 
    bPer_T*S_peruvianum*trt2,
  a ~ dnorm(33.35,10),
  c(bT,bChil,bHab,bPen,bPer,bChil_T,bHab_T,bPen_T,bPer_T) ~ dnorm(0,10),
  sigma ~ dunif(0,20)),
  data2.species.trt,
  chains = 4)

plot(species.trt.int.stan)
precis(species.trt.int.stan)
par(mfrow=c(1,1),mfcol=c(1,1))
plot(precis(species.trt.int.stan))

posterior.int <- extract.samples(species.trt.int.stan)
names(posterior.int)
head(posterior.int$bPen_T)
posterior.int <- posterior.int[names(posterior.int)!="sigma"]
post.summary <- sapply(names(posterior.int),function(n) {
  dens(posterior.int[[n]],main=n,show.HPDI = .95, show.zero = TRUE)
  sum(posterior.int[[n]]<=0) / length(posterior.int[[n]]) 
})
post.summary

