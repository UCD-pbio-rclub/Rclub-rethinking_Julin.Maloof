library(rethinking)

d <- read.csv("Assignment_Chatper_10/LironDataTime1.csv")

summary(d)

head(d)

colnames(d) <- gsub("\\.","",colnames(d))

d$Temperature_b <- d$Temperature-min(d$Temperature)

d$Penn <- ifelse(d$Species=="pen",1,0)

d$Germination2 <- ifelse(d$Germination=="Yes",1,0)

m1 <- map(alist(
  Germination2 ~ dbinom(1,p),
  logit(p) <- a + b_pen * Penn + b_temp * Temperature_b,
  a ~ dnorm(0,2),
  c(b_pen,b_temp) ~ dnorm(0,10)),
  data=d)

precis(m1)

#m1.stan <- map2stan(m1,chains=4)
      
pairs(m1.stan)

m2 <- map(alist(
  Germination2 ~ dbinom(1,p),
  logit(p) <- a + b_pen * Penn + b_temp * Temperature_b + b_pen_temp*Penn*Temperature_b,
  a ~ dnorm(0,2),
  c(b_pen,b_temp,b_pen_temp) ~ dnorm(0,10)),
  data=d)

precis(m2)
compare(m1,m2)

