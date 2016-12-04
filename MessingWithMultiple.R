## Playing around with multiple testing...

## set up a data.frame

df <- data.frame(
  y=rnorm(400,20,2),
  gt=rep(LETTERS[1:20],each=20))

library(rethinking)
df$gt_id <-coerce_index(df$gt)

model1 <- map(alist(
  y ~ dnorm(mu,sd),
  mu <- a[gt_id],
  a[gt_id] <- dnorm(20,1),
  sd <- dunif(0,10)),
  data=df)

precis(model1,depth=2)

plot(precis(model1,depth=2))

#all against A

post <- extract.samples(model1)

names(post)

head(post)[1:10]

post.a <- post$a

head(post.a)

diffs <- sapply(2:20,function(x) {
  post.a[,x] - post.a[,1]
}
)

head(diffs)

precis(data.frame(diffs),prob=.95)
