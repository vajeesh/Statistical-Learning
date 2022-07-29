set.seed(123)
repl<-10000
b0<- -1
b1 <- 0.5
sde<- sqrt(0.25)
b0e<-rep(NA,repl)
b1e<-rep(NA,repl)
b2e<-rep(NA,repl)
for(i in 1:repl){
  x<-rnorm(100)
  eps<-rnorm(100,sd=sde)
  y<- b0+b1*x+eps
  l1<-lm(y~x+I(x^2))
  cf1<-coef(l1)
  b0e[i]<-cf1[1]
  b1e[i]<-cf1[2]
  b2e[i]<-cf1[3]
}
summary(b2e)