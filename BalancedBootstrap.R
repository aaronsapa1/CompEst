phi<-function(x){
  2*exp(-2*x)
}
sample.size<-1000
U <- runif(sample.size, 0, 1) 
X <- -(1/3)*log(1 - (1 - exp(-2*(3)))*U)
w <- dexp(X,rate=3)/(1-exp(-2*(3)))
PhiX <- phi(X)
estim.v <- PhiX/w

nboot<-50
X.rep <-rep(estim.v,times=nboot)
X.i <- sample(1:length(X.rep),replace=FALSE)
X.b <- split(X.rep[X.i],1:nboot)

#Balanceo
estim.v2 <-sapply(names(X.b),function(x){
  mean(X.b[[x]])
})

estim <- mean(estim.v2)
sol <- 1-exp(-2*2)

X.sort<-sort(estim.v)
X.sortinv<-sort(estim.v,dec = T)
#cor(X.sort,X.sortinv)
X.sum <- (X.sort + X.sortinv)*0.5
estim.antithetic <- mean(X.sum)
#merge
X.b2<-lapply(names(X.b),function(x){
 #sort(X.b[[x]])
   X.sort<-sort(X.b[[x]])
  X.sortinv<-sort(X.b[[x]],dec = T)
  X.sum <- (X.sort + X.sortinv)*0.5
})
estim.v3 <-sapply(X.b2,function(x){
  mean(x)
})


estim2 <- mean(estim.v3)
sol <- 1-exp(-2*2)
