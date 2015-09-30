uno <- function(x){
  2*sqrt(4-x**2)
}
dos <- function(x){
  4/(1+x**2)
  
}
tres<- function(x){
  6/(sqrt((4-x**2)))
}

montecarlo<- function(nsim,alfa=0.05){
  x<-runif(nsim,0,1)
  phi<-sapply(x,tres)
  theta<-mean(phi)
  z<-qnorm(alfa/2,lower.tail=FALSE)
  s<-var(phi)
  lim.up <-theta + z*sqrt((s**2)/nsim)
  lim.inf <-theta - z*sqrt((s**2)/nsim)
  return(list(est=theta,limup=lim.up,liminf=lim.inf))
}
n<-seq(100,10000,by=10)
res=t(sapply(n,montecarlo))
plot(as.numeric(res[,1]),type="l")
lines(as.numeric(res[,2]),col="blue")
lines(as.numeric(res[,3]),col="red")
abline(h=pi)
