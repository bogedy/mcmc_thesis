# Likelihood function for Hardy-Weinberg model with inbreeding.
# n1, n2, n3 refer to nAA, nAa, naa respectively.
inblike = function(n1,n2,n3,f,p){
    return((f*p+(1-p)*p^2)^n1*((1-f)*2*p*(1-p))^n2 *(f*(1-p)+(1-f)*(1-p))^n3)
}

# Metropolis-Hastings algorithm applied to Hardy-Weinberg with inbreeding.
inbreeding = function(n1=50, n2=21, n3=29, niter=10000){
  f=rep(0, niter)
  p=rep(0, niter)
  f[1] <- 0
  p[1] <- 0
  for (i in 2:niter){
    f.p <- runif(1)
    p.p <- runif(1)
    r <- inblike(n1,n2,n3,f.p,p.p)/inblike(n1,n2,n3,f[i-1],p[i-1])
    flip <- rbinom(1,1,min(r,1))
    if(flip==1){
      f[i]=f.p
      p[i]=p.p
    }
    else{
      f[i]=f[i-1]
      p[i]=p[i-1]
    }
  }
  return(list(f=f,p=p))
}

res=inbreeding()
#plot(hexbin(res$f, res$p, xlab='f', ylab='r', xbnds=c(0,1),ybnds=c(0,1)))
edited = table(cut(res$f,20), cut(res$p,20))
hist3D(z=edited,border="black", xlim=c(0,1),ylim=c(0,1), xlab="f", ylab="r", zlab='')

#acf(ts(res0$p),main="ACF for p (MH)")