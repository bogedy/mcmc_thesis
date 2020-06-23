normmxf = function(x, pi, mu1, mu2, sigma1, sigma2){
  return(pi*dnorm(x,mu1, sigma1)+(1-pi)*dnorm(x,mu2,sigma2))
}

plotmixture = function(pi, a,b,c,d){
  x=seq(0,1,.01)
  y=pi*dbeta(x,a,b)+(1-pi)*dbeta(x,c,d)
  plot(x,y, type='l')
}

mhnormal = function(pi, mu1, mu2, sigma1, sigma2, niter, Psigma, init=0){
  theta=rep(0, niter)
  theta[1] <- init
  for (i in 2:niter){
    theta.p <- rnorm(1, theta[i-1], Psigma)
    r <- normmxf(theta.p, pi, mu1, mu2, sigma1, sigma2) / normmxf(theta[i-1], pi, mu1, mu2, sigma1, sigma2)
    flip <- rbinom(1,1,min(r,1))
    if(flip==1){
      theta[i]=theta.p
    }
    else{theta[i]=theta[i-1]}
  }
  return(theta)
}

mhnormalunif = function(pi, mu1, mu2, sigma1, sigma2, niter, Psigma){
  theta=rep(0, niter)
  theta[1] <- 0
  for (i in 2:niter){
    theta.p <- runif(1,-1,7)
    r <- normmxf(theta.p, pi, mu1, mu2, sigma1, sigma2) / normmxf(theta[i-1], pi, mu1, mu2, sigma1, sigma2)
    flip <- rbinom(1,1,min(r,1))
    theta[i] <- if(flip==1) theta.p else theta[i-1]
  }
  return(theta)
}


traceExp=function(niter=1000,psigma=.5, xaxis='Iteration'){
  res1=mhnormal(.5,-5,7,1,3,niter,psigma,init=-4)
  res2=mhnormal(.5,-5,7,1,3,niter,psigma,init=0)
  res3=mhnormal(.5,-5,7,1,3,niter,psigma,init=10)
  plot(1:niter,res1,type='l',xlab = xaxis,ylab='', ylim=c(-7,13))
  lines(1:niter,res2,lty=2)
  lines(1:niter,res3,lty=3)
}