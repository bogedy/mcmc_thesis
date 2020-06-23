#normal mixture

normmxf = function(x, pi, mu1, mu2, sigma1, sigma2){
  return(pi*dnorm(x,mu1, sigma1)+(1-pi)*dnorm(x,mu2,sigma2))
}


mhnormal = function(pi, mu1, mu2, sigma1, sigma2, niter, Psigma){
  theta=rep(0, niter)
  theta[1] <- 0
  for (i in 2:niter){
    theta.p <- theta[i-1]+rnorm(1, 0, Psigma)
    r <- normmxf(theta.p, pi, mu1, mu2, sigma1, sigma2)*pnorm(theta[i-1]) / normmxf(theta[i-1], pi, mu1, mu2, sigma1, sigma2)*pnorm(theta.p-theta[i-1])
    flip <- rbinom(1,1,min(r,1))
    theta[i] <- if(flip==1) theta.p else theta[i-1]
  }
  return(theta)
}

plotdensity = function(pi, mu1, mu2, sigma1, sigma2){
  x=seq(mu1-3*sigma1,mu2+3*sigma2,.1)
  y=normmxf(x, pi, mu1, mu2, sigma1, sigma2)
  plot(x,y,type='l', ylab='Density')
}