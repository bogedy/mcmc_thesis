#This function returns the function for our unormalized Beta PDF.
betakern=function(x, alpha, beta){
  return(x^(alpha-1)*(1-x)^(beta-1))
}

mh = function(alpha, beta, niter, init=NA){
  theta=rep(0, niter)
  #By default, the chain is initialized from a uniform distribution.
  if(is.na(init)){
    theta[1] <- runif(1)
  }
  #You can also specify an inital value.
  else{
    theta[1]=init
  }
  for (i in 2:niter){
    #Propose new value.
    theta.p <- runif(1)
    #Calculate ratio.
    r <- betakern(theta.p, alpha, beta)/betakern(theta[i-1], alpha, beta)
    #Sample from Bernoulli(acceptance probability).
    flip <- rbinom(1,1,min(r,1))
    theta[i] <- if(flip==1) theta.p else theta[i-1]
  }
  return(theta)
}

plotbeta=function(alpha, beta){
  x=seq(0,1,.01)
  y=dbeta(x,alpha,beta)
  plot(x,y,type='l',xlab='',ylab='')
}

plotmixture = function(pi, a,b,c,d){
  x=seq(0,1,.01)
  y=pi*dbeta(x,a,b)+(1-pi)*dbeta(x,c,d)
  plot(x,y, type='l')
}

normalkern = function(x, mu, sigma){
  
}

#??????
mhbeta = function(alpha, beta, niter){
  theta=rep(0, niter)
  theta[1] <- runif(1, -1, 1)
  for (i in 2:niter){
    theta.p <- runif(1, -1, 1)
    r <- betakern(theta.p, alpha, beta)/betakern(theta[i-1], alpha, beta)
    flip <- rbinom(1,1,min(r,1))
    theta[i] <- if(flip==1) theta.p else theta[i-1]
  }
  return(theta)
}

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


betaexperiment=function(alpha, beta, niter){
  reshi=hist(mh(alpha,beta, niter), breaks=50, main='')
  plot(reshi, freq=F, xlab='',ylab='', xlim=c(0,1), main='')
  x=seq(0,1, .01)
  y=dbeta(x,2,6)
  lines(x,y)
}

betaexperiment4=function(alpha, beta, niter){
  par(mfrow=c(2,2))
  reshi=hist(mh(alpha,beta, niter), breaks=50)
  plot(reshi, main=paste("Histogram of Metropolis chain,", niter, "iterations"), freq=F, xlab='', xlim=c(0,1))
  x=seq(0,1, .01)
  y=dbeta(x,2,6)
  lines(x,y)
}


traceExpBeta=function(title=''){
  res1=mh(2,6,100, init=1)
  res2=mh(2,6,100, init=.5)
  res3=mh(2,6,100, init=0)
  plot(1:100,res1,type='l',xlab = 'theta',ylab='', main=title)
  lines(1:100,res2,lty=2)
  lines(1:100,res3,lty=3)
  legend('topright',legend=c('1','.5','0'),lty=c(1,2,3))
}