#https://stats.stackexchange.com/questions/64293/understanding-metropolis-hastings-with-asymmetric-proposal-distribution

Metropolis <- function(F_sample # R function for distribution we want to sample from
                       , y0 # initializing value
                       , F_prop  # proposal distribution
                       , niter=1e5   # iterations
){
  y = rep(0,niter)
  y[1] = y0    # starting location for random walk
  accepted = rep(0,niter-1)
  
  for(i in 2:niter)    {
    # Sample from proposal distribution.
    y.p <- F_prop(y[i-1])

    R=F_sample(y.p)/F_sample(y[i-1])
    
    a <- rbinom(1,1,min(R, 1))
    if(a){
      y[i] = y.p
      accepted[i-1] = 1
    }    
    else{
      y[i] = y[i-1]
    }    
  }
  return(list(y, accepted))
}

sample_dist = function(x){.5*dnorm(x,-5,1)+.5*dnorm(x,7,3)}
y0 = -1
prop_dist = function(x){rnorm(1, x, sqrt(0.5) )}
result = Metropolis(sample_dist, y0, prop_dist)

test = function(x){.5*dnorm(x,-5,1)+.5*dnorm(x,7,3)}
plot(seq(-4,8,.1),test(seq(-4,8,.1)),type='l', main = 'Normal Mixture \n N(-1,1) N(5,1) pi=.5', ylab='Density',xlab='')
response1 <- Metropolis(F_sample = test
                        ,F_prop = function(x){rnorm(1, x, sqrt(0.5) )}
                        , -1
)
response2 <- Metropolis(F_sample = test
                        ,F_prop = function(x){rnorm(1, x, sqrt(0.5) )}
                        , -1
)
response3 <- Metropolis(F_sample = test
                        ,F_prop = function(x){rnorm(1, x, sqrt(0.5) )}
                        , -1
)
response4 <- Metropolis(F_sample = test
                        ,F_prop = function(x){rnorm(1, x, sqrt(0.5) )}
                        , -1
)
x=seq(-10,15,.2)
y=test(x)
par(mfrow=c(2,2))
hist(response1[[1]], breaks=50, main='', freq=F, xlab='', ylab='', ylim=c(0,.25), yaxt='n')
lines(x,y)
hist(response2[[1]], breaks=50, main='', freq=F, xlab='', ylab='', ylim=c(0,.25), yaxt='n')
lines(x,y)
hist(response3[[1]], breaks=50, main='', freq=F, xlab='', ylab='', ylim=c(0,.25), yaxt='n')
lines(x,y)
hist(response4[[1]], breaks=50, main='', freq=F, xlab='', ylab='', ylim=c(0,.25), yaxt='n')
lines(x,y)
par(mfrow=c(1,1))