#gibbs inbreeding
library(hexbin)
library(forecast)

gibbsinbreeding <- function(ndata=c(50,21,29), 
                       prior = c(1, 1, 1, 1), 
                       init = c(.2, .5), 
                       N = 1000) {
  
  # Represent data as individual observations.
  obs_data <- rep(c("AA", "Aa", "aa"), times = ndata)
  obs_data <- factor(obs_data, levels = c("AA", "Aa", "aa"))

  f <- p <- rep(0, N)
  f[1] <- init[1]
  p[1] <- init[2]
  for (i in 2:N) {
    # Calculate p(Z|f,p) for each case of gi:
    z_probs <- ifelse(obs_data == "AA", f[i-1] * p[i-1] / (f[i-1] * p[i-1] + (1 - f[i-1]) * p[i-1] ^ 2),
               ifelse(obs_data == "Aa", 0,
               ifelse(obs_data == "aa", f[i-1] * (1 - p[i-1]) / (f[i-1] * (1 - p[i-1]) + (1 - f[i-1]) * (1 - p[i-1]) ^ 2), NA))) 
    
    z <- runif(n = length(z_probs)) < z_probs
    z_counts <- table(z)
    
    f[i] <- rbeta(n = 1, shape1 = z_counts[2] + prior[1], shape2 = z_counts[1] + prior[2])
    
    nz_genos <- table(obs_data[z == FALSE])
    
    nz_A = 2 * nz_genos["AA"] + nz_genos["Aa"] 
    nz_a = 2 * nz_genos["aa"] + nz_genos["Aa"]
    
    z_genos <- table(obs_data[z == TRUE])
    z_A = z_genos["AA"]
    z_a = z_genos["aa"]
    
    p[i] <- rbeta(n = 1, shape1 = prior[3] + nz_A + z_A, shape2 = prior[4] + nz_a + z_a)
  }
  return(data.frame(f=f,p=p))
}

res=gibbsinbreeding()
edited = table(cut(res$f,20), cut(res$p,20))
#hist3D(z=edited,border="black", xlim=c(0,1),ylim=c(0,1), xlab="f", ylab="r", zlab='')


experiment = function(nobs=c(50,21,29),N=1000,title=''){
  res=gibbsinbreeding(nobs,N=N)
  plot(hexbin(res$f, res$p, xlab='f', ylab='r', xbnds=c(0,1),ybnds=c(0,1)),main=title)
}

#acf(ts(res$f),main='ACF for f')
#acf(ts(res$p),main="ACf for p (Gibbs)")
