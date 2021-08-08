ex=function(n=100, a=1, b=1, N=1000){
  p=rep(0,N)
  x=rep(0,N)
  p[1]=runif(1)
  x[1]=rbinom(1, n, p[1])
  for(i in 2:N){
    p[i]=rbeta(1, x[i-1]+a, n-x[i-1]+b)
    x[i]=rbinom(1, n, p[i])
  }
  return(data.frame(x=x, p=p))
}

exex=function(n=100, a=1, b=1, N=1000){
  out=ex(n, a, b, N)
  plot(hexbin(out))
}

res=ex()
edited = table(cut(res$x,20), cut(res$p,20))
#hist3D(z=edited, border="black", xlim=c(0,100),ylim=c(0,1), xlab="x", ylab="p", zlab='')


strip = res$x[res$p>.45 & res$p<.55]


x=seq(0,100,1)
p=seq(0,1,.01)
dense = function(n,x,p){
  return(choose(n,x)*p^(x-1)*(1-p)^(n-x-1))
}

z=dense(n=100, x, p)
help(hex)