# Plot a few examples of LOCO analyses
library('conformalInference')

##### First the local type #####
# data initialization
set.seed(12)
n = 1000;
p = 4;
x = matrix(runif(n*p, -1, 1), n, p)
f1 = function(u){sin(u*pi)}
f3 = function(u){cos(u*pi)}

mu = rowSums(cbind(f1(x[,1]), f3(x[,3])))
noise <- 0.5 # UP TO CHANGES
y = mu + rnorm(n,0,noise)

# setup train and pred
library('gam')
train.fun = function(x,y){
  p = ncol(x)
  datafr = data.frame(x)
  str = paste("y ~", paste(paste0("s(X", 1:p , ",df=", 3, ")"), collapse = "+"))
  return(gam(formula(str), data = datafr))
}

predict.fun = function(out,x0){
  return(predict(out, data.frame(x0)))
}

out.loco = loco.roo(x, y, alpha = 0.1, vars = 0, train.fun = train.fun, predict.fun = predict.fun, verb=TRUE)

cvals = colMeans(out.loco$lo[,,1] <= 0)
cvals

# For each variable in consideration, plot the intervals
for (j in 1:p) {
  pdf(file=sprintf("fig/loco.local.largenoise%i.pdf",j),w=w,h=h)
  par(mar=mar)
  plot(x[,j],x[,j],ylim=range(c(out.loco$lo[,j,1],out.loco$up[,j,1])),
       xlab=xlab,ylab=ylab,main=paste("Component",j),col=NA)
  cols = ifelse(out.loco$lo[,j,1] <= 0, 1, 3)
  segments(x[,j],out.loco$lo[,j,1],x[,j],out.loco$up[,j,1],col=cols)
  abline(h=0, lty=2, lwd=2, col=2)
  graphics.off()
  cat(sprintf("fig/loco.am.comp%i.pdf\n",j))
}
cat("\n")

##### Second the local type #####
# data initialization
set.seed(12)
n = 1000;
p = 4;
rho <- 0.1
sigma <- matrix(c(1, rho, 0, 0,
                  rho, 1, 0, 0,
                  0, 0, 1, 0,
                  0, 0, 0, 1), nrow = 4, ncol = 4, byrow = TRUE)
library('mvtnorm')
x = rmvnorm(n, mean = c(0,0,0,0), sigma = sigma)
# f1 = function(u){sin(u*pi)}
# f3 = function(u){cos(u*pi)}
f1 = function(u){100*(u^2)}
f3 = function(u){50*abs(u)}

# f1 = function(u){2*abs(u)}
# f3 = function(u){abs(u)}

mu = rowSums(cbind(f1(x[,1]), f3(x[,3])))
y = mu + rnorm(n,0,0.1)

# setup train and pred
library('gam')
train.fun = function(x,y){
  p = ncol(x)
  datafr = data.frame(x)
  str = paste("y ~", paste(paste0("s(X", 1:p , ",df=", 3, ")"), collapse = "+"))
  return(gam(formula(str), data = datafr))
}

predict.fun = function(out,x0){
  return(predict(out, data.frame(x0)))
}

out.loco = loco.roo(x, y, alpha = 0.1, vars = 0, train.fun = train.fun, predict.fun = predict.fun, verb=TRUE)

cvals = colMeans(out.loco$lo[,,1] <= 0)
cvals

# For each variable in consideration, plot the intervals
for (j in 1:p) {
  pdf(file=sprintf("fig/loco.local.gauss.smallcor%i.pdf",j),w=w,h=h)
  par(mar=mar)
  plot(x[,j],x[,j],ylim=range(c(out.loco$lo[,j,1],out.loco$up[,j,1])),
       xlab=xlab,ylab=ylab,main=paste("Component",j),col=NA)
  cols = ifelse(out.loco$lo[,j,1] <= 0, 1, 3)
  segments(x[,j],out.loco$lo[,j,1],x[,j],out.loco$up[,j,1],col=cols)
  abline(h=0, lty=2, lwd=2, col=2)
  graphics.off()
  cat(sprintf("fig/loco.am.comp%i.pdf\n",j))
}
cat("\n")