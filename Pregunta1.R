mu = 95.3
sigma = 5.7
curve(dnorm(x,mean=mu, sd=sigma),
      xlim=c(80,120))

set.seed(123)
rnorm(4,mu,sigma)
pnorm(90,mu,sigma)
n=4
Y = function(i)(sum(rnorm(4,mu,sigma)))
Y10000 = sapply(1:100000,Y)
mean(Y10000)

#en teoriala media de la suma muestral de tamaño  n=4
4*mu

#varianza de la suma muestral
4*sigma^2

var(Y10000)

###
hist(Y10000, freq=FALSE)
curve(dnorm(x,mean = 4*mu,sd=2*sigma), add=TRUE)

#b
Y = function(i)(sum(rnorm(100,mu,sigma)))
Y10000 = sapply(1:100000,Y)
var(Y10000)

#em teoria
100*sigma^2
#c
1-pnorm(103,mu,sigma)


Y = function(i)(sum(rnorm(1,mu,sigma)))
Y10000 = sapply(1:100000,Y)
hist((Y10000))

mean(Y10000>103)
#d
pnorm(98,mu,sigma/sqrt(4))
xbar= function(i)(mean(rnorm(4,mu,sigma)))
xbar10000 = sapply(1:100000,xbar)
hist((xbar10000))
mean(xbar10000<98)

#e

Ssq = function(i)(var(rnorm(100,mu,sigma)))
Ssq10000 = sapply(1:100000,Ssq)
hist((Ssq10000))
mean(Ssq10000>32)

1-pchisq((100-1)*32/sigma^2, 100-1)

hist(Ssq10000*(100-1)/sigma^2,prob=TRUE)
curve(dchisq(x,100-1),add=TRUE,col="red")

(100-1)*32/sigma^2
