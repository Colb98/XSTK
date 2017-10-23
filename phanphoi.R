#Bai 1
sieuboi = function(N,M,n,x){
  choose(M,x)*choose(N-M,n-x)/choose(N,n)
}
plot(0:15,sieuboi(100,25,15,0:15),type="h")

############################
#bai 2
sieuboi = function(N,M,n,x){
  choose(M,x)*choose(N-M,n-x)/choose(N,n)
}

sum(sieuboi(100,25,15,5:12))

phanphoisb = function(N,M,n,x) {
  sum(sieuboi(N,M,n,0:x))
}

phanphoisb(100,25,15,12)-phanphoisb(100,25,15,5)

############################
#Bai 3
#a
curve(dexp(x,0.6),0,10)

#b
curve(dexp(x,0.6),0,10,add=T)
curve(dexp(x,0.3),0,10,add=T)

#c

pexp(10,0.6)
pexp(10,0.6)

############################
#Bai 4
plot(0:8,dpois(0:8,1),'l')


#Bai 5
curve(dchisq(x,3),0,10)

############################
#Bai 6
layout(matrix(c(2,1),nrow=2))
plot(stepfun(0:50,c(0,dbinom(0:50,50,0.08))),ylim=c(0,0.25),main="B(50,0.08)")
plot(stepfun(0:50,c(0,dpois(0:50,4))),ylim=c(0,0.25),main="P(4)")

############################
#bai 7
layout(matrix(1,nrow=1))
plot(dbinom(0:50,50,0.4),type='l')

curve(dnorm(x,20,sqrt(12)),0,50,add=T)