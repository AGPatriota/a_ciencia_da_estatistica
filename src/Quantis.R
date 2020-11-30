#Quantis

f = function(x, prob){
  x = sort(x)
  pn = prob*(length(x)+1)
  if(pn < 1) return(min(x))
  if(pn > length(x)) return(max(x))
  if(pn%%1==0) return(x[pn])
  if(pn%%1!=0){
    p1 = floor(pn)
    p2 = ceiling(pn)
    quantil = x[p1]*(p2-pn) + x[p2]*(pn-p1)
    return(quantil)
  }
}

#Genarando números aleatórios
x = rnorm(100)

f1 = function(prob) f(x, prob)
t1 = sapply(seq(0,1,0.01),f1)

#Algoritmo tip6 6 do R
t2 = as.numeric(quantile(x, prob=seq(0,1,0.01), type=6))

all.equal(t1,t2)

par(mfrow=c(1,3))
plot(t1~seq(0,1,0.01), xlab= "p", ylab="Quantil p", main = "Função implementada")
plot(t2~seq(0,1,0.01), xlab= "p", ylab="Quantil p", main = "Função do R")
plot(t1,t2, type="l")
