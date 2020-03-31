#number of heads
CoinPr <- function(coin,p,trial){
  Simlist <- replicate(coin,sample(c(0,1), trial, replace=TRUE), prob=c(1-p,p))
  totalHeads <- rowSums(Simlist)
  PrHead <- numeric(coin+1)
  PrHead <- c(mean(totalHeads==0), mean(totalHeads==1), mean(totalHeads==2), mean(totalHeads==3))
  ExpectedHead <- mean(totalHeads)
  return(c("Pr"=PrHead, "Mean"=ExpectedHead))
}


#ace of cards
trials<-10000
s<-52
t<-4
p<-t/s
ace<-function(p){
  c<-0
  k<-0
  while(k==0){
    simlist=sample(c(1,2), 1, prob=c(1-p,p))
    if (simlist==1)
      c<-c+1
    else
      k<-1
  }
  return(c)
}
output<-replicate(trials,ace(p))
mean(output)
var(output)

#bus arrival
Bus <- function(t){
  k<- 0
  for (x in 100){
    simlist<-rexp(1,1/30)
    if(simlist<= t) k<-k+1
  }
  return(k)
}
output<- replicate(10000,Bus(20))
pro <- pexp(20,1/30)
mean(output)
mean(pro)


#Gamblers Ruin
trials<-10000
n<-100
k<-50
p<-0.5
Gamble<-function(n,p,k){
  stake<-k
  while(stake>0 & stake<n){
    simlist<-sample(c(-1,1), 1, prob=c(p,1-p))
    stake<-stake+simlist
  }
  if(stake==0) return(1) else return(0)
}
output<-replicate(trials, Gamble(n,p,k))
mean(output)


#pineapple pizza
trials<-10000
p<-0.25
q<-0.25
Pizza<-function(p,q){
  k<-0
  d<-0
  while(k==0){
    simlist<-sample(c(1,2,3),1,prob=c(p,q,1-p-q))
    if(simlist==1)
      d<-d+1
    if(simlist==2)
      k<-1
  }
  return(d)
}
output<-replicate(trials,pizza(p,q))
mean(output)


#Ellen's Insurance
Insurance<-function(ded){
  simlist<-rexp(1,1/500)
  if(simlist<100)
    simlist<-0
  else
    simlist<-simlist-ded
  }
  return(simlist)
}
output<-replicate(10000,Insurance(100))
mean(output)
var(output)