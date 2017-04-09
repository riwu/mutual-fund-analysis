 sample_size = c(5,10,20,30,50,100,200)
 for(i in 1:7){
  ns =100
  n = sample_size[i]
  d = 3
  T1 = numeric(ns)
  T2 = numeric(ns)
  T3 = numeric(ns)
  T4 = numeric(ns)
  set.seed(1234)
  for(i in 1:ns){
  x = rchisq(n,d)
  T1[i] = sqrt(sd(x)^2)
  T2[i] = 1.4826*mad(x)
  T3[i] = IQR(x)/(1.34898)
gmd = function(x){
tmp = 0
for (i in 1:n) {
for (j in i:n) {
tmp <- tmp + abs(x[i]-x[j])
}}
retval <- 0.5*sqrt(pi)*tmp/(n*(n-1)/2)
}
T4[i]=gmd(x)
}
simumean = apply(cbind(T1,T2,T3,T4),2,mean)
simubias = simumean - rep(mu,4)
simusd = apply(cbind(T1,T2,T3,T4),2,sd)
simumse = simubias^2 + simusd^2
simuIQR = apply(cbind(T1,T2,T3,T4),2,IQR)
ests = c("T1","T2","T3","T4")
names = c("True value","sample size","no. of simu","simumean","simubias","simusd","simumse","simuIQR")
sumdat = rbind(c(sqrt(2*d),sqrt(2*d),sqrt(2*d),sqrt(2*d)),n,ns,simumean,simubias,simusd,simumse,simuIQR)
dimnames(sumdat) = list(names,ests)
print(round(sumdat,4))
}