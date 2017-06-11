#GROUP Name = SSVJ
#Team Members = GADDA Veeraj, Netid: gadda2
#YOON Joonha, Netid: joonhay2
#HOU Sunjie, Netid: shou10
#HONG Sungwook, Netid: hong85

#part1
setwd("/Users/user/Documents/UIUC/2017Spring/FIN567/hw4")
SP=read.csv("F567C.s2017.HW4.data.csv",header=T)
Return<-SP$Return
Losses<--Return
mef<-matrix(0,21,1)
n<-matrix(0,21,1)
u<-matrix(0,21,1)
for (i in 1:21){
  u[i,1]<-0.01+(i-1)*0.002
  for (j in 1:7564){
    if (Losses[j]-u[i,1]>=0){
      n[i,1]<-n[i,1]+1
      mef[i,1]<-mef[i,1]+(Losses[j]-u[i,1])
    }
  }
  mef[i,1]<-mef[i,1]/n[i,1]
}
plot(u,mef, type='l')


# Part 2
nblosses<-0
for (j in 1:7564){
  if (Losses[j]-0.022>=0){
    nblosses<-nblosses+1
  }
}
print(nblosses)

#Part 3
Losses2<-matrix(0,221,1)
nblosses<-0
for (j in 1:7564){
  if (Losses[j]-0.022>=0){
    nblosses<-nblosses+1
    Losses2[nblosses,1]<-Losses[j]-0.022
  }
}
initialvalue<-c(1,1)
GPD<-function(Losses2,theta){(1+theta[2]*Losses2/theta[1])^(-1-1/theta[2])/theta[1]}
result=optim(initialvalue,fn=function(theta){-sum(log(GPD(Losses2,theta)))})
beta<-result$par[1]
zeta<-result$par[2]

v <-c(zeta,beta)
v

#Part 4
a=seq(0.022, 0.1,0.0001)
ConDF = function(points,theta){1/theta[2]*(1+theta[1]*(points-0.022)/theta[2])^(-1/theta[1]-1)}
plot(a,ConDF(a,v))

# Part 5
n<-matrix(0,40,1)
u<-matrix(0,40,1)
Prob<-matrix(0,40,1)
for (i in 1:40){
  u[i,1]<-0.02+(i)*0.002
  for (j in 1:7564){
    if (Losses[j]-u[i,1]>=0){
      n[i,1]<-n[i,1]+1
    }
  }
  Prob[i,1]<-n[i,1]/7564
}

plot(u,Prob,type='l')
# prob >= 0.022
Prob[1,1]
#prob >= 0.05
Prob[15,1]
#prob >=0.1
Prob[40,1]
