'''#GROUP Name = SSVJ
#Team Members = GADDA Veeraj, Netid: gadda2
#YOON Joonha, Netid: joonhay2
#HOU Sunjie, Netid: shou10
#HONG Sungwook, Netid: hong85
'''

setwd("/Users/user/Documents/UIUC/2017Spring/FIN567/hw5")
data=read.csv("HW5.csv",header=T)

data2 = cbind(log(data$GOOG),log(data$GOOGL))
colnames(data2) = c("logGOOG","logGOOGL")

#Q1a
mysample = data.frame(data2[1:693,])
library("tsDyn")
vecm = VECM(mysample, lag = 1, beta = -1, estim = "ML")
vecm
#Q1b
Intercepts = matrix(0,2,1)
ECTs = matrix(0,2,1)
lagCoe = matrix(0,2,2)
cointe = matrix(0,1,2)
cointe[1,1] = 1
cointe[1,2] = -1
Intercepts[1,1] = vecm$coefficients[1,2]
Intercepts[2,1] = vecm$coefficients[2,2]
ECTs[1,1] = vecm$coefficients[1,1]
ECTs[2,1] = vecm$coefficients[2,1]
lagCoe[1,1] =  vecm$coefficients[1,3]
lagCoe[1,2] =  vecm$coefficients[1,4]
lagCoe[2,1] =  vecm$coefficients[2,3]
lagCoe[2,2] =  vecm$coefficients[2,4]

pred_returns = array(0,dim = c(33,2))
for (i in 1:33){
	yt = matrix(0,2,1)
	yt[1,1] = data2[693+i,1]
	yt[2,1] = data2[693+i,2]
	dyt= matrix(0,2,1)
	dyt[1,1] = data2[693+i,1]-data2[692+i,1]
	dyt[2,1] = data2[693+i,2]-data2[692+i,2]

	pred_returns[i,] = Intercepts+ECTs%*%cointe%*%yt+lagCoe%*%dyt

}

print(c(pred_returns[1,2], pred_returns[1,1]))
#Q1c
#A:
print(pred_returns[,2])
#C:
print(pred_returns[,1])

#Q2
compare = pred_returns[,2]-pred_returns[,1] >= 0
Ht = array(0,dim = length(compare))
for (i in 1:length(compare)){
	if (compare[i] == TRUE){
		Ht[i] = 1
	}
	else{
		Ht[i] = -1
	}
}
data3 = data[694:726,2:3]
actual_returns = data2[695:726,]-data2[694:725,]
PnL = array(0,dim = 32)
for (i in 1:32){
	PnL[i] = Ht[i]*(data3[i,2]*(exp(actual_returns[i,2])-1)-data3[i,1]*(exp(actual_returns[i,1])-1))
}
mean(PnL)
sd(PnL)

#Q3
vecm2 = VECM(mysample, lag = 4, beta = -1, estim = "ML")
vecm2
#Q3b
Intercepts = matrix(0,2,1)
ECTs = matrix(0,2,1)
lagCoe = matrix(0,2,8)
cointe = matrix(0,1,2)
cointe[1,1] = 1
cointe[1,2] = -1
Intercepts[1,1] = vecm2$coefficients[1,2]
Intercepts[2,1] = vecm2$coefficients[2,2]
ECTs[1,1] = vecm2$coefficients[1,1]
ECTs[2,1] = vecm2$coefficients[2,1]

lagCoe[1,1] =  vecm2$coefficients[1,3]
lagCoe[1,2] =  vecm2$coefficients[1,4]
lagCoe[1,3] =  vecm2$coefficients[1,5]
lagCoe[1,4] =  vecm2$coefficients[1,6]
lagCoe[1,5] =  vecm2$coefficients[1,7]
lagCoe[1,6] =  vecm2$coefficients[1,8]
lagCoe[1,7] =  vecm2$coefficients[1,9]
lagCoe[1,8] =  vecm2$coefficients[1,10]


lagCoe[2,1] =  vecm2$coefficients[2,3]
lagCoe[2,2] =  vecm2$coefficients[2,4]
lagCoe[2,3] =  vecm2$coefficients[2,5]
lagCoe[2,4] =  vecm2$coefficients[2,6]
lagCoe[2,5] =  vecm2$coefficients[2,7]
lagCoe[2,6] =  vecm2$coefficients[2,8]
lagCoe[2,7] =  vecm2$coefficients[2,9]
lagCoe[2,8] =  vecm2$coefficients[2,10]


pred_returns2 = array(0,dim = c(33,2))
for (i in 1:33){
	yt = matrix(0,2,1)
	yt[1,1] = data2[693+i,1]
	yt[2,1] = data2[693+i,2]
	dyt= matrix(0,8,1)
	dyt[1,1] = data2[693+i,1]-data2[692+i,1]
	dyt[2,1] = data2[693+i,2]-data2[692+i,2]
	dyt[3,1] = data2[692+i,1]-data2[691+i,1]
	dyt[4,1] = data2[692+i,2]-data2[691+i,2]
	dyt[5,1] = data2[691+i,1]-data2[690+i,1]
	dyt[6,1] = data2[691+i,2]-data2[690+i,2]
	dyt[7,1] = data2[690+i,1]-data2[689+i,1]
	dyt[8,1] = data2[690+i,2]-data2[689+i,2]


	pred_returns2[i,] = Intercepts+ECTs%*%cointe%*%yt+lagCoe%*%dyt

}

print(c(pred_returns2[1,2], pred_returns2[1,1]))
#Q3c
#A:
print(pred_returns2[,2])
#C:
print(pred_returns2[,1])

#Q4
vecm3 = VECM(mysample, lag = 1, beta = NULL , estim = "ML")
vecm3
summary(vecm3)