russell = read.csv("/Users/user/Documents/UIUC/2017Spring/FIN567/hw8/Russell.csv")
MSCI = read.csv("/Users/user/Documents/UIUC/2017Spring/FIN567/hw8/MSCI.csv")

data = cbind((log(russell$Adj.Close[2:3001])-log(russell$Adj.Close[1:3000])), (log(MSCI$Adj.Close[2:3001])-log(MSCI$Adj.Close[1:3000])))
colnames(data)=c("Russell", "MSCI")
ret1 = data[,1]
ret2 = data[,2]

##NGARCH
Initial_values = c(0.07,0.54,2.1,0.01,0.01)

#Objective function #
NGARCH_1 <- function(x) {  
	sigmasqhat = rep(0,length(ret1))
	sigmasqhat[1] = x[5]^2 
	z = rep(0,length(ret1))
	z[1] = ret1[1]/sqrt(sigmasqhat[1])

	for (i in 1:2999) {
		sigmasqhat[i+1] = (1-x[1]*(1+x[3]^2)-x[2])*x[4]^2+x[1]*sigmasqhat[i]*(z[i]-x[3])^2+x[2]*sigmasqhat[i]
		z[i+1] = ret1[i+1]/sqrt(sigmasqhat[i+1])
	}	

	#Likelihood 
	LH <- (1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret1^2/sigmasqhat)
	NeglogLH = -sum(log(LH))

	#Constraints
	if (x[1]+x[2]>=1 || x[1]>1 || x[1]<0 || x[2]>1 || x[2]<0 || x[3]>4 || x[3]< -4 || x[4]> 0.1 || x[4]< 0.001 || x[5]> 0.1 || x[5]< 0.001){
	NeglogLH = 0
	}

	return(NeglogLH)
}

NGARCH_2 <- function(x) {  
	sigmasqhat = rep(0,length(ret2))
	sigmasqhat[1] = x[5]^2 
	z = rep(0,length(ret2))
	z[1] = ret2[1]/sqrt(sigmasqhat[1])

	for (i in 1:2999) {
		sigmasqhat[i+1] = (1-x[1]*(1+x[3]^2)-x[2])*x[4]^2+x[1]*sigmasqhat[i]*(z[i]-x[3])^2+x[2]*sigmasqhat[i]
		z[i+1] = ret2[i+1]/sqrt(sigmasqhat[i+1])
	}	

	#Likelihood 
	LH <- (1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret2^2/sigmasqhat)
	NeglogLH = -sum(log(LH))

	#Constraints
	if (x[1]+x[2]>=1 || x[1]>1 || x[1]<0 || x[2]>1 || x[2]<0 || x[3]>4 || x[3]< -4 || x[4]> 0.1 || x[4]< 0.001 || x[5]> 0.1 || x[5]< 0.001){
	NeglogLH = 0
	}

	return(NeglogLH)
}

#Optimization
Russell_NGARCH=optim(Initial_values,NGARCH_1)
Russell_NGARCH$par
-Russell_NGARCH$value

MSCI_NGARCH=optim(Initial_values,NGARCH_1)
MSCI_NGARCH$par
-MSCI_NGARCH$value







