
'''#GROUP Name = SSVJ
#Team Members: 
#GADDA Veeraj, Netid: gadda2
#YOON Joonha, Netid: joonhay2
#HOU Sunjie, Netid: shou10
#HONG Sungwook, Netid: hong85
'''


#Question 1:
#Part a:
library(quantmod)

#get the data
getSymbols("^GSPC")
sp <- GSPC[,6]
sp <- as.data.frame(sp)
colnames(sp) <- "Prices"
rm(GSPC)
sp <- na.omit(sp)

#compute log returns
sp$temp <- log(sp$Prices)
sp$logreturns[2:nrow(sp)]<- diff(sp$temp, lag=1)
sp<- sp[-2]

#Initial guesses
start=2552-999
end=2552

sp_subset<- (sp$logreturns[start:end])^2
sp_subset<- as.data.frame(sp_subset)
sample_variance<- mean(sp_subset[,1])
first_variance<- mean(sp_subset$sp_subset[1:10])

#MLE Function
log_likelihood <- function(theta,x){
  sigma_square=vector(mode="numeric",length=1000)
  sigma_square[1]=theta[4]^2
  for(i in 2:1000){
    sigma_square[i]=(1-theta[1]-theta[2])*theta[3]^2+theta[1]*x[i-1]+theta[2]*sigma_square[i-1]
  }
  logl<-sum(log(1/sqrt(2*pi*sigma_square))+(-0.5)*x/sigma_square)
  #logl <- sum(log((1/(sqrt(2*pi*sigma_square)))*exp(-0.5*x^2/sigma_square)))
  return(-logl)
}
initial_value=c(0.1,0.8,0.01,0.01)
mle=optim(initial_value,log_likelihood,x=sp_subset[,1])
mle$par


#part b:
log_likelihood_1 <- function(theta,x){
  sigma_square_1=vector(mode="numeric",length=1000)
  sigma_square_1[1]=theta[3]^2
  for(i in 2:1000){
    sigma_square_1[i]=(1-theta[1]-theta[2])*sample_variance+theta[1]*x[i-1]+theta[2]*sigma_square_1[i-1]
  }
  #logl<-(-0.5)*(sum(log(sigma_square_1)+x/sigma_square_1))
  logl<-sum(log(1/sqrt(2*pi*sigma_square_1))+(-0.5)*x/sigma_square_1)
  return(-logl)
}

initial_value_1=c(0.18,0.7,sqrt(first_variance))
mle_1<- optim(initial_value_1,log_likelihood_1, x=sp_subset[,1])
mle_1$par
print(sqrt(sample_variance))


#part c:
log_likelihood_2 <- function(theta,x){
  sigma_square_2=vector(mode="numeric",length=1000)
  sigma_square_2[1]=sample_variance
  for(i in 2:1000){
    sigma_square_2[i]=(1-theta[1]-theta[2])*theta[3]^2+theta[1]*x[i-1]+theta[2]*sigma_square_2[i-1]
  }
  #logl<-(-0.5)*(sum(log(sigma_square_2)+x/sigma_square_2))
  logl<-sum(log(1/sqrt(2*pi*sigma_square_2))+(-0.5)*x/sigma_square_2)
  return(-logl)
}
initial_value_2=c(0.18,0.7,sqrt(sample_variance))
mle_2=optim(initial_value_2,log_likelihood_2,x=sp_subset[,1])
mle_2$par

#part d:
log_likelihood_3 <- function(theta,x){
  sigma_square_3=vector(mode="numeric",length=1000)
  sigma_square_3[1]=sample_variance
  for(i in 2:1000){
    sigma_square_3[i]=(1-theta[1]-theta[2])*sample_variance+theta[1]*x[i-1]+theta[2]*sigma_square_3[i-1]
  }
  #logl<-(-0.5)*(sum(log(sigma_square_3)+x/sigma_square_3))
  logl<-sum(log(1/sqrt(2*pi*sigma_square_3))+(-0.5)*x/sigma_square_3)
  return(-logl)
}
initial_value_3=c(0.18,0.7)
mle_3=optim(initial_value_3,log_likelihood_3,x=sp_subset[,1])
mle_3$par

#part e:
m<- as.data.frame(rbind(c(mle$par[1:2], mle$par[3]^2,mle$par[4]^2), c(mle_1$par[1:2],sample_variance,mle_1$par[3]^2), c(mle_2$par[1:2],mle_2$par[3]^2,first_variance), c(mle_3$par, sample_variance,first_variance)))
colnames(m)<- c("alpha", "beta", "long_run_var","first_variance")
rownames(m)<- c("part a", "part b", "part c", "part d")
m$var_garch <- NA
sigma_sq = vector(mode="numeric",length=1000)

for (j in 1:4)
{
  sigma_sq[1]=m$first_variance[j]
  
  for (i in 2:1000)
  {
    sigma_sq[i]= (1-m$alpha[j]-m$beta[j])*m$long_run_var[j]+m$alpha[j]*sp_subset[i-1,1]+m$beta[j]*sigma_sq[i-1]
  }
  m$var_garch[j]= (1-m$alpha[j]-m$beta[j])*m$long_run_var[j]+m$alpha[j]*sp_subset[1000,1]+m$beta[j]*sigma_sq[1000]
}

sqrt(m$var_garch)

#Question 2:
#part a:
var_forecast<- rep(0,4)
for (j in 1:4)
{
  for (i in 1:21)
  {
    var_forecast[j]= (m$alpha[j]+m$beta[j])^(i-1)*(m$var_garch[j]-m$long_run_var[j])
  }
 var_forecast[j]= var_forecast[j] + 21*m$long_run_var[j]
}

#part b:
annualized_st.dev <- sqrt(var_forecast*252/21)
forecasts<- cbind(var_forecast,annualized_st.dev)
forecasts

#Question 3:
#part a:
log_likelihood_4<-function(theta,x){
  sigma_square_4=vector(mode="numeric",length=1000)
  sigma_square_4[1]=theta[4]^2
  for(i in 2:1000){
    sigma_square_4[i]=(1-theta[1]*(1+theta[5]^2)-theta[2])*theta[3]^2+theta[1]*(sqrt(x[i-1])-theta[5]*sqrt(sigma_square_4[i-1]))^2+theta[2]*sigma_square_4[i-1]
  }
  #logl<-(-0.5)*sum(log(sigma_square_4)+x/sigma_square_4)
  logl<-sum(log(1/sqrt(2*pi*sigma_square_4))+(-0.5)*x/sigma_square_4)
  return(-logl)
}
initial_value_4=c(mle$par[1],mle$par[2],mle$par[3],mle$par[4],0)
mle_4=optim(initial_value_4,log_likelihood_4,x=sp_subset[,1])
mle_4$par

#part b:
mle$value
mle_4$value

LR = 2*log(mle_4$value/mle$value)
LR

qchisq(0.05, df=1) 
qchisq(0.01, df=1)

