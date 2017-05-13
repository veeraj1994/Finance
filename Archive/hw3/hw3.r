data =read.csv("~/Documents/UIUC/2017Spring/FIN567/hw3/hw3.csv")
#data = data[rev(rownames(data)), ]
#rownames(data) <- NULL
#data2 = log(data[1:4053,c(2,3,4,5)])-log(data[2:4054,c(2,3,4,5)])
data2 = log(data[4054:2,c(2,3,4,5)])-log(data[4053:1,c(2,3,4,5)])
data2 = data2[rev(rownames(data2)), ]
colnames(data2) = c("SPXreturn","DJXreturn","VIXreturn","VXDreturn")
data = cbind(data[2:4054,],data2)

#Q1
covariance = array(0,dim=c(4,4,4054))
for (i in 1:4053){
	for(j in 1:4){
		for (k in 1:4){
			covariance[j,k,i+1]=0.06*data2[i,j]*data2[i,k]+0.94*covariance[j,k,i]
		}

	}
}

cov_matrix = covariance[,,4054]

annual_cov_matrix = cov_matrix*252

std_dev = array(0,dim=4)
day_std_dev = array(0,dim=4)
for (n in 1:4){
	std_dev[n]=sqrt(cov_matrix[n,n])*sqrt(252)
    day_std_dev[n]=sqrt(cov_matrix[n,n])
}

print("Question 1, Part (a)")
print("The covariance matrix is: ")
print(cov_matrix)

print("Part(b)")
print("The annualized standard deviations are: ")
print(std_dev)


#Q2

BS_Call_div<- function(S, K, r, q, sigma, T){
  x <- c(1)
  d1<-(log(S/K)+(r-q+0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2<-(log(S/K)+(r-q-0.5*sigma^2)*T)/(sigma*sqrt(T))
  
  x <- S*exp(-q*T)*pnorm(d1)-exp(-r*T)*K*pnorm(d2)
  
  x
  
}

BS_Put_div<- function(S, K, r, q, sigma, T){
  x <- c(1)
  d1<-(log(S/K)+(r-q+0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2<-(log(S/K)+(r-q-0.5*sigma^2)*T)/(sigma*sqrt(T))
  
  x <- -S*exp(-q*T)*pnorm(-d1) + exp(-r*T)*K*pnorm(-d2)
  
  x
  
}

IV_Call_SPX <- optimize(f=function(x){abs(BS_Call_div(data[4053,2], 1865, 0.0025, 0.0222, x, 35/365)-(49.5+50.1)/2)}, interval =c(0,1) )[1]
IV_Put_SPX <- optimize(f=function(x){abs(BS_Put_div(data[4053,2], 1865, 0.0025, 0.0222, x, 35/365)-(55.5+56.1)/2)}, interval =c(0,1) )[1]
IV_Call_DJX <- optimize(f=function(x){abs(BS_Call_div(data[4053,3], 160, 0.0025, 0.0248, x, 35/365)-(4.05+3.85)/2)}, interval =c(0,1) )[1]
IV_Put_DJX <- optimize(f=function(x){abs(BS_Put_div(data[4053,3], 160, 0.0025, 0.0248, x, 35/365)-(4.9+4.7)/2)}, interval =c(0,1) )[1]

print("Question 2")
print("The implied volatility for the four options are:")
print(c(IV_Call_SPX$minimum,IV_Put_SPX$minimum,IV_Call_DJX$minimum,IV_Put_DJX$minimum))



#Q3
library("MASS")

I = 10000
r = mvrnorm(I,rep(0,4),cov_matrix)
a = cbind(r[,1],r[,2],r[,3],r[,3],r[,4],r[,4])
current = as.numeric(c(data[4053, 2],data[4053,3], IV_Call_SPX,IV_Put_SPX,IV_Call_DJX,IV_Put_DJX))
new_index = aperm(array(current, dim = c(6,I)))
new_index = new_index*exp(a)

new_value = array(0, I)

for (i in 1:I){
	val_1 = -5000 *BS_Call_div(new_index[i,1], 1865, 0.0025, 0.0222, new_index[i,3], 35/365)
	val_2 = -5000 *BS_Put_div(new_index[i,1], 1865, 0.0025, 0.0222, new_index[i,4], 35/365)
	val_3 = 55000*BS_Call_div(new_index[i,2], 160, 0.0025, 0.0248, new_index[i,5], 35/365)
	val_4 = 55000*BS_Put_div(new_index[i,2], 160, 0.0025, 0.0248, new_index[i,6], 35/365)

	current_val = val_1 + val_2 + val_3 + val_4
	new_value[i] = current_val

}

val_1 = -5000 *BS_Call_div(current[1], 1865, 0.0025, 0.0222, current[3], 35/365)
val_2 = -5000 *BS_Put_div(current[1], 1865, 0.0025, 0.0222, current[4], 35/365)
val_3 = 55000*BS_Call_div(current[2], 160, 0.0025, 0.0248, current[5], 35/365)
val_4 = 55000*BS_Put_div(current[2], 160, 0.0025, 0.0248, current[6], 35/365)
current_port = val_1 + val_2 + val_3 + val_4

PnL = new_value - current_port
VaR = quantile(PnL, 0.05)
print("Question 3")
print("The 5% Value at Risk of the portfolio is:")
print(as.numeric(VaR))

    
#Q4


print("Question 4")
print("The expected shortfall is:")
print(mean(PnL[PnL<=VaR]))

