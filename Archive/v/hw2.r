'''
@author: SSVJ
'''

data =read.csv("~/Downloads/2- HW__DATA.csv")
data = data[1:6]
# Calculate DAX and FTSE values in USD
data2 = cbind(data[,3]*data[,6], data[,4]*data[,5])
colnames(data2) = c("DAXin$","FTSEin$")
data = cbind(data, data2)
# Calculate daily returns
data2 = (data[1:2723,c(2,7,8)]/data[2:2724,c(2,7,8)])-1
colnames(data2) = c("sp500return","DAXreturn","FTSEreturn")
data = data[1:2723,]
data = cbind(data, data2)
remove(data2)

profit = (data[,9]+data[,10]+data[,11])*1000000
data = cbind(data,profit)
index = sequence(2723)
data = cbind(index, data)

result = data.frame(data[1:245,2])
colnames(result) = "Date"

#Q1
Q1 = vector('numeric')
for (i in 1:245){#all 2016 trading days
	temp = sort(data[(i+1):(i+2000),'profit'],decreasing = TRUE)#sort the profit data of previous 2000 days
	# 99% of 2000 trading days is 2000*0.99 = 1980.
	# Hence 99% VaR is the the 1980th highest profit among all 2000 days
	Q1 = c(Q1, -temp[1980])
}
result = cbind(result, Q1)

#Q2
Q2 = vector('numeric')
for (i in 1:245){
	#varsp500 = var(data[(i+1):(i+2000),'sp500return'])
	varsp500 = sum(data[(i+1):(i+2000),'sp500return']*data[(i+1):(i+2000),'sp500return'])/2000
	vardax = sum(data[(i+1):(i+2000),'DAXreturn']*data[(i+1):(i+2000),'DAXreturn'])/2000
	varftse = sum(data[(i+1):(i+2000),'FTSEreturn']*data[(i+1):(i+2000),'FTSEreturn'])/2000
	covspdax = sum(data[(i+1):(i+2000),'sp500return']*data[(i+1):(i+2000),'DAXreturn'])/2000
	covspaftse = sum(data[(i+1):(i+2000),'sp500return']*data[(i+1):(i+2000),'FTSEreturn'])/2000
	covdaxftse = sum(data[(i+1):(i+2000),'DAXreturn']*data[(i+1):(i+2000),'FTSEreturn'])/2000
	stdev = sqrt((varsp500+vardax+varftse+2*(covspdax+covspaftse+covdaxftse))*1000000^2)

	temp = qnorm(0.01)*stdev
	Q2 = c(Q2, -temp)
}
result = cbind(result, Q2)

#Q3
Q3 = vector('numeric')
for (i in 1:245){
	varsp500 = (1-0.94)*sum(0.94^(data[1:2000,'index'])*data[(i+1):(i+2000),'sp500return']*data[(i+1):(i+2000),'sp500return'])
	vardax = (1-0.94)*sum(0.94^(data[1:2000,'index'])*data[(i+1):(i+2000),'DAXreturn']*data[(i+1):(i+2000),'DAXreturn'])
	varftse = (1-0.94)*sum(0.94^(data[1:2000,'index'])*data[(i+1):(i+2000),'FTSEreturn']*data[(i+1):(i+2000),'FTSEreturn'])
	covspdax = (1-0.94)*sum(0.94^(data[1:2000,'index'])*data[(i+1):(i+2000),'sp500return']*data[(i+1):(i+2000),'DAXreturn'])
	covspaftse = (1-0.94)*sum(0.94^(data[1:2000,'index'])*data[(i+1):(i+2000),'sp500return']*data[(i+1):(i+2000),'FTSEreturn'])
	covdaxftse = (1-0.94)*sum(0.94^(data[1:2000,'index'])*data[(i+1):(i+2000),'DAXreturn']*data[(i+1):(i+2000),'FTSEreturn'])

	stdev = sqrt((varsp500+vardax+varftse+2*(covspdax+covspaftse+covdaxftse))*1000000^2)

	temp = qnorm(0.01)*stdev
	Q3 = c(Q3, -temp)
}
result = cbind(result, Q3)

#Q4
Q4 = vector('numeric')
varsp500 = vector('numeric')
vardax = vector('numeric')
varftse = vector('numeric')
for (i in 1:2500){
	varsp500 = c(varsp500,(1-0.94)*sum(0.94^(data[2:201,'index'])*data[(i+1):(i+200),'sp500return']*data[(i+1):(i+200),'sp500return']))
	vardax = c(vardax,(1-0.94)*sum(0.94^(data[2:201,'index'])*data[(i+1):(i+200),'DAXreturn']*data[(i+1):(i+200),'DAXreturn']))
	varftse = c(varftse,(1-0.94)*sum(0.94^(data[2:201,'index'])*data[(i+1):(i+200),'FTSEreturn']*data[(i+1):(i+200),'FTSEreturn']))
}
denominator = cbind(varsp500,vardax,varftse)
for (i in 1:245){
	varsp500 = (1-0.94)*sum(0.94^(data[2:2001,'index'])*data[(i+1):(i+2000),'sp500return']*data[(i+1):(i+2000),'sp500return'])
	musp500 = (sqrt(varsp500)/sqrt(denominator[(i+1):(i+2000),1]))*data[(i+1):(i+2000),'sp500return']
	vardax = (1-0.94)*sum(0.94^(data[2:2001,'index'])*data[(i+1):(i+2000),'DAXreturn']*data[(i+1):(i+2000),'DAXreturn'])
	mudax = (sqrt(vardax)/sqrt(denominator[(i+1):(i+2000),2]))*data[(i+1):(i+2000),'DAXreturn']
	varftse = (1-0.94)*sum(0.94^(data[2:2001,'index'])*data[(i+1):(i+2000),'FTSEreturn']*data[(i+1):(i+2000),'FTSEreturn'])
	muftse = (sqrt(varftse)/sqrt(denominator[(i+1):(i+2000),3]))*data[(i+1):(i+2000),'FTSEreturn']

	loss = (musp500+mudax+muftse)*1000000
	temp = sort(loss,decreasing = TRUE)[1980]
	Q4 = c(Q4,-temp)
}
result = cbind(result, Q4)

write.csv(result, file = "result.csv")










