
'''#GROUP Name = SSVJ
#Team Members: 
#GADDA Veeraj, Netid: gadda2
#YOON Joonha, Netid: joonhay2
#HOU Sunjie, Netid: shou10
#HONG Sungwook, Netid: hong85
'''
setwd("/Users/user/Documents/UIUC/2017Spring/FIN567/hw7")

data_GE =read.csv("GE.csv", header=FALSE)
data_GOOG = read.csv("GOOG.csv", header=FALSE)


close_GOOG = data_GOOG[1:391,8]

for(i in 1:9){
  close_price = data_GOOG[(((391*i)+1):(391*(i+1))), 8]
  close_GOOG = cbind(close_GOOG, close_price)
}


close_GE = data_GE[1:391,8]

for(i in 1:9){
  close_price = data_GE[(((391*i)+1):(391*(i+1))), 8]
  close_GE = cbind(close_GE, close_price)
}


returns_GE =array(0, dim=c(390, 10))
returns_GOOG =array(0, dim=c(390, 10))

for ( i in 1:10){
returns_GOOG[,i] = log(close_GOOG[2:391, i]) - log(close_GOOG[1:390,i])
returns_GE[,i]  = log(close_GE[2:391, i]) - log(close_GE[1:390,i])
}

#data.frame(close_GOOG)

#Q 1
RV_open_GOOG = array(0, dim = c(10,1))
RV_open_GE = array(0, dim = c(10,1))


for ( i in 1:10){
 RV_open_GOOG[i] = sum(returns_GOOG[,i]^2) 
}
RV_open_GOOG
mean(RV_open_GOOG)



#Q_2
#(a)
prices_2_mins_GOOG <- array(0, dim=c(196,10))
prices_5_mins_GOOG <- array(0, dim=c(79,10))
prices_10_mins_GOOG <- array(0, dim=c(40,10))
prices_15_mins_GOOG <- array(0, dim=c(27,10))

returns_2_mins_GOOG <- array(0, dim=c(195,10))
returns_5_mins_GOOG <- array(0, dim=c(78,10))
returns_10_mins_GOOG <- array(0, dim=c(39,10))
returns_15_mins_GOOG <- array(0, dim=c(26,10))

RV_open_2_mins_GOOG = array(0, dim = c(10,1))
RV_open_5_mins_GOOG = array(0, dim = c(10,1))
RV_open_10_mins_GOOG = array(0, dim = c(10,1))
RV_open_15_mins_GOOG = array(0, dim = c(10,1))

RV_means_GOOG = array(0, dim=c(1,5))

for(j in 1:10){
  for(i in 1:196) {
    prices_2_mins_GOOG[i,j] = close_GOOG[(i-1)*2+1, j]
  }
  for(i in 1:79) {
    prices_5_mins_GOOG[i,j] = close_GOOG[(i-1)*5+1, j]
  }
  for(i in 1:40) {
    prices_10_mins_GOOG[i,j] = close_GOOG[(i-1)*10+1, j]
  }
  for(i in 1:27) {
    prices_15_mins_GOOG[i,j] = close_GOOG[(i-1)*15+1, j]
  }
}

for(i in 1:10){

returns_2_mins_GOOG[,i] = log(prices_2_mins_GOOG[2:196,i]) - log(prices_2_mins_GOOG[1:195,i])
returns_5_mins_GOOG[,i] = log(prices_5_mins_GOOG[2:79,i]) - log(prices_5_mins_GOOG[1:78,i])
returns_10_mins_GOOG[,i] = log(prices_10_mins_GOOG[2:40,i]) - log(prices_10_mins_GOOG[1:39,i])
returns_15_mins_GOOG[,i] = log(prices_15_mins_GOOG[2:27,i]) - log(prices_15_mins_GOOG[1:26,i])


RV_open_2_mins_GOOG[i] = sum(returns_2_mins_GOOG[,i]^2)
RV_open_5_mins_GOOG[i] = sum(returns_5_mins_GOOG[,i]^2)
RV_open_10_mins_GOOG[i] = sum(returns_10_mins_GOOG[,i]^2)
RV_open_15_mins_GOOG[i] = sum(returns_15_mins_GOOG[,i]^2)

}


RV_open_1_min_GOOG <- data.frame(RV_open_GOOG)
RV_open_2_mins_GOOG <- data.frame(RV_open_2_mins_GOOG)
RV_open_5_mins_GOOG <- data.frame(RV_open_5_mins_GOOG)
RV_open_10_mins_GOOG <- data.frame(RV_open_10_mins_GOOG)
RV_open_15_mins_GOOG <- data.frame(RV_open_15_mins_GOOG)

RV_open_GOOG = cbind(RV_open_1_min_GOOG,RV_open_2_mins_GOOG, RV_open_5_mins_GOOG, RV_open_10_mins_GOOG, RV_open_15_mins_GOOG)
colnames(RV_open_GOOG) <- c("1min", "2mins", "5mins", "10mins", "15mins")

for(i in 1:5){
RV_means_GOOG[1,i] = mean(RV_open_GOOG[,i])
}
colnames(RV_means_GOOG) <- c("1min", "2mins", "5mins", "10mins", "15mins")

RV_open_GOOG <- rbind(RV_open_GOOG,RV_means_GOOG )

View(RV_open_GOOG)

#(b)
#if price starts from 8:31, then 
prices_15_mins_GOOG_b <- array(0, dim=c(26,10))
returns_15_mins_GOOG_b <- array(0, dim=c(25,10))

for(j in 1:10){
  for(i in 1:26) {
    prices_15_mins_GOOG_b[i,j] = close_GOOG[(i-1)*15+2, j]
  }
}

for(i in 1:10){
returns_15_mins_GOOG_b[,i] = log(prices_15_mins_GOOG_b[2:26,i]) - log(prices_15_mins_GOOG_b[1:25,i])
}


RV_open_15_mins_GOOG_b = array(0, dim = c(10,1))
for(i in 1:10){
  RV_open_15_mins_GOOG_b[i] = sum(returns_15_mins_GOOG_b[,i]^2)
}
RV_open_15_mins_GOOG_b <- data.frame(RV_open_15_mins_GOOG_b)

mean((RV_open_15_mins_GOOG[,1]))
mean(RV_open_15_mins_GOOG_b[,1])

# scalefactor = multiply 1+(time_interval - 1) * (data_points/(datapoints-1))
scalefactor = array(0, dim = c(5))
scalefactor[1] = 1
scalefactor[2] = (1+(2-1)*(196/195))/2
scalefactor[3] = (1+(5-1)*(79/78))/5
scalefactor[4] = (1+(10-1)*(40/39))/10
scalefactor[5] = (1+(15-1)*(26/25))/15


RV_open_1_min_GOOG_scaled = scalefactor[1]*RV_open_1_min_GOOG
RV_open_2_mins_GOOG_scaled = scalefactor[2]*RV_open_2_mins_GOOG
RV_open_5_mins_GOOG_scaled = scalefactor[3]*RV_open_5_mins_GOOG
RV_open_10_mins_GOOG_scaled = scalefactor[4]*RV_open_10_mins_GOOG
RV_open_15_mins_GOOG_scaled = scalefactor[5]*RV_open_15_mins_GOOG
RV_means_GOOG_scaled = array(0, dim=c(1,5))

RV_open_GOOG_scaled = cbind(RV_open_1_min_GOOG_scaled,RV_open_2_mins_GOOG_scaled, RV_open_5_mins_GOOG_scaled, RV_open_10_mins_GOOG_scaled, RV_open_15_mins_GOOG_scaled)
colnames(RV_open_GOOG_scaled) <- c("1min", "2mins", "5mins", "10mins", "15mins")

for(i in 1:5){
RV_means_GOOG_scaled[1,i] = mean(RV_open_GOOG_scaled[,i])
}
colnames(RV_means_GOOG_scaled) <- c("1min", "2mins", "5mins", "10mins", "15mins")
RV_open_GOOG_scaled <- rbind(RV_open_GOOG_scaled,RV_means_GOOG_scaled )

#(c)
#as time interval increases, average RV generally decreases.  
RV_means_GOOG_scaled[5]/RV_means_GOOG_scaled[1]
RV_means_GOOG_scaled[5]/RV_means_GOOG_scaled[4]

#Q3
#(a)
for ( i in 1:10){
 RV_open_GE[i] = sum(returns_GE[,i]^2) 
}
RV_open_GE
mean(RV_open_GE)

prices_2_mins_GE <- array(0, dim=c(196,10))
prices_5_mins_GE <- array(0, dim=c(79,10))
prices_10_mins_GE <- array(0, dim=c(40,10))
prices_15_mins_GE <- array(0, dim=c(27,10))

returns_2_mins_GE <- array(0, dim=c(195,10))
returns_5_mins_GE <- array(0, dim=c(78,10))
returns_10_mins_GE <- array(0, dim=c(39,10))
returns_15_mins_GE <- array(0, dim=c(26,10))

RV_open_1_min_GE = array(0, dim=c(10,1))
RV_open_2_mins_GE = array(0, dim = c(10,1))
RV_open_5_mins_GE = array(0, dim = c(10,1))
RV_open_10_mins_GE = array(0, dim = c(10,1))
RV_open_15_mins_GE = array(0, dim = c(10,1))

RV_means_GE = array(0, dim=c(1,5))

for(j in 1:10){
  for(i in 1:196) {
    prices_2_mins_GE[i,j] = close_GE[(i-1)*2+1, j]
  }
  for(i in 1:79) {
    prices_5_mins_GE[i,j] = close_GE[(i-1)*5+1, j]
  }
  for(i in 1:40) {
    prices_10_mins_GE[i,j] = close_GE[(i-1)*10+1, j]
  }
  for(i in 1:27) {
    prices_15_mins_GE[i,j] = close_GE[(i-1)*15+1, j]
  }
}

for(i in 1:10){
  
  returns_2_mins_GE[,i] = log(prices_2_mins_GE[2:196,i]) - log(prices_2_mins_GE[1:195,i])
  returns_5_mins_GE[,i] = log(prices_5_mins_GE[2:79,i]) - log(prices_5_mins_GE[1:78,i])
  returns_10_mins_GE[,i] = log(prices_10_mins_GE[2:40,i]) - log(prices_10_mins_GE[1:39,i])
  returns_15_mins_GE[,i] = log(prices_15_mins_GE[2:27,i]) - log(prices_15_mins_GE[1:26,i])
  
  
  RV_open_2_mins_GE[i] = scalefactor[2]*sum(returns_2_mins_GE[,i]^2)
  RV_open_5_mins_GE[i] = scalefactor[3]*sum(returns_5_mins_GE[,i]^2)
  RV_open_10_mins_GE[i] = scalefactor[4]*sum(returns_10_mins_GE[,i]^2)
  RV_open_15_mins_GE[i] = scalefactor[5]*sum(returns_15_mins_GE[,i]^2)
  
}


RV_open_1_min_GE <- data.frame(RV_open_GE)
RV_open_2_mins_GE <- data.frame(RV_open_2_mins_GE)
RV_open_5_mins_GE <- data.frame(RV_open_5_mins_GE)
RV_open_10_mins_GE <- data.frame(RV_open_10_mins_GE)
RV_open_15_mins_GE <- data.frame(RV_open_15_mins_GE)

RV_open_GE = cbind(RV_open_1_min_GE,RV_open_2_mins_GE, RV_open_5_mins_GE, RV_open_10_mins_GE, RV_open_15_mins_GE)
colnames(RV_open_GE) <- c("1min", "2mins", "5mins", "10mins", "15mins")

for(i in 1:5){
  RV_means_GE[1,i] = mean(RV_open_GE[,i])
}
colnames(RV_means_GE) <- c("1min", "2mins", "5mins", "10mins", "15mins")

RV_open_GE <- rbind(RV_open_GE,RV_means_GE )

View(RV_open_GE)

#(b)
#as time interval increases, again, average RV generally decreases. 
RV_open_GE[11,5]/RV_open_GE[11,1]
RV_open_GE[11,5]/RV_open_GE[11,4]

#(c)
#More dependent

#Q4
CPrice = as.matrix(log(close_GOOG[391,]))
CPrice = rbind(log(763.64),CPrice)
R_sqaured = (CPrice[2:11]-CPrice[1:10])^2

slides_57 = (sum(R_sqaured)/(RV_means_GOOG_scaled[5]*10))*RV_open_GOOG_scaled[,"15mins"]


daily_close_prices = array(0, dim=c(1,10))
daily_close_prices[1] = 763.64

daily_open_prices = array(0, dim=c(1,10))
RV_GOOG_24h_approach_2 = array(0, dim=c(1,10))


open_GOOG = data_GOOG[1:391,8]

for(i in 1:9){
  open_price = data_GOOG[(((391*i)+1):(391*(i+1))), 5]
  open_GOOG = cbind(open_GOOG, open_price)
}

for(i in 1:10){
  daily_open_prices[i] = open_GOOG[1,i]
}

for( i in 2:10){
  daily_close_prices[i] = close_GOOG[391,(i-1)]
}

for(i in 1:10){
  RV_GOOG_24h_approach_2[1,i] = RV_open_15_mins_GOOG_scaled[i,1] + (log((daily_open_prices[i]/daily_close_prices[i])))^2
}

RV_GOOG_24h_approach_2






