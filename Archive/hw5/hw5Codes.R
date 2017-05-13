## Data Cleaning ##
mydata <- read.table("D:/data/hw5data.csv",
                 header = TRUE,sep = ",")
GooG = mydata[,2]
GooGL = mydata[,3]

lnSCt = log(GooG)
lnSAt = log(GooGL)

lnSCtL1<-c(NA,lnSCt[1:length(lnSCt)-1])
lnSAtL1<-c(NA,lnSAt[1:length(lnSAt)-1])

RCt = lnSCt - lnSCtL1
RAt = lnSAt - lnSAtL1

RCtL1 <-c(NA,RCt[1:length(RCt)-1])
RAtL1 <-c(NA,RAt[1:length(RAt)-1])

RCtL2 <-c(NA,RCtL1[1:length(RCtL1)-1])
RAtL2 <-c(NA,RAtL1[1:length(RAtL1)-1])

RCtL3 <-c(NA,RCtL2[1:length(RCtL2)-1])
RAtL3 <-c(NA,RAtL2[1:length(RAtL2)-1])

RCtL4 <-c(NA,RCtL3[1:length(RCtL3)-1])
RAtL4 <-c(NA,RAtL3[1:length(RAtL3)-1])

difCAL1 <- lnSCtL1 - lnSAtL1  

## Question 1 ## 
#(a)#
Q1data <-data.frame(RCt[3:693],RAt[3:693],difCAL1[3:693],RCtL1[3:693],RAtL1[3:693])
colnames(Q1data) <- c('y1','y2', 'x1','x2','x3')

#(b)#
Q1.reg1 = lm(y1~x1+ x2+ x3, data = Q1data) 
newData1 <- data.frame(difCAL1[695:726],RCtL1[695:726],RAtL1[695:726])
colnames(newData1) <- c('x1','x2','x3')
FRCt<-predict(Q1.reg1,newData1)

#(c)#
Q1.reg2 = lm(y2~x1+ x2+ x3, data = Q1data) 
FRAt<-predict(Q1.reg2,newData1)


## Question 2 ##
H = ifelse(FRCt-FRAt>0,-1,1)
PnL = H*(GooGL[694:725]*(exp(RAt[695:726])-1)-GooG[694:725]*(exp(RCt[695:726])-1))

mean(PnL)
sd(PnL)

## Question 3 ##
Q3data <-data.frame(RCt[7:693],RAt[7:693],difCAL1[7:693],RCtL1[7:693],RAtL1[7:693],RCtL2[7:693],RAtL2[7:693],RCtL3[7:693],RAtL3[7:693],RCtL4[7:693],RAtL4[7:693])
colnames(Q3data) <- c('y1','y2', 'x1','x2','x3','x4','x5','x6','x7','x8','x9')

Q3.reg1 = lm(y1~x1+ x2+ x3+x4+x5+x6+x7+x8+x9, data = Q3data) 
newData2 <- data.frame(difCAL1[695:726],RCtL1[695:726],RAtL1[695:726],RCtL2[695:726],RAtL2[695:726],RCtL3[695:726],RAtL3[695:726],RCtL4[695:726],RAtL4[695:726])
colnames(newData2) <- c('x1','x2','x3','x4','x5','x6','x7','x8','x9')
FRCtQ3<-predict(Q3.reg1,newData2)

Q3.reg2 = lm(y2~x1+ x2+ x3+x4+x5+x6+x7+x8+x9, data = Q3data) 
FRAtQ3<-predict(Q3.reg2,newData2)

HQ3 = ifelse(FRCtQ3-FRAtQ3>0,-1,1)
PnLQ3 = HQ3*(GooGL[694:725]*(exp(RAt[695:726])-1)-GooG[694:725]*(exp(RCt[695:726])-1))

mean(PnLQ3)
sd(PnLQ3)

cor(FRCt,FRCtQ3)
cor(FRAt,FRAtQ3)

## Question 4 ##
Q4data <- data.frame(RCt[3:693],RAt[3:693],lnSCtL1[3:693],lnSAtL1[3:693], RCtL1[3:693],RAtL1[3:693])
colnames(Q4data) <- c('y1','y2', 'x1','x2','x3','x4')
Q4.reg1 = lm(y1~x1+ x2+ x3+x4, data = Q4data) 
Q4.reg2 = lm(y2~x1+ x2+ x3+x4, data = Q4data)

Q4.reg1 
Q4.reg2

