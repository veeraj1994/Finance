## Data Cleaning ##
mydata <- read.table("D:/data/hw6data.csv",
                 header = TRUE,sep = ",")

attach(mydata)

#Sample standard deviation
spstd = sqrt(sum(ret^2)/length(ret))


## Initial guess for paramter ##
Ini = c(0.1,0.8,0.01,0.01)

##Question 1 
##Q1A
#Objective function #
fr1 <- function(x) {  
sigmasqhat = rep(0,length(ret))
sigmasqhat[1] = x[4]^2 

for (i in 1:999) {
sigmasqhat[i+1] = (1-x[1]-x[2])*x[3]^2+x[1]*ret[i]^2+x[2]*sigmasqhat[i]
}

#Likelihood 
f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
NeglogLH = -sum(log(f))

#Constraints
if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001||  x[4]<0.001){
logLH = 9999
}

return(NeglogLH)
}

#Optimization
Q1A =optim(Ini,fr1)
Q1A

##Q1B
#Objective function #
fr2 <- function(x) {  
sigmasqhat = rep(0,length(ret))
sigmasqhat[1] = x[3]^2 

for (i in 1:999) {
sigmasqhat[i+1] = (1-x[1]-x[2])*spstd^2+x[1]*ret[i]^2+x[2]*sigmasqhat[i]
}

#Likelihood 
f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
NeglogLH = -sum(log(f))

#Constraints
if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 ||  x[3]<0.001){
logLH = 9999
}

return(NeglogLH)
}

# Optimization #
Q1B =optim(c(0.1,0.8,0.001),fr2)
Q1B


##Q1C
#Objective function #
fr3 <- function(x) {  
sigmasqhat = rep(0,length(ret))
sigmasqhat[1] = spstd^2 

for (i in 1:999) {
sigmasqhat[i+1] = (1-x[1]-x[2])*x[3]^2+x[1]*ret[i]^2+x[2]*sigmasqhat[i]
}

#Likelihood 
f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
NeglogLH = -sum(log(f))

#Constraints
if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 ||  x[3]<0.001){
logLH = 9999
}

return(NeglogLH)
}

# Optimization #
Q1C =optim(c(0.1,0.8,0.001),fr3)
Q1C

##Q1D
#Objective function #
fr4 <- function(x) {  
sigmasqhat = rep(0,length(ret))
sigmasqhat[1] = spstd^2 

for (i in 1:999) {
sigmasqhat[i+1] = (1-x[1]-x[2])*spstd^2+x[1]*ret[i]^2+x[2]*sigmasqhat[i]
}

#Likelihood 
f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
NeglogLH = -sum(log(f))

#Constraints
if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 ){
logLH = 9999
}

return(NeglogLH)
}

# Optimization #
Q1D =optim(c(0.1,0.8),fr4)
Q1D

##Q1E
#Q1A prediction
sigmasqhatA = rep(0,length(ret)+1)
sigmasqhatA[1] = Q1A$par[4]^2 

for (i in 1:1000) {
sigmasqhatA[i+1]=(1-Q1A$par[1]-Q1A$par[2])*Q1A$par[3]^2+Q1A$par[1]*ret[i]^2+Q1A$par[2]*sigmasqhatA[i]
}

sqrt(sigmasqhatA[1001])

#Q1B prediction
sigmasqhatB = rep(0,length(ret)+1)
sigmasqhatB[1] = Q1B$par[3]^2 

for (i in 1:1000) {
sigmasqhatB[i+1]=(1-Q1B$par[1]-Q1B$par[2])*spstd^2+Q1B$par[1]*ret[i]^2+Q1B$par[2]*sigmasqhatB[i]
}

sqrt(sigmasqhat[1001])

#Q1C prediction
sigmasqhatC = rep(0,length(ret)+1)
sigmasqhatC[1] = spstd^2

for (i in 1:1000) {
sigmasqhatC[i+1]=(1-Q1C$par[1]-Q1C$par[2])*Q1C$par[3]^2+Q1C$par[1]*ret[i]^2+Q1C$par[2]*sigmasqhatC[i]
}

sqrt(sigmasqhatC[1001])

#Q1D prediction
sigmasqhatD = rep(0,length(ret)+1)
sigmasqhatD[1] = spstd^2

for (i in 1:1000) {
sigmasqhatD[i+1]=(1-Q1D$par[1]-Q1D$par[2])*spstd^2+Q1D$par[1]*ret[i]^2+Q1D$par[2]*sigmasqhatD[i]
}

sqrt(sigmasqhatD[1001])


##Question 2
#A
sigmasqhatA[1000]
ret[1000]^2

devsigA <- rep(0,21)
devsigA[1]<-Q1A$par[1]*(ret[1000]^2-Q1A$par[3]^2)+ Q1A$par[2]*(sigmasqhatA[1000]-Q1A$par[3]^2)

for (i in 1:20) {
devsigA[i+1]= (Q1A$par[1]+Q1A$par[2])*devsigA[i]
}

EsigA = devsigA + Q1A$par[3]^2
sum(EsigA)
sqrt(sum(EsigA))*sqrt(252/21)

#B
sigmasqhatB[1000]
ret[1000]^2

devsigB <- rep(0,21)
devsigB[1]<-Q1B$par[1]*(ret[1000]^2-spstd^2)+ Q1B$par[2]*(sigmasqhatB[1000]-spstd^2)

for (i in 1:20) {
devsigB[i+1]= (Q1B$par[1]+Q1B$par[2])*devsigB[i]
}

EsigB = devsigB + spstd^2
sum(EsigB)
sqrt(sum(EsigB))*sqrt(252/21)

#C
sigmasqhatC[1000]
ret[1000]^2

devsigC <- rep(0,21)
devsigC[1]<-Q1C$par[1]*(ret[1000]^2-Q1C$par[3]^2)+ Q1C$par[2]*(sigmasqhatC[1000]-Q1C$par[3]^2)

for (i in 1:20) {
devsigC[i+1]= (Q1C$par[1]+Q1C$par[2])*devsigC[i]
}

EsigC = devsigC + Q1C$par[3]^2
sum(EsigC)
sqrt(sum(EsigC))*sqrt(252/21)


#D
sigmasqhatD[1000]
ret[1000]^2

devsigD <- rep(0,21)
devsigD[1]<-Q1D$par[1]*(ret[1000]^2-spstd^2)+ Q1D$par[2]*(sigmasqhatD[1000]-spstd^2)

for (i in 1:20) {
devsigD[i+1]= (Q1D$par[1]+Q1D$par[2])*devsigD[i]
}

EsigD = devsigD + spstd^2
sum(EsigD)
sqrt(sum(EsigD))*sqrt(252/21)



##Question 3 
Ini3 = c(0.07,0.54,2.1,0.01,0.01)

#Objective function #
frQ3 <- function(x) {  
sigmasqhat = rep(0,length(ret))
sigmasqhat[1] = x[5]^2 
z = rep(0,length(ret))
z[1] = ret[1]/sqrt(sigmasqhat[1])

for (i in 1:999) {
sigmasqhat[i+1] = (1-x[1]*(1+x[3]^2)-x[2])*x[4]^2+x[1]*sigmasqhat[i]*(z[i]
-x[3])^2+x[2]*sigmasqhat[i]
z[i+1] = ret[i+1]/sqrt(sigmasqhat[i+1])
}

#Likelihood 
f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
NeglogLH = -sum(log(f))

#Constraints
if (x[1]+x[2]>=1 || x[1]>1 || x[1]<0 || x[2]>1 || x[2]<0 || x[3]>4 || x[3]< -4 || x[4]> 0.1 || x[4]< 0.001 || x[5]> 0.1 || x[5]< 0.001){
logLH = 9999
}

return(NeglogLH)
}

#Optimization
Q3=optim(Ini3,frQ3)
Q3$par
-Q3$value











