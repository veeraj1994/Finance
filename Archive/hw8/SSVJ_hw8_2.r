library(fImport)

date_from <- "08/05/2009"
date_to <- "08/05/2014"
Ticker = c("EFA", "IWM")


for(i in 1:length(Ticker)){
  ETF.data <- yahooSeries(Ticker[i], from = date_from, to= date_to)
  if(i == 1) {
    ETFs.data <- as.numeric(ETF.data[,4])
  } else {
    ETFs.data <- cbind(ETFs.data, as.numeric(ETF.data[,4]))
  }
}

returns = (log(ETFs.data[1:length(ETFs.data[,1])-1, ]) - log(ETFs.data[2:length(ETFs.data[,1]),]))


std <- array(0, dim = c(2))
std[1] = sqrt(sum(returns[,1]^2)/length(returns[,1]))
std[2] = sqrt(sum(returns[,2]^2)/length(returns[,2]))

ret1 <- returns[,1]
ret2 <- returns[,2]


Ini = c(0.1,0.8,0.01,0.01)


fr1 <- function(x) {  
  sigmasqhat = rep(0,length(ret1))
  sigmasqhat[1] = x[4]^2 
  
  for (i in 1:(length(ret1)-1)) {
    sigmasqhat[i+1] = (1-x[1]-x[2])*x[3]^2+x[1]*ret1[i]^2+x[2]*sigmasqhat[i]
  }
  
  #Likelihood 
  f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret1^2/sigmasqhat)
  NeglogLH = -sum(log(f))
  
  #Constraints
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001||  x[4]<0.001){
    NeglogLH = 0
  }
  
  return(NeglogLH)
}


MSCI_GARCH =optim(Ini,fr1)

fr2 <- function(x) {  
  sigmasqhat = rep(0,length(ret2))
  sigmasqhat[1] = x[4]^2 
  
  for (i in 1:(length(ret2)-1)) {
    sigmasqhat[i+1] = (1-x[1]-x[2])*x[3]^2+x[1]*ret2[i]^2+x[2]*sigmasqhat[i]
  }
  
  #Likelihood 
  f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret2^2/sigmasqhat)
  NeglogLH = -sum(log(f))
  
  #Constraints
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001||  x[4]<0.001){
    NeglogLH = 0
  }
  
  return(NeglogLH)
}

Russell_GARCH = optim(Ini,fr2)

##NGARCH
Initial_value_1 = c(0.091822845, 0.738250207,0.01, 0.008315190, 0.009792746)
Initial_value_2 = c(0.089862460, 0.636519008,0.01, 0.009322215, 0.010851281)


#Objective function #
NGARCH_1 <- function(x) {  
  sigmasqhat = rep(0,length(ret1))
  sigmasqhat[1] = x[5]^2 
  z = rep(0,length(ret1))
  z[1] = ret1[1]/sqrt(sigmasqhat[1])
  
  for (i in 1:(length(ret1)-1)) {
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
  
  for (i in 1:(length(ret2)-1)) {
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
MSCI_NGARCH=optim(Initial_value_1,NGARCH_1)
MSCI_NGARCH$par
-MSCI_NGARCH$value

Russell_NGARCH=optim(Initial_value_2,NGARCH_2)
Russell_NGARCH$par
-Russell_NGARCH$value

past_sigma_MSCI = array(0,length(ret1))
past_sigma_MSCI[1] = MSCI_NGARCH$par[5]^2
past_sigma_Russell = array(0,length(ret2))
past_sigma_Russell[1] = Russell_NGARCH$par[5]^2


past_z_MSCI = rep(0,length(ret1))
past_z_MSCI[1] = ret1[1]/sqrt(past_sigma_MSCI[1])

for( i in 1:(length(ret1)-1)){
  past_sigma_MSCI[i+1] = (1-MSCI_NGARCH$par[1]*(1+MSCI_NGARCH$par[3]^2)-MSCI_NGARCH$par[2])*MSCI_NGARCH$par[4]^2+MSCI_NGARCH$par[1]*past_sigma_MSCI[i]*(past_z_MSCI[i]-MSCI_NGARCH$par[3])^2+MSCI_NGARCH$par[2]*past_sigma_MSCI[i]
  past_z_MSCI[i+1] = ret1[i+1]/sqrt(past_sigma_MSCI[i+1])
}

past_z_Russell = rep(0,length(ret2))
past_z_Russell[1] = ret2[1]/sqrt(past_sigma_Russell[1])

for( i in 1:(length(ret2)-1)){
  past_sigma_Russell[i+1] = (1-Russell_NGARCH$par[1]*(1+Russell_NGARCH$par[3]^2)-Russell_NGARCH$par[2])*Russell_NGARCH$par[4]^2+Russell_NGARCH$par[1]*past_sigma_Russell[i]*(past_z_Russell[i]-Russell_NGARCH$par[3])^2+Russell_NGARCH$par[2]*past_sigma_Russell[i]
  past_z_Russell[i+1] = ret2[i+1]/sqrt(past_sigma_Russell[i+1])
}

#DCC

###########
production = past_z_MSCI*past_z_Russell
long_run_rho12 = mean(production)
long_run_rho11 = 1
long_run_rho22 = 1

DCC = function(theta){
  rho12 <- c()
  q12 <- c()
  q11 <- c()
  q22 <- c()
  q11[1] = 1
  q22[1] = 1
  q12[1] = long_run_rho12
  rho12[1] = q12[1]/sqrt(abs(q11[1]*q22[1]))
  for (i in 2:1258){
    q12[i] = long_run_rho12 + theta[1]*(production[i-1]-long_run_rho12) + theta[2]*(q12[i-1]-long_run_rho12)
    q11[i] = long_run_rho11 + theta[1]*(past_z_MSCI[i-1]^2-long_run_rho11) + theta[2]*(q11[i-1]-long_run_rho11)
    q22[i] = long_run_rho22 + theta[1]*(past_z_Russell[i-1]^2-long_run_rho22) + theta[2]*(q22[i-1]-long_run_rho22)
    rho12[i] = q12[i]/sqrt(abs(q11[i]*q22[i]))
  }
  LH <- (1/(2*pi*sqrt(1-rho12^2)))*exp(-0.5*(past_z_MSCI^2-2*rho12*production+past_z_Russell^2)/(1-rho12^2))
  NeglogLH = -sum(log(LH))
  
  #Constraints
  if (theta[1]+theta[2]>=1 || theta[1]>1 || theta[1]<0 || theta[2]>1 || theta[2]<0){
    NeglogLH = 9999
  }
  return (NeglogLH)
}

DCC_model = optim(c(0.7,0.1), DCC)
DCC_pars = DCC_model$par
DCC_pars
print(DCC_model$value)


#Simulation

var11 <- c()
var22 <- c()
var11[1] = MSCI_NGARCH$par[5]^2
var22[1] = Russell_NGARCH$par[5]^2
q11 <- c()
q22 <- c()
q12 <- c()
rho12 <- c()
q11[1] = 1
q22[1] = 1
q12[1] = cor(past_z_MSCI,past_z_Russell)
rho12[1] = q12[1]/sqrt(q11[1]*q22[1])
z_1 <- c()
z_2 <- c()
z_1[1] = rnorm(1,0,1)
z_2[1] = rho12[1]*z_1[1] + sqrt(1-rho12[1]^2)*rnorm(1,0,1)
new_ret_1 <- c()
new_ret_2 <- c()
new_ret_1[1] = z_1[1]*sqrt(var11[1])
new_ret_2[1] = z_2[1]*sqrt(var22[1])

no_of_trials = 1000
sum_payment = 0
r = 0.0012#1-year Treasury Rate on Aug. 4, 2014

for (n in 1:no_of_trials){
  for (i in 2:255){
    var11[i] = (1-MSCI_NGARCH$par[1]*(1+MSCI_NGARCH$par[3]^2)-MSCI_NGARCH$par[2])*MSCI_NGARCH$par[4]^2+MSCI_NGARCH$par[1]*(new_ret_1[i-1]-MSCI_NGARCH$par[3]*sqrt(var11[i-1]))^2+MSCI_NGARCH$par[2]*var11[i-1]
    var22[i] = (1-Russell_NGARCH$par[1]*(1+Russell_NGARCH$par[3]^2)-Russell_NGARCH$par[2])*Russell_NGARCH$par[4]^2+Russell_NGARCH$par[1]*(new_ret_2[i-1]-Russell_NGARCH$par[3]*sqrt(var22[i-1]))^2+Russell_NGARCH$par[2]*var22[i-1]
   
    q11[i] = long_run_rho11+DCC_pars[1]*(z_1[i-1]^2-long_run_rho11)+DCC_pars[2]*(q11[i-1]-long_run_rho11)
    q22[i] = long_run_rho22+DCC_pars[1]*(z_2[i-1]^2-long_run_rho22)+DCC_pars[2]*(q22[i-1]-long_run_rho22)
    q12[i] = long_run_rho12+DCC_pars[1]*(z_1[i-1]*z_2[i-1]-long_run_rho12)+DCC_pars[2]*(q12[i-1]-long_run_rho12)
    rho12[i] = q12[i]/sqrt(q11[i]*q22[i])
    z_1[i] = rnorm(1,0,1)
    z_2[i] = rho12[i]*z_1[i] + sqrt(1-rho12[i]^2)*rnorm(1,0,1)
    new_ret_1[i]  = z_1[i] *sqrt(var11[i])
    new_ret_2[i]  = z_2[i] *sqrt(var22[i])
  }
  if(sum(new_ret_1) >= (-0.15) && sum(new_ret_2) >= (-0.15)){
    payment = 1000
  }
  if(sum(new_ret_1) < (-0.15) || sum(new_ret_2) < (-0.15)){
    payment = 1000 + (1000*(min(sum(new_ret_1),sum(new_ret_2))+0.15))*1.1765 
  }
  payment = exp(-r)*payment + sum(4.1833 * exp(-r*seq(1/12,1,1/12)))
  sum_payment = sum_payment + payment
}
value = sum_payment/no_of_trials
value


