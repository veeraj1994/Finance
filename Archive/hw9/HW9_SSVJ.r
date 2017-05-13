#Q1 a
trials = rep(0,10000)
for (j in 1:10000){
	a = rep(0 , 100)
	for (i in 1:80){
		a[i] = 10
	}
	for (i in 81:100){
		a[i] = 20
	}
	b = runif(100, 0, 1);
	for (i in 1:100){
		if (b[i] < 0.02){
			b[i] = 0
		}
		else{
			b[i] = 1
		}
	}
	trials[j] = sum(a*b)
}
hist(-(trials-1200), main = "Distribution of loss")

###Q1 b
no_of_simul <- 10000
N <- 100
cov <- 0.3
V = matrix(1,N,1)
V = (1-cov) * V
M <- diag(0.7, N, N)
M <- M + cov

mu <- matrix(0,N,1)
R <- mvrnorm(no_of_simul,mu,M)
##R is 100 x 10000
results <- array(0,dim = c(100,no_of_simul))
temp <- c()
for(i in 1:no_of_simul){
for( k in 1:100){
  temp[k] <- dnorm(R[i,k],0,1)
    if(temp[k] < 0.02){
      results[k,i] <- 1
    }
  }
}

results <- as.matrix(results)

assets<- matrix(10,1,100)

for( i in 81:100){
  assets[i] = assets[i]+10
}
loss <- c()
for( i in 1:no_of_simul){
loss[i] <- assets%*%results[,i]
}

hist(loss,main = "Distribution of loss")

###Q1 c
avg_prob = 0
prob <-c()
count = 0
for(i in 1:10000){
  count = 0
  count = sum(results[,i])
  prob = (count-1)*(count)/2
  prob = prob / ((100-1)*(100)/2)
  avg_prob = avg_prob + prob
}

avg_prob = avg_prob / 10000
default_corr = (avg_prob - 0.02^2)/(0.02*(1-0.02))
default_corr

##Q1 d
Gaussian_copula<- function(cov){
      no_of_simul <- 10000
      N <- 100
      V = matrix(1,N,1)
      V = (1-cov) * V
      M <- diag(0.7, N, N)
      M <- M + cov
      
      mu <- matrix(0,N,1)
      R <- mvrnorm(no_of_simul,mu,M)
      ##R is 100 x 10000
      results <- array(0,dim = c(100,no_of_simul))
      temp <- c()
      
      for(i in 1:no_of_simul){
        for( k in 1:100){
          temp[k] <- dnorm(R[i,k],0,1)
          if(temp[k] < 0.02){
            results[k,i] <- 1
          }
        }
      }
      results <- as.matrix(results)
      
      avg_prob = 0
      prob <-c()
      count = 0
      for(i in 1:no_of_simul){
        count = 0
        count = sum(results[,i])
        prob = (count-1)*(count)/2
        prob = prob / ((100-1)*(100)/2)
        avg_prob = avg_prob + prob
      }
      
      avg_prob = avg_prob / no_of_simul
      default_corr = (avg_prob - 0.02^2)/((0.02*(1-0.02)))
      
      default_corr
}

cov_value <- optimize(f=function(x){abs(Gaussian_copula(x)-0.05)}, interval =c(0,1) )
cov_value


#Q2 a
trials = rep(0,10000)
for (j in 1:10000){
	a = rep(0 , 100)
	for (i in 1:80){
		a[i] = 10
	}
	for (i in 81:100){
		a[i] = 20
	}
	b = runif(100, 0, 1);
	for (i in 1:100){
		if (b[i] < 0.02){
			b[i] = 0
		}
		else{
			b[i] = 1
		}
	}
	trials[j] = sum(a*b)
}
e = 1200 - trials
f = c()
for (j in 1:10000){
	if (e[j] != 0){
		f = c(f,e[j])
	}
}
ec = quantile(f, 0.999) - mean(e)

#Q2 b
p=0.3
trials = rep(0,10000)
for (j in 1:10000){
	a = rep(0 , 100)
	for (i in 1:80){
		a[i] = 10
	}
	for (i in 81:100){
		a[i] = 20
	}
	b = rnorm(100);
	for (i in 1:100){
		b[i] = pnorm(sqrt(p)*b[i]+sqrt(1-p)*rnorm(1))
	}
	for (i in 1:100){
		if (b[i] < 0.02){
			b[i] = 0
		}
		else{
			b[i] = 1
		}
	}
	trials[j] = sum(a*b)
}
e = 1200 - trials
f = c()
for (j in 1:10000){
	if (e[j] != 0){
		f = c(f,e[j])
	}
}
ec = quantile(f, 0.999) - mean(e)

#Q3 a
d = rep(0,10000)
for (j in 1:10000){
	a = rep(0 , 100)
	for (i in 1:80){
		a[i] = 10
	}
	for (i in 81:100){
		a[i] = 20
	}
	b = runif(100, 0, 1);
	for (i in 1:100){
		if (b[i] < 0.02){
			b[i] = 1
		}
		else{
			b[i] = 0
		}
	}
	e = sum(b[81:100])
	if (e>=2){
		d[j] = sum(a*b)-40
	}
	else{
		d[j] = sum(a*b)
	}
}
hist((d), main = "Distribution of loss")

#Q3 b
d = rep(0,10000)
for (j in 1:10000){
	a = rep(0 , 100)
	for (i in 1:80){
		a[i] = 10
	}
	for (i in 81:100){
		a[i] = 20
	}
	b = runif(100, 0, 1);
	for (i in 1:100){
		if (b[i] < 0.02){
			b[i] = 1
		}
		else{
			b[i] = 0
		}
	}
	e = sum(b[81:100])
	if (e==1){
		d[j] = sum(a*b)-20
	}
	else if(e>=2){
		d[j] = sum(a*b)-40
	}
	else{
		d[j] = sum(a*b)
	}
}
f = c()
for (j in 1:10000){
	if (d[j] != 0){
		f = c(f,d[j])
	}
}
ec = quantile(f, 0.999) - mean(d)

#Q3 c
d = rep(0,10000)
for (j in 1:10000){
	a = rep(0 , 100)
	for (i in 1:80){
		a[i] = 10
	}
	for (i in 81:100){
		a[i] = 20
	}
	b = runif(100, 0, 1);
	for (i in 1:100){
		b[i] = pnorm(sqrt(0.3)*b[i]+sqrt(1-0.3)*rnorm(1))
	}
	for (i in 1:100){
		if (b[i] < 0.02){
			b[i] = 1
		}
		else{
			b[i] = 0
		}
	}
	e = sum(b[81:100])
	if (e==1){
		d[j] = sum(a*b)-20
	}
	else if(e>=2){
		d[j] = sum(a*b)-40
	}
	else{
		d[j] = sum(a*b)
	}
}
f = c()
for (j in 1:10000){
	if (d[j] != 0){
		f = c(f,d[j])
	}
}
ec = quantile(f, 0.999) - mean(d)

#Q4 a
d = rep(0,10000)
for (j in 1:10000){
	a = rep(0 , 100)
	for (i in 1:80){
		a[i] = 10
	}
	for (i in 81:100){
		a[i] = 20
	}
	b = runif(100, 0, 1);
	for (i in 1:100){
		if (b[i] < 0.02){
			b[i] = 1-rbeta(1, 2, 2)
		}
		else{
			b[i] = 0
		}
	}
	d[j] = sum(a*b)
}
hist((d), main = "Distribution of loss")

#Q4 b
d = rep(0,10000)
for (j in 1:10000){
	a = rep(0 , 100)
	for (i in 1:80){
		a[i] = 10
	}
	for (i in 81:100){
		a[i] = 20
	}
	b = runif(100, 0, 1);
	for (i in 1:100){
		b[i] = pnorm(sqrt(0.3)*b[i]+sqrt(1-0.3)*rnorm(1))
	}
	for (i in 1:100){
		if (b[i] < 0.02){
			b[i] = 1-rbeta(1, 2, 2)
		}
		else{
			b[i] = 0
		}
	}
	d[j] = sum(a*b)
}
hist((d), main = "Distribution of loss")
