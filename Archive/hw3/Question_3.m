function [var1, gain_loss]=Question_3

%loading data
format shortE;
path(path,'/Users/user/Documents/UIUC/2017Spring/FIN567/hw3');
a=xlsread('HW3');
b=[a(:,2),a(:,5),a(:,8),a(:,11)];
data1=b;

%calculating log returns
change(1,1:4)=1;
for i=2:length(data1(:,1))
    change(i,:)=log(data1(i,:)./data1(i-1,:));
end

%calculating covariances
cova(1:4,1:4)=change(2).*change(2);
        for j=1:length(data1(:,1))
            for k=1:4
                for l=1:4
                    cova(k,l,j+1)=0.06*change(j,k)*change(j,l)+0.94*cova(k,l,j);
                end
            end
        end
covariance_matrix=cova(:,:,length(data1(:,1))+1);
% covariance_matrix
for k=1:4
    std_dev(k)=sqrt(covariance_matrix(k,k));
%     day_std_dev(k)=sqrt(covariance_matrix(k,k));
end
for k=1:4
    for l=1:4
        corr(k,l)=covariance_matrix(k,l)/(std_dev(k)*std_dev(l));
    end
end
N=10000;
% generate random variables with multivariate normal distribution
% r = mvnrnd(zeros(1,4),corr,N);
r = mvnrnd(zeros(1,4),covariance_matrix,N);
r = [r(:,1) r(:,2) r(:,3) r(:,3) r(:,4) r(:,4)];
p=Question_2;
current=[b(end,1) b(end,2) p(1) p(2) p(3) p(4)];
% calculate new index and new portfolio value based on the results of simulation
% for i=1:4
%     new_index(:,i)=(ones(N,1)*b(end,i)).*exp(r(:,i));
% end
new_index=(ones(N,1)*current).*(exp(r));
% new_index
new_value=zeros(N,1);

for i=1:N
  new_value(i)=portfolio_value(new_index(i,:));
end
% new_value
% ones(N,1)*portfolio_value(b(end,:))
gain_loss=new_value-ones(N,1)*portfolio_value(current);
% calculate VaR
var1=quantile(gain_loss,0.05);
end

function [pvalue]=portfolio_value(index)
%value of portfolio
v1=-5000*call(index(1),1865,0.0222,0.0025,index(3),35/365);
v2=-5000*put(index(1),1865,0.0222,0.0025,index(4),35/365);
v3=55000*call(index(2),160,0.0248,0.0025,index(5),35/365);
v4=55000*put(index(2),160,0.0248,0.0025,index(6),35/365);
pvalue=v1+v2+v3+v4;
end

function [ x ] = call(S,K,dividend,r,sigma,t)
% call option with constant dividend
d1=(log(S/K)+(r-dividend+0.5*sigma^2)*t)/(sigma*sqrt(t));
d2=(log(S/K)+(r-dividend-0.5*sigma^2)*t)/(sigma*sqrt(t));
x=S*exp(-dividend*t)*normcdf(d1,0,1)-exp(-r*t)*K*normcdf(d2,0,1);
end

function [ x ] = put(S,K,dividend,r,sigma,t)
% put option with constant dividend
d1=(log(S/K)+(r-dividend+0.5*sigma^2)*t)/(sigma*sqrt(t));
d2=(log(S/K)+(r-dividend-0.5*sigma^2)*t)/(sigma*sqrt(t));
x=-S*exp(-dividend*t)*normcdf(-d1,0,1)+exp(-r*t)*K*normcdf(-d2,0,1);
end