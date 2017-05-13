function [p,fval]=Question_2

%loading data
format shortE;
path(path,'/Users/user/Documents/UIUC/2017Spring/FIN567/hw3');
a=xlsread('HW3');
b=[a(:,2),a(:,5),a(:,8),a(:,11)];
data1=b;

%The bias between theoretical value and emperical value of each option, and
%make functions
v1=@(sigma1) abs(call(data1(end,1),1865,0.0222,0.0025,sigma1,35/365)-(49.50+50.1)/2);
v2=@(sigma2) abs(put(data1(end,1),1865,0.0222,0.0025,sigma2,35/365)-(55.50+56.1)/2);
v3=@(sigma3) abs(call(data1(end,2),160,0.0248,0.0025,sigma3,35/365)-(4.05+3.85)/2);
v4=@(sigma4) abs(put(data1(end,2),160,0.0248,0.0025,sigma4,35/365)-(4.90+4.7)/2);

[p(1),fval(1)]=fminsearch(v1,0.2);
[p(2),fval(2)]=fminsearch(v2,0.2);
[p(3),fval(3)]=fminsearch(v3,0.2);
[p(4),fval(4)]=fminsearch(v4,0.2);
end

function [ x ] = call( S,K,dividend,r,sigma,t )
% call option with constant dividend
d1=(log(S/K)+(r-dividend+0.5*sigma^2)*t)/(sigma*sqrt(t));
d2=(log(S/K)+(r-dividend-0.5*sigma^2)*t)/(sigma*sqrt(t));
x=S*exp(-dividend*t)*normcdf(d1,0,1)-exp(-r*t)*K*normcdf(d2,0,1);
end

function [ x ] = put( S,K,dividend,r,sigma,t )
% put option with constant dividend
d1=(log(S/K)+(r-dividend+0.5*sigma^2)*t)/(sigma*sqrt(t));
d2=(log(S/K)+(r-dividend-0.5*sigma^2)*t)/(sigma*sqrt(t));
x=-S*exp(-dividend*t)*normcdf(-d1,0,1)+exp(-r*t)*K*normcdf(-d2,0,1);
end
