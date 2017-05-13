function hw4question_5
format long;
path('/Users/user/Documents/UIUC/2017Spring/FIN567/hw4',path);
a=xlsread('F567C.s2017.HW4.data.xlsx');
b=-a(1:end-1,6);
e=(b-0.022*ones(size(b))).*(b>0.022*ones(size(b)));
e=e(e~=0);
x = fminsearch(@(x)loglike(x,e), [0.5;0.5]);
prob=sum(b>0.022*ones(size(b)))/length(b);
e=[0.022;0.05;0.1]-0.022;
disp('For P>=0.022  P>=0.05  P>=0.1, the probabilities are:');
for i=1:3
disp(prob*power((1+x(1)*(e(i))/x(2)),(-1/x(1))));
end
y=[0.022:0.00001:0.1];
plot(y,prob*power((1+x(1)*(y-0.022)/x(2)),(-1/x(1))));
end

function loglike1=loglike(x1,e)
loglike1=0;
    for i=1:length(e)
        loglike1=log(x1(2))-(-1/x1(1)-1)*log(1+x1(1)*e(i)/x1(2))+loglike1;
    end
end