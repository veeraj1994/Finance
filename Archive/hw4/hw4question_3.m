function [x,l]=hw4question_3
path('/Users/user/Documents/UIUC/2017Spring/FIN567/hw4',path);
a=xlsread('F567C.s2017.HW4.data.xlsx');
b=-a(1:end-1,6);
e=(b-0.022*ones(size(b))).*(b>0.022*ones(size(b)));
e=e(e~=0);
[x,l] = fminsearch(@(x)loglike(x,e), [0.5;0.5]);
end

function loglike1=loglike(x1,e)
loglike1=0;
    for i=1:length(e)
        loglike1=log(x1(2))-(-1/x1(1)-1)*log(1+x1(1)*e(i)/x1(2))+loglike1;
    end
end

