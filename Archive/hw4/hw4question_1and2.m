function [e,num]=hw4question_1and2
path('/Users/user/Documents/UIUC/2017Spring/FIN567/hw4',path);
a=xlsread('F567C.s2017.HW4.data.xlsx');
b=-a(1:end-1,6);
% length=size(b);

for u=0.01:0.002:0.05
    e(int64((u-0.01)/0.002+1))=(b>u*ones(size(b)))'*(b-u*ones(size(b)))/sum((b>u*ones(size(b))));
end
num=sum((b>0.022*ones(size(b))+(b==0.022*ones(size(b)))));
plot([0.01:0.002:0.05],e);
