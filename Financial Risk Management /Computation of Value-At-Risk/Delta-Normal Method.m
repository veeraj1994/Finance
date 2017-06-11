function va=modify(w)
data=w;
j(1)=0;
for a=1:length(data(:,1))
    if(sum(isnan(data(a,:))))
       j(end+1)=a;
    end
end
for t=2:length(j)
    data(j(t)-t+2,:)=[];
end

change(1,1:4)=1;
for i=2:length(data(:,1))
    change(i,:)=log(data(i,:)./data(i-1,:));
end
% changepercent=change-1;

sum1(1)=0;

    for i=1:length(data(:,1))-2000
        for j=i:2000+i
            b(j-i+1)=change(j,1)+change(j,2)+change(j,4)+change(j,3);  
            sum1(i+1)=b(j-i+1)+sum1(i);
        end
        covariance=cov(change(i:2000+i,:));
        stdvariances(i)=sqrt([1 1 1 1]*covariance*[1 1 1 1]');
        va2(i)=+1000000*2.33*stdvariances(i);
    end
    va=va2(length(va2)-252+1:length(va2));
end