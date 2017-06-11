function va=mooodify(w)
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

change(1,1:5)=1;

for i=2:length(data(:,1))
    change(i,:)=data(i,:)./data(i-1,:);
end
changepercent=change-1;

sum1=0;

for m=191:2438
    for k=1:5
        for t=1:m-1
            sum1=sum1+0.94^(t-1)*changepercent(m-t,k)^2;
        end
        sum1=sum1*0.06;
        cova(m-190,k)=0.94*sum1+0.06*changepercent(m,k)^2;
    end
end

    for i=1:248
        for j=1:2000
            u(j,:)=sqrt(cova(2000+i,:)./cova(2000+i-j,:)).*changepercent(2000+190+i-j,:);
            b(j)=1+u(1)+(1+u(2))*(1+u(4))+(1+u(3))*(1+u(5))-3;
        end
      va(i)=1000000*abs(quantile(b,0.01));
    end
end