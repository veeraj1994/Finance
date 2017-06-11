function va=varcalc(w)
data=w;
for a=1:length(data(:,1))
    for b=1:length(data(1,:))
        if(isnan(data(a,b)) && ~isnan(data(a+1,b)))
            data(a,b)= 0.5*data(a+1,b)+0.5*data(a-1,b);
        end
        if(isnan(data(a,b)) && isnan(data(a+1,b)))
            data(a,b)= 0.5*data(a+2,b)+0.5*data(a-1,b);
        end
    end
end

j(1)=0;
for a=1:length(data(:,1))
    if(sum(isnan(data(a,:))))
       j(end+1)=a;
    end
end
for t=2:length(j)
    data(j(t)-t+2,:)=[];
end

length(data(:,1))
change(1,1:5)=1;
for i=2:length(data(:,1))
    change(i,:)=data(i,:)./data(i-1,:);
end

    for i=1:length(data(:,1))-2000
        for j=i:1999+i
            b(j-i+1)=change(j,1)+change(j,2)*change(j,4)+change(j,3)*change(j,5)-3;
        end
       va2(i)=-1000000*quantile(b,0.01);
    end
    va=va2(length(va2)-248+1:length(va2));
end