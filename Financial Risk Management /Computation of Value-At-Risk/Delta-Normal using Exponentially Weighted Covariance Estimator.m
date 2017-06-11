function va=moodify(w)
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

sum1(1)=0;
vari(1)=0;
cova(1:5,1:5)=changepercent(2).*changepercent(2);
    for i=1:length(data(:,1))-2000
        for j=i:2000+i
            b(j-i+1)=change(j,1)+change(j,2)+change(j,4)+change(j,3)+change(j,5)-5;  
            sum1(i+1)=b(j-i+1)+sum1(i);
            for k=1:5
                for l=1:5
                    cova(k,l,j+1)=0.06*changepercent(j,k)*changepercent(j,l)+0.94*cova(k,l,j);
                end
            end
        end
        stdvariances(i)=sqrt([1 1 1 1 1]*cova(:,:,2000+i)*[1 1 1 1 1]');
        va2(i)=-1000000*(sum1(i+1)/2001-2.33*stdvariances(i));
    end
    va=va2(length(va2)-248+1:length(va2));
end