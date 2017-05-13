function Question_1

%loading data
format shortE;
path(path,'/Users/user/Documents/UIUC/2017Spring/FIN567/hw3');
a=xlsread('HW3');
b=[a(:,2),a(:,5),a(:,8),a(:,11)];
data=b;

%calculating log returns
change(1,1:4)=1;
for i=2:length(data(:,1))
    change(i,:)=log(data(i,:)./data(i-1,:));
end

%calculating covariances
cova(1:4,1:4)=change(2).*change(2);
        for j=1:length(data(:,1))
            for k=1:4
                for l=1:4
                    cova(k,l,j+1)=0.06*change(j,k)*change(j,l)+0.94*cova(k,l,j);
                end
            end
        end
covariance_matrix=cova(:,:,length(data(:,1))+1);
        
%calculating annualized covariance and std.
annualized_covariance_matrix=252*covariance_matrix;
for k=1:4
    std_dev(k)=sqrt(covariance_matrix(k,k))*sqrt(252);
    day_std_dev(k)=sqrt(covariance_matrix(k,k));
end
%display results       
    disp('The Covariance Matrix is:');
    disp(covariance_matrix);
    disp('The Annualized Standard Deviation is:')
    disp(std_dev);
    disp('The Daily Standard Deviation is:')
    disp(day_std_dev);
    disp('The Annualized Covariance Matrix is:')
    disp(annualized_covariance_matrix);
end
