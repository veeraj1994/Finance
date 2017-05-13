function Question_4
[var1,gain_loss]=Question_3;
Shortfall=gain_loss(find(gain_loss<var1));
% calculate the expectations
E=mean(Shortfall);
display(E);
end