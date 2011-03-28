function [f,o,dfdu,dfdp]= func(par,u,ijac)
%
% equations file for demo exp
%
f=[];
o=[];
dfdu=[];
dfdp=[];

f(1)= u(2);
f(2)=-par(1) * exp(u(1));

