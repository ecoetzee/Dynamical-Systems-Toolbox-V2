function [f,o,dfdu,dfdp]= func(par,u,ijac)
%
% equations file for lorenz demo lrz
%
f=[];
o=[];
dfdu=[];
dfdp=[];

f(1)= par(3) * (u(2)- u(1));
f(2)= par(1)*u(1) - u(2) - u(1)*u(3);
f(3)= u(1)*u(2) -  par(2)*u(3);

