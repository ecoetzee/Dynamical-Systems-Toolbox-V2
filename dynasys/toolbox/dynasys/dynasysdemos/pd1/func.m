function [f,o,dfdu,dfdp]= func(par,u,ijac)
%
% equations file for demo pd1
%
f=[];
o=[];
dfdu=[];
dfdp=[];

f(1)= par(1) * u(1) * ( 1. - u(1) );

