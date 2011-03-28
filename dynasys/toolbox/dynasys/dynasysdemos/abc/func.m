function [f,o,dfdu,dfdp]=func(par,u,ijac)
%
% function file for demo abc
%
f=[];
o=[];
dfdu=[];
dfdp=[];

% enter your equations here
x1=u(1);
x2=u(2);
x3=u(3);

d=par(1);
alpha=par(2);
beta=par(3);
b=par(4);
s=par(5);

e=exp(x3);
x1c=1-x1;

f(1)=-x1 + d*x1c*e;
f(2)=-x2 + d*e*(x1c - s*x2);
f(3)=-x3 - beta*x3 + d*b*e*(x1c + alpha*s*x2);

