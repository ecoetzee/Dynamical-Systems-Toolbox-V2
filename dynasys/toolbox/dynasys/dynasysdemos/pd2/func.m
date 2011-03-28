function [f,o,dfdu,dfdp]= func(par,u,ijac)
%
% equations file for demo pd2
%
f=[];
o=[];
dfdu=[];
dfdp=[];

% set the nonlinear term
f(1)= par(1)*u(1)*( 1. - u(1) ) - u(1)*u(2);
f(2)= -u(2) + u(1)*u(2);

