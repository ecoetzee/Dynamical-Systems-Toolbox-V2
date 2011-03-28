function [f,o,dfdu,dfdp]=abmatfunc(par,u,ijac)
%
% function file for demo ab
%
f=[];       % derivative values, same size as Ndim
o=[];       % additional outputs, size automatically detected
dfdu=[];    % user-defined derivatives for states, this parameter empty when Jac=0 
dfdp=[];    % user-defined derivatives for parameters, this parameter empty when Jac=0

u1=u(1);
u2=u(2);

e=double(exp(u2));

f(1)=-u1 + par(1)*(1-u1)*e;
f(2)=-u2 + par(1)*par(2)*(1-u1)*e - par(3)*u2;