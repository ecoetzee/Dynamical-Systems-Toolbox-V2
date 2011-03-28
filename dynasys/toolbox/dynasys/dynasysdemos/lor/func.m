function [f,o,dfdu,dfdp]=func(par,u,ijac)
%
% function file for demo ab
%
f=[];       % derivative values, same size as Ndim
o=[];       % additional outputs, size automatically detected
dfdu=[];    % user-defined derivatives for states, this parameter empty when Jac=0
dfdp=[];    % user-defined derivatives for parameters, this parameter empty when Jac=0

f(1)= par(3) * (u(2)- u(1));
f(2)= par(1)*u(1) - u(2) - u(1)*u(3);
f(3)= u(1)*u(2) -  par(2)*u(3);


