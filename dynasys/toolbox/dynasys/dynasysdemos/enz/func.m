function [f,o,dfdu,dfdp]= func(par,u,ijac)
%
% function file for enz demo
% 
f=[];
o=[];
dfdu=[];
dfdp=[];
%
s0=par(1);
rm=par(2);
rh=par(3);
rk=par(4);
%
s1=u(1);
s2=u(2);
%
rs1=s1/(1+s1+rk*s1.^2);
rs2=s2/(1+s2+rk*s2.^2);
%
f(1)=(s0-s1) + (s2-s1) - rh * rs1;
f(2)=(s0+rm-s2) + (s1-s2) - rh * rs2;


