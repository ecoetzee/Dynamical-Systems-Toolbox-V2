function [fb,o,dbc]= bcnd(par,u0,u1,ijac)
%
% boundary conditions for demo exp
%
fb=[];
o=[];
dbc=[];

fb(1)=u0(1);
fb(2)=u1(1);

