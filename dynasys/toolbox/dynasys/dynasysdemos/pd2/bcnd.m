function [fb,o,dbc]= bcnd(par,u0,u1,ijac)
%
% boundary conditions for demo pd2
%
fb=[];
o=[];
dbc=[];

% define the boundary conditions.
fb(1)=u0(1);
fb(2)=u0(2)-1.;
fb(3)=u1(1);
fb(4)=u1(2)-1.;

