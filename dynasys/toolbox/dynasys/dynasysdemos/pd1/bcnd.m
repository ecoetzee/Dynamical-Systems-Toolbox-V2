function [fb,o,dbc]= bcnd(par,u0,u1,ijac)
%
% boundary conditions for demo pd1
%
fb=[];
o=[];
dbc=[];

% define the boundary conditions (Dirichlet, in this demo).
fb(1)=u0(1);
fb(2)=u1(1);

