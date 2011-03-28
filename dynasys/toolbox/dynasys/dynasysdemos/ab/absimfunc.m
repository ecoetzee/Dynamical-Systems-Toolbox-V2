function [f,o,dfdu,dfdp]=absimfunc(par,u,ijac)
%
% function file for demo ab
%
f=[0,0];
o=[];
dfdu=[];
dfdp=[];

% Call outputs first, otherwise derivatives calulated incorrectly. Not
% sure why.
o=ab(0,u,par(1:3),'outputs');
f=ab(0,u,par(1:3),'derivs');

