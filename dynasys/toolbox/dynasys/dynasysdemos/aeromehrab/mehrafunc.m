function [f,o,dfdu,dfdp]=mehrafunc(par,u,ijac)
%
% function file for demo ab
%
f=[0,0,0,0,0];
o=[];
dfdu=[];
dfdp=[];

% Remember that we only use the first five states for the bifurcation
% analysis, because the Euler angles do not play a role, hence set euler
% angles to zero.
u=[u;0;0;0];

% Obtain derivative values
o=AircraftB(0,u,par(1:3),'outputs');
f=AircraftB(0,u,par(1:3),'derivs');

% Extract first five state derivatives
f=f(1:5);

