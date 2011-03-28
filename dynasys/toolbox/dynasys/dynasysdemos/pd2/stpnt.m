function [par,u,o]= stpnt(t)
%
% starting point for demo int
par=zeros(36,1);
o=[];

% set the (constant) parameter
par(1) = 12.;
%
% set the actual width of the space interval [0,par(11)]
par(11) = 1.;

% set the initial data in the (scaled) interval [0,1]
u(1) = sin(pi*t);
u(2) = 1.;

% also set the space derivative of the initial data
% note the scaling by 1/par(11) 
u(3) = pi*cos(pi*t)/par(11);
u(4) = 0./par(11);

% set the diffusion constants
par(15) = 1.;
par(16) = 1.;

