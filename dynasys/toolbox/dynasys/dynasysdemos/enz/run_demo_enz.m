%% Stationary Solutions of an Enzyme Model (Demo : enz) 
% The equations, that model a two-compartment enzyme system
% (Kernevez,1980), are given by
% 
% :   $s_1' = (s_0 - s_1) + (s_2 - s_1) - \rho R (s_1)$, 
%
% :   $s_2' = (s_0 + \mu - s_2) + (s_1 - s_2) - \rho R (s_2)$, 
%
% where
%  
% * $R (s) = \frac{s}{1 + s + \kappa s^{2}}$.
% * The free parameter is $s_0$.
% * Other parameters are fixed. 
%
% This equation is also considered by (Doedel, Keller and Kern, 1991).
% 
% 
clear all

%% 
% Create continuation object.
a{1}=auto;

%%
% Print function file to screen.
type(a{1}.s.FuncFileName);

%%
% Set initial conditions.
[a{1}.s.Par0,a{1}.s.U0,a{1}.s.Out0]=stpnt;

%%
% Set constants.
a{1}.c=cenz1(a{1}.c);

%%
% Run continuation.
a{1}=runauto(a{1});

%%
% Data is contained in the autof7 object.
a{1}.f7

%%
% Special points are contained in the autof8 object.
a{1}.f8

%%
% Create plot object and plot diagram.
% 
% * Blue solid lines represent stable solutions
% * Red dashed lines represent unstable solutions
% 
p=plautobj;
set(p,'xLab','Par','yLab','L2norm');
ploteq(p,a{1});


