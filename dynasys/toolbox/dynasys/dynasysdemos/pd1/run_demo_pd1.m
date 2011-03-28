%% Stationary States (1D Problem)(Demo : pd1)
% This demo uses Euler's method to locate a stationary solution of
% a nonlinear parabolic PDE, followed by continuation of this stationary
% state in a free problem parameter. The equation is
%
% : $\frac{\partial{u}}{\partial{t}}=D\frac{\partial^2{u}}{\partial{x^2}}+p_1u(1-u)$,
%
% * on the space interval *|[0,L]|* , 
% * where *|L=PAR(11)=10|* is fixed throughout, 
% * as is the diffusion constant *|D=PAR(15)=0.1|*.
% * the boundary conditions are *|u(0) = u(L) = 0|* for all time.
% 
% Euler time integration is only first order accurate, so that
% the time step must be sufficiently small to ensure correct results.
% Indeed, this option has been added only as a convenience, and should
% generally be used only to locate stationary states.
% 

%% Initialise workspace
% Clear workspace
clear all

% Create a continuation object.
a{1}=auto;

%% Definition of function file
% Display function file contents.
type(a{1}.s.FuncFileName);

%% Definition of initial conditions file
% Display initial conditions file contents.
type(a{1}.s.StpntFileName);

%% Set intial conditions
% In this case we load data from the starting point file. The |stpnt.m| file is
% called repeatedly for this option of Ips
%
% Note that in the subroutine |stpnt.m| the initial data must be scaled to
% the unit interval, and that the scaled derivative must also be provided;
% see the equations-file |func.f|.
%
% Initial data are $u(x)=\sin(\pi x/L)$ at time zero.
%
[a{1}.s.Par0,a{1}.s.U0,a{1}.s.Out0]=stpnt(0);

%% Load and display constants
% In the first run the continuation parameter is the independent time variable,
% namely |PAR(14)|, while $p_1=1$ is fixed.
%
% The constants |DS|, |DSMIN|, and |DSMAX| then control the step size
% in space-time, here consisting of |PAR(14)| and  $u(x)$.
a{1}.c=cpd11(a{1}.c);

% Display the constants.
a{1}.c

%% Time integration towards stationary state
% Find stationary state.
a{1}=runauto(a{1});

%% Continuation of stationary states.
% In the second run the continuation parameter is $p_1$. Restart from 
% autof8 object from previous run
a{2}=a{1};
a{2}.c=cpd12(a{2}.c);
a{2}=runauto(a{2});

%% Plot the solution
% Create plaut object and plot solution.
p=plautobj;
set(p,'xLab','Par','yLab','L2norm');
ploteq(p,a);

