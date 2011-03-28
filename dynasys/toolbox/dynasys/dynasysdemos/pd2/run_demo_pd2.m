%% Stationary States (2D Problem)(Demo : pd2)
% This demo uses Euler's method to locate a stationary solution of
% a nonlinear parabolic PDE, followed by continuation of this stationary
% state in a free problem parameter. The equations are
%
% : ${\partial u_1 / \partial t} = D_1~{\partial^2 u_1 / \partial x^2}~+~p_1~ u ~( 1-u) ~-~ u_1 u_2$ , 
% : ${\partial u_2 / \partial t} = D_2~{\partial^2 u_2 / \partial x^2} ~-~ u_2 ~+~ u_1 u_2$ , 
%
%
% * on the space interval |[0,L]|,
% * where |L=PAR(11)=1| is fixed throughout,
% * as are the diffusion constants |D_1=PAR(15)=Z| and |D_2=PAR(16)=1|.
% * The boundary conditions are |u_1(0) = u_1(L) = 0| and |u_2(0) = u_2(L) = 1|,
%   for all time.
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
% Initial data are $u(x)=\sin(\pi x/L)$ and $u_2(x)=1$ at time zero.
%
[a{1}.s.Par0,a{1}.s.U0,a{1}.s.Out0]=stpnt(0);

%% Load and display constants
% In the first run the continuation parameter is the independent time variable,
% namely |PAR(14)|, while $p_1=12$ is fixed.
%
% The constants |DS|, |DSMIN|, and |DSMAX| then control the step size
% in space-time, here consisting of |PAR(14)| and  $(u_1(x),u_2(x))$.
a{1}.c=cpd21(a{1}.c);

% Display the constants.
a{1}.c

%% Time integration towards stationary state
% Find stationary state.
a{1}=runauto(a{1});

%% Continuation of stationary states.
% In the second run the continuation parameter is $p_1$. Restart from 
% autof8 object from previous run. A branch point is located during this run.
a{2}=a{1};
a{2}.c=cpd22(a{2}.c);
a{2}=runauto(a{2});

%% Plot the solution
% Create plaut object and plot solution.
p=plautobj;
set(p,'xLab','Par','yLab','L2norm');
ploteq(p,a);

