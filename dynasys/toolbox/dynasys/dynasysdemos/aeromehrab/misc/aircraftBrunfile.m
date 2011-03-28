%% Run file for the Mehra Aircraft B model
%This demo demonstrates the application of Dynamical Systems Toolbox in DST
%mode to the analysis of aircraft flight dynamics, with this specific
%aircraft example showing pitch/roll coupling.

%% Calculate an initial starting point solution
%To run the AUTO program an initial equilibrium point is needed so as to
%run the continuation method.  This example uses the in built Matlab ODE
%solver ODE45 to run the simulation until the derivatives are zero.

clear all
%Guess an initial conditions.
Uini=[0 0 0 0 0];

%Set run time for the ode45 solver.
ts=[0 20];

%Define the aileron angle to be used as the continuation starting point.
da=40;

%Run ode45 solver.
[T,Y]=ode45(@(t,U) testfunction(t,U,da),ts,Uini,odeset('RelTol',1e-7));

%%
% Plot the results out to check that the states have converged to values
% where the derivatives are zero.
h=plot(T,Y);
title('Time history for Matlab Demo: Aircraft B');
xlabel('Time (s)');
ylabel('alpha, beta, p, q, r');
legend({'\alpha','\beta','p','q','r'});
axis([0 20 -15 5]);
snapnow;
close(gcf);

%% Set up the AUTO continuation run.
% Create an auto object.
a{1}=auto;

%%
% Define the function file and intial conditions.
a{1}.s.Par0=da;
a{1}.s.U0=Y(end,:);
a{1}.s.Out0=[];

%%
% Print function file to screen.
type(a{1}.s.FuncFileName);

%% Set the AUTO constants
% Load constants from the constant m-file.
a{1}.c=aircraftbconst(a{1}.c);

%%
% Show format of set commands.
type('aircraftbconst.m');

%%
% Print constants to screen.
a{1}.c

%% Run an equilibrium solution 
% Compute stationary solutions by using the |runauto| method in the |auto|
% object.
a{1}=runauto(a{1});

%% Check contents written to objects
% Check the contents of thefort.7 and fort.8 files.
% 
% Print f7 contents to screen.
a{1}.f7

%%
% Print f8 contents to screen.
a{1}.f8

%% Plot the equilibrium surface
% In this demo the selected solution measure is roll rate p.  For other
% solution measures change the value of c.Iplt in aircraftbconst to the
% following:
%c.Iplt=1 for alpha
%c.Iplt=2 for beta
%c.Iplt=3 for roll rate p
%c.Iplt=4 for pitch rate q
%c.Iplt=5 for yaw rate r

%The solution measure is now plotted against the continuation parameter.
p=plautobj;
ploteq(p,a);
xlabel('da (deg)')
ylabel('p (deg/s)')
xlim([-40 40])