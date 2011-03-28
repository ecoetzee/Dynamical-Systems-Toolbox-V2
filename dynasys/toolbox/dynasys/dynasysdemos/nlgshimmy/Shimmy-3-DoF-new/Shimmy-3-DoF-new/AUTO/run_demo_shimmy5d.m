%% Simulink example with demo 'shimmy5d'
% This demonstration uses shows how to analyse a 5D nose landing gear
% vibration problem, also known as shimmy. Shimmy of a nose landing gear is
% similar to what you see when a wheel on a shopping trolley wobbles.
%
% Author : Phani Thota

clear all

%% 
% Adjust equations file to contain correct initial conditions
% The intial conditions are set to zero.

%%
% Create an auto object
a{1}=auto;

%%
% Define the function file and intial conditions
a{1}.s.FuncFileName='shimmy5d';
a{1}.s.Par0=[0.16,0.1,110,60000,0.1,300000,570,0.2571,3.24e+6,1]';
a{1}.s.U0=zeros(5,1);
a{1}.s.Out0=[];

%% 
% Print function file to screen.
type(a{1}.s.FuncFileName);

%% Set the constants and run the stationary solutions
% We can set the constants from the command line, but for ease of use we
% can convert the constants file from the original fortran code with the |convertchc|
% function, and then we can read the constants in from the m-file. 
a{1}.c=c5d1(a{1}.c);

%%
% Show format of set commands
type('c5d1.m');

%%
% Print constants to screen.
a{1}.c

%% Run an equilibrium solution 
% Compute stationary solutions by using the |runauto| method in the |auto|
% object. The Simulink model will be opened if it is not open, and then
% compiled automatically before the analysis starts.
a{1}=runauto(a{1});



displayEndOfDemoMessage(mfilename)
