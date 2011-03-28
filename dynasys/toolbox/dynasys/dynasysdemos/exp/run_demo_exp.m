%% Bratu's Equation (Demo : exp)
% This demo illustrates the computation of a solution family to
% the boundary value problem
%
% : $u_1' = u_2$,
%
% : $u_2' = -p_1  e^{u_1}$,
%
% with boundary conditions $u_1(0)=0$,  $u_1(1)=0$
% This equation is also considered by Doedel, Keller, and Kernevez (1991).

%% Initialise workspace
% Clear workspace
clear all

% Create a continuation object.
a{1}=auto;

%% Definition of function file
% Display function file contents.
type(a{1}.s.FuncFileName);

%% Definition of boundary conditions file
% Display boundary conditions file contents.
type(a{1}.s.BcndFileName);

%% Set intial conditions
% We can either load data from the starting point file, or we 
% can define the initial conditions directly into variable.
[a{1}.s.Par0,a{1}.s.U0,a{1}.s.Out0]=stpnt;

%% Load and display constants
% Load the constants file.
a{1}.c=cexp1(a{1}.c);

% Display the constants.
a{1}.c

%% Compute the solution family containing the fold
% Run equilbrium solutions.
a{1}=runauto(a{1});

%% Restart at a labeled solution, using increased accuracy
a{2}=a{1};
a{2}.c=cexp2(a{2}.c);
a{2}=runauto(a{2});

%% Plot the solution
% Create plaut object and plot solution.
p=plautobj;
set(p,'xLab','Par','yLab','L2norm');
ploteq(p,a);

