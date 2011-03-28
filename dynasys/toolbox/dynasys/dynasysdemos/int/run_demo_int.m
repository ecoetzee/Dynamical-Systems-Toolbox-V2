%% Boundary and Integral Constraints (Demo : int)
% This demo illustrates the computation of a solution family to
% the equation
%
% : $u_1'=u_2$,
% 
% : $u_2'=-p_1  e^{u_1}$,
%
% * with a non-separated boundary condition : $ u_1(0)-u_1(1)-p_2=0 $
% * and an integral constraint : $\int_0^{1}u(t)dt-p_3=0$
% 
% The solution family contains a fold, which, in the second run, is
% continued in two equation parameters.

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

%% Definition of integral conditions file
% Display integral conditions file contents.
type(a{1}.s.IcndFileName);

%% Set intial conditions
% We can either load data from the starting point file, or we 
% can define the initial conditions directly into variable.
[a{1}.s.Par0,a{1}.s.U0,a{1}.s.Out0]=stpnt;

%% Load and display constants
% Load the constants file.
a{1}.c=cint1(a{1}.c);

%%
% Display the constants.
a{1}.c

%% Compute the solution family containing the fold
% Run equilbrium solutions.
a{1}=runauto(a{1});

%% Generate starting data for a curve of folds
a{2}=a{1};
a{2}.c=cint2(a{2}.c);
a{2}=runauto(a{2});

%% Compute a curve of folds; restart from second run
a{3}=a{2};
a{3}.c=cint3(a{3}.c);
a{3}=runauto(a{3});

%% Plot the solution
% Create plaut object and plot solution.
p=plautobj;
set(p,'xLab','Par','yLab','L2norm');
ploteq(p,a);

