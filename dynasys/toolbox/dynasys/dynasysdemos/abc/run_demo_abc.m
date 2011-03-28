%% Periodic solutions, A->B->C reaction (Demo:abc)
% This demo illustrates the computation of stationary solutions, Hopf 
% bifurcations and periodic solutions in the $A \to B \to C$ reaction
% (Doedel & Heinemann, 1983).
%
% $u_1^\prime =  -u_1 + p_1 (1-u_1) e^{u_3}$
%
% $u_2^\prime =  -u_2 +  p_1 e^{u_3} ( 1-u_1 - p_5 u_2 )$
%
% $u_3^\prime =  -u_3 - p_3 u_3 + p_1 p_4 e^{u_3}( 1-u_1 + p_2 p_5 u_2 )$
%
%
% with $p_2=1$, $p_3=1.55$, $p_4=8$, and $p_5=0.04$. The free parameter is $p_1$.
%
% The equations are programmed in the equations-file |abc.m|. The 
% starting point, an equilibrium of the equations is defined in the auto
% object. A more advanced version, that continues branch points in three 
% parameters is provided by the demo |abcb|.
%
%% Calculate the family of stationary solutions.
% The following constants are set in |cabc1.m|
%
% * |IPS=1|: a family of stationary solutions is computed.
% * |IRS=0|: the starting point defined in the object.
% * |ICP(1)|: the continuation parameter is |PAR(1)|
% * |NUZR=1|: there is one user output point, namely at |PAR(1)=0.4|.
%   Moreover, since the index |"-1"| in the last line of the constants-file
%   |cabc1.m| is negative, the calculation will terminate when the 
%   calculation reaches the value |PAR(1)=0.4|..
%
clear all

%%
% Create continuation object
a{1}=auto;

%%
% Display the function
type(a{1}.s.FuncFileName);

%%
% Set initial conditions
[a{1}.s.Par0,a{1}.s.U0,a{1}.s.Out0]=stpnt;

% Load constants
a{1}.c=cabc1(a{1}.c);

%%
% Display constants
a{1}.c

%%
% Run equilbrium solutions
a{1}=runauto(a{1});

%% Compute a family of periodic solutions from the first Hopf point.
% In the constants-file (|cabc2.m|) for the second run. we note that:
%
% * |IPS=2|: a family of periodic solutions is computed.
% # |IRS=2|: the starting point is the solution with label 2,
%   (a Hopf bifurcation point), to be read from the solutions-object 
%   |a{1}.f8|.
% # |NICP=2|: there are two continuation parameters (namely |PAR(1)|, 
%   and the period, |PAR(11)|).
% # |NUZR=1|: there is one user output point, now at |PAR(1)=0.25|,
% where the calculation is to terminate, since the index ("-1") is negative.
a{2}=a{1};
a{2}.c=cabc2(a{2}.c);
a{2}=runauto(a{2});


%% Compute a family of periodic solutions from the second Hopf point
a{3}=a{1};
a{3}.c=cabc3(a{3}.c);
a{3}=runauto(a{3});

%% Compute a family of periodic solutions from the third Hopf point
a{4}=a{1};
a{4}.c=cabc4(a{4}.c);
a{4}=runauto(a{4});

%% Compute a family of periodic solutions from the fifth Hopf point
a{5}=a{1};
a{5}.c=cabc5(a{5}.c);
a{5}=runauto(a{5});

%% Plot the information
p=plautobj;
ploteq(p,a);

