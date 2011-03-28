%% The Lorenz Equations (Demo : lrz)
% This demo computes two symmetric homoclinic orbits in the Lorenz equations
%
% : $u_1' =  p_3 (u_2 - u_1)$,
%
% : $u_2' =  p_1 u_1 - u_2 - u_1 u_3$,  
%
% : $u_3' =  u_1 u_2 - p_2 u_3$.
% 
% 
% 
% * Here $p_1$ is the free parameter, and $p_2=8/3$, $p_3=10$.
% * The two homoclinic orbits correspond to the final, large period orbits
%   on the two periodic solution families.
%

%% 
% Create continuation object and set initial conditions.
a{1}=auto;

%%
% Print function file to screen.
type(a{1}.s.FuncFileName);

%%
% Set initial conditions.
[a{1}.s.Par0,a{1}.s.U0,a{1}.s.Out0]=stpnt;

%%
% Set constants.
a{1}.c=clrz1(a{1}.c);

%%
% Run equilibrium continuation.
a{1}=runauto(a{1});

%% 
% Create second object for restart
a{2}=auto;
a{2}.f8=a{1}.f8;
a{2}.c=clrz2(a{1}.c);

%%
% Compute periodic solutions; the final orbit is near-homoclinic from label
% 4
a{2}=runauto(a{2});

%% 
% Create third object for restart
a{3}=auto;
a{3}.f8=a{1}.f8;
a{3}.c=clrz3(a{3}.c);

%%
% Compute the symmetric periodic solution family from label 6
a{3}=runauto(a{3});

%% 
% Create fourth object for restart
a{4}=auto;
a{4}.f8=a{3}.f8;
a{4}.c=clrz4(a{4}.c);

%%
% Compute the symmetric periodic solution family from label 9
a{4}=runauto(a{4});

%% 
% Plot the solution
% Create plaut object and plot solution. 
p=plautobj;
set(p,'xLab','Par','yLab','L2norm');
ploteq(p,a);
