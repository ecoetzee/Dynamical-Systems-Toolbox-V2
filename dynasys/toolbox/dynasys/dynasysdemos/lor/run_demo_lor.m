%% Starting an Orbit from Numerical Data (Demo : lor)
% This demo illustrates how to start the computation of a family of periodic 
% solutions from numerical data obtained, for example, from an initial value 
% solver. As an illustrative application we consider the Lorenz equations
%  
% : $u_1' =  p_3 (u_2 - u_1)$,
%  
% : $u_2' =  p_1 u_1 - u_2 - u_1 u_3$,  
%  
% : $u_3' =  u_1 u_2 - p_2 u_3$.
%  
% Numerical simulations with a simple initial value solver show the existence of 
% a stable periodic orbit when $p_1 = 280$, $p_2 = 8/3$, $p_3 = 10$. Numerical 
% data representing one complete periodic oscillation are contained in the file 
% |lor.dat| and then placed in the f8 object. 
%  

clear all

%% Starting data
% Numerical data representing one complete periodic oscillation are contained in 
% the file lor.dat. Each row in lor.dat contains four real numbers, namely, the 
% time variable t, u1, u2 and u3. These values can also be obtained from a 
% simulation and then placed in the f8 object. The correponding parameter values 
% are defined in |a{1}.s.Par0|. The constant a{1}.c.Ips then allows for using
% the data in |lor.dat| where we also specify a{1}.c.Irs=0. The mesh will be
% suitably adapted to the solution, using the number of mesh intervals a{1}.c.Ntst
% and the number of collocation point per mesh interval a{1}.c.Ncol specified in the 
% constants-file |clor1.m|.
dat=textread('lor.dat');

%% Compute a solution family, restart from |lor.dat| 
% Create first object
a{1}=auto;
a{1}.c=clor1(a{1}.c);
a{1}.s.Par0=[280,8/3,10];

% Populate f8 object with data from text file
a{1}=dat2f8(a{1},dat);

% First run
a{1}=runauto(a{1});

%%
% Plot the phase plane of the limit cycles with labels 2 and 3
p=plautobj;
set(p,'lcLab',[2,3],'xEqStr','U(1)','xLab','U(1)','yEqStr','U(2)','yLab','U(2)');
plotlcph(p,a{1});
snapnow;
close(gcf);

%%
% Plot the response against the normalised period
plotlcpr(p,a);
snapnow;
close(gcf);

%% Switch branches at a period-doubling detected in the first run
% Create second object
a{2}=a{1};
a{2}.c=copy(a{1}.c);
a{2}.c=clor2(a{2}.c);

a{2}=runauto(a{2});

%%
% Plot the phase plane of the limit cycles with labels 4 and 5
set(p,'lcLab',[4,5],'xEqStr','U(1)','xLab','U(1)','yEqStr','U(2)','yLab','U(2)');
plotlcph(p,a{2});
snapnow;
close(gcf);

%%
% Plot the response against the normalised period
plotlcpr(p,a{2});
snapnow;
close(gcf);

%% Switch branches at a period-doubling detected in the second run
% Create third object
a{3}=a{2};
a{3}.c=copy(a{1}.c);
a{3}.c=clor3(a{2}.c);

a{3}=runauto(a{3});

%%
% Plot the phase plane of the limit cycles with labels 6 and 7
set(p,'lcLab',[6,7],'xEqStr','U(1)','xLab','U(1)','yEqStr','U(2)','yLab','U(2)');
plotlcph(p,a{3});
snapnow;
close(gcf);

%%
% Plot the response against the normalised period
plotlcpr(p,a{3});
snapnow;
close(gcf);

displayEndOfDemoMessage(mfilename)
