%% Matlab example with demo 'ab' in 07P mode
% This demonstration uses the 'ab' example of AUTO to demonstrate the use
% of the Dynamical Systems Toolbox in the 07P (AUTO) mode. Input and output
% files are similar to the ones used in AUTO.
%
%% Find the equilibrium of the system
% The continuation run has to start from an equilibrium state, or a stable
% limit cycle condition. Most applications will start from an equilibrium 
% condition. This equilibrium is also known as a quasi-steady state, 
% or a trim condition. There are two ways to obtain such an equilibrium:
% 
% # Run the simulation for a sufficient period of time, until the
% derivatives (of the states of interest) are equal to zero. This is the
% easiest option.
% # Use a trim routine to obtain a trim point.
%

%%
% Choose/guess initial conditions
PAR0=[0,14,2;0,14,2]';
ts=[0,10]';
U0=[0,0]';

%%
% Run simulation. The equations need to be defined in a separate function 
% file |abode.m| to that of the continuation function file, so that we can 
% use the ode45 solver.
options=simset('MaxStep',0.1);
[T,Y]=ode45(@(t,U) abode(t,U,ts,PAR0),ts,U0,options);

%%
% Plot the results out. We can see that the state derivatives are zero.
% This is a trivial example seeing that we could have checked this by hand,
% but the method is applicable for more complex models.
h=plot(T,Y);
set(h,'LineWidth',2);
title('Time history for Demo: ab');
xlabel('Time (s)');
ylabel('U(1),U(2)');
snapnow;
close(gcf);

%% Adjust equations file to contain correct initial conditions
% We can now use the final states of the simulation as starting conditions
% for the continuation runs. If the derivatives for these states are not 
% zero, the continuation runs will fail. Update the |stpnt.m| function
% to reflect the initial conditions.

%%
% Create an auto object
a=auto;

%%
% Define input and output files
a.s.RunMode='07P';
a.s.FuncFileName='abmatfunc';
a.s.OutFileName='ab';

%% Compute stationary solutions
% Generate an AUTO restart file by running AUTO.
%
%%
% Make sure no restart files are present
delete s.*

%%
% Copy constants file
copyfile('c.ab.1','c.ab');
 
%%
% Run continuation
runauto(a);
 
%%
% Save data to output files - |b.ab|, |d.ab|, |s.ab|
sv('matab1');

%% Plot stationary solutions
% We have seen from the output to the MATLAB command window that there are 
% two limit points and one Hopf bifurcation. We have written a small 
% routine to extract the data from the |fort.7| output file. You can try to
% write your own routine, or you can use the simple routine |ploteq|. Plot
% the results out to see what the bifurcation diagram looks like. We plot 
% the continuation paramter |PAR(2)| against the |L2-NORM| parameter. Note the 
% plotting conventions that are used. Solid lines are used for stable 
% equilibrium states, and broken lines for unstable equilibrium states.
% The limit points (LP) indicate a qualitative change in the stability, 
% while the Hopf bifurcation (HB) indicates a transition from a steady 
% state to a limit cycle. 
ploteq('matab1','PAR(2)','L2');
snapnow;

%% Compute periodic solutions
% We want to do the continuation from the Hopf Bifurcation to determine the 
% amplitude  and frequency of the limit cycle as we change the parameter.

%%
% Copy the constants file
copyfile('c.ab.2','c.ab');
 
%%
% Run continuation
runauto(a);
 
%%
% Save the data into different files
sv('matab2');

%% Plot periodic solutions
% Add the L2-NORM of the Hopf-bifurcation. This shows up as magenta in the
% plot below.
ploteq('matab2','PAR(2)','L2');
snapnow;

%% Two-parameter continuation, follow locus of lower limit point (Forward)
% Trace out the locus of the lower limit point while stepping in a 
% *forward* direction. We vary the first and the thrid parameters for the 
% continuation, so restart from label 2 in this first continuation, 
% hence we need to copy the restart file from the first run to the 
% appropriate name.
copyfile('s.matab1','s.ab');

%%
% Copy the constants file
copyfile('c.ab.3','c.ab');
 
%%
% Run continuation
runauto(a);
 
%%
% Save the data
sv('matab3');

%% Two-parameter continuation, follow locus of lower limit point (Backward)
% Trace out the locus of the lower limit point while stepping in a 
% *backward* direction. We vary the first and the thrid parameters for the 
% continuation, so restart from label 2 in this first continuation, 
% hence we need to copy the restart file from the first run to the 
% appropriate name.
copyfile('s.matab1','s.ab');

%%
% Copy the constants file
copyfile('c.ab.4','c.ab');
 
%%
% Run continuation
runauto(a);
 
%%
% Save the data
sv('matab4'); 
 

%% Two-parameter continuation, follow locus of Hopf point (Backward)
% Trace out the locus of the Hopf Bifurcation while stepping in a *backward*
% direction. We vary the first and the thrid parameters for the 
% continuation, so restart from label 4 in this first continuation, 
% hence we need to copy the restart file from the first run to the 
% appropriate name.
copyfile('s.matab1','s.ab');

% Copy the constants file
copyfile('c.ab.5','c.ab');
 
%%
% Run continuation
runauto(a);
 
%%
% Save the data
sv('matab5');

%% Plot the locii of the limit points and Hopf bifurcation
% Add the locii of the limit points and Hopf bifurcation
ploteq('matab3','PAR(2)','L2');
ploteq('matab4','PAR(2)','L2');
ploteq('matab5','PAR(2)','L2');
snapnow;
close(gcf);

%% Plot the limit cycle response
% We have written a small routine to extract the data from the |fort.8|
% output file for the second run. Plot the response of the limit cycle
% behaviour at points 6, 8, 10. AUTO normalises the response so that the
% period is equal to 1 second. The plot on the left shows the response of
% state |U(2)|, while the plot on the right shows a phase plot.
plotlc('matab2');
snapnow;
close(gcf);

displayEndOfDemoMessage(mfilename)
