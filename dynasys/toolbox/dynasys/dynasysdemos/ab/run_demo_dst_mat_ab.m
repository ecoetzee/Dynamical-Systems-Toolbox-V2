%% Matlab example with demo 'ab'in DST mode
% This demonstration uses the 'ab' example of AUTO to demonstrate the use
% of Matlab functions with the Dynamical Systems Toolbox in the DST mode.
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
clear all

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
[T,X]=ode45(@(t,U) abode(t,U,ts,PAR0),ts,U0,options);

%%
% Plot the results out. We can see that the state derivatives are zero.
% This is a trivial example seeing that we could have checked this by hand,
% but the method is applicable for more complex models.
h=plot(T,X);
set(h,'LineWidth',2);
title('Time history for Matlab Demo: ab');
xlabel('Time (s)');
ylabel('U(1),U(2)');
snapnow;
close(gcf);

%% Set simulation options and initial conditions
% We can now use the final states of the simulation as starting conditions
% for the continuation runs. If the derivatives for these states are not 
% zero, the continuation runs will fail. 

%%
% Create an auto object. Some of the inherited objects are of handle class,
% hence the properties in the class act like pointers. If you copy the
% objects directly, the properties in both objects will change, if the
% properties in one changes. Hence make sure to create a new object. In our
% case we are not interested in saving the objects at the end, so we allow
% the values to change. Go read up on the difference between Value and
% Handle classes.
a{1}=auto;

%%
% Define the function file and intial conditions
a{1}.s.FuncFileName='abmatfunc';
a{1}.s.Par0=PAR0(:,end);
a{1}.s.U0=X(end,:);
a{1}.s.Out0=[];

%%
% Print function file to screen.
type(a{1}.s.FuncFileName);

%% Set the constants and run the stationary solutions
% We can set the constants from the command line, but for ease of use we
% can convert the constants file from the original fortran code with the |convertchc|
% function, and then we can read the constants in from the m-file. 
a{1}.c=cab1(a{1}.c);

%%
% Show format of set commands
type('cab1.m');

%%
% Print constants to screen.
a{1}.c

%% Run an equilibrium solution 
% Compute stationary solutions by using the |runauto| method in the |auto|
% object
a{1}=runauto(a{1});

%% Check contents written to objects
% In the Fortran version of AUTO the results are written to the fort.7,
% fort.8 and fort.9 files. This is still an option in the DST mode, but
% it is not the default.
% 
% Print f7 contents to screen.
a{1}.f7

%%
% Print f8 contents to screen.
a{1}.f8

%% Plot stationary solutions
% We have seen from the output to the MATLAB command window that there are 
% two limit points and one Hopf bifurcation. We have written a small 
% routine to extract the data from the |fort.7| output file. You can try to
% write your own routine, or you can use the simple routine |ploteq|. Plot
% the results out to see what the bifurcation diagram looks like. We plot 
% the continuation paramter |PAR(1)| against the |U(1)| parameter. Note the 
% plotting conventions that are used. Solid lines are used for stable 
% equilibrium states, and broken lines for unstable equilibrium states.
% The limit points (LP) indicate a qualitative change in the stability, 
% while the Hopf bifurcation (HB) indicates a transition from a steady 
% state to a limit cycle. 
p=plautobj;
set(p,'xEqStr','P(1)','xLab','Par(1)','yEqStr','U(1)','yLab','U(1)','axLim',[0,0.16,0,1.1]);
ploteq(p,a);
snapnow;


%% Compute periodic solutions
% We want to do the continuation from the Hopf Bifurcation to determine the 
% amplitude  and frequency of the limit cycle as we change the parameter.
%
% Copy the information in the first object to the second one. Set
% constants by cloning constants object. Remember that we are using handle 
% classes and that we do not want them to act like pointers, hence use copy
% method from the autoconstants object.
a{2}=a{1};
a{2}.c=copy(a{2}.c);
a{2}.c=cab2(a{2}.c);

% Run continuation
a{2}=runauto(a{2});
 

%% Plot periodic solutions 
% Add the minimum/maximum of the limit cycle. 
plotlceq(p,a);
snapnow;
close(gcf);

%%
% Plot the phase plane of the limit cycles with labels 6, 8 and 10
set(p,'lcLab',[6,8,10],'xEqStr','U(1)','xLab','U(1)','yEqStr','U(2)','yLab','U(2)');
plotlcph(p,a);
snapnow;
close(gcf);

%%
% Plot the response against the normalised period
plotlcpr(p,a);
snapnow;
close(gcf);

%% Two-parameter continuation, follow locus of lower limit point (Forward)
% Trace out the locus of the lower limit point while stepping in a 
% *forward* direction. We vary the first and the thrid parameters for the 
% continuation, so restart from label 2 in this first continuation, 
% hence we need to copy the restart file from the first run to the 
% appropriate name. Create new object and set constants.
a{3}=a{1};
a{3}.c=copy(a{3}.c);
a{3}.c=cab3(a{3}.c);

% Run continuation
a{3}=runauto(a{3});


%% Two-parameter continuation, follow locus of lower limit point (Backward)
% Trace out the locus of the lower limit point while stepping in a 
% *backward* direction. We vary the first and the thrid parameters for the 
% continuation, so restart from label 2 in this first continuation, 
% hence we need to copy the restart file from the first run to the 
% appropriate name. Create new object and set constants.
a{4}=a{1};
a{4}.c=copy(a{4}.c);
a{4}.c=cab4(a{4}.c);

% Run continuation
a{4}=runauto(a{4});
 
%% Two-parameter continuation, follow locus of hopf point (Backward)
% Trace out the locus of the Hopf Bifurcation while stepping in a *backward*
% direction. We vary the first and the third parameters for the 
% continuation, so restart from label 4 in this first continuation, 
% hence we need to copy the restart file from the first run to the 
% appropriate name. Create a new object and set constants.
a{5}=a{1};
a{5}.c=copy(a{5}.c);
a{5}.c=cab5(a{5}.c);

% Run continuation
a{5}=runauto(a{5});

%% Plot the locii of the limit points and Hopf bifurcation
% Add the locii of the limit points and hopf bifurcation
p=plautobj;
set(p,'xEqStr','P(1)','xLab','Par(1)','yEqStr','U(1)','yLab','U(1)','axLim',[0,0.16,0,1.1]);
ploteq(p,a);
snapnow;
close(gcf);

displayEndOfDemoMessage(mfilename)
