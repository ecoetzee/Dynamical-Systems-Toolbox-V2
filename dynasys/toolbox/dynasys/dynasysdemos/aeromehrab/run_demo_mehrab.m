%% Bifurcation analysis of nonlinear Mehra-B flight mechanics model
%% Introduction
% One of the classic examples in nonlinear flights dynamics, was 
% demonstrated by Mehra and Carroll. They developed a model called the 
% Mehra-B aircraft model, which has become a standard model for the 
% testing and familiarisation of bifurcation analysis software.  The actual 
% model itself was originally presented by Etkin (1972) and is a generic 
% small manoeuvrable single-engined jet aeroplane.  It was devised to 
% be used as an example of pitch/roll coupling.  
% 
%% Assumptions
% The model aerodynamic data is made up of constant coefficients rather
% than complex tables which allows fast runtime and ensures the 
% smoothness required by the toolbox. Assumptions made for this model are 
% that the speed (V) and air density are constant.  Another simplification 
% made was that gravity terms can be ignored which, while this would 
% not be an appropriate assumption in developed spins and would reduce 
% peak responses it would not have an appreciable effect of the dynamics 
% for Aircraft B as with V being large the term g/V would be small and 
% hence negligible.  These assumptions and simplifications were used 
% so at to be comparable to Mehra (1977), Gilmour (1981) and 
% Lowenberg (1991).  
%
%% Equations of motion
% For implementation a 5th order model was used with the equations of 
% motion being as follows:
% 
% $\dot{\alpha} = q - p\beta + z_{\alpha}\Delta\alpha + z_{\delta_e}\delta_e$
% 
% $\dot{\beta} = y_{\beta}\beta+p(sin\alpha_0 + \Delta\alpha) - rcos\alpha_0 + y_{\delta_a} \delta_\alpha + y_{\delta_r}\delta_r$
% 
% $\dot{p} = l_{\beta} \beta + l_qq + l_rr + l_pp - i_1qr + l_{\delta_a}\delta_a + l_{\delta_r}\delta_r$
%
% $\dot{q} = = \bar{m}_{\alpha} \Delta_{\alpha} + \bar{m}_q + i_2pr + m_{\delta_e}\delta_e - m_{\dot{\alpha}}p\beta$
%
% $\dot{r} = n_\beta\beta + n_rr + n_pp - i_3pq + n_{\delta_a}\delta_a + n_{\delta_r}\delta_r$
% 
% Where:
% 
% $\alpha = \alpha_0 + \Delta\alpha$
% 
% $\bar{m}_\alpha = m_\alpha + m_{\dot{\alpha}}z_\alpha$
% 
% $\bar{m}_q = m_q + m_{\dot{\alpha}}$
% 
% $i_1 = \frac{I_z-I_y}{I_x}$ 
% 
% $i_2 = \frac{I_z-I_x}{I_y}$ 
% 
% $i_3 = \frac{I_y-I_x}{I_z}$
% 
% The main source of non-linearity in the model is from the inertial 
% coupling terms $-i_1qr$, $i_2pr$ and $-i_3pq$.  Aerodynamic non-linearities 
% exist in the form of $-m_\alpha$, $p\beta$ and $-p\beta$. All 
% results presented here have a rudder deflection value of $0^\circ$ as all 
% coefficients with respect to the rudder are zero and hence it has no 
% effect on any of the results regardless of the deflection.

%% Using Aileron Deflection as the Continuation Parameter
% The results presented in this section are where the aileron deflection 
% $\delta_a$ was used as the parameter to be varied in the bifurcation 
% diagram.  The bifurcation diagrams shown are for different elevator 
% settings with the specific $\delta_e$ chosen to match those diagrams presented 
% in reference Lowenberg (1991).
clear all

%% 
% Calculate an initial starting point solution
%To run the AUTO program an initial equilibrium point is needed so as to
%run the continuation method.  This example uses the in built Matlab ODE
%solver ODE45 to run the simulation until the derivatives are zero.

% Create a model object. We use an object that creates all the default
% values for the model, and methods for simulation etc.
ac=modelobj;

% Choose/guess initial conditions, use defaults.
ac=siminit(ac);

% Display control surface input values in degrees.
ac.inp

% Simulate
ac=modelsim(ac);

%%
% Plot the results out to check that the states have converged to values
% where the derivatives are zero.
plot(ac.smd.T,ac.smd.X(:,1:5));
title('Time history for Bifurcation States, Demo: Aircraft B');
xlabel('Time (s)');
ylabel('alpha, beta, p, q, r');
legend({'\alpha','\beta','p','q','r'});
axis([0 20 -15 5]);
snapnow;
close(gcf);

% Plot the Euler angles
plot(ac.smd.T,ac.smd.X(:,6:8));
title('Time history for Euler Angles, Demo: Aircraft B');
xlabel('Time (s)');
ylabel('\psi, \theta, \phi');
legend({'\psi','\theta', '\phi'});
axis([0 20 -15 5]);
snapnow;
close(gcf);

%% 
% Set up the AUTO continuation run.
% Create an auto object.
a{1}=auto;

%%
% Define the function file and intial conditions.
a{1}.s.FuncFileName='mehrafunc';
a{1}.s.SimulinkModel=ac.SimulinkModel;
a{1}.s.Par0=ac.bif.Par0;
a{1}.s.U0=ac.bif.U0;
a{1}.s.Out0=ac.bif.Out0;

%%
% Print function file to screen.
type(a{1}.s.FuncFileName);

%% 
% Set the AUTO constants
% Load constants from the constant m-file.
a{1}.c=aircraftbconst(a{1}.c);

%%
% Print constants to screen.
a{1}.c

%% 
% Run an equilibrium solution 
% Compute stationary solutions by using the |runauto| method in the |auto|
% object.
a{1}=runauto(a{1});

%% 
% Plot the equilibrium surface
% In this demo the selected solution measure is roll rate p.  For other
% solution measures change the value of c.Iplt in aircraftbconst to the
% following:
%
% * c.Iplt=1 for alpha
% * c.Iplt=2 for beta
% * c.Iplt=3 for roll rate p
% * c.Iplt=4 for pitch rate q
% * c.Iplt=5 for yaw rate r

%The solution measure is now plotted against the continuation parameter.
p=plautobj;
set(p,'yEqStr','u(3)');
set(p,'xLab','da (deg)');
set(p,'yLab','p (deg/s)');
ploteq(p,a);
set(gcf,'Name','Bifurcation Diagram','NumberTitle','off');
xlim([-40 40]);

%% 
% Run a simulation of the model with a slow ramp on the aileron
% This section shows how the aircraft would actually behave when subjected to
% a slow ramp on the aileron.  This is equivalent to the pilot initiating a
% gradual roll.
PAR0=[0,ac.inp.de,ac.inp.dr;
     3,ac.inp.de,ac.inp.dr;
     5,ac.inp.de,ac.inp.dr;
      5,ac.inp.de,ac.inp.dr;
      0,ac.inp.de,ac.inp.dr;
      20,ac.inp.de,ac.inp.dr;
      20,ac.inp.de,ac.inp.dr;
      0,ac.inp.de,ac.inp.dr;
      0,ac.inp.de,ac.inp.dr];
ac.smd.Te=100;
t=[0,15,40,58,60,65,80,82,ac.smd.Te]';
ac.smd.Pi=[t,PAR0];

ac=modelsim(ac);

% Overlay simulation result onto the bifurcation diagram.
bifth=[ac.smd.Y(:,9),ac.smd.X(:,3)];
plot(bifth(:,1),bifth(:,2),'m','LineWidth',2);
snapnow;

%% 
% Create Animation with four different figures
h=[0;0];
h(1)=p.handle;

ac.AnimModel='GRIPEN';

% Create figure group and dock 
group = setfigdocked('GroupName','F100 Inertia Coupling','GridSize',[1 2],'Maximize',1,'groupdocked',0);
group = setfigdocked('GroupName','F100 Inertia Coupling','Figure',p.handle,'Figindex',1);

% Create animation of aircraft
h(2)=flightviz(ac.AnimModel);
group = setfigdocked('GroupName','F100 Inertia Coupling','Figure',h(2),'Figindex',2);

% Animate
flightanim(ac,h,bifth)
return

%% Using Elevator Deflection as the Continuation Parameter.
% In this section the presented bifurcation diagrams used $\delta_e$ as the 
% continuation parameter while keeping $\delta_a$ constant.

% Set control surface input values.
ac.inp.da=0;
ac.inp.de=0;
ac.inp.dr=0;

ac=siminit(ac);

% Simulate and set initial conditions for bifurcation analysis
ac=modelsim(ac);

a{2}=a{1};
a{2}.c=copy(a{1}.c);
a{2}.c.Icp=2;
a{2}.c.Ds=0.001;
a{2}.s.Par0=ac.bif.Par0;
a{2}.s.U0=ac.bif.U0;
a{2}.s.Out0=ac.bif.Out0;

a{2}=runauto(a{2});
p=plautobj;

PAR0=[0,0,0;
      0,-20,0];
ac.smd.Te=60;
t=[0,100]';
ac.smd.Pi=[t,PAR0];

ac=modelsim(ac);

h=flightviz('F100D');

% Animate
flightanim(ac,h)
