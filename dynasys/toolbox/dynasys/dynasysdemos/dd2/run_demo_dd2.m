%% Fixed Points of a Discrete Dynamical System (Demo : dd2)
% This demo illustrates the computation of a solution family and its 
% bifurcating families for a discrete dynamical system. Also illustrated is
% the continuation of Naimark-Sacker (or Hopf) bifurcations. The equations,
% a discrete predator-prey system, are
%
% : $u_1^{k+1} =p_1u_1^{k}(1-u_1^{k})-p_2u_1^{k} u_2^{k}$,
%
% : $u_2^{k+1} =(1-p_3)u_2^{k}+p_2u_1^{k}u_2^{k}$,
% 
% * In the first run $p_1$ is free.
% * In the second run, both $p_1$ and $p_2$ are free.
% * The remaining equation parameter, $p_3$, is fixed in both runs.
% 

%% 
% Create continuation object and set initial conditions.
a{1}=auto;

%%
% Print function file to screen. Note in this case that the user is
% supplying the derivative values, hence |ijac > 0|.
type(a{1}.s.FuncFileName);

%%
% Set initial conditions.
[a{1}.s.Par0,a{1}.s.U0,a{1}.s.Out0]=stpnt;

%%
% Set constants.
a{1}.c=cdd21(a{1}.c);

%%
% Run equilibrium continuation.
a{1}=runauto(a{1});

%% 
% Create second object for restart
a{2}=a{1};
a{2}.c=cdd22(a{1}.c);

%%
% Run two parameter continuation
a{2}=runauto(a{2});

%%
% Plot the solutions. The plotting routine needs to be modified, because it
% seems as if the end points are being connected (represented by the straight lines).
p=plautobj;
set(p,'xLab','Par','yLab','L2-norm');
ploteq(p,a);
snapnow;


