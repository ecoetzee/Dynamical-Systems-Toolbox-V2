%% MATLAB EXAMPLE WITH DEMO 'ab'
% This demonstration uses the 'ab' example of AUTO to demonstrate the use
% of the different files and also how to script several runs of AUTO.

%   Written by Phani THOTA, James RANKIN, Etienne COETZEE
%   University of Bristol
% $Revision: 1.0.0.0 $ $Date: 2009/03/19 09:57:05 $

%% Benchmark for auto. Uses demo:ab for the benchmark
ptype='NEW_MATLAB_AUTO07P_SPLIT_FUNC_FILES_OUT_200';
iters=10;
t=[];

for i=1:iters

%% 1. FIND EQUILIBRIUM OF SYSTEM
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

% % Choose/guess initial conditions
% PAR0=[0,14,2;0,14,2]';
% ts=[0,100]';
% U0=[0,0]';
% 
% % Run simulation. The equations need to be defined in a separate function 
% % file 'ab.m' to that of 'AUTOEQN.m', so that we can use the ode45 solver.
% options=simset('MaxStep',0.1);
% [T,Y]=ode45(@(t,U) abode(t,U,ts,PAR0),ts,U0,options);

% % Plot the results out. We can see that the state derivatives are zero.
% % This is a trivial example seeing that we could have checked this by hand,
% % but the method is applicable for more complex models.
% h=plot(T,Y);
% set(h,'LineWidth',2);
% title('Time history for Demo: ab');
% xlabel('Time (s)');
% ylabel('U(1),U(2)');
% snapnow;
% close(gcf);

%% 2. ADJUST EQUATIONS FILE TO CONTAIN CORRECT INITIAL CONDITIONS
% We can now use the final states of the simulation as starting conditions
% for the continuation runs. If the derivatives for these states are not 
% zero, the continuation runs will fail. Update the 'STPNT' function in the
% 'AUTOEQN.m' file to reflect the initial conditions.

% We already have a file ('AUTOEQN_ab.m') that is set up correctly, so copy
% the file to the standard naming convention of 'AUTOEQN.m'  
%copyfile('AUTOEQN_matab.m','AUTOEQN.m');

%% 3. COMPUTE STATIONARY SOLUTIONS
% Generate an AUTO restart file by running AUTO.

% make sure no restart files are present
delete s.*

% copy constants file
copyfile('c.ab.1','c.ab');
 
% Run AUTO
tic;
auto07run('ab');
t(1,i)=toc;

% save data to output file
sv('matab1');

% %% 4. PLOT STATIONARY SOLUTIONS
% % We have seen from the output to the MATLAB command window that there are 
% % two limit points and one Hopf bifurcation. We have written a small 
% % routine to extract the data from the fort.7 output file. You can try this
% % yourself or you can use the following routine. Plot this out to see what  
% % the bifurcation diagram looks like. We plot the continuation paramter
% % PAR(2) against the L2-NORM parameter. Note the plotting conventions that 
% % are used. Solid green lines are used for stable uquilibrium states, and 
% % broken red lines for unstable equilibrium states. The limit points (LP) 
% % indicate a qualitative change in the stability, while the Hopf 
% % bifurcation (HB) indicates a transition from a steady state to a 
% % limit cycle. 
% ploteq('matab1','PAR(2)','L2');
% snapnow;

%% 5. COMPUTE PERIODIC SOLUTIONS
% We want to do the continuation from the Hopf Bifurcation to determine the 
% amplitude  and frequency of the limit cycle as we change the parameter.

% Copy the constants file
copyfile('c.ab.2','c.ab');
 
% Run AUTO
tic;
auto07run('ab');
t(2,i)=toc;
 
% append data
sv('matab2');

% %% 6. PLOT PERDIODIC SOLUTIONS
% % Add the L2-NORM of the Hopf-bifurcation. This shows up as magenta in the
% % plot below.
% ploteq('matab2','PAR(2)','L2');
% snapnow;

%% 7. TWO-PARAMETER CONTINUATION, FOLLOW LOCUS OF LOWER LIMIT POINT (Forward)
% Trace out the locus of the lower limit point while stepping in a 
% *forward* direction. We vary the first and the thrid parameters for the 
% continuation, so restart from label 2 in this first continuation, 
% hence we need to copy the restart file from the first run to the 
% appropriate name.
copyfile('s.matab1','s.ab');

% Copy the constants file
copyfile('c.ab.3','c.ab');
 
% Run AUTO
tic;
auto07run('ab');
t(3,i)=toc;
 
% append data
sv('matab3');


%% 8. TWO-PARAMETER CONTINUATION, FOLLOW LOCUS OF LOWER LIMIT POINT (Backward)
% Trace out the locus of the lower limit point while stepping in a 
% *backward* direction. We vary the first and the thrid parameters for the 
% continuation, so restart from label 2 in this first continuation, 
% hence we need to copy the restart file from the first run to the 
% appropriate name.
copyfile('s.matab1','s.ab');

% Copy the constants file
copyfile('c.ab.4','c.ab');
 
% Run AUTO
tic;
auto07run('ab');
t(4,i)=toc;
 
% append data
sv('matab4'); 
 

%% 9. TWO-PARAMETER CONTINUATION, FOLLOW LOCUS OF HOPF POINT (Backward)
% Trace out the locus of the Hopf Bifurcation while stepping in a *backward*
% direction. We vary the first and the thrid parameters for the 
% continuation, so restart from label 4 in this first continuation, 
% hence we need to copy the restart file from the first run to the 
% appropriate name.
copyfile('s.matab1','s.ab');

% Copy the constants file
copyfile('c.ab.5','c.ab');
 
% Run AUTO
tic;
auto07run('ab');
t(5,i)=toc;
 
% append data
sv('matab5');

% %% 10. PLOT THE LOCII OF THE LIMIT POINTS AND HOPF BIFURCATION
% % Add the locii of the limit points and hopf bifurcation
% ploteq('matab3','PAR(2)','L2');
% ploteq('matab4','PAR(2)','L2');
% ploteq('matab5','PAR(2)','L2');
% snapnow;
% close(gcf);

% %% 11. PLOT THE LIMIT CYCLE RESPONSE
% % We have written a small routine to extract the data from the fort.8
% % output file for the second run. Plot the response of the limit cycle
% % behaviour at points 6, 8, 10. AUTO normalises the response so that the
% % period is equal to 1 second. The plot on the left shows the response of
% % state U(2), while the plot on the right shows a phase plot.
% plotlc('matab2');
% snapnow;
% close(gcf);

% displayEndOfDemoMessage(mfilename)

end

%% Write output to file
fid=fopen([ptype,'.txt'],'w');
t=t(:,1:iters);
[m,n]=size(t);
str=upper(ptype);
str=strrep(str,'_',' ');
fprintf(fid,'BENCHMARK FOR AUTO : %s\n\n',str);
fprintf(fid,'RUN           %s',sprintf('%12.0f',[1:n]));
fprintf(fid,'         AVG\n\n');
fprintf(fid,'STATIONARY    :  %s',sprintf('%12.5f',t(1,:)));
fprintf(fid,' %10.5f\n',sum(t(1,:))/n);
fprintf(fid,'PERIODIC      :  %s',sprintf('%12.5f',t(2,:)));
fprintf(fid,' %10.5f\n',sum(t(2,:))/n);
fprintf(fid,'LP FORWARD    :  %s',sprintf('%12.5f',t(3,:)));
fprintf(fid,' %10.5f\n',sum(t(3,:))/n);
fprintf(fid,'LP BACKWARD   :  %s',sprintf('%12.5f',t(4,:)));
fprintf(fid,' %10.5f\n',sum(t(4,:))/n);
fprintf(fid,'HB BACKWARD   :  %s',sprintf('%12.5f',t(5,:)));
fprintf(fid,' %10.5f\n',sum(t(5,:))/n);

fclose(fid);

disp('Finished with benchmark')