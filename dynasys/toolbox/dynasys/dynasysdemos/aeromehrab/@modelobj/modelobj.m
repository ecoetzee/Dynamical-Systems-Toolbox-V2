classdef modelobj
    % MODELOBJ create a model object
    %   This object contains all the information that is needed for running
    %   and finding equilibrium states.
    
    properties
        SimulinkModel='AircraftB';  % Simulink model name
        FuncFileName =[];           % ODE function file name
        AnimModel='F100D'           % Model for animation
        dat = [];                   % Data placeholder  
        inp = [];                   % Inputs placeholder
        smd = [];                   % Simulation data placeholder
        bif = [];                   % Bifurcation starting values placeholder
    end
         
    methods

        %------------------------------------------------------------------
        %% CONSTRUCTOR - BUILD OBJECT 
        %------------------------------------------------------------------       
        function obj=modelobj(varargin)
             [obj.dat,obj.inp]=acdat;         % load from data file
             obj.smd=acsmd;                   % simulation data
             obj.bif=acbif;                   % bifurcation initial data
        end

        %------------------------------------------------------------------
        %% SET UP DEFAULT INITIAL CONDITIONS
        %------------------------------------------------------------------
        function obj=siminit(obj)
            
            % check if control inputs are defined
            if isempty(obj.inp.da)
                warning('MehraModel:MissingInput','Aileron input undefined, setting da=0');
                obj.inp.da=0;
            end
            if isempty(obj.inp.de)
                warning('MehraModel:MissingInput','Elevator input undefined, setting de=0');
                obj.inp.de=0;
            end
            if isempty(obj.inp.dr)
                warning('MehraModel:MissingInput','Rudder input undefined, setting dr=0');
                obj.inp.dr=0;
            end
            
            % Set inputs and intial conditions conditions
            u=[obj.inp.da,obj.inp.de,obj.inp.dr;obj.inp.da,obj.inp.de,obj.inp.dr];
            t=[0,obj.smd.Te]';
            obj.smd.Pi=[t,u];
            
            obj.smd.Xi=[0,0,0,0,0,0,0,0];
        end
        
        %------------------------------------------------------------------
        %% RUN SIMULATION
        %------------------------------------------------------------------
        function obj=modelsim(obj)
               
            if isempty(obj.smd.Xi) || isempty(obj.smd.Pi)
                error('MehraModel:SimulationError','No input states or input parameters defined for simulation');
            else
                
                % Set initial conditions and run simulation
                open_system(obj.SimulinkModel);
                options=simset('InitialState',obj.smd.Xi);
                [t,x,y]=sim(obj.SimulinkModel,0:obj.smd.Ts:obj.smd.Te,options,obj.smd.Pi);
                
                % Populate object with simulation results for later use
                obj.smd.T=t;
                obj.smd.X=x;
                obj.smd.Y=y;
                obj.smd.Xe=x(end,:)';
                obj.smd.Ye=y(end,:)';
                
                % Set continuation starting values. The continuation states
                % might be less than the states in the model. Note that the
                % euler angles are not needed for the bifurcation analysis,
                % but are calculated for the visualisation later on. Hence
                % only the first five states are used.
                obj.bif.Par0=obj.smd.Pi(end,2:end)';
                obj.bif.U0=obj.smd.Xe(1:5);
                obj.bif.Out0=obj.smd.Ye;
            end
    
        end
        
        
        %------------------------------------------------------------------
        %% ANIMATION
        %------------------------------------------------------------------
        function obj=flightanim(obj,varargin)
            
            h=0;
            bifth=[];
            
            if length(varargin)==1
              h=varargin{1};
            elseif length(varargin)==2
              h=varargin{1};
              bifth=varargin{2};
            end
            
            % Use flightviz function from Matlab FEX for animation.
            % Original function written by Gus Brown.

            FPS = 25;       % Frames per second
            Tzero = 0;      % start time
            timescale = 1;  % animation speedup over realtime
            outfile=[];
            
            % a frame buffer will speed up animation file creation
            FrameBuff = 0.1;  % Frame buffer in seconds
            
            
            %% Load data from object
            ln=length(obj.smd.T);
            vizdata.time = obj.smd.T;
            vizdata.alpha= obj.smd.Y(:,1);
            vizdata.beta = obj.smd.Y(:,2);
            vizdata.euler= obj.smd.Y(:,6:8);
            vizdata.vel  = zeros([ln 3]);
            vizdata.alt  = zeros([ln 1]);
            vizdata.mach = zeros([ln 1]);
            vizdata.acc  = zeros([ln 1]);
            vizdata.ail  = obj.smd.Y(:,9);
            vizdata.ele  = obj.smd.Y(:,10);
            vizdata.rud  = obj.smd.Y(:,11);
            vizdata.flap = zeros([ln 1]);
            
            %% Animate
            
            % Create figure if it does not exist
            FIG=findobj('Tag','flightviz');
            
            if isempty(FIG)
               FIG = flightviz('F100');  
            else
               FIG=FIG(1);
            end
            
            % fps holder
            HFPS = uicontrol('Style','text','Parent',FIG,'Units','normalized','HorizontalAlignment','center', ...
                'Tag','Status','BackgroundColor',[0.7 0.8 0.9],'Position',[ 0.001 0.001 0.1 0.05],'TooltipString', ...
                'Frames per second','string','fps','fontsize',10);
            
            % clock holder
            TKPS = uicontrol('Style','text','Parent',FIG,'Units','normalized','HorizontalAlignment','left', ...
                'Tag','Clock','BackgroundColor',[0.7 0.8 0.9],'Position',[ 0.001 0.95 0.15 0.05],'TooltipString', ...
                'Time','string','Time:   0.00 s','fontsize',10);
            
            TIME = [Tzero:timescale/(FPS):max(vizdata.time)]';
            
            % interpolate data, watch out for wrapping data such as roll angle
            % interpolating here should be faster than interpolating before each frame
            alpha = interp1(vizdata.time,vizdata.alpha,TIME);
            beta  = interp1(vizdata.time,vizdata.beta ,TIME);
            euler = interp1(vizdata.time,vizdata.euler,TIME);
            vel   = interp1(vizdata.time,vizdata.vel  ,TIME);
            alt   = interp1(vizdata.time,vizdata.alt  ,TIME);
            mach  = interp1(vizdata.time,vizdata.mach ,TIME);
            acc   = interp1(vizdata.time,vizdata.acc  ,TIME);
            ele   = interp1(vizdata.time,vizdata.ele  ,TIME);
            ail   = interp1(vizdata.time,vizdata.ail  ,TIME);
            rud   = interp1(vizdata.time,vizdata.rud  ,TIME);
            flap  = interp1(vizdata.time,vizdata.flap ,TIME);
            
            % Animate
            drawnow;
            tic;
            toc_old = toc;
            frame = 1;
            fram0 = frame;
            fps_old = 0;
            
            while (frame<=length(TIME)),
                               
                % calculate velocity
                ALPHA=alpha(frame,1)*pi/180;
                BETA=beta(frame,1)*pi/180;
                PHI=euler(frame,1)*pi/180;
                THETA=euler(frame,2)*pi/180;
                PSI=euler(frame,3)*pi/180;
                [UVW]=windv2worldv(ALPHA,BETA,PHI,THETA,PSI,100);
                
                % update animation of aircraft figure
                fig = flightviz(obj.AnimModel,alpha(frame,1),beta(frame,1),euler(frame,:)*pi/180,UVW,alt(frame),ele(frame)*pi/180,ail(frame)*pi/180,rud(frame)*pi/180,flap(frame)*pi/180);
                              
                % Realtime mode, FPS is dynamic
                fps = (fps_old + 1/(toc-toc_old))/2; fps_old = fps; toc_old = toc;
                set(HFPS,'string',sprintf('%3.1ffps',fps));
                set(TKPS,'string',sprintf('Time: %5.2f s',TIME(frame)));
                frame = ceil(abs(FPS)*toc);
                T = max(TIME)-TIME(min(frame,length(TIME)));
                set(FIG,'name',sprintf('Realtime %imin %04.1fs to end',floor(T/60),round(mod(T,60)/timescale)));
                
                if frame>=length(TIME)
                  break
                end
                
                % update any other open graphs and figures
                for i=1:length(h)
                    if fig==h(i)
                        continue
                    end
                    animgraph(obj,h(i),TIME(frame),bifth);
                end
                
            end
            
            % final fps rendered
            fps = length(TIME)/toc;
            fprintf(1,'FPS = %g\n',fps);
                       
            disp('Done.');
            
        end
    end
            
end

%--------------------------------------------------------------------------
%%  CREATE ADDITIONAL PROPERTY SUB-FIELDS
%--------------------------------------------------------------------------
function d=acsmd(varargin)
  d.Te  = 20;              % Simulation time
  d.Ts  =0.1;              % Output data at Ts increments
  d.Pi  = [];              % Input vector for simulation
  d.Xi  = [];              % Initial simulink state values
  d.T   = [];              % Time vector
  d.X   = [];              % Simulation state values
  d.Y   = [];              % Simulation output values
  d.Xe  = [];              % End simulink state values
  d.Ye  = [];              % End simulink output values
end

function d=acbif(varargin)
  d.Par0= [];              % Continuation initial parameter values
  d.U0  = [];              % Initial state vector for continuation
end

%--------------------------------------------------------------------------
%%  ANIMATE MARKER ON GRAPH
%--------------------------------------------------------------------------
function animgraph(obj,h,timept,bifth)

axh=findobj(get(h,'Children'),'Type','axes');
axh=sort(axh);
axh=axh(1);
axes(axh);
ylim=get(axh,'Ylim');

delete(findobj(axh,'Tag','animtype'));

if strcmp(get(h,'Tag'),'ThFig')
    plot([timept,timept]',ylim,'Tag','animtype','LineWidth',2,'Color','m');
elseif strcmp(get(h,'Tag'),'BifFig')
    xy = interp1(obj.smd.T,bifth,timept);
    plot(xy(1),xy(2),'ro','MarkerSize',8,'MarkerFaceColor','r','Tag','animtype');
end

end

%--------------------------------------------------------------------------
%%  CONVERT FROM WIND TO WORLD AXIS
%--------------------------------------------------------------------------
function [UVW]=windv2worldv(ALPHA,BETA,PHI,THETA,PSI,Vt)

uvw=[Vt*cos(ALPHA)*cos(BETA);
                Vt*sin(BETA);
     Vt*sin(ALPHA)*cos(BETA)];

% convert to world axis
% Phillips Eq (11.2.4)
% linear velocities
C11=cos(THETA)*cos(PSI);
C12=sin(PHI)*sin(THETA)*cos(PSI)-cos(PHI)*sin(PSI);
C13=cos(PHI)*sin(THETA)*cos(PSI)+sin(PSI)*sin(PHI);
C21=cos(THETA)*sin(PSI);
C22=sin(PHI)*sin(THETA)*sin(PSI)+cos(PHI)*cos(PSI);
C23=cos(PHI)*sin(THETA)*sin(PSI)-sin(PHI)*cos(PSI);
C31=-sin(THETA);
C32=sin(PHI)*cos(THETA);
C33=cos(PHI)*cos(THETA);

UVW=[C11,C12,C13;C21,C22,C23;C31,C32,C33]*uvw;

end
