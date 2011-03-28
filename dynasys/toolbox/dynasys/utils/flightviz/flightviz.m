function [fig] = flightviz(modelName,alpha,beta,euler,vel,alt,elevator,aileron,rudder,flap)
%  Display F4 Phantom aircraft orientation and flight information
% 
%  function fig = flightviz(euler,vel,alt,mach,acc,elevator,aileron,rudder,flap)
%
%  Creates a display in the next available figure of an F4 Phantom jet
%  with some bar meters to display altitude, mach number and g's.
%
%  The inputs are as follows...
%  euler : euler angles [phi theta psi] in radians
%  vel   : velocity vector [vx vy vz]
%  alt   : altitude in meters
%  mach  : Mach number
%  acc   : acceleration in g's
%  elevator : elevator angle in radians, positive deflection causes pitch up
%  aileron : aileron angle in radians, positive deflection is port wing down
%  rudder   : rudder angle in radians, positive deflection is nose right
%  flap     : flap angle in radians
%
%  Returns the number of the figure used.
%
%  Not all inputs need to be specified as there are zero default values.
%
%  Typically one would use this inside a loop to display flight data as an
%  animation. Just remember to call drawnow to force each frame to update.
%
%  Example:
%  % create a figure and draw the aircraft
%  fig = flightviz;
%  % update the figure
%  fig = flightviz([0 10 90]*pi/180,[0 1 0.1],1000,0.4,1,5*pi/180,0,0,30*pi/180);
%
%  see also:
%  <a href="matlab: help plotAxes">plotAxes</a>, <a href="matlab: help plotTraj">plotTraj</a>, <a href="matlab: help drawModel">drawModel</a>, <a href="matlab: help updateModel">updateModel</a>

% Default settings
if ~exist('alpha','var'),   alpha = 0;       end;
if ~exist('beta','var'),    beta = 0;        end;
if ~exist('vel','var'),     vel = [0 0 0];   end;
if ~exist('euler','var'),   euler = [0 0 0]; end;
if ~exist('alt','var'),     alt = 0;         end;
if ~exist('mach','var'),    mach = 0;        end;
if ~exist('acc','var'),     acc = 0;         end;
if ~exist('elevator','var'),elevator = 0;    end;
if ~exist('aileron','var'), aileron = 0;     end; 
if ~exist('rudder','var'),  rudder = 0;      end;
if ~exist('flap','var'),    flap  = 0;       end;
if ~exist('slat','var'),    slat  = 0;       end;

% check for existence of figure
fig = findobj(0,'type','fig','tag','flightviz');
% create figure
if isempty(fig),
  fig = figure; 
  set(fig,'tag','flightviz','name','Flight Viz','numberTitle','off','Visible','off');
  drawnow;
  
  % axis circles
  h.ax = plotAxes(fig,[0 0 0],9.5,3);
  % trajectory arrow
  h.trj = plotTraj(fig,[0 0 0],9);
  % model
  model = drawModel(fig,modelName); 
  axis equal; 
  camlight('headlight');

  % bar meters
  [h.alt] = GTbarmeter(0,3000,'Alt (km)','right',1);
  [h.alpha] = GTbarmeter(-30,30,'\alpha (deg)','left',0);
  [h.beta] = GTbarmeter(-30,30,'\beta (deg)','bottom',0);

  % create handle to model
  h.body = struct('hgt',model.hgt.all,'ref',model.ref.all);

  % create control surfaces
  connames=fieldnames(model.ref.body);
  
  for i=1:length(connames)
    str=sprintf('h.%s=struct(''hgt'',model.hgt.body.%s,''ref'',model.ref.body.%s);',connames{i},connames{i},connames{i});
    eval(str);
    str=sprintf('h.conh(i)=h.%s;',connames{i});
    eval(str);
    str=sprintf('h.con(%1.0f).name=''%s'';',i,connames{i});
    eval(str);
  end

  % save handles for later
  set(fig,'userdata',h);

  % nice camera position
  campos([ -140  60  -70]);
  % add some light from above
  camlight(0,80);
  
  set(fig,'Visible','on');

end;

% get handles from figure
h = get(fig,'userdata');

% update information
plotAxes(h.ax,euler);
plotTraj(h.trj,vel);

% update aircraft rotation
updateModel(h.body,[0 0 0],euler);

% update control surfaces

for i=1:length(h.con)

if regexp(h.con(i).name,'\w*elev\w*')
   def=[0 -elevator 0];
elseif regexp(h.con(i).name,'\w*flap\w**')
   def=[0 flap 0];
elseif regexp(h.con(i).name,'\w*slat\w*') % not implemented yet
   def=[0 -slat 0];
elseif regexp(h.con(i).name,'\w*left\w*ail\w*')
   def=[0 -aileron 0];   
elseif regexp(h.con(i).name,'\w*right\w*ail\w*')
   def=[0 aileron 0];   
elseif regexp(h.con(i).name,'\w*rud\w*')
   def=[0 0 -rudder];   
else
   def=[0 0 0];
end

updateModel(h.conh(i),[0 0 0],def);

end

% update barmeters
GTbarmeter(h.alt,alt/1000);
GTbarmeter(h.alpha,alpha);
GTbarmeter(h.beta,beta);

% force draw
drawnow;
