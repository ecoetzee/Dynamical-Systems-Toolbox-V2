function [traj] = plotTraj(FIG,TRAJ,Len)
% Plots a trajectory arrow
%
% [hg] = plotTraj(H,TRAJ,Len)
%
%  Inputs:
%   TRAJ: vector of the trajectory
%   Len : length of the line
%   H   : figure in which to put the trajectory arrow
%         or handle previously returned by the function
%
%   Returns the hg transform of the trajectory arrow.
%   hg : hgtransform of trajectory arrow
%
%  Example:
%    % Create a trajectory arrow
%    h.trj = plotTraj(fig,[0 0 0],9);
%    % Update trajectory arrow
%    plotTraj(h.trj,[1 1 1]);
%
%  see also:
%  <a href="matlab: help plotAxes">plotAxes</a>, <a href="matlab: help plotTraj">plotTraj</a>, <a href="matlab: help drawModel">drawModel</a>, <a href="matlab: help updateModel">updateModel</a>



if (~exist('FIG','var')), FIG = 1000; end;
if (~exist('TRAJ','var')), TRAJ = [1 0 0]; end;
if (~exist('Len','var')), Len = 1; end;


if (~ishandle(FIG)),
  figure(FIG);
  set(gcf,'color',[0.7 0.8 0.9]);
end;

if (mod(FIG,1)==0),

  if strcmpi(get(FIG,'Type'),'Axes'),
    AX = FIG;
  else
    AX = findobj(FIG,'Type','Axes','tag','3DAxes');
    if isempty(AX),
      AX = axes('position',[0 0 1 1]);
      set(AX,'color','none','visible','off','tag','3DAxes');
    else
      AX = AX(1);
    end;
  end;
  axes(AX);
  hold on;
  % --------------------------------------------------------------------

  traj = hgtransform('Parent',AX,'tag','trajectory');

  hold on;
  % Trajectory arrow
  HA(1) = plot3([-1 1]*Len,[0 0],[0 0],'w','linewidth',3,'tag','traj','parent',traj);
  phi = [-pi:pi/10:pi]';
  ay = [0; sin(phi)*Len*0.03];
  az = [0; cos(phi)*Len*0.03];
  ax = Len*ones(size(ay)); ax(1) = Len*1.18;
  tri = [ones([length(ax)-1 1]) [(2:length(ax))-1]' [2:length(ax)]'];
  HA(2) = trisurf(tri,ax,ay,az,'edgecolor','none','facecolor',[1 1 1],'facealpha',0.75,'parent',traj);
  material(HA,'metal');

  set(HA(2),'AmbientStrength',0.75)

  % --------------------------------------------------------------------
  axis('equal');
  grid off;
  hold off;

  lighting gouraud;

  camproj('perspective');

  camva(6);
  cameratoolbar('show');
  cameratoolbar('SetCoordSys','z');
  cameratoolbar('setmode','orbit');
  camup([0 0 -1]);
  campos([10 10 10]);
  camtarget([0 0 0]);
else
    traj = FIG;
end;

% Trajectory
[Xr,Yr,Z] = cart2sph(TRAJ(1),TRAJ(2),TRAJ(3));
M = makehgtform('zrotate',Xr,'yrotate',-Yr);
set(traj,'Matrix',M);

