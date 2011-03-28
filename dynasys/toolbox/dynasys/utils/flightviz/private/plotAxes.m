function [euler_hgt] = plotAxes(FIG,EULER,R,EXTRAS)
% Plot red-green-blue circles to mark axes.
%
% [hg] = plotAxes(H,EULER,R,EXTRAS)
%
% EULER: euler angles of axes
% R: Radius of circles
% EXTRAS: 2- just circles
%         3- add annotations and line
% H: figure in which to place axes
%    or the handle returned by the function
%    previously.
% 
% hg: list of hgtransforms
%
%   Example:
%     % Create a plot axis if figure 7, 9.5 meters big 
%     % with all options on
%     h.ax = plotAxes(7,[0 0 0],9.5,3);
%     % update axes
%     plotAxes(h.ax,[45 45 45]*pi/180);
%
%  see also:
%  <a href="matlab: help plotAxes">plotAxes</a>, <a href="matlab: help plotTraj">plotTraj</a>, <a href="matlab: help drawModel">drawModel</a>, <a href="matlab: help updateModel">updateModel</a>

if (~exist('FIG','var')), FIG = 1000; end;
if (~exist('EULER','var')), EULER = [0 0 0]; end;
if (~exist('R','var')), R = 1; end;
if (~exist('EXTRAS','var')), EXTRAS = 3; end;


if (~ishandle(FIG)),
  figure(FIG);
  set(gcf,'color',[0.7 0.8 0.9]);
end;

if (length(FIG)==1),

  if strcmpi(get(FIG,'Type'),'Axes'),
    AX = FIG;
  else
    AX = findobj(FIG,'Type','Axes','tag','3DAxes');
    if isempty(AX),
      AX = axes('position',[0 0 1 1]);
      set(AX,'color','none','visible','off','tag','3DAxes');
      set(FIG,'CurrentAxes',AX);
    else
      AX = AX(1);
    end;
  end;
  axes(AX);
  hold on;
  % --------------------------------------------------------------------

  euler_hgt(1)  = hgtransform('Parent',AX,'tag','OriginAxes');
  euler_hgt(2)  = hgtransform('Parent',euler_hgt(1),'tag','roll_disc');
  euler_hgt(3)  = hgtransform('Parent',euler_hgt(1),'tag','pitch_disc');
  euler_hgt(4)  = hgtransform('Parent',euler_hgt(1),'tag','heading_disc');
  euler_hgt(5)  = hgtransform('Parent',euler_hgt(2),'tag','roll_line');
  euler_hgt(6)  = hgtransform('Parent',euler_hgt(3),'tag','pitch_line');
  euler_hgt(7)  = hgtransform('Parent',euler_hgt(4),'tag','heading_line');


  % Plot axis lines
  S = 1.3;
  if (EXTRAS>3),
    plot3([0 R*S],[0 0],[0 0],'r-.','linewidth',2,'tag','Xaxis','Parent',euler_hgt(1));
    plot3([0 0],[0 R*S],[0 0],'g-.','linewidth',2,'tag','Yaxis','Parent',euler_hgt(1));
    plot3([0 0],[0 0],[0 R*S],'b-.','linewidth',2,'tag','Zaxis','Parent',euler_hgt(1));
    plot3([-R*S 0],[0 0],[0 0],'r-.','linewidth',1,'tag','Xaxis','Parent',euler_hgt(1));
    plot3([0 0],[-R*S 0],[0 0],'g-.','linewidth',1,'tag','Yaxis','Parent',euler_hgt(1));
    plot3([0 0],[0 0],[-R*S 0],'b-.','linewidth',1,'tag','Zaxis','Parent',euler_hgt(1));
  end;

  % Plot axis planes
  % Patches
  if (EXTRAS>1),
    phi = [-pi:pi/36:pi]';
    D1 = [sin(phi) cos(phi) zeros(size(phi))];
    HP(1) = patch(R*D1(:,1),R*D1(:,2),+R*D1(:,3),'b','facealpha',0.1,'EdgeColor','b','tag','Zplane','edgealpha',0.5,'Parent',euler_hgt(4));
    HP(2) = patch(R*D1(:,2),R*D1(:,3),+R*D1(:,1),'g','facealpha',0.1,'EdgeColor',[0 0.8 0],'tag','Yplane','edgealpha',0.5,'Parent',euler_hgt(3));
    HP(3) = patch(R*D1(:,3),R*D1(:,1),+R*D1(:,2),'r','facealpha',0.1,'EdgeColor','r','tag','Xplane','edgealpha',0.5,'Parent',euler_hgt(2));
    material(HP,'shiny');
  end;

  if (EXTRAS>2),
    S = 0.9;
    phi = [-pi+pi/2:pi/2:pi];
    D1 = [sin(phi); cos(phi); zeros(size(phi))];
    HL = plot3([S*R*D1(1,:); R*D1(1,:)],[S*R*D1(2,:); R*D1(2,:)],[S*R*D1(3,:); R*D1(3,:)],'Color','b','tag','Zplane','Parent',euler_hgt(4));
    HL = plot3([S*R*D1(2,:); R*D1(2,:)],[S*R*D1(3,:); R*D1(3,:)],[S*R*D1(1,:); R*D1(1,:)],'Color',[0 0.8 0],'tag','Yplane','Parent',euler_hgt(3));
    HL = plot3([S*R*D1(3,:); R*D1(3,:)],[S*R*D1(1,:); R*D1(1,:)],[S*R*D1(2,:); R*D1(2,:)],'Color','r','tag','Xplane','Parent',euler_hgt(2));
    HT = text(R*D1(1,:),R*D1(2,:),R*D1(3,:),{'S','E','N','W'},'Fontsize',10,'color',[1 1 1],'HorizontalAlign','center','VerticalAlign','middle');

    S = 0.94;
    phi = [-pi+pi/4:2*pi/4:pi];
    D1 = [sin(phi); cos(phi); zeros(size(phi))];
    HL = plot3([S*R*D1(1,:); R*D1(1,:)],[S*R*D1(2,:); R*D1(2,:)],[S*R*D1(3,:); R*D1(3,:)],'Color','b','tag','Zplane','Parent',euler_hgt(4));
    HL = plot3([S*R*D1(2,:); R*D1(2,:)],[S*R*D1(3,:); R*D1(3,:)],[S*R*D1(1,:); R*D1(1,:)],'Color',[0 0.8 0],'tag','Yplane','Parent',euler_hgt(3));
    HL = plot3([S*R*D1(3,:); R*D1(3,:)],[S*R*D1(1,:); R*D1(1,:)],[S*R*D1(2,:); R*D1(2,:)],'Color','r','tag','Xplane','Parent',euler_hgt(2));
    HT = text(R*D1(1,:),R*D1(2,:),R*D1(3,:),{'SW','SE','NE','NW'},'Fontsize',8,'color',[1 1 1],'HorizontalAlign','center','VerticalAlign','middle');

    S = 0.96;
    phi = [-pi+pi/12:3*pi/12:pi, -pi+2*pi/12:3*pi/12:pi];
    D1 = [sin(phi); cos(phi); zeros(size(phi))];
    HL = plot3([S*R*D1(1,:); R*D1(1,:)],[S*R*D1(2,:); R*D1(2,:)],[S*R*D1(3,:); R*D1(3,:)],'Color','b','tag','Zplane','Parent',euler_hgt(4));
    HL = plot3([S*R*D1(2,:); R*D1(2,:)],[S*R*D1(3,:); R*D1(3,:)],[S*R*D1(1,:); R*D1(1,:)],'Color',[0 0.8 0],'tag','Yplane','Parent',euler_hgt(3));
    HL = plot3([S*R*D1(3,:); R*D1(3,:)],[S*R*D1(1,:); R*D1(1,:)],[S*R*D1(2,:); R*D1(2,:)],'Color','r','tag','Xplane','Parent',euler_hgt(2));

    S = 0.985;
    phi = [-pi+pi/36:3*pi/36:pi, -pi+2*pi/36:3*pi/36:pi];
    D1 = [sin(phi); cos(phi); zeros(size(phi))];
    HL = plot3([S*R*D1(1,:); R*D1(1,:)],[S*R*D1(2,:); R*D1(2,:)],[S*R*D1(3,:); R*D1(3,:)],'Color','b','tag','Zplane','Parent',euler_hgt(4));
    HL = plot3([S*R*D1(2,:); R*D1(2,:)],[S*R*D1(3,:); R*D1(3,:)],[S*R*D1(1,:); R*D1(1,:)],'Color',[0 0.8 0],'tag','Yplane','Parent',euler_hgt(3));
    HL = plot3([S*R*D1(3,:); R*D1(3,:)],[S*R*D1(1,:); R*D1(1,:)],[S*R*D1(2,:); R*D1(2,:)],'Color','r','tag','Xplane','Parent',euler_hgt(2));

  end;

  if (EXTRAS>2),
    HL(1) = plot3([-R R],[0 0],[0 0],'b-','tag','heading_line','parent',euler_hgt(7));
    HL(2) = plot3([0 R],[0 0],[0 0],'g-','tag','pitch_line','parent',euler_hgt(6),'color',[0 0.8 0]);
    HL(3) = plot3([0 0],[-R R],[0 0],'r-','tag','roll_line','parent',euler_hgt(5));
    HL(4) = plot3([0 0],[-R R],[0 0],'b:','tag','heading_line','parent',euler_hgt(7));
  end;

  hold off;


  % --------------------------------------------------------------------
  axis('equal');
  grid off;
  hold off;

  camproj('perspective');

  camva(6);
  cameratoolbar('show');
  cameratoolbar('SetCoordSys','z');
  cameratoolbar('setmode','orbit');
  camup([0 0 -1]);
  campos([10 10 10]);
  camtarget([0 0 0]);

else
  euler_hgt = FIG;
end;


% Euler lines
M = makehgtform('zrotate',EULER(3));
set(euler_hgt(3),'Matrix',M)

M = makehgtform('zrotate',EULER(3));
set(euler_hgt(2),'Matrix',M)

M = makehgtform('xrotate',EULER(1));
set(euler_hgt(5),'Matrix',M)

M = makehgtform('yrotate',EULER(2));
set(euler_hgt(6),'Matrix',M)

M = makehgtform('zrotate',EULER(3));
set(euler_hgt(7),'Matrix',M)

%M = makehgtform('zrotate',0,'yrotate',0,'xrotate',0);
%set(euler_hgt(1),'Matrix',M)














