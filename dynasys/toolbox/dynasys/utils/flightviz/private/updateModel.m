function [] = updateModel(H,pos,euler,varargin)
% Draw 3D model if figure.
%
% [] = updateModel(H,pos,euler,scale)
%
%   Update a model created with the drawModel function.
%   Allows one to rotate and move the model or parts of it.
%
%   The inputs are as follows.
%
%   H : handle structure to model
%   pos : position
%   euler : angles
%   scale : scale factor {optional}
%
%   Examples:
%   % draw a model
%   model = drawModel(fig,'Phantom'); 
%
%   % Create handles to the model
%   h.body = struct('hgt',model.hgt.all,'ref',model.ref.all);
%   h.tail = struct('hgt',model.hgt.body.tail,'ref',model.ref.body.tail);
%
%   % To update the model, one should use the updateModel function
%   updateModel(h.body,[0 0 0],[0 20 45]*pi/180);
%   updateModel(h.tail,[0 0 0],[0 0 -20]*pi/180);
%
%  see also:
%   <a href="matlab: help drawModel">drawModel</a>


if nargin>=4,
  scale = varargin{1};
  if length(scale)==1,
    scale = scale*[1 1 1];
  end;  
else
  scale = [1 1 1];
end;

for ii = 2:2:length(varargin)-2,
  va.(lower(varargin{ii})) = varargin{ii+1};
end;


if (isfield(H,'point')),
  set(H.point,'xdata',pos(1),'ydata',pos(2),'zdata',pos(3));
end;


if (isfield(H,'trail')),
  xdata = [get(H.trail,'xdata') pos(1)];
  ydata = [get(H.trail,'ydata') pos(2)];
  zdata = [get(H.trail,'zdata') pos(3)];
  L = length(xdata);
  I = max(1,L-300):L;
  set(H.trail,'xdata',xdata(I),'ydata',ydata(I),'zdata',zdata(I));
end;


if (isfield(H,'shadow')),
  xdata = [get(H.shadow,'xdata') pos(1)];
  ydata = [get(H.shadow,'ydata') pos(2)];
  zdata = [get(H.shadow,'zdata') 0];
  L = length(xdata);
  I = max(1,L-300):L;
  set(H.shadow,'xdata',xdata(I),'ydata',ydata(I),'zdata',zdata(I));
end;



if isstruct(H),
  if (isfield(H,'hgt')),
    if isstruct(H.hgt),
      HGT = H.hgt.all;
    else
      HGT = H.hgt;
    end;
  elseif (isfield(H,'all')),
    HGT = H.all;
  end;

  if (isfield(H,'ref')),
    if isstruct(H.ref),
      REF = H.ref.all;
    else
      REF = H.ref;
    end;
  else
    REF = get(HGT,'userdata');
  end;

else
  HGT = H;
  REF = get(H,'userdata')
end;

% Rotate
M = makehgtform('scale',[scale(1) scale(2) scale(3)] ... 
               ,'translate',pos(1:3) ...
               ,'translate',REF(1:3) ...
               ,'zrotate',REF(6)  ,'yrotate',REF(5)  ,'xrotate',REF(4) ...
               ,'zrotate',euler(3),'yrotate',euler(2),'xrotate',euler(1) ...
               ,'xrotate',-REF(4) ,'yrotate',-REF(5) ,'zrotate',-REF(6) ...
               ,'translate',-REF(1:3));
set(HGT,'Matrix',M);
