function [H] = drawModel(fig,MOD,TAG,pos,euler,varargin);
% Draw a 3D model.
%
% function [H] = drawModel(fig,MOD,TAG,pos,euler,varargin);
%
%   Draw a model loaded from the model file specified by MOD.
%   HG transform handels are created to allow easy manipulation
%   of the model.
%
%   fig : figure window to use
%   MOD : model name, name of *.mat file containing model data
%   TAG : model name tag, sets the 'tag' property of the graphics 
%         objects
%   pos : position at which to draw model 
%   euler : model rotation angles
%   varargin : optional parameters name value pairs such as the 
%              following. The default values are given below.
%            color = 'none';
%            shadowcolor = 'none';
%            erasemode = 'normal';
%            linestyle = '-';
%            linewidth = 0.5000;
%            marker = 'none';
%            markersize = 6;
%            markeredgecolor = 'auto';
%            markerfacecolor = 'none';
%
%   Examples:
%   % To get a list of available models
%   drawModel
%
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
%   <a href="matlab: help updateModel">updateModel</a>

if exist('fig','var'),
  if isstr(fig),
    error('The first argument should be a figure number');
  end;
else    
  disp('The following models are available');
  dir([mfilename('fullpath') filesep '*.mat']);
  return;
end;  

if ~exist('TAG','var'),  TAG = MOD; end;
if ~exist('pos','var'),  pos = [0 0 0]; end;
if ~exist('euler','var'),  euler = [0 0 0]; end;

% Defaults
va.color = 'none';
va.shadowcolor = 'none';
va.erasemode = 'normal';
va.linestyle = '-';
va.linewidth = 0.5000;
va.marker = 'none';
va.markersize = 6;
va.markeredgecolor = 'auto';
va.markerfacecolor = 'none';

va.disp = 0;

for ii = 1:2:length(varargin)-1,
  va.(lower(varargin{ii})) = varargin{ii+1};
end;

% disp(sprintf('Loading model : "%s"',MOD));
if ~exist(MOD,'file'),
  load([mfilename('fullpath') filesep MOD '.mat']);
else  
  load(MOD);
end;

figure(fig);
IH = ishold(fig); hold on;

if ~strcmpi(va.marker,'none')
  H.point = plot3(pos(1),pos(2),pos(3),'marker',va.marker,'markersize',va.markersize,'markerfacecolor',va.markerfacecolor,'tag',TAG);
end;
if ~strcmpi(va.color,'none')
  H.trail = plot3(pos(1),pos(2),pos(3),'linestyle',va.linestyle,'linewidth',va.linewidth,'color',va.color,'tag',TAG);
end;
if ~strcmpi(va.shadowcolor,'none')
  H.shadow = plot3(pos(1),pos(2),0,'k-','linestyle',va.linestyle,'linewidth',va.linewidth,'color',va.shadowcolor,'tag',TAG);
end;


[H.hgt,H.obj,SZ] = displayM(M,gca,TAG,ref,va.disp);
H.ref = ref;
H.numtris = SZ;

M = makehgtform('translate',pos,'translate',H.ref.all(1:3),'zrotate',euler(3),'yrotate',euler(2),'xrotate',euler(1),'translate',-H.ref.all(1:3));
set(H.hgt.all,'Matrix',M);

if (~IH), hold off; end;


%% ________________________________________________
function [Hhgt,H,SZ] = displayM(M,parent,TAG,ref,DISP,SZ);

  if ~exist('SZ','var'), SZ = 0; end;

  H = [];
  if (isfield(M,'x') && ~isstruct(M(1).x));
    if length(M)>1,
      Hhgt = hgtransform('Parent',parent,'tag',TAG,'userdata',ref);
    else
      Hhgt = hgtransform('Parent',parent,'tag',TAG,'userdata',ref);;
    end;
    for ii = 1:length(M)
      if isfield(M,'mat'),
        material(M(ii).mat);
      end;
      if isfield(M,'tri'),
        if (DISP), disp(sprintf('  trisurf %g,  %g tris',ii,length(M(ii).tri))); end;
        H(ii) = trisurf(M(ii).tri,M(ii).x,M(ii).y,M(ii).z,'parent',Hhgt,'tag',TAG);
        SZ = SZ + size(M(ii).tri,1);
      else
        if (DISP), disp(sprintf('  surface %g,  %gx%g',ii,size(M(ii).x))); end;
        H(ii) = surf(M(ii).x,M(ii).y,M(ii).z,'parent',Hhgt,'tag',TAG);
        SZ = SZ + 2*prod(size(M(ii).x));
      end;
      if isfield(M,'ref'),
        set(H(ii),'userdata',M(ii).ref);
      end;
      if isfield(M,'col'),
        set(H(ii),'facecolor',M(ii).col(1,:));
      end;
      if isfield(M,'edg'),
        set(H(ii),'edgecolor',M(ii).edg);
      else
        set(H(ii),'edgecolor','none');
      end;
      if isfield(M,'lit'),
        set(H(ii),'facelighting',M(ii).lit);
      end;
      if isfield(M,'alp'),
        set(H(ii),'facealpha',M(ii).alp);
      end;
      if isfield(M,'tex');
        set(H(ii),'FaceColor','texturemap','CDataMapping','direct','Cdata',M(ii).tex);
      end;

    end;
  else
    FF = fields(M);
    Hhgt.all = hgtransform('Parent',parent,'tag',TAG);
    if (isstruct(ref) && isfield(ref,'all')),
      set(Hhgt.all,'userdata',ref.all);
    else
      set(Hhgt.all,'userdata',ref);
    end;
    for cc = 1:length(FF),
      if (isstruct(M(1).(FF{cc}))),
        if (DISP), disp(sprintf('subobj %g : %s',cc,FF{cc})); end;
        if isfield(ref,FF{cc}),
          REF = ref.(FF{cc});
        else
          REF = ref;
        end;
        [Hhgt.(FF{cc}),H.(FF{cc}),SZ] = displayM(M.(FF{cc}),Hhgt.all,[TAG '.' FF{cc}],REF,DISP,SZ);
      end;
    end;
  end;
