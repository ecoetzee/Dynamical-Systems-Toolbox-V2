classdef plautobj  < hgsetget
% PLAUTOBJ class  is used to plot the results from a continuation run.
%
%   This class is used to plot the results from a continuation run. 
%   The following commands represent typical usage.
% 
%   >>p=plautobj;
%   >>plotds(p,a);
% 
%  1. The first line creates the plautobj object.
%  2. The second line plots the bifurcation diagrams for the information 
%     contained in the auto object a. If a is a cell array of objects, 
%     the routine will loop through all the objects.
% 
%   The plaut class exists for plotting AUTO format output files. Instead 
%   of passing an object into the plotds routine, you pass a file name. 
%   The properties and methods are the same as the plautobj class. 
%   The following commands represent typical usage.
% 
%   >>p=plaut;
%   >>plotds(p,'b.abc');
%

%   Created by Etienne COETZEE, James RANKIN,and Phani THOTA, University of Bristol
%
%   $Revision: 2.0.0.0 $ $Date: 2010/07/29 14:07:00$

    %% Equilibrium plot parameters
    properties
        handle=[];
        xEqStr='p(1)';                    % x-parameter string for equilibrium solution.
        yEqStr='l2';                      % y-parameter string for equilibrium solution.
        zEqStr='';                        % z-parameter string for equilibrium solution.
        xLab='';                          % x-axis label
        yLab='';                          % y-axis label
        zLab='';                          % z-axis label
        xCropLims=[];                     % Crop limits for x-axis data.                  
        yCropLims=[];                     % Crop limits for y-axis data.                         
        zCropLims=[];                     % Ccrop limits for z-axis data.
        title='Bifurcation Diagram';      % Title of plot.
        axScales={1, 1, 1};               % Scale for each axis. 3 x 1 vector.
        axLim=[];                         % Axis limits. 3 x 1 vector.
        
        % Line color order.
        clrs={[89 118 255]/255;           % Blue
               [255 71 71]/255;           % Red
                 [0 204 0]/255;           % Green
             [0.655 0.012 1.0];           % Purple
              [235 227 69]/255;           % Yellow
               [1 1 1]*140/255;           % Gray
               [25 80 182]/255;           % Dark blue             
               [31 143 55]/255;           % Dark green
               [255 21 21]/255;           % Dark red
                [60 60 60]/255;           % Dark gray
               [1 1 1]*180/255};          % Light gray       

        lineStyles={'-','--'};            % Line style order.
        lineWidth=2;                      % Line width.
        newPlot='on';                     % Create new figure for diagram.
        plotFirstBranchOnly='off';        % Plot all branches detected during a run, 'on' | {'off'}.
        labelPts='on';                    % Plot labels, {'on'} | 'off'.
        labelBifOnly='on';                % Label only bifurcation points, {'on'} | 'off'.
        markPts='on';                     % Plot markers, {'on'} | 'off'.
        markBifOnly='on';                 % Plot only markers for bifurcation points, {'on'} | 'off'.
    end
    
    %% Limit cycle plot parameters
    properties   
       lcLab=[];                          % Limit cycle labels
       lcInterpEq='off';                  % Interpolate to get nice even spacing
       lcInterpPts=10;                    % Amount of evenly spaced points to interpolate at
       lcStyle='-';                       % Use line '-' or markers 'o'
       lcRepCycles=4;                     % repetition cycles
       lcNormTime='on';
    end
    properties  (Dependent, Hidden)
        clr_stab;
        ls_stab;
        clr_unst;
        ls_unst;
        clr_lp;
        clr_hb;
    end%properties dependent,hidden
    properties  (Hidden)
        sfile_cols_cutoff=7;
        do_wc=0;
        wc_filelist={};
    end%properties hidden
    %----------------------------------------------------------------------
    % CONSTRUCTOR
    methods
        function obj = plautobj(obj)

        end %plaut
    end %methods
    
    %----------------------------------------------------------------------
    methods
        % Plot equilibrium solutions
        function obj = ploteq(obj,autoobj) 

            % check object types for consitency
            [obj,autoobj]=checktypes(obj,autoobj);
            
            % equilibrium plots
            [h_fig h_ax]=setup_plot_window(obj);
            obj.handle=h_fig;
            set(obj.handle,'Tag','BifFig');
            
            for i=1:length(autoobj)
              segs=[];
              
              if iscell(autoobj)
                  objdata=autoobj{i};
              else
                  objdata=autoobj;
              end
              
              % find different segments in run
              idx=find(abs(objdata.f7.Mtot)==1);
              idx=[idx;length(objdata.f7.Mtot)+1];
              
              for j=1:length(idx)-1
                  segs{j}=[idx(j):idx(j+1)-1];
              end
              
              % Only plot first branch if requested
              if strcmp(obj.plotFirstBranchOnly,'on')
                iters=1;
              else
                iters=length(segs);
              end
              
              % Plot branches
              for j=1:iters            
                      [plot_data pts]=generate_plot_data(obj,objdata,segs{j});
                      plotbc(obj,plot_data,pts);
              end
            end

        end%plotds
        
        %----------------------------------------------------------------------
        % Add limit cycle data to equilibrium plots 
        function obj = plotlceq(obj,autoobj)
            
            % check object types for consitency
            [obj,autoobj]=checktypes(obj,autoobj);
            
            for i=1:length(autoobj)
                objf8=autoobj{i}.f8;
                iters=size(objf8.Tm,2);
                               
                for j=1:iters
                   data=findf8data(obj,objf8,j);
                   x(j)=data(1,1);
                   ymax(j)=max(data(:,2)); 
                   ymin(j)=min(data(:,2)); 
                end
                
                if iters>0
                    % Recalculate nicely spaced points by interpolating the
                    % exisiting points. This might give incorrect answers,
                    % so use with caution.
                    if strcmp(obj.lcInterpEq,'on')
                        dx=x(end)-x(1);
                        xintrp=x(1):dx/obj.lcInterpPts:x(end);
                    else
                        xintrp=x;
                    end
                   
                    ymaxi=interp1(x,ymax,xintrp,'linear');
                    ymini=interp1(x,ymin,xintrp,'linear');
                    
                    for j=1:length(xintrp)
                        if strcmp(obj.lcStyle,'o')
                            h(1)=plot(xintrp(j),ymaxi(j),'o');
                            h(2)=plot(xintrp(j),ymini(j),'o');
                        else
                            h=plot([xintrp(j),xintrp(j)],[ymini(j),ymaxi(j)],'LineWidth',obj.lineWidth,'Color',obj.clrs{4});
                        end
                    end
                end
            end
            
            obj.handle=gcf;
            set(obj.handle,'Tag','BifFig');
        end
        %----------------------------------------------------------------------
        % Create phase plane plot of limit cycles 
        function obj = plotlcph(obj,autoobj)

            % check object types for consitency
            [obj,autoobj]=checktypes(obj,autoobj);
            
            for i=1:length(autoobj)
                
                objf8=autoobj{i}.f8;
                iters=size(objf8.Tm,2);
                
                if iters>0
                    [ia,ib,idx]=intersect(obj.lcLab,objf8.Lab);

                    if isempty(idx)
                      error('PLAUTOBJ:InvalidLabel','Labels not found in autof8 object');
                    end
                    
                    for k=1:length(idx)
                       data{k}=findf8data(obj,objf8,idx(k));hold on;  
                       Lab{k}=num2str(objf8.Lab(idx(k)));
                       plot(data{k}(:,1),data{k}(:,2),'LineStyle',obj.lcStyle,'Color',obj.clrs{k},'LineWidth',obj.lineWidth);
                    end
                    
                    obj.axLim=[get(gca,'xLim'),get(gca,'yLim')];
                    obj.title='Phase Plot';                    
                    
                    setup_plot_window(obj,gcf);
                    legend(Lab,'Location','Best');
                    
                end
                
            end
            
            obj.handle=gcf;
            set(obj.handle,'Tag','BifFig');
        end

        %----------------------------------------------------------------------
        % Create period plot with time, or normalised time, on bottom axis
        function obj = plotlcpr(obj,autoobj)

            % check object types for consitency
            [obj,autoobj]=checktypes(obj,autoobj);
            
            for i=1:length(autoobj)
                
                objf8=autoobj{i}.f8;
                iters=size(objf8.Tm,2);
                
                if iters>0
                    [ia,ib,idx]=intersect(obj.lcLab,objf8.Lab);

                    if isempty(idx)
                      error('PLAUTOBJ:InvalidLabel','Labels not found in autof8 object');
                    end
                                       
                    for j=1:length(idx)
                       tmp2=[];
                        
                       tmp1=findf8data(obj,objf8,idx(j));hold on;  
                       tmp1(:,1)=objf8.Tm(:,idx(j));
                       Lab{j}=num2str(objf8.Lab(idx(j)));
                       tmp2=tmp1;
                       
                       for k=1:obj.lcRepCycles-1 
                           tmp1(:,1)=tmp1(:,1)+1;
                           tmp2=[tmp2;tmp1];
                       end
                       
                       if strcmp(obj.lcNormTime,'off');
                         tmp2(:,1)=tmp2(:,1)*objf8.Par(idx(j),11);
                         obj.xLab='Time (s)';
                       else
                         obj.xLab='Normalised Time (1/Hz)'; 
                       end
                       
                       data{j}=tmp2;
                       plot(data{j}(:,1),data{j}(:,2),'LineStyle',obj.lcStyle,'Color',obj.clrs{j},'LineWidth',obj.lineWidth);
   
                    end
                    
                    obj.axLim=[get(gca,'xLim'),get(gca,'yLim')];
                    obj.title='Limit Cycle Response';
                    
                    setup_plot_window(obj,gcf);
                    legend(Lab,'Location','Best');
                    
                end
                
            end
            
            obj.handle=gcf;
            set(obj.handle,'Tag','BifFig');
        end
        

    end%methods
    
    %----------------------------------------------------------------------
    % SET METHODS
    methods   
        %----------------------------------------------------------------------
        % Plot states properties:
        % SET 'xEqStr'
        function obj = set.xEqStr(obj,value)
            obj.xEqStr=eqstr_check(value,'xEqStr');
        end%set.xEqStr
        % SET 'yEqStr'
        function obj = set.yEqStr(obj,value)
            obj.yEqStr=eqstr_check(value,'yEqStr');
        end%set.yEqStr
        % SET 'zEqStr'
        function obj = set.zEqStr(obj,value)
            obj.zEqStr=eqstr_check(value,'zEqStr');
        end%set.zEqStr
        %----------------------------------------------------------------------
       
        
        %----------------------------------------------------------------------
        % Crop limit properties:
        % SET 'xCropLims'
        function obj = set.xCropLims(obj,value)
            obj.xCropLims=lims_check(value,'xCropLims');
        end%set.xCropLims
        % SET 'yCropLims'
        function obj = set.yCropLims(obj,value)
            obj.yCropLims=lims_check(value,'yCropLims');
        end%set.yCropLims
        % SET 'zCropLims'
        function obj = set.zCropLims(obj,value)
            obj.zCropLims=lims_check(value,'zCropLims');
        end%set.zCropLims
        %----------------------------------------------------------------------
                
        %----------------------------------------------------------------------
        % Label-type properties:
        % SET 'xLab'
        function obj = set.xLab(obj,value)
            obj.xLab=label_check(value,'xLab');
        end%set.xLab
        % SET 'yLab'
        function obj = set.yLab(obj,value)
            obj.yLab=label_check(value,'yLab');
        end%set.yLab
        % SET 'zLab'
        function obj = set.zLab(obj,value)
            obj.zLab=label_check(value,'zLab');
        end%set.zLab
        % SET 'title'
        function obj = set.title(obj,value)
            obj.title=label_check(value,'title');
        end%set.title
        %----------------------------------------------------------------------
        
        %----------------------------------------------------------------------
        % Boolean properties:
        % SET 'newPlot'
        function obj = set.newPlot(obj,value)
            obj.newPlot=test_onoff(value,'newPlot');
        end%set.newPlot
        % SET 'plotFirstBranchOnly'
        function obj = set.plotFirstBranchOnly(obj,value)
            obj.plotFirstBranchOnly=test_onoff(value,'plotFirstBranchOnly');
        end%set.plotFirstBranchOnly
        % SET 'labelPts'
        function obj = set.labelPts(obj,value)
            obj.labelPts=test_onoff(value,'labelPts');
        end%set.labelPts
        % SET 'labelBifOnly'
        function obj = set.labelBifOnly(obj,value)
            obj.labelBifOnly=test_onoff(value,'labelBifOnly');
        end%set.labelBifOnly
        % SET 'markPts'
        function obj = set.markPts(obj,value)
            obj.markPts=test_onoff(value,'markPts');
        end%set.markPts
        % SET 'markBifOnly'
        function obj = set.markBifOnly(obj,value)
            obj.markBifOnly=test_onoff(value,'markBifOnly');
        end%set.markBifOnly
        %----------------------------------------------------------------------
        
        % SET 'clrs'
        function obj = set.clrs(obj,value)
            if ~(length(value)==4)
                error('''clrs'' must be a cell containing four color strings or rgb ([1x3] normalised) vectors. e.g. set(obj,''clrs'',{''r'',''g'',[1 0 1],''magenta''}');
            else
                for i=1:length(value)
                    clr_tmp=value{i};
                    switch class(clr_tmp)
                        case 'double'
                            if size(clr_tmp,1)==1&&size(clr_tmp,2)==3;
                                obj.clrs{i}=clr_tmp;
                                continue
                            else
                                error(['RGB vector ',num2str(i),' in clrs is not 1x3']);
                            end
                        case 'char'
                            clr_tmp=lower(clr_tmp);
                            str_ok={'r','g','b','c','m','y','k','w',...
                                'cyan','magenta','black','white'};
                            if sum(~strcmp(clr_tmp,str_ok)==zeros(1,length(str_ok)))~=0
                                obj.clrs{i}=clr_tmp;
                                continue
                            else
                                switch clr_tmp
                                    case 'blue'
                                        obj.clrs{i}=[89 118 255]/255;
                                    case 'dark_blue'
                                        obj.clrs{i}=[25 80 182]/255;
                                    case 'red'
                                        obj.clrs{i}=[255 71 71]/255;
                                    case 'dark_red'
                                        obj.clrs{i}=[255 21 21]/255;
                                    case 'green'
                                        obj.clrs{i}=[0 204 0]/255;
                                    case 'dark_green'
                                        obj.clrs{i}=[31 143 55]/255;
                                    case 'purple'
                                        obj.clrs{i}=[0.655 0.012 1.0];
                                    case 'yellow'
                                        obj.clrs{i}=[235 227 69]/255;
                                    case {'dark_grey','dark_gray'}
                                        obj.clrs{i}=[60 60 60]/255;
                                    case {'grey','gray'}
                                        obj.clrs{i}=[1 1 1]*140/255;
                                    case {'light_grey','light_gray'}
                                        obj.clrs{i}=[1 1 1]*180/255;
                                    otherwise
                                        error('Invalid color string: must be from standard matlab list r,g,b,c,y,m,k etc or extended AUTO07plot list, see help');
                                end%switch
                            end%if
                        otherwise
                            error('Invalid input type for clrs: must be string (char) or a 1x3 vector');
                    end%switch
                end%for
            end%if
        end%set.clrs
        
        % SET 'lcNormTime'
        function obj = set.lcNormTime(obj,value)
            if strcmp(value,'on')||strcmp(value,'off')
              obj.lcNormTime=value;
            end
        end%set.lcNormTime
        
        %----------------------------------------------------------------------
    end%methods
    
    
    
    
    %----------------------------------------------------------------------
    % GET METHODS
    methods
        % GET 'clr_stab'
        function value=get.clr_stab(obj)
            value=obj.clrs{1};
        end%get.clr_stab
        % GET 'ls_stab'
        function value=get.ls_stab(obj)
            value=obj.lineStyles{1};
        end%get.ls_stab
        % GET 'clr_unst'
        function value=get.clr_unst(obj)
            value=obj.clrs{2};
        end%get.clr_unst
        % GET 'ls_unst'
        function value=get.ls_unst(obj)
            value=obj.lineStyles{2};
        end%get.ls_unst
        % GET 'clr_lp'
        function value=get.clr_lp(obj)
            value=obj.clrs{3};
        end%get.clr_lp
        % GET 'clr_hb'
        function value=get.clr_hb(obj)
            value=obj.clrs{4};
        end%get.clr_hb
    end%methods
    
end % classdef

%----------------------------------------------------------------------
% LOCAL FUNCTIONS
%----------------------------------------------------------------------
% LOCAL FCN: eqstr_check
function value=eqstr_check(value,varargin)
if isempty(value)
    return
end
if isnumeric(value)
    value=['u(',num2str(value),')'];
    return
end
value=lower(value);
value=strrep(value,'par','p');
value=strrep(value,'norm','l2');
if ischar(value) && ...
        (~isempty(strmatch('u(',value)) ||...
         ~isempty(strmatch('o(',value)) ||...
         ~isempty(strmatch('p(',value)) ||...
        ~isempty(strmatch('l2',value)) ||...
        ~isempty(strmatch('c',value)))
    return
else
    prop=varargin{1};
    error(['Invalid state selection ',prop,'. Should be of format ''u(1)'',''o(1)'',''p(3)'',''l2'' etc.'])
end
end%eqstr_check
%----------------------------------------------------------------------
% LOCAL FCN: label_check
function value=label_check(value,varargin)
if ischar(value)
    return
else
    prop=varargin{1};
    error(['Invalid label for ',prop])
end
end%label_check
%----------------------------------------------------------------------
% LOCAL FCN: lims_check
function value=lims_check(value,varargin)
if isnumeric(value) && length(value)==2
    return
else
    prop=varargin{1};
    error(['Invalid crop limit for ',prop,'. Should be 1x2 vector'])
end
end%lims_check
%----------------------------------------------------------------------
% LOCAL FCN: test_onoff
function onoff=test_onoff(value,varargin)
switch class(value)
    case {'double','single','logical'}
        if value==1
            onoff='on';
            return
        elseif value==0
            onoff='off';
            return
        end 
    case 'char'
        if strcmp(value,'on')
            onoff=value;
            return
        elseif strcmp(value,'off')
            onoff=value;
            return
        end
end%switch
if nargin==1
    error(['Property(?) is boolean: must be set to ''on'',''off'',0 or 1']);
elseif nargin==2
    prop=varargin{1};
    error([prop,' is boolean: must be set to ''on'',''off'',0 or 1']);
end%if
end%test_onoff
%----------------------------------------------------------------------
% LOCAL FCN: setup_plot_window
function [h_fig h_ax]=setup_plot_window(obj,varargin)

if ~isempty(varargin)
    h_fig=varargin{1};
elseif strcmp(obj.newPlot,'on')
    h_fig=figure;
else
    h_fig=gcf;
end
h_ax=gca;
xlabel(obj.xLab)
ylabel(obj.yLab)
zlabel(obj.zLab)
title(obj.title)
set(gca,'box','on')
set(gcf,'color','w')
axLim=obj.axLim;
if ~isempty(axLim)
    switch length(axLim)
        case 4
            set(gca,'xlim',[axLim(1) axLim(2)]);
            set(gca,'ylim',[axLim(3) axLim(4)]);
            if ~isempty(obj.zEqStr)
                warning('z-axis limits not supplied when zEqStr is non-empty')
            end
        case 6
            set(gca,'xlim',[axLim(1) axLim(2)]);
            set(gca,'ylim',[axLim(3) axLim(4)]);
            set(gca,'zlim',[axLim(5) axLim(6)]);
        otherwise
            error('axLim should be a 1x4 (2d) or 1x6 (3d) vector');
    end
end
end%setup_plot_window

%----------------------------------------------------------------------
% Check object types
function [obj,autoobj]=checktypes(obj,autoobj_)

if ~iscell(autoobj_)
    autoobj{1}=autoobj_;
else
    autoobj=autoobj_;
end

if ~strcmp(class(autoobj{1}),'auto')
    error('PLAUTOBJ:ObjectTypeError','Incorrect object passed as second argument. Needs to be of auto type.')
end
end

%----------------------------------------------------------------------
% Check object types
function data=findf8data(obj,objf8,idxlab)

xEqStr=lower(obj.xEqStr);
yEqStr=lower(obj.yEqStr);

EqStrVec={xEqStr;yEqStr};

for i=1:2
    EqStr=EqStrVec{i};
    if strmatch('p(',EqStr)
        idx=strrep(EqStr,'p(','');
        idx=strrep(idx,')','');
        idx=str2num(idx);
        data(:,i)=objf8.Par(idxlab,idx)*ones(objf8.Ntpl(idxlab),1);
    elseif strmatch('u(',EqStr)
        idx=strrep(EqStr,'u(','');
        idx=strrep(idx,')','');
        idx=str2num(idx);
        data(:,i)=objf8.Ups(:,idx,idxlab);
    elseif strmatch('l2',EqStr)
        error('PLAUTOBJ:ParameterSelection','L2-norm not contained in autof8 object');
    elseif strmatch('o(',yEqStr)
        idx=strrep(yEqStr,'o(','');
        idx=strrep(idx,')','');
        idx=str2num(idx);
        data(:,i)=objf8.Out(:,idx,idxlab);
    else
        error('PLAUTOBJ:ParameterSelection','Select a valid y-axis parameter');
    end
end

end

