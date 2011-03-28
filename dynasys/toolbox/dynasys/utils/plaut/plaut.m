classdef plaut  < hgsetget
    properties
        fName ='fort.7';                    % AUTO file name
        xEqStr='p';                       % x-parameter equilibrium
        yEqStr='l2';                        % y-parameter equilibrium
        zEqStr='';                          % z-parameter equilibrium
        xLab='';                          % x-axis label
        yLab='';                          % y-axis label
        zLab='';                          % z-axis label
        xCropLims=[];                         
        yCropLims=[];                         
        zCropLims=[];      
        wildCardVals=[];
        title='Bifurcation Diagram';     % equilibrium title
        axScales={1, 1, 1};
        axLim=[];                         % axis limits equilibrium
  %      lcPerLab=[6,8,10];                  % limit cycle labels
  %      lcPerStIndx=2;                      % limit cycle state index
  %      lcPerPlot='on';                     % limit cycle period plot
  %      lcPhaLab=[6,8,10];                  % limit cycle phase plot labels
  %      lcPhaStIndx=[1,2];                  % limit cycle phase state index
  %      lcPhaPlot='on';                     % limit cycle phase plot
  %      lcSubPlot='on';                     % separate figures or subplot
        clrs={[0.349 0.463 1],[1 0.278 0.278],'k','k'};             % colour order
        lineStyles={'-','--'};
  %      holdPlot='on';                      % hold data or clear axes
        newPlot='on';                      % new figure
        plotFirstBranchOnly='on';           % only plot first branch from data files
        labelPts='on';
        labelBifOnly='off';
        markPts='on';
        markBifOnly='off';
        parIdxZero='off';
    end%properties
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
        function obj = plaut(varargin)
            if nargin==0
                obj.fName='fort';
            elseif nargin==1
                obj.fName=varargin{1};
            elseif nargin>1
                error('Too many input args for constructor');
            end
        end %plaut
    end %methods
    
    %----------------------------------------------------------------------        
    % PLOT FUNCTION
    methods
        function obj = plotds(obj,fName) 
            switch nargin
                case 1 %% All is fine: do nothing                    
                case 2 %% Change object's filename
                    obj.fName=fName;
                otherwise %% Too many args
                    error('AUTOmatlab:PlotError','Use one or two input arguments for PLOTDS');
            end
                       
            [h_fig h_ax]=setup_plot_window(obj);
            first_orbit_only=1;
            old_fname=obj.fName;
            if obj.do_wc
                plots_idx=1:length(obj.wildCardVals);
            else
                plots_idx=1;
            end
            for i=plots_idx
                if obj.do_wc
                    obj.fName=obj.wc_filelist{i};
                end
                [b s]=auto_read(obj,first_orbit_only);
                for j=1:length(b)
                    [plot_data pts]=generate_plot_data...
                    (obj,b(j),s(1));
                    plotbc(obj,plot_data,pts);
                end
                obj.fName=old_fname;
            end
        end%plotds
    end%methods
    
    %----------------------------------------------------------------------
    % SET METHODS
    methods
        %----------------------------------------------------------------------
        % SET 'fName'
        function obj = set.fName(obj,value)
            if ~ischar(value)
                error('Incorrect input type for fName, must be a string')
            end
            [pathName fileName ext]=fileparts(value);
            if ~isempty(pathName)
                disp('Full path not required. Ensure files can be found on matlab path');
            end
            if ~isempty(strmatch('fort',fileName,'exact'))
                fnout='fort';
                b_in='fort.7';
                s_in='fort.8';
            elseif strcmp(fileName,'b') || strcmp(fileName,'s')
                fnout=ext(2:end);
                b_in=['b.',fnout];
                s_in=['s.',fnout];
            elseif isempty(ext)
                fnout=fileName;
                b_in=['b.',value];
                s_in=['s.',value];
            else
                error('Could not parse filename, extra ''.'' or non-standard character in filename');
            end
            if length(strfind(fnout,'*'))==1
                %disp('Data files set to:');disp(b_in);disp(s_in);
                %disp('Filename supplied with wildcard (''*'') update wildCardVals');
            elseif length(strfind(fnout,'*'))>1
                error('Multiple wildcards (''*'') in fName')
            elseif isempty(strfind(fnout,'*'))
                if ~exist(b_in,'file') || ~exist(s_in,'file')
                    disp('Warning: File(s) not found:');disp(b_in);disp(s_in);
                    disp('Ensure files are on the matlab path!')
                %else
                %   disp('Data files set to:');disp(b_in);disp(s_in);
                end
            end
            
            obj.fName=fnout;
        end%set.'fName
        %----------------------------------------------------------------------

        %----------------------------------------------------------------------
        % SET 'wildCardVals'
        function obj = set.wildCardVals(obj,value)
            ext=obj.fName;
            if isempty(strfind(ext,'*')) && ~isempty(value)
                error('No wildcard found in fName, cannot set wildCardVals');
            elseif isempty(value)
                obj.wildCardVals=[];
                obj.wc_filelist={};
                obj.do_wc=0;
                return
            end
            obj.wc_filelist={};
            switch class(value)
                case 'double'
                    for i=1:length(value)
                        ext_new=strrep(ext,'*',num2str(value(i)));
                        ext_new=strrep(ext_new,'.','p');
                        ext_new=strrep(ext_new,'-','m');
                        files_exist_check(ext_new)
                        obj.wc_filelist{i}=ext_new;
                        obj.do_wc=1;
                    end%for
                case 'cell'
                    for i=1:length(value)
                        if ~ischar(value{i})
                           error('Cell must contain strings if passed to wildCardVals') 
                        end
                        ext_new=strrep(ext,'*',value{i});
                        files_exist_check(ext_new)
                        obj.wc_filelist{i}=ext_new;
                        obj.do_wc=1;
                    end%for
                otherwise
                error('Invalid type for wildCardVals')
            end%switch
            obj.wildCardVals=value;
        end%set.wildCardVals
        %----------------------------------------------------------------------
        
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
        function obj = set.parIdxZero(obj,value)
            obj.parIdxZero=test_onoff(value,'parIdxZero');
        end%set.parIdxZero
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
% LOCAL FCN: files_exist_check
function files_exist_check(value,varargin)
if strmatch('fort',value)
    b_in=['fort.7'];
    s_in=['fort.8'];
else
    b_in=['b.',value];
    s_in=['s.',value];
end
if ~exist(b_in,'file') || ~exist(s_in,'file')
    disp('Warning: File(s) not found:');disp(b_in);disp(s_in);
    disp('Ensure files are on the matlab path!')
end
end%files_exist_check
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
        ~isempty(strmatch('p',value)) ||...
        ~isempty(strmatch('l2',value)) ||...
        ~isempty(strmatch('c',value)))
    return
else
    prop=varargin{1};
    error(['Invalid state selection ',prop,'. Should be of format ''u(1)'',''p(3)'',''l2'' etc.'])
end
end%eqstr_check
% LOCAL FCN: label_check
function value=label_check(value,varargin)
if ischar(value)
    return
else
    prop=varargin{1};
    error(['Invalid label for ',prop])
end
end%label_check
% LOCAL FCN: lims_check
function value=lims_check(value,varargin)
if isnumeric(value) && length(value)==2
    return
else
    prop=varargin{1};
    error(['Invalid crop limit for ',prop,'. Should be 1x2 vector'])
end
end%lims_check
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
% LOCAL FCN: setup_plot_window
function [h_fig h_ax]=setup_plot_window(obj)
if strcmp(obj.newPlot,'on')
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

