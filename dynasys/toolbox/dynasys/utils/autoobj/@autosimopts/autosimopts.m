classdef autosimopts < hgsetget
% AUTOSIMOPTS is used to define all the options related to the overall 
% simulation, such as the functions that are used, models etc.
%
% This class is used to define several global settings, such as the mode in 
% which the analysis should be run, the input functions that are called, 
% input and output files (if required), Simulink models that are called, etc.    
%   

%   Created by Etienne COETZEE, James RANKIN,and Phani THOTA, University of Bristol
%
%   $Revision: 2.0.0.0 $ $Date: 2010/07/29 14:07:00$
    properties
        RunMode='DST';        % {'DST'} | '07P', Can either run model where
                              % outputs and inputs are obtained from objects 
                              % ('DST'), or you can use the traditional way 
                              % in which AUTO is used ('07P').
        FuncFileName='func';  % Name of function file. This can be any name.
        StpntFileName='stpnt';% Starting conditions. Only needed for '07P' 
                              % mode, or when calculating Parabolic PDE's. 
                              % Can be any name.
        BcndFileName='bcnd';  % Function containing boundary conditions. 
                              % Can be any name.
        IcndFileName='icnd';  % Function containing boundary value and integral 
                              % constraints. Can be any name.
        FoptFileName='fopt';  % Not used
        PvlsFileName='pvls';  % Not used
        OutFileName='';       % Any string, The name of the extension for 
                              % the constants file if using '07P' mode, i.e. 
                              % c.OutFileName. Or the extension name of the 
                              % output files when files are requiredin 
                              % 'DST' mode. The following files will be
        SimulinkModel='';     % Model name, Name of Simulink model. Model 
                              % will be automatically opened and compiled.
        Fort7='off';          % {'off'} | 'on', Write fort.7 file if requested.
                              % File name will be b.OutFileName
        Fort8='off';          % {'off'} | 'on', Write fort.8 file if requested. 
                              % File name will be s.OutFileName
        Fort9='off';          % {'off'} | 'on', Write fort.7 file if requested. 
                              % File name will be d.OutFileName
        BatchVals=[];         % [n x m], Used for storing additional simulation 
                              % information, i.e. tables for DOE etc, where 
                              % n and m can be any value
        Par0=[];              % [NPAX x 1], Initial values for parameters. 
                              % This is only used in 'DST' mode. n needs to 
                              % be smaller than NPARX, and also remember 
                              % that PAR(11) is reserved for limit cycle 
                              % continuations.
        U0=[];                % [NDIM x 1], Initial values for continuation states.
        Out0=[];              % [], Initial values for additional Outputs. 
                              % This does not have to be filled in, as runautodst
                              % method tries to determine initial outputs. 
    end
    
    
    methods
        function obj=autosimopts()
            
        end
        %
        function obj=set.RunMode(obj,value)
            if strcmp(value,'DST')
                obj.Fort7='off';
                obj.Fort8='off';
                obj.Fort9='off';
            elseif strcmp(value,'07P')
                obj.Fort7='on';
                obj.Fort8='on';
                obj.Fort9='on';
            else
                error('DST:RunMode','Incorrect run mode specification. Select either DST or 07P modes');
            end
            obj.RunMode=value;
        end
        %
        function obj=set.FuncFileName(obj,value)
            if exist([value,'.m'],'file')~=2
               error('DST:FuncFileName','Function file specified not on the path');  
            else
               obj.FuncFileName=value;
            end
        end
        %
        function obj=set.StpntFileName(obj,value)
               obj.StpntFileName=value;
        end
        %
        function obj=set.BcndFileName(obj,value)
               obj.BcndFileName=value;
        end
        %
        function obj=set.IcndFileName(obj,value)
               obj.IcndFileName=value;
        end
        %
        function obj=set.FoptFileName(obj,value)
               obj.FoptFileName=value;
        end
        %
        function obj=set.PvlsFileName(obj,value)
               obj.PvlsFileName=value;
        end
        %
        function obj=set.Fort7(obj,value)
            if ~isempty(strmatch('off',value,'exact')) && ~isempty(strmatch('07P',obj.RunMode,'exact'))
                error('DST:OutputFiles','Can not switch Fort.7 file output off in 07P mode');
            elseif ~isempty(strmatch('on',value,'exact')) && ~isempty(strmatch('DST',obj.RunMode,'exact'))
                obj.Fort7=lower(value);
            else
                %error('DST:OutputFiles','Incorrect option specified. Use either [''on'' | ''off'']');
            end
        end
        %
        function obj=set.Fort8(obj,value)
            if ~isempty(strmatch('off',value,'exact')) && ~isempty(strmatch('07P',obj.RunMode,'exact'))
                error('DST:OutputFiles','Can not switch Fort.8 file output off in 07P mode');
            elseif ~isempty(strmatch('on',value,'exact')) && ~isempty(strmatch('DST',obj.RunMode,'exact'))
                obj.Fort8=lower(value);
            else
%                error('DST:OutputFiles','Incorrect option specified. Use either [''on'' | ''off'']');
            end
        end
        %
        function obj=set.Fort9(obj,value)
            if ~isempty(strmatch('off',value,'exact')) && ~isempty(strmatch('07P',obj.RunMode,'exact'))
                error('DST:OutputFiles','Can not switch Fort.9 file output off in 07P mode');
            elseif ~isempty(strmatch('on',value,'exact')) && ~isempty(strmatch('DST',obj.RunMode,'exact'))
                obj.Fort9=lower(value);
            else
%                error('DST:OutputFiles','Incorrect option specified. Use either [''on'' | ''off'']');
            end
        end
    end
    
end
