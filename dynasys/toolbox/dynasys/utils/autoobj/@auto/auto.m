classdef auto
    % AUTO class  is the main class for the Dynamical Systems Toolbox, and
    % inherits other properties from simulation, constants and output objects.
    
    %   Created by Etienne COETZEE, James RANKIN,and Phani THOTA, University of Bristol
    %
    %   $Revision: 2.0.0.0 $ $Date: 2010/07/29 14:07:00$
    properties
        s=[];     % autosimopts class
        c=[];     % autoconstants class
        f7=[];    % autof7 class
        f8=[];    % autof8 class
    end
    
    methods
        function obj=auto()
            obj.s=autosimopts;
            obj.c=autoconstants;
            obj.f7=autof7;
            obj.f8=autof8;
        end
        
        function obj=runauto(obj)
            
            %check fopr proper mex file first
            if exist(['AUTO07gateway.',mexext])~=3
                error('DST:MexFileMissing','Could not find appropriate AUTO07gateway mex file for this platform');
            end
            
            if strcmp(obj.s.RunMode,'07P')
                runauto07p(obj);
            elseif strcmp(obj.s.RunMode,'DST')
                % ensure data types correct, otherwise fortran crashes
                obj.c=assigntypec(obj.c);
                obj.f7=assigntypef7(obj.f7);
                obj.f8=assigntypef8(obj.f8);
                
                % run
                obj=runautodst(obj);
            else
                error('DST:SimStartError','Inappropriate run mode description');
            end
            
        end
        
        %% Populate f8 object if limit cycle data is provided
        function obj=dat2f8(obj,dat)
            obj=fconfcn(obj,dat);
        end
    end
end
