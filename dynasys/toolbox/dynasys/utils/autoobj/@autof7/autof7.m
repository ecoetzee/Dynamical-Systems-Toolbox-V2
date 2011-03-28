classdef autof7
% AUTOF7 class is used to store the outputs from the continuation runs.
%  

%   Created by Etienne COETZEE, James RANKIN,and Phani THOTA, University of Bristol
%
%   $Revision: 2.0.0.0 $ $Date: 2010/07/29 14:07:00$
    properties
        Ibr=[];      % Branch number.
        Mtot=[];     % Index of point in output vector. Negative values 
                     % indicate stable solutions, and positive unstable solutions.
        Itp=[];      % Solution type.
        Lab=[];      % Label of special point, i.e. limit point, or 
                     % user-requested point.
        Par=[];      % Parameter values from continuation run.
        L2norm=[];   % L2-norm value.
        U=[];        % State values from continuation run.
        Out=[];      % Additional outputs from continuation run.
    end
    
    methods
        function obj=autof7()

        end
        
        function obj=assigntypef7(obj)
            % Make sure variables are of similar type than those in AUTO
            obj.Ibr=int32(obj.Ibr);
            obj.Mtot=int32(obj.Mtot);
            obj.Itp=int32(obj.Itp);
            obj.Lab=int32(obj.Lab);
            obj.Par=double(obj.Par);
            obj.L2norm=double(obj.L2norm);
            obj.U=double(obj.U);
            obj.Out=double(obj.Out);
        end        
    end
    
end


