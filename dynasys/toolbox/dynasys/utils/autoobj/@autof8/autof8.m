classdef autof8 
% AUTOF8 class  is used to store the outputs of special detected points or 
% limit cycle information from continuation runs.
% 

%   Created by Etienne COETZEE, James RANKIN,and Phani THOTA, University of Bristol
%
%   $Revision: 2.0.0.0 $ $Date: 2010/07/29 14:07:00$

    properties
        Ibr=[];      % The index of the branch.
        Mtot=[];     % Index of point in output vector. Negative values 
                     % indicate stable solutions, and positive unstable solutions.
        Itp=[];      % Solution type.
        Lab=[];      % Label of special point, i.e. limit point, or user-requested point.
        Nfpr=[];     % Number of free parameters.
        Isw=[];      % The value of ISW used in the computation.
        Ntpl=[];     % The number of points in the time interval [0,1] for 
                     % which solution values are written to the fort8. file.
        Nar=[];      % The number of values written per point. (NAR=NDIM+1, 
                     % since T and U(i), i=1,..,NDIM are written).
        Nrowpr=[];   % The number of lines printed following the identifying 
                     % line and before the next data set or the end of the file. 
                     % Number of rows in whole data block written to fort.8 file.
        Ntst=[];     % Number of time intervals used in diretization.
        Ncol=[];     % Number of collocation points.
        Nparx=[];    % The dimension of the array PAR
        Ifpr=[];     % Indices of the free parameters in the PAR vector.
        T=[];        % Normalised time vector. Equal to zero for stationary 
                     % solutions. Empty when periodic solutions are calculated.
        Tm=[];       % Normalised time vector. Length equal to Ntst*Ncol+1.
        Par=[];      % Parameter values from continuation run.
        Rldot=[];    % Direction of branch for parameter values when 
                     % periodic solutions are calculated.
        U=[];        % State values from continuation run for steady state
                     % solutions. Empty when periodic solutions are calculated.
        Ups=[];      % State values when periodic solutions are calculated.
        Udotps=[];   % Direction vector of state values when periodic 
                     % solutions are calculated.
    end
       
    methods
        function obj=autof8()
%            obj=assigntypef8(obj);
        end
        
        function obj=assigntypef8(obj)
            % Make sure variables are of similar type than those in AUTO
            obj.Ibr=int32(obj.Ibr);
            obj.Mtot=int32(obj.Mtot);
            obj.Itp=int32(obj.Itp);
            obj.Lab=int32(obj.Lab);
            obj.Nfpr=int32(obj.Nfpr);
            obj.Isw=int32(obj.Isw);
            obj.Ntpl=int32(obj.Ntpl);
            obj.Nar=int32(obj.Nar);
            obj.Nrowpr=int32(obj.Nrowpr);
            obj.Ntst=int32(obj.Ntst);
            obj.Ncol=int32(obj.Ncol);
            obj.Nparx=int32(obj.Nparx);
            obj.Ifpr=int32(obj.Ifpr);
            obj.T=double(obj.T);
            obj.Tm=double(obj.Tm);
            obj.Par=double(obj.Par);
            obj.Rldot=double(obj.Rldot);
            obj.U=double(obj.U);
            obj.Ups=double(obj.Ups);
            obj.Udotps=double(obj.Udotps);
        end
    end
    
end
