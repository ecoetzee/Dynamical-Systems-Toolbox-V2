function [dsthname] = dynasysroot( varargin)
%DYNASYSROOT Dynamical Systems Toolbox root directory 
% This function provides the root directory for the toolbox functions, 
% seeing that the toolboxdir command might not always give the correct answer
% if the DST toolbox was installed before

%   Altered from shell commands by Etienne COETZEE, James RANKIN,and
%   Phani THOTA, University of Bristol
%
%   $Revision: 2.0.0.0 $ $Date: 2010/07/29 14:07:00$
dsthstr=mfilename('fullpath');
[dsthname,fname]=fileparts(dsthstr);

end

