function [dsthname] = dynasyshelproot( varargin)
%DYNASYSHELPROOT Dynamical Systems Toolbox root directory for help documents
% This function provides the root directory for the toolbox help functions, 
% seeing that the toolboxdir command might not always give the correct answer
% if the DST toolbox was installed before

dsthstr=mfilename('fullpath');
[dsthname,fname]=fileparts(dsthstr);

end

