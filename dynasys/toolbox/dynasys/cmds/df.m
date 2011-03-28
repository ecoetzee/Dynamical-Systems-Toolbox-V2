% DF   delete output files generated from AUTO
%
%   DF to delete the output-files fort.7, fort.8, fort.9.
%       
%   See also AP, CL, CP, DL, MV, SV

%   Altered from shell commands by Phani THOTA, James RANKIN, 
%   ETIENNE COETZEE, University of Bristol
%
%   $Revision: 1.0.0.0 $ $Date: 2008/12/17 10:11:00$
%
function []=df(varargin)

if nargin==0
   delete('fort.*');
   disp('Deleting fort.* ... done');
else
    error('AUTOmatlab:InputArguments','No input arguments required');
end