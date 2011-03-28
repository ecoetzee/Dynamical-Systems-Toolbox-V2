% CL   delete output files generated from AUTO and mex compilation
%
%   CL to clean the current directory. This command will delete 
%   all files of the form fort.*, *.obj, and *.mexw32.
%       
%   See also AP, CP, DF, DL, MV, SV

%   Altered from shell commands by Phani THOTA, James RANKIN, 
%   ETIENNE COETZEE, University of Bristol
%
%   $Revision: 1.0.0.0 $ $Date: 2008/12/17 10:11:00$
%
function []=cl(varargin)

if nargin==0
   delete('fort.*');
   delete('*.obj');
   delete('*.mexw32');
   disp('Deleting fort.* *.obj *.mexw32 *.*~ ... done');
else
    error('AUTOmatlab:InputArguments','No input arguments required');
end