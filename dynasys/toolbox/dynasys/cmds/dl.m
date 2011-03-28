% DL   delete output files generated from AUTO
%
%   DL('XXX') to delete the data-files b.XXX, s.XXX, d.XXX.
%
%   See also AP, CL, CP, DF, MV, SV

%   Altered from shell commands by Phani THOTA, James RANKIN, 
%   ETIENNE COETZEE, University of Bristol
%
%   $Revision: 1.0.0.0 $ $Date: 2008/12/17 10:11:00$
%
function []=dl(varargin)

if nargin==1
    try
        fileExt=varargin{1};
        delete(['b.',fileExt]);
        delete(['s.',fileExt]);
        delete(['d.',fileExt]);
        disp('Deleting b.',fileExt,' ... done');
        disp('Deleting s.',fileExt,' ... done');
        disp('Deleting d.',fileExt,' ... done');
    catch
        error(lasterr);
    end
else
    error('AUTOmatlab:InputArguments','One input argument required');
end