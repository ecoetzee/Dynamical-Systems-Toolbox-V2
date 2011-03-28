% MV   move output files generated from AUTO
%
%   MV('XXX','YYY') to move the data-files b.XXX, s.XXX, d.XXX, c.XXX 
%   to b.YYY, s.YYY, d.YYY, c.YYY, respectively.
%       
%   See also AP, CL, CP, DF, DL, SV

%   Altered from shell commands by Phani THOTA, James RANKIN, 
%   ETIENNE COETZEE, University of Bristol
%
%   $Revision: 1.0.0.0 $ $Date: 2008/12/17 10:11:00$
%
function []=mv(varargin)

if nargin==2
    try
        fileExt1=varargin{1};
        fileExt2=varargin{2};
        movefile(['b.',fileExt1],['b.',fileExt2]);
        movefile(['s.',fileExt1],['s.',fileExt2]);
        movefile(['d.',fileExt1],['d.',fileExt2]);
        movefile(['c.',fileExt1],['c.',fileExt2]);
        disp(['Renaming b.',fileExt1,' to b.',fileExt2,'... done']);
        disp(['Renaming s.',fileExt1,' to s.',fileExt2,'... done']);
        disp(['Renaming d.',fileExt1,' to d.',fileExt2,'... done']);
        disp(['Renaming c.',fileExt1,' to c.',fileExt2,'... done']);
    catch
        error(lasterr);
    end
else
    error('AUTOmatlab:InputArguments','Check number of input arguments');
end