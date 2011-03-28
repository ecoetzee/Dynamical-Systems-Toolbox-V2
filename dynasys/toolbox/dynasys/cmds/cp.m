% CP   copy output files generated from AUTO
%
%   CP('XXX','YYY') to copy the data-files b.XXX, s.XXX, d.XXX, c.XXX
%   to b.YYY, s.YYY, d.YYY, c.YYY, respectively.
%       
%   See also AP, CL, DF, DL, MV, SV

%   Altered from shell commands by Phani THOTA, James RANKIN, 
%   ETIENNE COETZEE, University of Bristol
%
%   $Revision: 1.0.0.0 $ $Date: 2008/12/17 10:11:00$
%
function []=cp(varargin)

if nargin==2
    try
        fileExt1=varargin{1};
        fileExt2=varargin{2};
        copyfile(['b.',fileExt1],['b.',fileExt2]);
        copyfile(['s.',fileExt1],['s.',fileExt2]);
        copyfile(['d.',fileExt1],['d.',fileExt2]);
        copyfile(['c.',fileExt1],['c.',fileExt2]);
        disp(['Copying b.',fileExt1,' to b.',fileExt2,'... done']);
        disp(['Copying s.',fileExt1,' to s.',fileExt2,'... done']);
        disp(['Copying d.',fileExt1,' to d.',fileExt2,'... done']);
        disp(['Copying c.',fileExt1,' to c.',fileExt2,'... done']);
    catch
        error(lasterr);
    end
else
    error('AUTOmatlab:InputArguments','Check number of input arguments');
end
