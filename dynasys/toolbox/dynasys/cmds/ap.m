% AP   append output files generated from AUTO
%
%   AP('XXX') to append the output-files fort.7, fort.8, fort.9, 
%   to existing data-files b.XXX, s.XXX, d.XXX, resp.
%       
%   AP('XXX','YYY') to append b.XXX, s.XXX, d.XXX, to b.YYY, s.YYY, 
%   d.YYY, respectively.
%
%   See also CL, CP, DF, DL, MV, SV

%   Altered from shell commands by Phani THOTA, James RANKIN, 
%   ETIENNE COETZEE, University of Bristol
%
%   $Revision: 1.0.0.0 $ $Date: 2008/12/17 10:11:00$
%
function []=ap(varargin)

if nargin==1
    try
        fileExt=varargin{1};
        catf('fort.7',['b.',fileExt]);
        catf('fort.8',['s.',fileExt]);
        catf('fort.9',['d.',fileExt]);
        disp(['Appending fort.7 to b.',fileExt,'... done']);
        disp(['Appending fort.8 to s.',fileExt,'... done']);
        disp(['Appending fort.9 to d.',fileExt,'... done']);
    catch
        error(lasterr);
    end
elseif nargin==2
    try
        fileExt1=varargin{1};
        fileExt2=varargin{2};
        catf(['b.',fileExt1],['b.',fileExt2]);
        catf(['s.',fileExt1],['s.',fileExt2]);
        catf(['d.',fileExt1],['d.',fileExt2]);
        disp(['Appending b.',fileExt1,' to b.',fileExt2,'... done']);
        disp(['Appending s.',fileExt1,' to s.',fileExt2,'... done']);
        disp(['Appending d.',fileExt1,' to d.',fileExt2,'... done']);
    catch
        error(lasterr);
    end
else
    error('AUTOmatlab:InputArguments','Check number of input arguments');
end

%--------------------------------------------------------------------------
% Private function
%--------------------------------------------------------------------------
function []=catf(filer,filea)

try
    fida=fopen(filea,'a');
    fidr=fopen(filer,'r');

    while feof(fidr)==0
        lstr=fgets(fidr);
        fprintf(fida,'%c',lstr);
    end

    fclose(fida);
    fclose(fidr);

catch
    fclose(fida);
    fclose(fidr);
    error(lasterr);
end
