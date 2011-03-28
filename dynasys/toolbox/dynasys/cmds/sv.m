% SV   save output files generated from AUTO
%
%   SV('XXX') to save the output-files fort.7, fort.8, fort.9, as 
%   b.XXX, s.XXX, d.XXX, respectively. Existing files by these names 
%   will be deleted.
%
%   See also AP, CL, CP, DF, DL, MV

%   Altered from shell commands by Phani THOTA, James RANKIN, 
%   ETIENNE COETZEE, University of Bristol
%
%   $Revision: 1.0.0.0 $ $Date: 2008/12/17 10:11:00$
%
function []=sv(varargin)

if nargin==1 
    fileExt=varargin{1};
	catf('fort.7',['b.',fileExt]);
	catf('fort.8',['s.',fileExt]);
	catf('fort.9',['d.',fileExt]);
	disp(['Saving fort.7 as b.',fileExt,'... done']);
	disp(['Saving fort.8 as s.',fileExt,'... done']);
	disp(['Saving fort.9 as d.',fileExt,'... done']);
else
	error('AUTOmatlab:InputArguments','Check number of input arguments');
end

%--------------------------------------------------------------------------
% Private function
%--------------------------------------------------------------------------
function []=catf(filer,filew)

try
    fidw=fopen(filew,'w');
    fidr=fopen(filer,'r');

    while feof(fidr)==0
        lstr=fgets(fidr);
        fprintf(fidw,'%c',lstr);
    end

    fclose(fidw);
    fclose(fidr);

catch
    fclose(fidw);
    fclose(fidr);
    error(lasterr);
end
