% UZ   unzip zip file containing output files generated from AUTO
%
%   UZ to unzip all zip files in current working directory that 
%   contain output files that are generated from AUTO. 
%
%   UZ('XXX') to unzip XXX.zip
%
%   See also GZ

%   Altered from shell commands by Phani THOTA, James RANKIN, 
%   ETIENNE COETZEE, University of Bristol
%
%   $Revision: 1.0.0.0 $ $Date: 2008/12/17 10:11:00$
%
function []=uz(varargin)

if nargin==0
    z=ls('*.zip');
elseif nargin==1
    fileName=varargin{1};
    z=[fileName,'.zip'];
else
    error('AUTOmatlab:InputArguments','Check number of input arguments');
end

try
    for i=1:size(z,1)
        [pathName,fileName,extName,versn]=fileparts(z(i,:));
        unzip([fileName,'.zip']);
        disp(['Unzipped ',fileName,'.zip ... containing b.',fileName,', s.',fileName,', d.',fileName]);
    end
catch
    error(lasterr);
end

