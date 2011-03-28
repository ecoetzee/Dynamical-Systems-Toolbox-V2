% GZ   zip output files generated from AUTO
%
%   GZ to compress all output files in current working directory that 
%   are generated from AUTO. 
%
%   GZ('XXX') to  compress b.XXX, s.XXX, d.XXX into a file called 
%   XXX.zip
%
%   See also UZ

%   Altered from shell commands by Phani THOTA, James RANKIN, 
%   ETIENNE COETZEE, University of Bristol
%
%   $Revision: 1.0.0.0 $ $Date: 2008/12/17 10:11:00$
%
function []=gz(varargin)

if nargin==0
    b=ls('b.*');
    s=ls('s.*');
    d=ls('d.*');
elseif nargin==1
    extName=varargin{1};
    b=['b.',extName];
    s=['s.',extName];
    d=['d.',extName];
else
    error('AUTOmatlab:InputArguments','Check number of input arguments');
end

try
    for i=1:size(b,1)
        [pathName,fileName,extName,versn]=fileparts(b(i,:));
        extName=deblank(extName);
        zip(extName(2:end),{['b',extName],['s',extName],['d',extName]});
        disp(['Zipped b',extName,', s',extName,', d',extName,' into ... ',extName(2:end),'.zip']);
    end
catch
    error(lasterr);
end

