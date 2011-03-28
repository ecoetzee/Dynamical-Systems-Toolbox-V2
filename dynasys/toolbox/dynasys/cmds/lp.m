% LP   list the value of the limit point function generated from AUTO
%
%   LP to list the value of the “limit point function” in the 
%   output-file fort.9. This function vanishes at a limit point (fold).
%           
%   LP('XXX') to list the value of the “limit point function” in the 
%   data-file d.XXX. This function vanishes at a limit point (fold).
%
%   See also BP, EV, FL, HB, IT, SP, ST, SZ

%   Altered from shell commands by Phani THOTA, James RANKIN, 
%   ETIENNE COETZEE, University of Bristol
%
%   $Revision: 1.0.0.0 $ $Date: 2008/12/17 10:11:00$
%
function []=lp(varargin)

if nargin==0
    fileName='fort.9';
elseif nargin==1
    fileExt=varargin{1};
    fileName=['d.',fileExt];
else
    error('AUTOmatlab:InputArguments','Check number of input arguments');
end

try
    a=read_file(fileName);
    b=regexp(a,'Fold');
    c=find(cellfun(@isempty,b)==0);
    
    for i=1:size(c,2)
        disp(a{c(i)});
    end
catch
    error(lasterr);
end

%--------------------------------------------------------------------------
% Private functions
%--------------------------------------------------------------------------
function [A, no_lines]=read_file(filename)

% This function reads in an ascii file and create a cell array from
% recogizing the  end of line character.

FAILED=-1;

no_lines=0;

fid = fopen(filename,'rt');

if (fid~=FAILED)

    % Read in, note it reads as a one dimensional array (column).
    F = fread(fid) ;
    fclose(fid);
   
    % Find the end of line characters
    eol = [0, find(F' == 10) ] ;
    
    % Convert into a character array(row)
    F = char(F');
    
    no_lines = length(eol) -1 ;
    
    for ( i = 1 : no_lines )
        A{i} = F( eol(i)+1 : eol(i+1)-1 ) ;
    end
    
    % Add the last line, in case there's no newline at the end of the file
    lastline = F(eol(no_lines+1)+1:end);
    if ~isempty(lastline)
        A{no_lines+1} = lastline;
    end

else
    % report error to log file and command window
    error(['Could not read file: ',filename]);
end
