function text = removeExtraNewlines(text,type)
%  REMOVEEXTRANEWLINES  Remove extra newlines from the beginning and/or end.

% Copyright 1984-2007 The MathWorks, Inc.
% $Revision: 1.1.6.2 $  $Date: 2007/12/14 14:51:53 $

newLine = char(10);

if (nargin < 2)
    type = 'both';
end

if isempty(strmatch(type,{'beginning','trailing','both'},'exact'))
    error('MATLAB:removeExtraNewlines:BadType','Type must be "beginning", "trailing", or "both".')
end

if ~isequal(type,'trailing')
    % Remove extra newlines from the beginning.
    newLine = char(10);
    text = text(min(find((text ~= newLine))):end);
end

if ~isequal(type,'beginning')
    % Remove extra newlines from the end of the string.
    chop = regexp(text,[newLine '*$']);
    if ~isempty(chop)
        text = text(1:(chop-1));
    end
end
