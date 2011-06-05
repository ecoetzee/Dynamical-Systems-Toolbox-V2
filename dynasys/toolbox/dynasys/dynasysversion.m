function [version, versionDate] = dynasysversion()
%dynasysVersion  get the toolbox version and date
%
%   V = dynasysversion() returns just the version string
%
%   [V,D] = dynasysVersion() returns both the version string and the date of
%   creation (format is ISO8601, i.e. YYYY-MM-DD)
%
%   Examples:
%   >> [v,d] = layoutVersion()
%   v = '1.0'
%   d = '2010-05-28'
%
%   See also: dynasysroot

%   $Revision: 356 $    
%   $Date: 2010-11-02 10:02:00 +0000 (Tue, 02 Nov 2010) $


% Changed error traps to rethrow last error. 
% Changed Noutx to set default outputs to 20 instead of empty.
% Fixed option to be able to set file outputs to on and off. It did not
% switch off once it was set to on.
version = '2.4.5';
versionDate = '2011-06-05';

% version = '2.4.4';
% versionDate = '2011-02-26';

