function []=runauto07p(obj)

% RUNAUTO07P - calling function for running AUTO with input and output
%              files
%
%   RUNAUTO07P(OBJ) where OBJ is an AUTO.SIMOPTS object.
%
%   This function is the main function for running AUTO from MATLAB.
%
%   It copies the correct files for use by AUTO.
%   It also compiles a Simulink model if needed.
%   It then calls AUTO via the AUTO07GATEWAY mex function.
%
%   The following files are needed or generated from AUTO:
%
%   fort.2 : Constants file, copied from c.xxx
%   fort.3 : Restart file, copied from s.xxx (fort.8) 
%   fort.7 : Output file, resave as b.xxx
%   fort.8 : Output file, resave as s.xxx
%   fort.9 : Output file, resave as d.xxx
%
%   where xxx is the original name of the constants file, c.xxx   
%
%   Written by Phani THOTA, James RANKIN, Etienne COETZEE
%   University of Bristol
%
%   $Revision: 2.0.0.0 $ $Date: 2010/02/27 10:11:00$
%
slfile=[];

try
%--------------------------------------------------------------------------
% CLEAR ANY ALLOCATED ARRAYS
%--------------------------------------------------------------------------
% Clear mex variables from memory. This is important if crash occurs,
% seeing that memory has been allocated to certain variables. Program will
% crash if AUTO is then run again, because AUTO will try to allocate memory
% to variables that have already been allocated.
clear mex

%--------------------------------------------------------------------------
% CHECK FILE ARGUMENTS
%--------------------------------------------------------------------------
if nargin ~= 1
    error('DST:NumberOfArguments',...
        'Incorrect number of arguments passed to RUNAUTODST subroutine in RUNAUTO method of class AUTO');
end

if ~isobject(obj) || ~strcmp(class(obj),'auto')
    error('DST:AutoObject',...
        'The input argument of RUNAUTODST needs to be an object of AUTO class');
end

prop=properties(obj);

if eval(['~strcmp(class(obj.',prop{1},'),''autosimopts'')'])
    error('DST:SimulationOptions',...
        'The first property of RUNAUTODST needs to contain an object of AUTOSIMOPTS class');
end

eqnfile=deblank(obj.s.OutFileName);
slfile=deblank(obj.s.SimulinkModel);

if isempty(eqnfile)
    error('DST:OutputFileName',...
        'The FileName property needs to be specified if you want to run in 07P mode');
end

%--------------------------------------------------------------------------
% COPY CONSTANTS FILE
%--------------------------------------------------------------------------
copyfile(['c.',eqnfile],'fort.2');

%--------------------------------------------------------------------------
% DISPLAY SOME INFORMATION TO SCREEN
%--------------------------------------------------------------------------
disp([' ']);
disp(['    ----------------------    AUTO07 FOR MATLAB    ---------------------     ']);
disp([' ']);
disp(['USER NAME      : ',upper(getenv('USERNAME'))]);
disp(['DATE           : ',datestr(now,'dd/mm/yyyy HH:MM:SS')]);
disp([' ']);
disp(['MODEL NAME     : ',eqnfile]);
disp(['CONSTANTS FILE : c.',eqnfile]);

if ~isempty(slfile)
disp(['SIMULINK MODEL : ',slfile]);    
end

%--------------------------------------------------------------------------
% RESTART INFORMATION
%--------------------------------------------------------------------------
% Determine whether or not the s. file is present, and if so, copy it to
% fort.3 so it may be used by AUTO
restartfile=['s.',eqnfile];

if exist(fullfile(pwd,restartfile))==2
    copyfile(restartfile,'fort.3');
    disp(['RESTART FILE   : ', restartfile]);
end

%--------------------------------------------------------------------------
% COMPILE SIMULINK MODELS IF NEEDED
%--------------------------------------------------------------------------
% This function is only really used if a Simulink model is being used to
% obtain the derivatives. The model needs to be compiled before it is
% possible to obtain the derivatives
if ~isempty(slfile) && strcmp(get_param(slfile, 'SimulationStatus'),'stopped')
  feval(slfile,[],[],[],'compile');
end
 

%--------------------------------------------------------------------------
% CALL AUTO GATEWAY ROUTINE, START AUTO
%--------------------------------------------------------------------------
disp([' ']);
disp(['<']);

AUTO07gateway(obj.s);

disp(['>']);

%--------------------------------------------------------------------------
% RELEASE SIMULINK MODELS IF NEEDED
%--------------------------------------------------------------------------
if ~isempty(slfile) && strcmp(get_param(slfile, 'SimulationStatus'),'paused')
  feval(slfile,[],[],[],'term');
end

%--------------------------------------------------------------------------
% COPY OUTPUT FILES
%--------------------------------------------------------------------------
if strcmp(obj.s.Fort7,'on')
    if exist('fort.7')==2
      copyfile('fort.7',['b.',eqnfile]);
    end
end
if strcmp(obj.s.Fort8,'on')
    if exist('fort.8')==2
      copyfile('fort.8',['s.',eqnfile]);
    end
end
if strcmp(obj.s.Fort9,'on')
    if exist('fort.9')==2
      copyfile('fort.9',['d.',eqnfile]);
    end
end

%--------------------------------------------------------------------------
% CLEAN UP MEX VARIABLES AND CLOSE FILES
%--------------------------------------------------------------------------
cleanup(slfile);

catch runauto07pError
    cleanup(slfile);
    error(runauto07pError.message);
end

%--------------------------------------------------------------------------
function []=cleanup(slfile)

% Clear mex variables from memory. This is important if crash occurs,
% seeing that memory has been allocated to certain variables. Program will
% crash if AUTO is then run again, because AUTO will try to allocate memory
% to variables that have already been allocated.
clear mex

% close all possible open files
fclose('all');

if isempty(slfile)
    return
end

% close open simulink model
if strcmp(get_param(slfile, 'SimulationStatus'),'paused')
  feval(slfile,[],[],[],'term');
end




