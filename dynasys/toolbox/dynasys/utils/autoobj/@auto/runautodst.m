function obj=runautodst(obj)

% RUNAUTODST - calling function for running AUTO with input and output
%              files
%
%   obj=RUNAUTODST(obj) 
%
%   This function is the main function for running the pure MATLAB version 
%   of AUTO, also known as the Dynamical Systems Toolbox
%
%   It compiles a Simulink model if needed.
%   It then calls AUTO via the AUTO07GATEWAY mex function.
%
%   The following objects are needed from the AUTO object:
%
%   obj.s  : Simulation options object (autosimopts)
%   obj.c  : Constants object (autoconstants)
%   obj.f7 : Output object (autof7)
%   obj.f8 : Restart object (autof8)
% 

%   Written by Phani THOTA, James RANKIN, Etienne COETZEE
%   University of Bristol
%
%   $Revision: 2.0.0.0 $ $Date: 2010/02/27 17:48:00$
%

slfile=obj.s.SimulinkModel;

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

if length(prop)~=4
    error('DST:PropertyDefinition',...
        'The number of properties contained in AUTO object is incorrect. Needs 4 properties');    
end

if eval(['~strcmp(class(obj.',prop{1},'),''autosimopts'')'])
    error('DST:SimulationOptions',...
        'The first property of RUNAUTODST needs to contain an object of AUTOSIMOPTS class');
end

if eval(['~strcmp(class(obj.',prop{2},'),''autoconstants'')'])
    error('DST:SimulationOptions',...
        'The second property of RUNAUTODST needs to contain an object of AUTOCONSTANTS class');
end

if eval(['~strcmp(class(obj.',prop{3},'),''autof7'')'])
    error('DST:SimulationOptions',...
        'The third property of RUNAUTODST needs to contain an object of AUTOF7 class');
end

if eval(['~strcmp(class(obj.',prop{4},'),''autof8'')'])
    error('DST:SimulationOptions',...
        'The fourth property of RUNAUTODST needs to contain an object of AUTOF8 class');
end

if strcmp(obj.s.Fort7,'on') || strcmp(obj.s.Fort8,'on') || strcmp(obj.s.Fort9,'on')
    if isempty(obj.s.OutFileName)
    error('DST:OutputFileName',...
        'The OutFileName property needs to be specified in the AUTO object if you want to write output files');
    else 
      eqnfile=obj.s.OutFileName;
    end
end

%--------------------------------------------------------------------------
% COMPILE SIMULINK MODELS IF NEEDED
%--------------------------------------------------------------------------
% This function is only really used if a Simulink model is being used to
% obtain the derivatives. The model needs to be compiled before it is
% possible to obtain the derivatives
if ~isempty(slfile) 
    open_system(slfile);    
    if strcmp(get_param(slfile, 'SimulationStatus'),'stopped')
      feval(slfile,[],[],[],'compile');
    end
end

%--------------------------------------------------------------------------
% INITIAL AND RESTART INFORMATION
%--------------------------------------------------------------------------
% Make sure U vector is correct size, otherwise wrong 
if obj.c.Ndim ~= length(obj.s.U0) && obj.c.Ips~=2
      str='Truncating U0 to length defined by Ndim';
      warning('DST:InitialInputVectorLength',str);    
      obj.s.U0=obj.s.U0(1:obj.c.Ndim);
end

if obj.c.Irs==0
  if obj.c.Noutx > 0
    if isempty(obj.s.Out0)  
      str='No initial outputs given yet Noutx is larger than 0. Trying to initalise';
      warning('DST:InitialOutputsVector',str);
      [F,Out0]=feval(obj.s.FuncFileName,obj.s.Par0,obj.s.U0);
      Nout=length(Out0);
      if Nout>obj.c.Noutx
        obj.s.Out0=Out0(1:obj.c.Noutx);
      else 
        obj.s.Out0=Out0;
      end
    end
  end
end

% Determine whether or not restart data is present according to label
if obj.c.Irs>0
    if isempty(obj.f8.Lab)
    error('DST:RestartDataObject',...
        'Restart label specified, but F8 object is empty');
    end
    if isempty(find(obj.f8.Lab==obj.c.Irs,1))
    error('DST:RestartDataObject',...
        'Restart label specified in Irs property not found in F8 object');        
    end
end

%--------------------------------------------------------------------------
% DISPLAY SOME INFORMATION TO SCREEN
%--------------------------------------------------------------------------
disp([' ']);
disp(['    --------------- DYNAMICAL SYSTEMS TOOLBOX ---------------------     ']);
disp([' ']);
disp(['USER NAME      : ',upper(getenv('USERNAME'))]);
disp(['DATE           : ',datestr(now,'dd/mm/yyyy HH:MM:SS')]);
disp([' ']);

if ~isempty(slfile)
disp(['SIMULINK MODEL : ',slfile]);    
end

%--------------------------------------------------------------------------
% CALL AUTO GATEWAY ROUTINE, START AUTO
%--------------------------------------------------------------------------
disp([' ']);
disp(['<']);

[obj.f7,obj.f8]=AUTO07gateway(obj.s,obj.c,obj.f7,obj.f8);

disp(['>']);

%--------------------------------------------------------------------------
% RELEASE SIMULINK MODELS IF NEEDED
%--------------------------------------------------------------------------
if ~isempty(slfile) && strcmp(get_param(slfile, 'SimulationStatus'),'paused')
  feval(slfile,[],[],[],'term');
end

%--------------------------------------------------------------------------
% COPY OUTPUT FILES IF REQUESTED
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

catch runautodstError
    cleanup(slfile);
    error(runautodstError.message);
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




