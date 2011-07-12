function varargout = installdynasys(varargin)
% INSTALLDYNASYS M-file for installdynasys.fig
%      INSTALLDYNASYS, by itself, creates a new INSTALLDYNASYS or raises the existing
%      singleton*.
%
%      H = INSTALLDYNASYS returns the handle to a new INSTALLDYNASYS or the handle to
%      the existing singleton*.
%
%      INSTALLDYNASYS('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in INSTALLDYNASYS.M with the given input arguments.
%
%      INSTALLDYNASYS('Property','Value',...) creates a new INSTALLDYNASYS or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before installdynasys_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to installdynasys_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help installdynasys

% Last Modified by GUIDE v2.5 12-Jul-2011 11:30:12

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @installdynasys_OpeningFcn, ...
                   'gui_OutputFcn',  @installdynasys_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

% check version numbers
v = ver( 'MATLAB' );
matlabVersionDST='MATLAB R2009A';

if  datenum(v.Date) < 733788
    prompt = sprintf('MATLAB version too old: This package requires %s or newer.\nDo you want to continue?', ...
        matlabVersionDST);
    title = 'Incorrect MATLAB version';
    response = questdlg( prompt, title, ...
        'Install anyway', 'No, don''t install', 'No, don''t install');
    switch upper(response)
        case 'INSTALL ANYWAY'
            % Do nothing
        otherwise
            disp('MATLAB version too old - installation abandoned.');
            return;
    end
    
end

% build gui
if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


%--------------------------------------------------------------------------
function installdynasys_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to installdynasys (see VARARGIN)

% Choose default command line output for installdynasys
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% Place matlabroot installation defaults in boxes. Check to see if you can
% install to Matlab root directory. If so, use this is as default.
% Otherwise use home directory of user.
[Success,Message,MessageID] = fileattrib([matlabroot,filesep,'Toolbox']);

if Message.UserWrite == 1
    InstallDir=[matlabroot,filesep,'toolbox'];
else
    if ispc
        InstallDir=[getenv('USERPROFILE'),filesep,'Desktop'];
    elseif isunix
        InstallDir=[getenv('HOME'),filesep,'Desktop'];
    else
        error('Could not recognise machine type')
    end
    set(handles.installcb, 'Value', 0, 'Enable', 'off');
end

installcb_Callback(handles.installcb, [], handles);
set(handles.srcfiledit,'String',InstallDir);      
movegui('center');

% UIWAIT makes installdynasys wait for user response (see UIRESUME)
% uiwait(handles.installdynasysfig);


%--------------------------------------------------------------------------
function varargout = installdynasys_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


%--------------------------------------------------------------------------
function installcb_Callback(hObject, eventdata, handles)
% hObject    handle to installcb (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of installcb
if get(hObject,'Value') == 1
    bslh=filesep;
    set(handles.srcfiledit,'String',[matlabroot,filesep,'toolbox']);
    set(handles.srcfiledit,'Enable','off');
    set(handles.srcbrowsepb,'Enable','off'); 
else
    set(hObject,'Value',0);
    set(handles.srcfiledit,'Enable','on');
    set(handles.srcbrowsepb,'Enable','on');     
end

%--------------------------------------------------------------------------
function srcfiledit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to srcfiledit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

%--------------------------------------------------------------------------
function srcfiledit_Callback(hObject, eventdata, handles)
% hObject    handle to helpfiledit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of helpfiledit as text
%        str2double(get(hObject,'String')) returns contents of helpfiledit
%        as a double

%--------------------------------------------------------------------------
function srcbrowsepb_Callback(hObject, eventdata, handles)
% hObject    handle to srcbrowsepb (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

dirname=uigetdir;

if dirname==0
    return
end

set(handles.srcfiledit,'String',dirname);
    
%--------------------------------------------------------------------------
function installpb_Callback(hObject, eventdata, handles)
% hObject    handle to installpb (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
warning('off','MATLAB:HandleGraphics:ObsoletedProperty:JavaFrame');

try
set(gcf,'Pointer','watch');
drawnow;

srcdir=get(handles.srcfiledit,'String');
startupdir=srcdir;

setstatusbar(gcf,'Checking installation directory attributes...');pause(2);
[Success,Message,MessageID] = fileattrib(srcdir);

if Message.UserWrite==0
    setstatusbar(gcf,'');
    set(gcf,'Pointer','arrow');
    errordlg('Installation directory does not have write access.');
    return
end
pause(2);

fs=filesep;

setstatusbar(gcf,'Creating installation directories...');pause(2);

if ~isempty(strmatch(matlabroot,srcdir))
    helpdir=sprintf('%s%stoolbox%sdynasys',docroot,fs,fs);
    srcdir=sprintf('%s%sdynasys',srcdir,fs);
else
    helpdir=sprintf('%s%sdynasys%shelp%stoolbox%sdynasys',srcdir,fs,fs,fs,fs);
    srcdir=sprintf('%s%sdynasys%stoolbox%sdynasys',srcdir,fs,fs,fs);
end

srcdir=strrep(srcdir,'\\','\');
srcdir=strrep(srcdir,'//','/');
srcdir=strrep(srcdir,'/',fs);
srcdir=strrep(srcdir,'\',fs);

helpdir=strrep(helpdir,'\\','\');
helpdir=strrep(helpdir,'//','/');
helpdir=strrep(helpdir,'/',fs);
helpdir=strrep(helpdir,'\',fs);

status1=mkdir(srcdir);
status2=mkdir(helpdir);
if status1==0 || status2==0
    error('Could not create installation directory. Check access rights or if directory already exists');
end

rtdir=sprintf('%s%sdynasys',pwd,fs);
setstatusbar(gcf,'Copying source files...');
copyfile(sprintf('%s%stoolbox%sdynasys',rtdir,fs,fs),srcdir,'f');
setstatusbar(gcf,'Copying help files...');
copyfile(sprintf('%s%shelp%stoolbox%sdynasys',rtdir,fs,fs,fs),helpdir,'f');

% update path definition
if ~isempty(strmatch(matlabroot,srcdir))
    setstatusbar(gcf,'Adding paths...');pause(2);

    % add paths to pathdef.m file if checkbox is off
    addpath(helpdir);
    addpath(srcdir);
    addpath(sprintf('%s%scmds',srcdir,fs));
    addpath(sprintf('%s%sicons',srcdir,fs));
    addpath(sprintf('%s%sdynasysdemos',srcdir,fs));
    addpath(sprintf('%s%ssrc',srcdir,fs));
    addpath(sprintf('%s%sutils%sautoobj',srcdir,fs,fs));
    addpath(sprintf('%s%sutils%sautoconst',srcdir,fs,fs));
    addpath(sprintf('%s%sutils%splaut',srcdir,fs,fs));
    addpath(sprintf('%s%sutils%sflightviz',srcdir,fs,fs));
    addpath(sprintf('%s%sutils%sfiguremanagement',srcdir,fs,fs));
    
    % update path definition file
    savepath;
else
    % add paths to a startup file if checkbox is on
    setstatusbar(gcf,'Creating startup file...');pause(2);

    fid=fopen('startup.m','w');
    
    if fid<0
        return
    end
    
    fprintf(fid,'addpath(''%s'');\n',helpdir);
    fprintf(fid,'addpath(''%s'');\n',srcdir);
    fprintf(fid,'addpath(''%s'');\n',sprintf('%s%scmds',srcdir,fs));
    fprintf(fid,'addpath(''%s'');\n',sprintf('%s%sicons',srcdir,fs));
    fprintf(fid,'addpath(''%s'');\n',sprintf('%s%sdynasysdemos',srcdir,fs));
    fprintf(fid,'addpath(''%s'');\n',sprintf('%s%ssrc',srcdir,fs));
    fprintf(fid,'addpath(''%s'');\n',sprintf('%s%sutils%sautoobj',srcdir,fs,fs));
    fprintf(fid,'addpath(''%s'');\n',sprintf('%s%sutils%sautoconst',srcdir,fs,fs));
    fprintf(fid,'addpath(''%s'');\n',sprintf('%s%sutils%splaut',srcdir,fs,fs));
    fprintf(fid,'addpath(''%s'');\n',sprintf('%s%sutils%sflightviz',srcdir,fs,fs));
    fprintf(fid,'addpath(''%s'');\n',sprintf('%s%sutils%sfiguremanagement',srcdir,fs,fs));
    
    fclose(fid);
    
    movefile('startup.m',[startupdir,'\startup.m']);
    
end

setstatusbar(gcf,'Coping info.xml file...');pause(2);
   
% change info.xml if not installing to matlab root directory
if get(handles.installcb,'Value')==0

   % read from template file and replace directory string
   A=readfile('info_template.xml');
   l=A{10};
   if ispc
      st=helpdir;
   else
      st=sprintf('..%s..%shelp%stoolbox%sdynasys',fs,fs,fs,fs); 
   end 
   l=strrep(l,'XXXX',st); 
   A{10}=l;
   
   % write to new info file
   fid=fopen('info.xml','w');
   
    if fid<0
        return
    end
    
   for i=1:length(A)
      fprintf(fid,'%s\n',A{i}); 
   end
   
   fclose(fid);
   
   movefile('info.xml',srcdir,'f');
end

% clean up
setstatusbar(gcf,'');
set(gcf,'Pointer','arrow');

catch
   setstatusbar(gcf,'');
   set(gcf,'Pointer','arrow');   fclose('all');
   errordlg(lasterr);
end

%--------------------------------------------------------------------------
function closepb_Callback(hObject, eventdata, handles)
% hObject    handle to closepb (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
close(gcf)

%--------------------------------------------------------------------------
function A=readfile(filename)

fid=fopen(filename,'r');

if fid < 0
    error('Failed top open AUTO output file');
end

count=1;

while feof(fid)==0
   A{count}=fgetl(fid);
   count=count+1;  
end

%--------------------------------------------------------------------------
function userDir = getuserdir(varargin)
%GETUSERDIR   return the user home directory.
%   USERDIR = GETUSERDIR returns the user home directory using the registry
%   on windows systems and using Java on non windows systems as a string
%
%   Example:
%      getuserdir() returns on windows
%           C:\Documents and Settings\MyName\Eigene Dateien

if ispc
    userDir = winqueryreg('HKEY_CURRENT_USER',...
        ['Software\Microsoft\Windows\CurrentVersion\' ...
         'Explorer\Shell Folders'],'Personal');
else
    userDir = char(java.lang.System.getProperty('user.home'));
end

%--------------------------------------------------------------------------
function []=setstatusbar(hFig,statusText)

if ~ishandle(hFig)
    return
else
    jFrame = get(hFig,'JavaFrame');
    jRootPane = jFrame.fFigureClient.getWindow;
    statusbarObj = com.mathworks.mwswing.MJStatusBar;
    jRootPane.setStatusBar(statusbarObj);
    statusbarObj.setText(statusText);
end
