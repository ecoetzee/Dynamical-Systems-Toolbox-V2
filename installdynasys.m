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

% Last Modified by GUIDE v2.5 05-Mar-2010 08:11:26

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

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before installdynasys is made visible.
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

% Place matlabroot installation defaults in boxes
bslh=filesep;
set(handles.srcfiledit,'String',matlabroot);

hdrive=getenv('HOMEDRIVE');
hpath=getenv('HOMEPATH');
fs=filesep;

if ispc
    hdrive=getenv('HOMEDRIVE');
    hpath=getenv('HOMEPATH');
    hpd=[hdrive,filesep,hpath,filesep];
    hpd=strrep(hpd,'\\',fs);
    hpd=strrep(hpd,'\',fs);
    hpd=strrep(hpd,'//',fs);
    hpd=strrep(hpd,'/',fs);    
    set(handles.startupfiledit,'String',hpd);
    set(handles.startupfiledit,'Enable','off');
    set(handles.startupbrowsepb,'Enable','off');
elseif isunix
    hdrive=getenv('HOME');
    set(handles.startupfiledit,'String',[hdrive,filesep,'Desktop']);
    set(handles.startupfiledit,'Enable','off');
    set(handles.startupbrowsepb,'Enable','off');
else
    error('Could not recognise machine type')
end
    
movegui('center');

% UIWAIT makes installdynasys wait for user response (see UIRESUME)
% uiwait(handles.installdynasysfig);


% --- Outputs from this function are returned to the command line.
function varargout = installdynasys_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


% --- Executes on button press in installcb.
function installcb_Callback(hObject, eventdata, handles)
% hObject    handle to installcb (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of installcb
if get(hObject,'Value') == 1
    bslh=filesep;
    set(handles.srcfiledit,'String',matlabroot);
    set(handles.srcfiledit,'Enable','off');
    set(handles.srcbrowsepb,'Enable','off');
else
    set(hObject,'Value',0);
    set(handles.srcfiledit,'Enable','on');
    set(handles.srcbrowsepb,'Enable','on');
end

% --- Executes during object creation, after setting all properties.
function srcfiledit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to srcfiledit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

function srcfiledit_Callback(hObject, eventdata, handles)
% hObject    handle to helpfiledit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of helpfiledit as text
%        str2double(get(hObject,'String')) returns contents of helpfiledit
%        as a double

% --- Executes on button press in srcbrowsepb.
function srcbrowsepb_Callback(hObject, eventdata, handles)
% hObject    handle to srcbrowsepb (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

dirname=uigetdir;

if dirname==0
    return
end

bslh=filesep;
set(handles.srcfiledit,'String',dirname);
    
% --- Executes on button press in startupbrowsepb.
function startupbrowsepb_Callback(hObject, eventdata, handles)
% hObject    handle to startupbrowsepb (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
dirname=uigetdir;

if dirname==0
    return
end

set(handles.startupfiledit,'String',dirname);


function startupfiledit_Callback(hObject, eventdata, handles)
% hObject    handle to startupfiledit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of startupfiledit as text
%        str2double(get(hObject,'String')) returns contents of startupfiledit as a double


% --- Executes during object creation, after setting all properties.
function startupfiledit_CreateFcn(hObject, eventdata, handles)
% hObject    handle to startupfiledit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --- Executes on button press in startupcb.
function startupcb_Callback(hObject, eventdata, handles)
% hObject    handle to startupcb (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of startupcb
if get(hObject,'Value') == 1
    set(handles.startupfiledit,'Enable','on');
    set(handles.startupbrowsepb,'Enable','on');
else
    set(handles.startupfiledit,'Enable','off');
    set(handles.startupbrowsepb,'Enable','off');
end

% --- Executes on button press in installpb.
function installpb_Callback(hObject, eventdata, handles)
% hObject    handle to installpb (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
try
disp('Installing Dynamical Systems Toolbox...');
set(gcf,'Pointer','watch');
drawnow;

srcdir=get(handles.srcfiledit,'String');
fs=filesep;

if strcmp(srcdir,matlabroot)
    helpdir=sprintf('%s%shelp%stoolbox%sdynasys',srcdir,fs,fs,fs);
    srcdir=sprintf('%s%stoolbox%sdynasys',srcdir,fs,fs);
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

startdir=get(handles.startupfiledit,'String');

status1=mkdir(srcdir);
status2=mkdir(helpdir);
if status1==0 || status2==0
    error('Could not create installation directory. Check access rights or if directory already exists');
end

rtdir=sprintf('%s%sdynasys',pwd,fs);
copyfile(sprintf('%s%stoolbox%sdynasys',rtdir,fs,fs),srcdir,'f');
copyfile(sprintf('%s%shelp%stoolbox%sdynasys',rtdir,fs,fs,fs),helpdir,'f');

% update path definition
if get(handles.startupcb,'Value')==0
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
    
    copyfile('startup.m',[startdir,'\startup.m']);
    
end

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
   
   copyfile('info.xml',srcdir,'f');
end

% clean up
disp('Finished with installation')
set(gcf,'Pointer','arrow');

catch
   set(gcf,'Pointer','arrow');
   fclose('all');
   error(lasterr);
end

% --- Executes on button press in closepb.
function closepb_Callback(hObject, eventdata, handles)
% hObject    handle to closepb (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
close(gcf)


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
