classdef ac3d < hgsetget
    %AC3D - extract information from AC3D file into a format that can be used
    %       by flightviz routine
    
    properties
        file=[];
        name=[];
        consurf=[];
        loc=[];
        mat=[];
        col=[];
        lit=[];
        alp=[];
        x=[];
        y=[];
        z=[];
        tri=[];
        ref=[];
        parent=[];
    end
    
    methods
        function obj=ac3d
            
        end
        
        function obj=ac3dparse(obj,varargin)
            
            if size(obj.file,2)>1
                file_=obj(1).file;
            else
                file_=obj.file;
            end
            
            if isempty(file_) || isempty(which(file_))
                error('No valid AC3D input file defined');
            end
            
            fprintf(1,'Reading AC3D file %s...\n',obj.file);
            dat=readfile(obj.file);
            fprintf(1,'Building objects...\n',obj.file);
            obj=parsedata(obj,dat);
            fprintf(1,'Finished\n',obj.file);
            
        end
        
        function obj=ac3d2mat(obj,varargin)
            
            if isempty(varargin)>0
                val=[];
            else
                val=varargin{1};
            end
            
            fprintf(1,'Converting to flightviz structure...\n',obj.file);
            obj=con2fvz(obj,val);
            
        end
        
        function obj=set.file(obj,val)
            
            if isempty(which(val))
                [file_,path]=uigetfile('*.ac','AC3D Input File');
            else
                file_=val;
            end
            
            if (isempty(file_))||(isinteger(file_))
                return
            end
            
            obj.file=file_;
            
        end
    end
    
end

%--------------------------------------------------------------------------
function dat=readfile(fName)

fid=fopen(fName,'r');

if fid < 0
    error('Failed to open AC3D input file');
end

count=1;

while feof(fid)==0
    dat{count}=deblank(fgets(fid));
    count=count+1;
end

fclose(fid);
end

%--------------------------------------------------------------------------
function obj=parsedata(obj,dat)

dbstop if error

numsurf_idx=strmatch('numsurf ',dat);
kids_idx=strmatch('kids ',dat);

% extract materials
idx=strmatch('MATERIAL "',dat);
r=length(idx);
color=cell(r,1);

for i=1:length(idx)
    str=dat{idx(i)};
    istart=findstr(str,'rgb');
    iend=findstr(str,'amb');
    str=str(istart+3:iend-1);
    color{i}=str2num(str);
end

% extract name of component
idx=strmatch('name "',dat);
lps=length(idx);

for i=1:length(idx)
    str=deblank(dat{idx(i)});
    str=strrep(str,'name "','');
    str=strrep(str,'"','');
    str=strrep(str,'-','_');
    obj(i).name=lower(str);
end

% extract name of component
idx=strmatch('loc ',dat);

for i=1:length(idx)
    str=deblank(dat{idx(i)});
    str=strrep(str,'loc ','');
    obj(i).loc=str2num(str);
end

% number of points in component with coordinates
idx=strmatch('numvert ',dat);

for i=1:length(idx)
    str=deblank(dat{idx(i)});
    str=strrep(str,'numvert','');
    numvert=str2num(str);
    idxyz=idx(i)+1:idx(i)+numvert;
    xyz=str2num(char(dat{idxyz}))+ones(numvert,1)*obj(i).loc;
    obj(i).x=xyz(:,1);
    obj(i).y=xyz(:,2);
    obj(i).z=xyz(:,3);
end

% extract triangles
numsurf_idx=strmatch('numsurf ',dat);
kids_idx=strmatch('kids ',dat);

if kids_idx(1)<numsurf_idx
    kids_idx(1)=[];
end

loops=length(numsurf_idx);

for i=1:loops
    idx=numsurf_idx(i)+1:kids_idx(i)-1;
    str=dat(idx)';
    
    mat_idx=strmatch('mat ',str);
    refs_idx=strmatch('refs ',str);
    c=[];
    tri=[];
    
    if i==8
        disp('');
    end
    
    for j=1:length(mat_idx)
        idxc=str2num(strrep(str{mat_idx(j)},'mat ',''))+1;
        ctmp=color{idxc};
        idxr=str2num(strrep(str{refs_idx(j)},'refs ',''));
        tritmp=str2num(char(str{refs_idx(j)+1:refs_idx(j)+idxr}));
        
        c=[c;ctmp];
        
        if idxr==3
            tri=[tri;tritmp(:,1)'];
        else
            % split polygon into two triangles
            tritmp=tritmp(:,1)';
            tri=[tri;tritmp([1,2,3])];
            tri=[tri;tritmp([3,4,1])];
        end
        
    end
    
    obj(i).col=color{idxc};
    obj(i).tri=tri+1;
    
end

% other settings
for i=1:lps
    obj(i).mat='dull';
    obj(i).lit='gouraud';
    obj(i).alp=1;
    obj(i).ref=[0 0 0 0 0 0];
    obj(i).parent=1;
end

% determine if control surface
for i=1:lps
    if ~isempty(regexp(obj(i).name,'ail','once'))||...
            ~isempty(regexp(obj(i).name,'ele','once'))||...
            ~isempty(regexp(obj(i).name,'rud','once'))||...
            ~isempty(regexp(obj(i).name,'slat','once'))||...
            ~isempty(regexp(obj(i).name,'canard','once'))||...
            ~isempty(regexp(obj(i).name,'spoil','once'))
        obj(i).consurf='yes';
        obj(i).col=[0.8706 0.1922 0.3882];
    elseif ~isempty(regexp(obj(i).name,'flap','once'))
        obj(i).consurf='yes';
        obj(i).col=[1 0.27 0.33];
    elseif ~isempty(regexp(obj(i).name,'turbine','once'))
        obj(i).col=[0.1 0.1 0.1];
    elseif ~isempty(regexp(obj(i).name,'fuselage','once'))||...
           ~isempty(regexp(obj(i).name,'wing','once'))||... 
           ~isempty(regexp(obj(i).name,'fin','once'))
           ~isempty(regexp(obj(i).name,'door','once'));
        obj(i).col=[0.5686 0.6392 0.6902];        
    elseif ~isempty(regexp(obj(i).name,'canopy','once'))
        obj(i).col=[0.9 0.9 0.9];
        obj(i).alp=0.7;
    elseif ~isempty(regexp(obj(i).name,'nozzle','once'))
        obj(i).col=[0.1 0.1 0.1];     
    elseif ~isempty(regexp(obj(i).name,'head','once'))
        obj(i).col=[1 1 1];    
    elseif ~isempty(regexp(obj(i).name,'body','once'))||...
           ~isempty(regexp(obj(i).name,'arm','once')) 
        obj(i).col=[0.604 0.726 0.451];            
    elseif ~isempty(regexp(obj(i).name,'glasses','once'))
        obj(i).col=[0.1 0.1 0.1]; 
    else
        obj(i).consurf='no';
        obj(i).col=[0.5686 0.6392 0.6902]; 
    end
end

end

%--------------------------------------------------------------------------
function obj=con2fvz(obj,val)

[pathname,acname]=fileparts(obj(1).file);

ref.all=[0 0 0 0 0 0];
j=1;

for i=1:length(obj)
    
    if strcmp(obj(i).consurf,'yes')
        eval(['M.body.',obj(i).name,'.mat=obj(',num2str(i),').mat;']);
        eval(['M.body.',obj(i).name,'.col=obj(',num2str(i),').col;']);
        eval(['M.body.',obj(i).name,'.lit=obj(',num2str(i),').lit;']);
        eval(['M.body.',obj(i).name,'.alp=obj(',num2str(i),').alp;']);
        eval(['M.body.',obj(i).name,'.x=obj(',num2str(i),').x;']);
        eval(['M.body.',obj(i).name,'.y=obj(',num2str(i),').y;']);
        eval(['M.body.',obj(i).name,'.z=obj(',num2str(i),').z;']);
        eval(['M.body.',obj(i).name,'.tri=obj(',num2str(i),').tri;']);
        eval(['M.body.',obj(i).name,'.ref=obj(',num2str(i),').ref;']);
        eval(['M.body.',obj(i).name,'.parent=obj(',num2str(i),').parent;']);
        
        eval(['ref.body.',obj(i).name,'=obj(',num2str(i),').ref;']);
        
    else
        eval(['M.',acname,'(',num2str(j),').mat=obj(',num2str(i),').mat;']);
        eval(['M.',acname,'(',num2str(j),').col=obj(',num2str(i),').col;']);
        eval(['M.',acname,'(',num2str(j),').lit=obj(',num2str(i),').lit;']);
        eval(['M.',acname,'(',num2str(j),').alp=obj(',num2str(i),').alp;']);
        eval(['M.',acname,'(',num2str(j),').x=obj(',num2str(i),').x;']);
        eval(['M.',acname,'(',num2str(j),').y=obj(',num2str(i),').y;']);
        eval(['M.',acname,'(',num2str(j),').z=obj(',num2str(i),').z;']);
        eval(['M.',acname,'(',num2str(j),').tri=obj(',num2str(i),').tri;']);
        eval(['M.',acname,'(',num2str(j),').ref=obj(',num2str(i),').ref;']);
        eval(['M.',acname,'(',num2str(j),').parent=obj(',num2str(i),').parent;']);
        j=j+1;
    end
       
end

if ~isempty(val)
    f=fieldnames(ref.body);
    
    for i=1:length(f)
        ref.body=setfield(ref.body,f{i},val{i});
    end
end

for i=1:length(obj)
    ac3d(i)=struct(obj(i));
end

save(acname,'ac3d','M','ref');
fprintf(1,'Saved as %s.mat file\n\n',acname);
    
end




