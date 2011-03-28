function []=convertcvc(fName)
% CONVERTCVC - convert c file into a value class format. 
%
%   Created by Etienne COETZEE, James RANKIN,and Phani THOTA, University of Bristol
%
%   $Revision: 2.0.0.0 $ $Date: 2010/07/29 14:07:00$
datf=readfile(fName);

fName=strrep(fName,'.','');
fid=fopen([fName,'.m'],'w');
fprintf(fid,'function c=%s(c)\n',fName);
fprintf(fid,'%% %s - Constants file converted with function %s from c. format\n',fName,mfilename); 
fprintf(fid,'%%\n');
fprintf(fid,'%%  Created by : %s\n',getenv('username'));
fprintf(fid,'%%  Date       : %s\n', datestr(now));
fprintf(fid,'%%\n');
fprintf(fid,'\n');

%% First line
dats=sscanf(datf{1},'%f',[1 200]);
ndim=dats(1);
ips=dats(2);
irs=dats(3);
ilp=dats(4);

fprintf(fid,'c.Ndim=%1.0f;\n',ndim);
fprintf(fid,'c.Noutx=200;\n');
fprintf(fid,'c.Ips=%1.0f;\n',ips);
fprintf(fid,'c.Irs=%1.0f;\n',irs);
fprintf(fid,'c.Ilp=%1.0f;\n',ilp);

%thu=ones(8*ndim,1);

%% Second line
dats=sscanf(datf{2},'%f',[1 200]);
nicp=dats(1);
icp=dats(2:end);

tmp=[];
for i=1:length(icp)
    tmp=[tmp,sprintf('%1.0f,',icp(i))];
end
tmp(end)=[];

fprintf(fid,'c.Icp=[%s];\n',tmp);

%% Third line
dats=sscanf(datf{3},'%f',[1 200]);
ntst=dats(1);
ncol=dats(2);
iad=dats(3);
isp=dats(4);
isw=dats(5);
iplt=dats(6);
nbc=dats(7);
nint=dats(8);

fprintf(fid,'c.Ntst=%1.0f;\n',ntst);
fprintf(fid,'c.Ncol=%1.0f;\n',ncol);
fprintf(fid,'c.Iad=%1.0f;\n',iad);
fprintf(fid,'c.Isp=%1.0f;\n',isp);
fprintf(fid,'c.Isw=%1.0f;\n',isw);
fprintf(fid,'c.Iplt=%1.0f;\n',iplt);
fprintf(fid,'c.Nbc=%1.0f;\n',nbc);
fprintf(fid,'c.Nint=%1.0f;\n',nint);

%% Fourth line
dats=sscanf(datf{4},'%f',[1 200]);
nmx=dats(1);
rl0=dats(2);
rl1=dats(3);
a0=dats(4);
a1=dats(5);

fprintf(fid,'c.Nmx=%1.0f;\n',nmx);
fprintf(fid,'c.Rl0=%g;\n',rl0);
fprintf(fid,'c.Rl1=%g;\n',rl1);
fprintf(fid,'c.A0=%g;\n',a0);
fprintf(fid,'c.A1=%g;\n',a1);

%% Fith line
dats=sscanf(datf{5},'%f',[1 200]);
npr=dats(1);
mxbf=dats(2);
iid=dats(3);
itmx=dats(4);
itnw=dats(5);
nwtn=dats(6);
jac=dats(7);

fprintf(fid,'c.Npr=%1.0f;\n',npr);
fprintf(fid,'c.Mxbf=%1.0f;\n',mxbf);
fprintf(fid,'c.Iid=%1.0f;\n',iid);
fprintf(fid,'c.Itmx=%1.0f;\n',itmx);
fprintf(fid,'c.Itnw=%1.0f;\n',itnw);
fprintf(fid,'c.Nwtn=%1.0f;\n',nwtn);
fprintf(fid,'c.Jac=%1.0f;\n',jac);

%% Sixth line
dats=sscanf(datf{6},'%f',[1 200]);
epsl=dats(1);
epsu=dats(2);
epss=dats(3);

fprintf(fid,'c.Epsl=%g;\n',epsl);
fprintf(fid,'c.Epsu=%g;\n',epsu);
fprintf(fid,'c.Epss=%g;\n',epss);

%% Seventh line
dats=sscanf(datf{7},'%f',[1 200]);
ds=dats(1);
dsmin=abs(dats(2));
dsmax=abs(dats(3));
iads=dats(4);

fprintf(fid,'c.Ds=%g;\n',ds);
fprintf(fid,'c.Dsmin=%g;\n',dsmin);
fprintf(fid,'c.Dsmax=%g;\n',dsmax);
fprintf(fid,'c.Iads=%g;\n',iads);

%% Eigth line
idx=find(cellfun('isempty',regexp(datf,'NTHL'))==0);
dats=sscanf(datf{idx},'%f',[1 200]);
nthl=dats(1);
tmp='';

if nthl>0
    ithl=zeros(nthl,1);
    vthl=zeros(nthl,1);
    for i=1:nthl
        dats=sscanf(datf{idx+i},'%f',[1 200]);
        ithl(i)=dats(1);
        vthl(i)=dats(2);
        tmp=[tmp,sprintf('%1.0f,%g;',ithl(i),vthl(i))];
    end
    tmp(end)=[];
end

fprintf(fid,'c.Thl=[%s];\n',tmp);

%% Ninth line
idx=find(cellfun('isempty',regexp(datf,'NTHU'))==0);
dats=sscanf(datf{idx},'%f',[1 200]);
nthu=dats(1);
tmp='';

if nthu>0
    ithu=zeros(nthu,1);
    thu=zeros(nthu,1);
        for i=1:nthu
            dats=sscanf(datf{idx+i},'%f',[1 200]);
            ithu(i)=dats(1);
            thu(i)=dats(2);
            tmp=[tmp,sprintf('%1.0f,%g;',ithu(i),thu(i))];
        end
    tmp(end)=[];
end

fprintf(fid,'c.Thu=[%s];\n',tmp);


%% Tenth line
idx=find(cellfun('isempty',regexp(datf,'NUZR'))==0);
dats=sscanf(datf{idx},'%f',[1 200]);
nuzr=dats(1);
tmp='';

if nuzr>0
    iuz=zeros(nuzr,1);
    vuz=zeros(nuzr,1);
    for i=1:nuzr
        dats=sscanf(datf{idx+i},'%f',[1 200]);
        iuz(i)=dats(1);
        vuz(i)=dats(2);
        tmp=[tmp,sprintf('%1.0f,%g;',iuz(i),vuz(i))];
    end
    tmp(end)=[];
end
fprintf(fid,'c.Uzr=[%s];\n',tmp);


function A=readfile(fName)

fid=fopen(fName,'r');

if fid < 0
    error('Failed to open AUTO output file');
end

count=1;

while feof(fid)==0
    A{count}=fgetl(fid);
    count=count+1;
end

fclose(fid);
