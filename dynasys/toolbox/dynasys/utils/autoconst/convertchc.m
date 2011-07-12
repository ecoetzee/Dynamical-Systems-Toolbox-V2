function []=convertchc(fName)
% CONVERTCHC - convert c-file into handle class format. 
%   CONVERTCHC(FILENAME) where FILENAME is the original AUTO constants file
%   name
%
%   Example:
%            >>  convertchc('c.ab')

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

fprintf(fid,'set(c,''Ndim'',%1.0f,''Noutx'',200,''Ips'',%1.0f,''Irs'',%1.0f,''Ilp'',%1.0f);\n',ndim,ips,irs,ilp);

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

fprintf(fid,'set(c,''Icp'',[%s]);\n',tmp);

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

fprintf(fid,'set(c,''Ntst'',%1.0f,''Ncol'',%1.0f,''Iad'',%1.0f,''Isp'',%1.0f,''Isw'',%1.0f,''Iplt'',%1.0f,''Nbc'',%1.0f,''Nint'',%1.0f);\n',ntst,ncol,iad,isp,isw,iplt,nbc,nint);

%% Fourth line
dats=sscanf(datf{4},'%f',[1 200]);
nmx=dats(1);
rl0=dats(2);
rl1=dats(3);
a0=dats(4);
a1=dats(5);

fprintf(fid,'set(c,''Nmx'',%1.0f,''Rl0'',%g,''Rl1'',%g,''A0'',%g,''A1'',%g);\n',nmx,rl0,rl1,a0,a1);

%% Fith line
dats=sscanf(datf{5},'%f',[1 200]);
npr=dats(1);
mxbf=dats(2);
iid=dats(3);
itmx=dats(4);
itnw=dats(5);
nwtn=dats(6);
jac=dats(7);

fprintf(fid,'set(c,''Npr'',%1.0f,''Mxbf'',%1.0f,''Iid'',%1.0f,''Itmx'',%1.0f,''Itnw'',%1.0f,''Nwtn'',%1.0f,''Jac'',%1.0f);\n',npr,mxbf,iid,itmx,itnw,nwtn,jac);

%% Sixth line
dats=sscanf(datf{6},'%f',[1 200]);
epsl=dats(1);
epsu=dats(2);
epss=dats(3);

tmp=sprintf('set(c,''Epsl'',%g,''Epsu'',%g,''Epss'',%g);\n',epsl,epsu,epss);
fprintf(fid,'%s',tmp);


%% Seventh line
dats=sscanf(datf{7},'%f',[1 200]);
ds=dats(1);
dsmin=abs(dats(2));
dsmax=abs(dats(3));
iads=dats(4);

tmp=sprintf('set(c,''Ds'',%g,''Dsmin'',%g,''Dsmax'',%g,''Iads'',%1.0f);\n',ds,dsmin,dsmax,iads);
fprintf(fid,'%s',tmp);

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

fprintf(fid,'set(c,''Thl'',[%s]);\n',tmp);

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

fprintf(fid,'set(c,''Thu'',[%s]);\n',tmp);


%% Tenth line
idx=find(cellfun('isempty',regexp(datf,'NUZR'))==0);
dats=sscanf(datf{idx},'%f',[1 200]);
nuzr=dats(1);
tmp='';

if nuzr>0
    iuz=zeros(nuzr,1);
    vuz=zeros(nuzr,1);
    for i=1:nthu
        dats=sscanf(datf{idx+i},'%f',[1 200]);
        iuz(i)=dats(1);
        vuz(i)=dats(2);
        tmp=[tmp,sprintf('%1.0f,%g;',iuz(i),vuz(i))];
    end
    tmp(end)=[];
end
fprintf(fid,'set(c,''Uzr'',[%s]);\n',tmp);


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
