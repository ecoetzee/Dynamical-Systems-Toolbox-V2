function []=plotlc(varargin)

if nargin==0
    [fileName,pathName]=uigetfile({'s.*','*.8'},'AUTO output file');
elseif nargin==1
    fileName=['s.',varargin{1}];
    pathName='';
end
    
%--------------------------------------------------------------------------
% PLOT VARIABLES
%--------------------------------------------------------------------------
lb=[6,8,10];

%--------------------------------------------------------------------------
% READ FILE
%--------------------------------------------------------------------------
a=readfile(fullfile(pathName,fileName));

%--------------------------------------------------------------------------
% PARSE DATA
%--------------------------------------------------------------------------
count=1;

while ~isempty(a)
r=str2num(a{1});
lab{count}=r(4);
szl=r(7);
szr=r(9);
npar=r(12);

b=a(2:szr+1);

i=[length(b)-ceil(npar/7)+1:length(b)];
b(i)=[];

d{count}=str2num(char(b(1:szl)));
b(1:szl+1)=[];

e{count}=str2num(char(b));

a(1:szr+1)=[];
count=count+1;
end

%--------------------------------------------------------------------------
% PLOT
%--------------------------------------------------------------------------
clrs{1}='b';
clrs{2}='g';
clrs{3}='r';
clrs{4}='m';

subplot(2,1,1);

% Plot cycles on time history plot
for i=1:length(lb)
  idx=find(cell2mat(lab)==lb(i));
  x=d{idx}(:,1);
  u1=d{idx}(:,2);
  u2=d{idx}(:,3);
  X=[];
  Y=[];
  
  for j=1:4
     X=[X;x+(j-1)];
     X(end)=[];
     Y=[Y;u2];
     Y(end)=[];
  end
    
  h=plot(X,Y,clrs{i});
  hold on;
  set(h,'LineWidth',2);
    
  legstr{i}=num2str(lb(i));
  
end

xlabel('Normalised Time (1/Hz)');
ylabel('U(2)');
title('Limit Cycle Response');
legend(legstr,'location','best');

% Phase plot
subplot(2,1,2);

for i=1:length(lb)
  idx=find(cell2mat(lab)==lb(i));
  u1=d{idx}(:,2);
  u2=d{idx}(:,3);
  h=plot(u1,u2,clrs{i});
  hold on
  set(h,'LineWidth',2);
end

xlabel('U(1)');
ylabel('U(2)');
title('Phase Plot');
legend(legstr,'location','best');

%--------------------------------------------------------------------------
function A=readfile(fname)


fid=fopen(fname,'r');

if fid < 0
    error('Failed top open AUTO output file');
end

count=1;

while feof(fid)==0
   A{count}=fgetl(fid);
   count=count+1;  
end

