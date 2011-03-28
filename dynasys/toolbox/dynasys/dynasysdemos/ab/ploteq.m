function []=ploteq(varargin)

if nargin==0
    [fileName,pathName]=uigetfile({'b.*','*.7'},'AUTO output file');
elseif nargin==1
    fileName=['b.',varargin{1}];
    pathName='';
    xstr='PAR';
    ystr='L2';
elseif nargin==3
    fileName=['b.',varargin{1}];
    pathName='';
    xstr=varargin{2};
    ystr=varargin{3};
else
    error('AUTOmatlab:plotArguments',...
          'Incorrect number of arguments for plotting.');
end
     
%--------------------------------------------------------------------------
% PLOT VARIABLES
%--------------------------------------------------------------------------
titstr='Bifurcation diagram for Demo:ab';
axlim=[0.08 0.14 0.5 4.5];   % axis limits

%--------------------------------------------------------------------------
% READ FILE
%--------------------------------------------------------------------------
A=readfile(fullfile(pathName,fileName));

%--------------------------------------------------------------------------
% PARSE DATA
%--------------------------------------------------------------------------
r=strmatch('   0',A);
A(r)=[];
B=str2num(char(A));
i=B(:,2);
i=[1:length(i)]'.*sign(i);
j=find(abs(i(2:end)-i(1:end-1))~=1);

TY=B(:,3);
LAB=B(:,4);
PAR=B(:,5);
L2=B(:,6);
U=B(:,7:end);


% Define x and y data
if ~isempty(findstr('PAR',xstr))
    x=PAR;
elseif ~isempty(findstr('L2',xstr))
    x=L2;
elseif ~isempty(findstr('U(',xstr))
    c=str2num(strrep(strrep(xstr,'U(',''),')',''));
    x=U(:,c);
else
    error('AUTOmatlab:plotDataLabels',...
          'Could not find required x-data definition.');
end

if ~isempty(findstr('PAR',ystr))
    y=PAR;
elseif ~isempty(findstr('L2',ystr))
    y=L2;
elseif ~isempty(findstr('U(',ystr))
    c=str2num(strrep(strrep(ystr,'U(',''),')',''));
    y=U(:,c);
else
    error('AUTOmatlab:plotDataLabels',...
          'Could not find required y-data definition.');
end
    
% split into line segments of stable and unstable lines  
if length(j)==0
    a{1}=i;
else
    if j(1)==1
        j(1)=[];
        j=[1;2;j];
    else
        j=[1;j];
    end
    j=[j;length(i)];
    
    for k=1:length(j)-1
        a{k}=i(j(k):j(k+1));
    end
end
    
%--------------------------------------------------------------------------
% PLOT
%--------------------------------------------------------------------------
for l=1:length(a)
    % periodic orbits magenta
    % stable lines are solid green,
    % unstable lines broken red
    if B(1,1) < 0
        str='m';
    elseif l==1 && a{1}(1) > 0
        str='r--';
    elseif sum(a{l}) < 0
        str='g';
    else
        str='r--';
    end
    
    ldx=abs(a{l});
    
    xdat=x(ldx);
    ydat=y(ldx);
    
    h=plot(xdat,ydat,str);
    set(h,'LineWidth',2);
    hold on;
    
end

labelplot(TY,LAB,x,y);

xlabel(xstr);
ylabel(ystr);
title(titstr);
axis(axlim);

%--------------------------------------------------------------------------
function A=readfile(fname)


fid=fopen(fname,'r');

if fid < 0
    error('Failed to open AUTO output file');
end

count=1;

while feof(fid)==0
   A{count}=fgetl(fid);
   count=count+1;  
end

%--------------------------------------------------------------------------
function A=labelplot(TY,LAB,x,y)

j=find(TY~=0);
l=find(LAB~=0);

j=unique([j;l]);

if isempty(j)
    return;
end

for i=1:length(j)
    lab=TY(j(i));
if lab==1
    plot(x(j(i)),y(j(i)),'bd');
    text(x(j(i)),y(j(i)),sprintf('  %2.0f  BP',LAB(j(i))));
elseif lab==2
    h=plot(x(j(i)),y(j(i)),'b*');
    text(x(j(i)),y(j(i)),sprintf('  %2.0f  LP',LAB(j(i))));
elseif lab==3
    h=plot(x(j(i)),y(j(i)),'b^');
    set(h,'MarkerFaceColor','b');
    text(x(j(i)),y(j(i)),sprintf('  %2.0f  HB',LAB(j(i))));
elseif lab==4
    h=plot(x(j(i)),y(j(i)),'ko'); 
    set(h,'MarkerSize',3);
    set(h,'MarkerFaceColor','k');
    text(x(j(i)),y(j(i)),sprintf('  %2.0f',LAB(j(i))));
elseif lab==5
    plot(x(j(i)),y(j(i)),'r*');
    text(x(j(i)),y(j(i)),sprintf('  %2.0f  LP',LAB(j(i))));
elseif lab==6
    plot(x(j(i)),y(j(i)),'rd');
    text(x(j(i)),y(j(i)),sprintf('  %2.0f  BP',LAB(j(i))));
elseif lab==7
    plot(x(j(i)),y(j(i)),'bp');
    text(x(j(i)),y(j(i)),sprintf('  %2.0f  PD',LAB(j(i))));
elseif lab==8
    plot(x(j(i)),y(j(i)),'bh');
    text(x(j(i)),y(j(i)),sprintf('  %2.0f  TR',LAB(j(i))));
elseif lab==9
    %plot(x(j(i)),y(j(i)),'b+');
elseif lab==-9
    %plot(x(j(i)),y(j(i)),'bx');
end
end

