function []=publishdynasys()
% Publish demo files from Dynamical Systems Toolbox

dsthstr=mfilename('fullpath');
[dsthname,fname]=fileparts(dsthstr);

% set directories
%tlbxdir=toolboxdir('dynasys');
tlbxdir=dynasysroot;
demodir=[tlbxdir,'\dynasysdemos'];
opts.stylesheet=[demodir,'\private\dynasysdemos.xsl'];

% get relevant directory names
ddir=dir(demodir);

% loop through each directory and publish any files that start with
% "run_demo"
for i=3:length(ddir)
    if ddir(i).isdir==1
       
       cd(fullfile(demodir,ddir(i).name));
       fn=dir([demodir,'\',ddir(i).name,'\run_demo*lor.m']);
       
       pause(0.1);
       
       % if run demo files found, publish
       for j=1:length(fn)   
          pathname=[demodir,'\',ddir(i).name];
          addpath(pathname);
          fprintf('Publishing : %s\n',fullfile(pathname,fn(j).name));
          
          mfname=fullfile(pathname,fn(j).name);
          publish(mfname,opts);
          
          % change hyperlinks in generated html to obtain correct directory
          % structure
          [p,fname,e]=fileparts(fn(j).name);
          hname=fullfile([demodir,'\',ddir(i).name,'\html'],[fname,'.html']);
          a=readfilefunc(hname);
          
          for k=1:length(a)
            Xstr=sprintf('edit(fullfile(dynasysroot,''dynasysdemos'',''%s'',''%s''))',ddir(i).name,fn(j).name);
            Ystr=sprintf('addpath(fullfile(dynasysroot,''dynasysdemos'',''%s''));echodemo %s',ddir(i).name,fname);
            a{k}=strrep(a{k},'XXXXXXXXXXXXX',Xstr);
            a{k}=strrep(a{k},'YYYYYYYYYYYYY',Ystr);
          end
          
          fid=fopen(hname,'w');
          for k=1:length(a)
             fprintf(fid,'%s\n',a{k}); 
          end
          fclose(fid);
       end      
    end
    cd(dsthname);
end

%-------------------------------------------------------------------------
function A=readfilefunc(fName)

fid=fopen(fName,'r');

if fid < 0
    error('Failed to open file');
end

count=1;

while feof(fid)==0
    A{count}=fgetl(fid);
    count=count+1;
end

fclose(fid);