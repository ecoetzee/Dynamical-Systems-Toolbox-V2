function []=renumyear()

rdir=pwd;
f=dir('*.htm*');

% parse
for i=1:length(f)
    fprintf(1,'Updating file : %s ...\n',f(i).name);
    A=readfile(f(i).name);
    for j=1:length(A);
      A{j}=strrep(A{j},'2009 University of Bristol','2011 University of Bristol');
    end
    
    fid=fopen(f(i).name,'w');
    for i=1:length(A)
        fprintf(fid,'%s\n',A{i});
    end
    fclose(fid);
end


disp('Finished!');

%--------------------------------------------------------------------------
function A=readfile(fName)

fid=fopen(fName,'r');

if fid < 0
    error('Failed to open tex file');
end

count=1;

while feof(fid)==0
   A{count,1}=fgetl(fid);
   count=count+1;  
end

fclose(fid);
