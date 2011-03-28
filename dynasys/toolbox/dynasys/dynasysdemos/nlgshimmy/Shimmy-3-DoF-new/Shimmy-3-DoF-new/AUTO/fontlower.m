function []=fontlower()

fname='5d.m';

a=readfile(fname);

fid=fopen(fname,'w');

for i=1:length(a)
    l=lower(a{i});
    fprintf(fid,'%s\n',l);
end

fclose(fid);

disp('Finished!')

%--------------------------------------------------------------------------
function A=readfile(fName)

fid=fopen(fName,'r');

if fid < 0
    error('Failed to open file');
end

count=1;

while feof(fid)==0
   A{count,1}=fgetl(fid);
   count=count+1;  
end

fclose(fid);
