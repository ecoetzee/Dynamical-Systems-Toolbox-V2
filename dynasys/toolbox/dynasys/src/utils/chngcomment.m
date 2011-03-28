function []=chngcomment()

dbstop if error

d=dir('*.f90');
fprintf(1,'Changing comment lines to f90 format ******* \n\n');

for i=1:length(d)
    
    fprintf(1,'Commenting file : %s \n',d(i).name);
    
    a=readfile(d(i).name);
    
    for j=1:length(a)
       if strcmp(a{j}(1),'C') 
         a{j}(1)='!';
       end
    end
    
    fid=fopen(d(i).name,'w');
    
    for j=1:length(a)
       fprintf(fid,'%s',a{j});
    end    
    
    fclose(fid);
    
    pause(1);
end

fprintf(1,'\n\nFinished ******* \n\n');

%--------------------------------------------------------------------------
function A=readfile(fName)

fid=fopen(fName,'r');

if fid < 0
    error('Failed to open AUTO output file');
end

count=1;

while feof(fid)==0
    A{count}=fgets(fid);
    count=count+1;
end

fclose(fid);
