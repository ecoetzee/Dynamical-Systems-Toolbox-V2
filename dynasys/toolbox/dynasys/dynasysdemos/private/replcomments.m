function []=replcomments(fName)

a=readfile(fName);

for i=1:length(a)
    str=a{i};
    if isempty(str)
        str='!';
    elseif strcmp(str(1),'C')
        str(1)='!';
    end
    a{i}=str;
end

fid=fopen([fName,'.conv'],'w');

for i=1:length(a)
  fprintf(fid,'%s\n',a{i});
end

fclose(fid);

%-------------------------------------------------------------------------
function A=readfile(fName)

fid=fopen(fName,'r');

if fid < 0
    error('Failed to open input file');
end

count=1;

while feof(fid)==0
    A{count}=fgetl(fid);
    count=count+1;
end

fclose(fid);