function []=plotauto()

fid=fopen('fort.7','r');

[A,nol]=read_file(fid);


B=strmatch('   1',A);

C=str2num([A{B}]);

plot(C(:,5),C(:,7));






%--------------------------------------------------------------------------
function [A, no_lines] = read_file(fid)
%Created By: Terence Frost B83-03, Date 26 Feb 2000

FAILED=-1;

no_lines=0;

% Read in
count=1;
while feof(fid)==0
    A{count}=fgets(fid);
    count=count+1;
end

no_lines=length(A);