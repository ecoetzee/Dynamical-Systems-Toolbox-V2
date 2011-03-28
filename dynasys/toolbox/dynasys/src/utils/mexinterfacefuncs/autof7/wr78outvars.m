function []=wr78outvars()

dbstop if error

%% f7 file variables
F7{1}={'IBR',1,'int'};
F7{2}={'MTOT',1,'int'};
F7{3}={'ITP',1,'int'};
F7{4}={'LAB',1,'int'};
F7{5}={'PAR',2,'dbl','NICP','ICU(J)'};
F7{6}={'VAXIS',1,'dbl'};
F7{7}={'U',2,'dbl','N2','J'};
F7{8}={'OUT',2,'dbl','ODIM','J'};

fid=fopen('F7Definition.txt','w');

if fid<0
    error('Could not open F7 definition file')
end

%% write header
str=['C HEADER INFORMATION --------------------------------------------------------------------'];
fprintf(fid,'C\n%s\nC\n',str(1:72));
str='USE AUTO_CONSTANTS, ONLY:';
for i=1:length(F7)
   str=[str,F7{i}{1},'F7,','old',F7{i}{1},'F7,'];
end
str=[str,'NPARX'];

while ~isempty(str)
    if length(str)>=66
       idx=1:66;
    else
       idx=1:length(str); 
    end

    fstr=str(idx); 
    str(idx)=[];
    if strmatch('USE',fstr)
      fprintf(fid,'      %s\n',fstr);
    else
      fprintf(fid,'     &%s\n',fstr);    
    end
end

    fprintf(fid,'\n\n');
    str=['C ARRAY ALLOCATION  --------------------------------------------------------------------'];
    fprintf(fid,'C\n%s\nC\n',str(1:72));
    
%% write allocation statements
for i=1:length(F7)
    fprintf(fid,'C %s ------------\n',F7{i}{1});
    
    if F7{i}{2} == 1
    fprintf(fid,'      R=SIZE(%sF7,1)\n',F7{i}{1});
    fprintf(fid,'      ALLOCATE(old%sF7(R))\n',F7{i}{1});
    fprintf(fid,'      old%sF7=%sF7\n',F7{i}{1},F7{i}{1});
    fprintf(fid,'      ALLOCATE(%sF7(R+1))\n',F7{i}{1});
    fprintf(fid,'      IF(R > 0) THEN\n');
    fprintf(fid,'          %sF7=old%sF7\n',F7{i}{1},F7{i}{1});
    fprintf(fid,'      ENDIF\n');
    fprintf(fid,'      %sF7(R+1)=%s\n',F7{i}{1},F7{i}{1});
    fprintf(fid,'      DEALLOCATE(old%sF7)\n',F7{i}{1});
    elseif F7{i}{2} == 2
    fprintf(fid,'      R=SIZE(%sF7,1)\n',F7{i}{1});
    fprintf(fid,'      C=SIZE(%sF7,2)\n',F7{i}{1});
    fprintf(fid,'      ALLOCATE(old%sF7(R,%s))\n',F7{i}{1},F7{i}{4});
    fprintf(fid,'      old%sF7=%sF7\n',F7{i}{1},F7{i}{1});
    fprintf(fid,'      ALLOCATE(%sF7(R+1),%s)\n',F7{i}{1},F7{i}{4});
    fprintf(fid,'      IF(R > 0) THEN\n');
    fprintf(fid,'          %sF7=old%sF7\n',F7{i}{1},F7{i}{1});
    fprintf(fid,'      ENDIF\n');   
    fprintf(fid,'      DO J=1,%s\n',F7{i}{4});
    fprintf(fid,'        %sF7(R+1,J)=%s(%s)\n',F7{i}{1},F7{i}{1},F7{i}{5});
    fprintf(fid,'      ENDDO\n');
    fprintf(fid,'      DEALLOCATE(old%sF7)\n',F7{i}{1});
    elseif F7{i}{2} == 3
    fprintf(fid,'      R=SIZE(%sF7,1)\n',F7{i}{1});
    fprintf(fid,'      C=SIZE(%sF7,2)\n',F7{i}{1});
    fprintf(fid,'      P=SIZE(%sF7,3)\n',F7{i}{1});
    fprintf(fid,'      ALLOCATE(old%sF7(R,C,%s))\n',F7{i}{1},F7{i}{4});
    fprintf(fid,'      old%sF7=%sF7\n',F7{i}{1},F7{i}{1});
    fprintf(fid,'      ALLOCATE(%sF7(R+1),%s)\n',F7{i}{1},F7{i}{4});
    fprintf(fid,'      IF(R > 0) THEN\n');
    fprintf(fid,'          %sF7=old%sF7\n',F7{i}{1},F7{i}{1});
    fprintf(fid,'      ENDIF\n');   
    fprintf(fid,'      DO J=1,%s\n',F7{i}{4});
    fprintf(fid,'        DO I=1,%s\n',F7{i}{6});
    fprintf(fid,'          %sF7(I,J,P+1)=%s(%s,%s)\n',F7{i}{1},F7{i}{1},F7{i}{7},F7{i}{5});
    fprintf(fid,'        ENDDO\n');
    fprintf(fid,'      ENDDO\n');    
    fprintf(fid,'      DEALLOCATE(old%sF7)\n',F7{i}{1});    
    end
    fprintf(fid,'C\n');    
end        
       
    fprintf(fid,'\n\n');
    
%% declare variables in module
str=['C MODULE DECLARATIONS --------------------------------------------------------------------'];
fprintf(fid,'C\n%s\nC\n',str(1:72));
    
for i=1:length(F7)
    if F7{i}{2}==1
        str='(:)';
    elseif F7{i}{2}==2
        str='(:,:)';
    elseif F7{i}{2}==3
        str='(:,:,:)';
    end
    
    if strmatch('int',F7{i}{3})
    fprintf(fid,'      INTEGER,ALLOCATABLE :: %s%s\n',F7{i}{1},str);
    fprintf(fid,'      INTEGER,ALLOCATABLE :: old%s%s\n',F7{i}{1},str); 
    else
    fprintf(fid,'      DOUBLE PRECISION,ALLOCATABLE :: %s%s\n',F7{i}{1},str);
    fprintf(fid,'      DOUBLE PRECISION,ALLOCATABLE :: old%s%s\n',F7{i}{1},str);         
    end
    
end
    
%% In copydstobjects.f allocate variables
str=['C ALLOCATE VARIABLES IN COPYDSTOBJECTS.F --------------------------------------------------------------------'];
fprintf(fid,'C\n%s\nC\n',str(1:72));

for i=1:length(F7)
    if F7{i}{2}==1
        str='0';
    elseif F7{i}{2}==2
        str='0,0';
    elseif F7{i}{2}==3
        str='0,0,0';
    end
    
    fprintf(fid,'      ALLOCATE(%sF7(%s))\n',F7{i}{1},str);    
end


%% In Autostop deallocate allocated variables
str=['C DEALLOCATE ALLOCATED VARIABLES --------------------------------------------------------------------'];
fprintf(fid,'C\n%s\nC\n',str(1:72));

for i=1:length(F7) 
    fprintf(fid,'      IF(ALLOCATED(%sF7))DEALLOCATE(%sF7)\n',F7{i}{1},F7{i}{1});
    fprintf(fid,'      IF(ALLOCATED(old%sF7))DEALLOCATE(old%sF7)\n',F7{i}{1},F7{i}{1}); 
end

fclose(fid);