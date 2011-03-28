function []=wr78outvars()

dbstop if error

%% f8 file variables
F8{1}={'IBR',1,'int'};
F8{2}={'MTOT',1,'int'};
F8{3}={'ITP',1,'int'};
F8{4}={'LAB',1,'int'};
F8{5}={'NFPR',1,'int'};
F8{6}={'ISW',1,'int'};
F8{7}={'NTPL',1,'int'};
F8{8}={'NAR',1,'int'};
F8{9}={'NROWPR',1,'int'};
F8{10}={'NTST',1,'int'};
F8{11}={'NCOL',1,'int'};
F8{12}={'NPARX',1,'int'};
F8{13}={'IFPR',2,'int','NFPR','ICP(J)'};
F8{14}={'T',1,'dbl'};
F8{15}={'TM',2,'dbl','NTST+1','J'};
F8{16}={'PAR',2,'dbl','NPARX','J'};
F8{17}={'RLDOT',2,'dbl','NFPR','J'};
F8{18}={'U',2,'dbl','NDIM','J','NTPL','I'};
F8{19}={'UPS',3,'dbl','NDIM','J','NTPL','I'};
F8{20}={'UDOTPS',3,'dbl','NDIM','J','NTPL','I'};

fid=fopen('F8NormalDefinition2.txt','w');

if fid<0
    error('Could not open F8 definition file')
end

%% write header
str=['C HEADER INFORMATION --------------------------------------------------------------------'];
fprintf(fid,'C\n%s\nC\n',str(1:72));
str='USE AUTO_CONSTANTS, ONLY:';
for i=1:length(F8)
   str=[str,F8{i}{1},'F8,','old',F8{i}{1},'F8,'];
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
for i=1:length(F8)
    fprintf(fid,'C %s ------------\n',F8{i}{1});
    
    if F8{i}{2} == 1
    fprintf(fid,'      R=SIZE(%sF8,1)\n',F8{i}{1});
    fprintf(fid,'      ALLOCATE(old%sF8(R))\n',F8{i}{1});
    fprintf(fid,'      old%sF8=%sF8\n',F8{i}{1},F8{i}{1});
    fprintf(fid,'      DEALLOCATE(%sF8)\n',F8{i}{1});
    fprintf(fid,'      ALLOCATE(%sF8(R+1))\n',F8{i}{1});
    fprintf(fid,'      IF(R > 0) THEN\n');
    fprintf(fid,'          DO I=1,R\n');
    fprintf(fid,'            %sF8(I)=old%sF8(I)\n',F8{i}{1},F8{i}{1});
    fprintf(fid,'          ENDDO\n');       
    fprintf(fid,'      ENDIF\n');
    fprintf(fid,'      %sF8(R+1)=%s\n',F8{i}{1},F8{i}{1});
    fprintf(fid,'      DEALLOCATE(old%sF8)\n',F8{i}{1});
    elseif F8{i}{2} == 2
    fprintf(fid,'      R=SIZE(%sF8,1)\n',F8{i}{1});
    fprintf(fid,'      C=SIZE(%sF8,2)\n',F8{i}{1});
    fprintf(fid,'      ALLOCATE(old%sF8(R,%s))\n',F8{i}{1},F8{i}{4});
    fprintf(fid,'      old%sF8=%sF8\n',F8{i}{1},F8{i}{1});
    fprintf(fid,'      DEALLOCATE(%sF8)\n',F8{i}{1});
    fprintf(fid,'      ALLOCATE(%sF8(R+1,%s))\n',F8{i}{1},F8{i}{4});
    fprintf(fid,'      IF(R > 0) THEN\n');
    fprintf(fid,'          DO I=1,R\n');
    fprintf(fid,'            DO J=1,%s\n',F8{i}{4});
    fprintf(fid,'              %sF8(I,J)=old%sF8(I,J)\n',F8{i}{1},F8{i}{1});
    fprintf(fid,'            ENDDO\n'); 
    fprintf(fid,'          ENDDO\n'); 
    fprintf(fid,'      ENDIF\n');   
    fprintf(fid,'      DO J=1,%s\n',F8{i}{4});
    fprintf(fid,'        %sF8(R+1,J)=%s(%s)\n',F8{i}{1},F8{i}{1},F8{i}{5});
    fprintf(fid,'      ENDDO\n');
    fprintf(fid,'      DEALLOCATE(old%sF8)\n',F8{i}{1});
    elseif F8{i}{2} == 3
    fprintf(fid,'      R=SIZE(%sF8,1)\n',F8{i}{1});
    fprintf(fid,'      C=SIZE(%sF8,2)\n',F8{i}{1});
    fprintf(fid,'      P=SIZE(%sF8,3)\n',F8{i}{1});
    fprintf(fid,'      ALLOCATE(old%sF8(%s,%s,P))\n',F8{i}{1},F8{i}{6},F8{i}{4});
    fprintf(fid,'      old%sF8=%sF8\n',F8{i}{1},F8{i}{1});
    fprintf(fid,'      DEALLOCATE(%sF8)\n',F8{i}{1});
    fprintf(fid,'      ALLOCATE(%sF8(%s,%s,P+1))\n',F8{i}{1},F8{i}{6},F8{i}{4});
    fprintf(fid,'      IF(R > 0) THEN\n');
    fprintf(fid,'        DO I=1,%s\n',F8{i}{6});
    fprintf(fid,'          DO J=1,%s\n',F8{i}{4});
    fprintf(fid,'            DO K=1,P\n');    
    fprintf(fid,'              %sF8(I,J,K)=old%sF8(I,J,K)\n',F8{i}{1},F8{i}{1});
    fprintf(fid,'            ENDDO\n');    
    fprintf(fid,'          ENDDO\n');
    fprintf(fid,'        ENDDO\n');    
    fprintf(fid,'      ENDIF\n');   
    fprintf(fid,'      DO I=1,%s\n',F8{i}{6});
    fprintf(fid,'        DO J=1,%s\n',F8{i}{4});
    fprintf(fid,'          %sF8(I,J,P+1)=%s(%s,%s)\n',F8{i}{1},F8{i}{1},F8{i}{7},F8{i}{5});
    fprintf(fid,'        ENDDO\n');
    fprintf(fid,'      ENDDO\n');    
    fprintf(fid,'      DEALLOCATE(old%sF8)\n',F8{i}{1});    
    end
    fprintf(fid,'C\n');    
end        
       
    fprintf(fid,'\n\n');
    
%% declare variables in module
str=['C MODULE DECLARATIONS --------------------------------------------------------------------'];
fprintf(fid,'C\n%s\nC\n',str(1:72));
    
for i=1:length(F8)
    if F8{i}{2}==1
        str='(:)';
    elseif F8{i}{2}==2
        str='(:,:)';
    elseif F8{i}{2}==3
        str='(:,:,:)';
    end
    
    if strmatch('int',F8{i}{3})
    fprintf(fid,'      INTEGER,ALLOCATABLE :: %sF8%s\n',F8{i}{1},str);
    fprintf(fid,'      INTEGER,ALLOCATABLE :: old%sF8%s\n',F8{i}{1},str); 
    else
    fprintf(fid,'      DOUBLE PRECISION,ALLOCATABLE :: %sF8%s\n',F8{i}{1},str);
    fprintf(fid,'      DOUBLE PRECISION,ALLOCATABLE :: old%sF8%s\n',F8{i}{1},str);         
    end
    
end
    
%% In copydstobjects.f allocate variables
str=['C ALLOCATE VARIABLES IN COPYDSTOBJECTS.F --------------------------------------------------------------------'];
fprintf(fid,'C\n%s\nC\n',str(1:72));

for i=1:length(F8)
    if F8{i}{2}==1
        str='0';
    elseif F8{i}{2}==2
        str='0,0';
    elseif F8{i}{2}==3
        str='0,0,0';
    end
    
    fprintf(fid,'      ALLOCATE(%sF8(%s))\n',F8{i}{1},str);    
end


%% In Autostop deallocate allocated variables
str=['C DEALLOCATE ALLOCATED VARIABLES --------------------------------------------------------------------'];
fprintf(fid,'C\n%s\nC\n',str(1:72));

for i=1:length(F8) 
    fprintf(fid,'      IF(ALLOCATED(%sF8))DEALLOCATE(%sF8)\n',F8{i}{1},F8{i}{1});
    fprintf(fid,'      IF(ALLOCATED(old%sF8))DEALLOCATE(old%sF8)\n',F8{i}{1},F8{i}{1}); 
end

fclose(fid);