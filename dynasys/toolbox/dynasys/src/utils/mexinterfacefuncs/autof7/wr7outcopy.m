function []=wr7outcopy()

F7{1}={'Ibr','IBRF7','int',1};
F7{2}={'Mtot','MTOTF7','int',1};
F7{3}={'Itp','ITPF7','int',1};
F7{4}={'Lab','LABF7','int',1};
F7{5}={'Par','PARF7','dbl',2};
F7{6}={'L2norm','VAXISF7','dbl',1};
F7{7}={'U','UF7','dbl',2};
F7{8}={'Out','OUTF7','dbl',2};

fid=fopen('f7copytodst.txt','w');

if fid < 0
  return
end

for i=1:length(F7)
    fprintf(fid,'      mwPointer %sptr1, %sptr2\n',F7{i}{2},F7{i}{2});
end

for i=1:length(F7)
    if F7{i}{4}==1
    fprintf(fid,'      REAL*8, POINTER::%sMWS(:)\n',F7{i}{2});
    else
    fprintf(fid,'      REAL*8, POINTER::%sMWS(:,:)\n',F7{i}{2});    
    end
end

fprintf(fid,'C\nC\n');

for i=1:length(F7)
fprintf(fid,'C Assign %s values --------------\n',F7{i}{1});
fprintf(fid,'      %sptr1=mxGetProperty(PLHS(1),1,''%s'')\n',F7{i}{2},F7{i}{1});
fprintf(fid,'C\n');
fprintf(fid,'      IF( %sptr1 == 0 ) THEN\n',F7{i}{2});
fprintf(fid,'        CALL mexErrMsgTxt("Property %s not found")\n',F7{i}{1});
fprintf(fid,'      ENDIF\n');
fprintf(fid,'C\n');
if F7{i}{4}==1
fprintf(fid,'      M=SIZE(%s)\n',F7{i}{2});
fprintf(fid,'      D1=M\n');
fprintf(fid,'      %sptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName(''double''),0)\n',F7{i}{2});
fprintf(fid,'      CALL MatlabAPI_COM_Apx1(%%VAL(mxGetPr(%sptr2)), 1, D1 )\n',F7{i}{2});
fprintf(fid,'      %sMWS => Apx1\n',F7{i}{2});
elseif F7{i}{4}==2
fprintf(fid,'      M=SIZE(%s,1)\n',F7{i}{2});
fprintf(fid,'      N=SIZE(%s,2)\n',F7{i}{2});
fprintf(fid,'      D2(1)=M\n');
fprintf(fid,'      D2(2)=N\n');
fprintf(fid,'      %sptr2 = mxCreateNumericArray(2,D2,mxClassIDFromClassName(''double''),0)\n',F7{i}{2});
fprintf(fid,'      CALL MatlabAPI_COM_Apx2(%%VAL(mxGetPr(%sptr2)), 1, D2 )\n',F7{i}{2});
fprintf(fid,'      %sMWS => Apx2\n',F7{i}{2});
end
fprintf(fid,'      IF( .NOT.ASSOCIATED(%sMWS) ) THEN\n',F7{i}{2});
fprintf(fid,'        CALL mexErrMsgTxt("Internal error pointing to %s pointer data")\n',F7{i}{1});
fprintf(fid,'      ENDIF\n');
fprintf(fid,'C\n');
if F7{i}{4}==1
fprintf(fid,'      DO I=1,M\n');
fprintf(fid,'        %sMWS(I)=%s(I)\n',F7{i}{2},F7{i}{2});
fprintf(fid,'      ENDDO\n');
elseif F7{i}{4}==2
fprintf(fid,'      DO I=1,M\n');
fprintf(fid,'        DO J=1,N\n');
fprintf(fid,'          %sMWS(I,J)=%s(I,J)\n',F7{i}{2},F7{i}{2});
fprintf(fid,'        ENDDO\n');    
fprintf(fid,'      ENDDO\n');    
end
fprintf(fid,'C\n');
fprintf(fid,'      CALL mxSetProperty(PLHS(1),1,''%s'',%sptr2)\n',F7{i}{1},F7{i}{2});
fprintf(fid,'      CALL mxDestroyArray(%sptr2)\n',F7{i}{2});
fprintf(fid,'      CALL mxDestroyArray(%sptr1)\n',F7{i}{2});
fprintf(fid,'C\n');
end
      
fclose(fid);

