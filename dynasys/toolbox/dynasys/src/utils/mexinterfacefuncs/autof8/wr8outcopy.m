function []=wr8outcopy()

dbstop if error

%% f8 file variables
F8{1}={'Ibr','IBRF8',1};
F8{2}={'Mtot','MTOTF8',1};
F8{3}={'Itp','ITPF8',1};
F8{4}={'Lab','LABF8',1};
F8{5}={'Nfpr','NFPRF8',1};
F8{6}={'Isw','ISWF8',1};
F8{7}={'Ntpl','NTPLF8',1};
F8{8}={'Nar','NARF8',1};
F8{9}={'Nrowpr','NROWPRF8',1};
F8{10}={'Ntst','NTSTF8',1};
F8{11}={'Ncol','NCOLF8',1};
F8{12}={'Nparx','NPARXF8',1};
F8{13}={'Ifpr','IFPRF8',2};
F8{14}={'T','TF8',1};
F8{15}={'Tm','TMF8',2};
F8{16}={'Par','PARF8',2};
F8{17}={'Rldot','RLDOTF8',2};
F8{18}={'U','UF8',2};
F8{19}={'Ups','UPSF8',3};
F8{20}={'Udotps','UDOTPSF8',3};

fid=fopen('f8copytodst2.txt','w');

if fid < 0
  return
end

for i=1:length(F8)
    fprintf(fid,'      mwPointer %sptr1, %sptr2\n',F8{i}{2},F8{i}{2});
end

for i=1:length(F8)
    if F8{i}{3}==1
    fprintf(fid,'      REAL*8, POINTER::%sMWS(:)\n',F8{i}{2});
    elseif F8{i}{3}==2
    fprintf(fid,'      REAL*8, POINTER::%sMWS(:,:)\n',F8{i}{2});    
    elseif F8{i}{3}==3
    fprintf(fid,'      REAL*8, POINTER::%sMWS(:,:,:)\n',F8{i}{2});    
    end
end

fprintf(fid,'C\nC\n');

for i=1:length(F8)
fprintf(fid,'C Assign %s values --------------\n',F8{i}{1});
fprintf(fid,'      %sptr1=mxGetProperty(PLHS(2),1,''%s'')\n',F8{i}{2},F8{i}{1});
fprintf(fid,'C\n');
fprintf(fid,'      IF( %sptr1 == 0 ) THEN\n',F8{i}{2});
fprintf(fid,'        CALL mexErrMsgTxt("Property %s not found")\n',F8{i}{1});
fprintf(fid,'      ENDIF\n');
fprintf(fid,'C\n');
if F8{i}{3}==1
fprintf(fid,'      M=SIZE(%s)\n',F8{i}{2});
fprintf(fid,'      D1=M\n');
fprintf(fid,'      %sptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName(''double''),0)\n',F8{i}{2});
fprintf(fid,'      CALL MatlabAPI_COM_Apx1(%%VAL(mxGetPr(%sptr2)), 1, D1 )\n',F8{i}{2});
fprintf(fid,'      %sMWS => Apx1\n',F8{i}{2});
elseif F8{i}{3}==2
fprintf(fid,'      M=SIZE(%s,1)\n',F8{i}{2});
fprintf(fid,'      N=SIZE(%s,2)\n',F8{i}{2});
fprintf(fid,'      D2(1)=M\n');
fprintf(fid,'      D2(2)=N\n');
fprintf(fid,'      %sptr2 = mxCreateNumericArray(2,D2,mxClassIDFromClassName(''double''),0)\n',F8{i}{2});
fprintf(fid,'      CALL MatlabAPI_COM_Apx2(%%VAL(mxGetPr(%sptr2)), 1, D2 )\n',F8{i}{2});
fprintf(fid,'      %sMWS => Apx2\n',F8{i}{2});
elseif F8{i}{3}==3
fprintf(fid,'      M=SIZE(%s,1)\n',F8{i}{2});
fprintf(fid,'      N=SIZE(%s,2)\n',F8{i}{2});
fprintf(fid,'      O=SIZE(%s,3)\n',F8{i}{2});
fprintf(fid,'      D3(1)=M\n');
fprintf(fid,'      D3(2)=N\n');
fprintf(fid,'      D3(3)=O\n');
fprintf(fid,'      %sptr2 = mxCreateNumericArray(3,D3,mxClassIDFromClassName(''double''),0)\n',F8{i}{2});
fprintf(fid,'      CALL MatlabAPI_COM_Apx3(%%VAL(mxGetPr(%sptr2)), 1, D3 )\n',F8{i}{2});
fprintf(fid,'      %sMWS => Apx3\n',F8{i}{2});
end
fprintf(fid,'      IF( .NOT.ASSOCIATED(%sMWS) ) THEN\n',F8{i}{2});
fprintf(fid,'        CALL mexErrMsgTxt("Internal error pointing to %s pointer data")\n',F8{i}{1});
fprintf(fid,'      ENDIF\n');
fprintf(fid,'C\n');
if F8{i}{3}==1
fprintf(fid,'      DO I=1,M\n');
fprintf(fid,'        %sMWS(I)=%s(I)\n',F8{i}{2},F8{i}{2});
fprintf(fid,'      ENDDO\n');
elseif F8{i}{3}==2
fprintf(fid,'      DO I=1,M\n');
fprintf(fid,'        DO J=1,N\n');
fprintf(fid,'          %sMWS(I,J)=%s(I,J)\n',F8{i}{2},F8{i}{2});
fprintf(fid,'        ENDDO\n');    
fprintf(fid,'      ENDDO\n');    
else
fprintf(fid,'      DO I=1,M\n');
fprintf(fid,'        DO J=1,N\n');
fprintf(fid,'          DO K=1,O\n');
fprintf(fid,'            %sMWS(I,J,K)=%s(I,J,K)\n',F8{i}{2},F8{i}{2});
fprintf(fid,'          ENDDO\n');  
fprintf(fid,'        ENDDO\n');    
fprintf(fid,'      ENDDO\n');    
end
fprintf(fid,'C\n');
fprintf(fid,'      CALL mxSetProperty(PLHS(2),1,''%s'',%sptr2)\n',F8{i}{1},F8{i}{2});
fprintf(fid,'      CALL mxDestroyArray(%sptr2)\n',F8{i}{2});
fprintf(fid,'      CALL mxDestroyArray(%sptr1)\n',F8{i}{2});
fprintf(fid,'C\n');
end
      
fclose(fid);