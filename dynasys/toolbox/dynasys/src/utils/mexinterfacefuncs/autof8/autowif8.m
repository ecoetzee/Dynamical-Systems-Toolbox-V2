a.Ibr={int32(0),1};
a.Mtot={int32(0),1};
a.Itp={int32(0),1};
a.Lab={int32(0),1};
a.Nfpr={int32(0),1};
a.Isw={int32(0),1};
a.Ntpl={int32(0),1};
a.Nar={int32(0),1};
a.Nrowpr={int32(0),1};
a.Ntst={int32(0),1};
a.Ncol={int32(0),1};
a.Nparx={int32(0),1};
a.Ifpr={int32(0),2};
a.T={double(0),1};
a.Tm={double(0),2};
a.Par={double(0),2};
a.Rldot={double(0),2};
a.U={double(0),2};
a.Ups={double(0),3};
a.Udotps={double(0),3};

b=fieldnames(a);

fid=fopen('f82auto2.txt','w');

for i=1:length(b)
fprintf(fid,'      mwPointer %sF8ptr1, %sF8ptr2\n',upper(b{i}),upper(b{i}));
%     if isinteger(getfield(a,b{i}))
%     fprintf(fid,'      INTEGER %sDST\n',upper(b{i}));
%     else
%     fprintf(fid,'      DOUBLE PRECISION %sDST\n',upper(b{i}));    
%     end
end

    fprintf(fid,'C\n');
    fprintf(fid,'C\n');
    
for i=1:length(b);    
fprintf(fid,'C Assign %s values --------------\n',b{i});
fprintf(fid,'      %sF8ptr1=mxGetProperty(PRHS(4),i,''%s'')\n',upper(b{i}),b{i});
fprintf(fid,'C\n');
fprintf(fid,'      IF( %sF8ptr1 == 0 ) THEN\n',upper(b{i}));
fprintf(fid,'        CALL mexErrMsgTxt("Property %s in object F8 not found")\n',b{i});
fprintf(fid,'      ENDIF\n');
fprintf(fid,'C\n');
tmp=getfield(a,b{i});
if isinteger(tmp{1})
fprintf(fid,'      IF( mxIsInt32(%sF8ptr1) == 0 ) THEN\n',upper(b{i}));
fprintf(fid,'        CALL mexErrMsgTxt("Property %s in object F8 is not an integer")\n',b{i});
else
fprintf(fid,'      IF( mxIsDouble(%sF8ptr1) == 0 ) THEN\n',upper(b{i}));    
fprintf(fid,'        CALL mexErrMsgTxt("Property %s in object F8 is not a double")\n',b{i});
end
fprintf(fid,'      ENDIF\n');
fprintf(fid,'C\n');
fprintf(fid,'      %sF8ptr2=mxGetPr(%sF8ptr1)\n',upper(b{i}),upper(b{i}));
fprintf(fid,'      IF( IRSDST > 0) THEN\n');
fprintf(fid,'        m=mxGetNumberOfElements(%sF8ptr1)\n',upper(b{i}));
if tmp{2}==1
fprintf(fid,'        D1ptr=mxGetDimensions(%sF8ptr1)\n',upper(b{i}));   
elseif tmp{2}==2
fprintf(fid,'        D2ptr=mxGetDimensions(%sF8ptr1)\n',upper(b{i}));    
else
fprintf(fid,'        D3ptr=mxGetDimensions(%sF8ptr1)\n',upper(b{i}));    
end
if tmp{2}==1 && isinteger(tmp{1})
fprintf(fid,'        CALL mxCopyPtrToInteger4(D1ptr,D1,1)\n');
elseif tmp{2}==1 && isfloat(tmp{1})
fprintf(fid,'        CALL mxCopyPtrToReal8(D1ptr,D1,1)\n');
end
if tmp{2}==2 && isinteger(tmp{1})
fprintf(fid,'        CALL mxCopyPtrToInteger4(D2ptr,D2,2)\n');
elseif tmp{2}==2 && isfloat(tmp{1})
fprintf(fid,'        CALL mxCopyPtrToReal8(D2ptr,D2,2)\n');
end
if tmp{2}==3 && isinteger(tmp{1})
fprintf(fid,'        CALL mxCopyPtrToInteger4(D3ptr,D3,3)\n');
elseif tmp{2}==3 && isfloat(tmp{1})
fprintf(fid,'        CALL mxCopyPtrToReal8(D3ptr,D3,3)\n');
end
if tmp{2}==1
fprintf(fid,'        ALLOCATE(%sF8(D1(1)))\n',upper(b{i}));
elseif tmp{2}==2
fprintf(fid,'        ALLOCATE(%sF8(D2(1),D2(2)))\n',upper(b{i}));    
else
fprintf(fid,'        ALLOCATE(%sF8(D3(1),D3(2),D3(3)))\n',upper(b{i}));    
end

fprintf(fid,'      ELSE\n');
fprintf(fid,'        m=0\n');
if tmp{2}==1
fprintf(fid,'        ALLOCATE(%sF8(0))\n',upper(b{i}));
elseif tmp{2}==2
fprintf(fid,'        ALLOCATE(%sF8(0,0))\n',upper(b{i}));    
else
fprintf(fid,'        ALLOCATE(%sF8(0,0,0))\n',upper(b{i}));    
end

fprintf(fid,'      ENDIF\n');

if isinteger(tmp{1})
fprintf(fid,'      CALL mxCopyPtrToInteger4(%sF8ptr2,%sF8,m)\n',upper(b{i}),upper(b{i}));
else
fprintf(fid,'      CALL mxCopyPtrToReal8(%sF8ptr2,%sF8,m)\n',upper(b{i}),upper(b{i}));    
end

fprintf(fid,'      CALL mxDestroyArray(%sF8ptr1)\n',upper(b{i}));
fprintf(fid,'C\n');  
end

fclose(fid);