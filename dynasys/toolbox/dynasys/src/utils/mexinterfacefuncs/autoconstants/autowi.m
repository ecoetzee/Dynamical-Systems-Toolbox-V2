% Generate the fortran code to interface objects with fortran

a.Ndim=int32(34);      % Problem dimension
a.Ips=int32(1);              % Problem type
a.Irs=int32(0);              % Start solution label
a.Ilp=int32(1);              % Fold detection
a.Nicp=int32(1);             % Continuation parameters
a.Icp=int32(1);            % Array of continuation parameters
a.Ntst=int32(10);            % Number of mesh intervals
a.Ncol=int32(4);             % Number of collocation points
a.Iad=int32(3);              % Mesh adaptation every IAD steps
a.Isp=int32(1);              % Bifurcation detection
a.Isw=int32(0);              % Branch switching
a.Iplt=int32(0);             % Select principal solution measure
a.Nbc=int32(0);              % Number of boundary conditions
a.Nint=int32(0);             % Number of integral conditions
a.Nmx=int32(1000);           % Maximum number of steps
a.Rl0=-3.1416;        % Parameter interval RL0 = $$\lamda$$ = RL1
a.Rl1=3.1416;         %
a.A0=-1e6;            % Interval of principal solution measure A0 = ||·|| = A1
a.A1=1e6;             %
a.Npr=int32(10);	           % Print and save restart data every NPR steps
a.Mxbf=int32(10);            % Automatic branch switching for the fit MXBF bifurcation points
a.Iid=int32(2);              % Control diagnostic output
a.Itmx=int32(8);	           % Maximum number of iterations for locating special solutions/points
a.Itnw=int32(5);             % Maximum number of correction steps
a.Nwtn=int32(3);             % Corrector uses full Newton for NWTN steps
a.Jac=int32(0);              % User defines derivatives
a.Epsl=1e-9;          % Convergence criterion for equation parameters
a.Epsu=1e-9;          % Convergence criterion for solution components
a.Epss=1e-8;          % Convergence criterion for special solutions/points
a.Ds=0.0175;          % Start step size
a.Dsmin=0.00175;      % Step size interval DSMIN =< DS =< DSMAX
a.Dsmax=0.175;        %
a.Iads=int32(1);             % Step size adaptation every IADS steps
a.Nthl=int32(0);           % List of parameter and solution weights
a.Thl=[];
a.Nthu=int32(0);	       %
a.Thu=[];
a.Nuzr=int32(0);           % List of values for user defined output
a.Uzr=[];

b=fieldnames(a);

fid=fopen('dsto2auto.txt','w');

for i=1:length(b);    
    fprintf(fid,'C Assign %s values\n',b{i});
    fprintf(fid,'      %sptr1=mxGetProperty(PLHS(1),i,''%s''\n',upper(b{i}),b{i});
    fprintf(fid,'C\n');
    fprintf(fid,'      IF( %sptr1 == 0 ) THEN\n',upper(b{i}));
    fprintf(fid,'        CALL mexErrMsgTxt("Property %s not found")\n',b{i});
    fprintf(fid,'      ENDIF\n');
    fprintf(fid,'C\n');
    if isinteger(getfield(a,b{i}))
    fprintf(fid,'      IF( mxIsInt32(%sptr1) == 0 ) THEN\n',upper(b{i}));
    else
    fprintf(fid,'      IF( mxIsDouble(%sptr1) == 0 ) THEN\n',upper(b{i}));        
    end
    fprintf(fid,'        CALL mexErrMsgTxt("Property %s is not an integer")\n',b{i});
    fprintf(fid,'      ENDIF\n');
    fprintf(fid,'C\n');
    fprintf(fid,'      %sptr2=mxGetPr(%sptr1)\n',upper(b{i}));
    if isinteger(getfield(a,b{i}))
    fprintf(fid,'      CALL mxCopyPtrToInteger4(%sptr2,%sDST,n)\n',upper(b{i}),upper(b{i}));
    else
    fprintf(fid,'      CALL mxCopyPtrToReal8(%sptr2,%sDST,n)\n',upper(b{i}),upper(b{i}));        
    end
    fprintf(fid,'      CALL mxDestroyArray(%sptr1)\n',upper(b{i}));
    fprintf(fid,'C\n');
end

fclose(fid);