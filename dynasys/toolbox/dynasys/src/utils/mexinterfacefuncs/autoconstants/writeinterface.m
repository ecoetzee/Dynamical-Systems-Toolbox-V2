Ndim=int8(34);      % Problem dimension
Ips=1;              % Problem type
Irs=0;              % Start solution label
Ilp=1;              % Fold detection
Nicp=1;             % Continuation parameters
Icp=[1];            % Array of continuation parameters
Ntst=10;            % Number of mesh intervals
Ncol=4;             % Number of collocation points
Iad=3;              % Mesh adaptation every IAD steps
Isp=1;              % Bifurcation detection
Isw=0;              % Branch switching
Iplt=0;             % Select principal solution measure
Nbc=0;              % Number of boundary conditions
Nint=0;             % Number of integral conditions
Nmx=1000;           % Maximum number of steps
Rl0=-3.1416;        % Parameter interval RL0 = $$\lamda$$ = RL1
Rl1=3.1416;         %
A0=-1e6;            % Interval of principal solution measure A0 = ||·|| = A1
A1=1e6;             %
Npr=10;	           % Print and save restart data every NPR steps
Mxbf=10;            % Automatic branch switching for the fit MXBF bifurcation points
Iid=2;              % Control diagnostic output
Itmx=8;	           % Maximum number of iterations for locating special solutions/points
Itnw=5;             % Maximum number of correction steps
Nwtn=3;             % Corrector uses full Newton for NWTN steps
Jac=0;              % User defines derivatives
Epsl=1e-9;          % Convergence criterion for equation parameters
Epsu=1e-9;          % Convergence criterion for solution components
Epss=1e-8;          % Convergence criterion for special solutions/points
Ds=0.0175;          % Start step size
Dsmin=0.00175;      % Step size interval DSMIN =< DS =< DSMAX
Dsmax=0.175;        %
Iads=1;             % Step size adaptation every IADS steps
Nthl=[0];           % List of parameter and solution weights
Thl=[];
Nthu=[0];	       %
Thu=[];
Nuzr=[0];           % List of values for user defined output
Uzr=[];

       
C Assign NDIM values      
      NDIMptr1=mxGetProperty(PLHS(1),i,'Ndim')
C
      IF( NDIMptr1 == 0 ) THEN
        CALL mexErrMsgTxt("Property 'Ndim' not found")
      ENDIF
C
      IF( mxIsInt8(NDIMptr1) == 0 ) THEN
        CALL mexErrMsgTxt("Property 'NDIM' is not an integer")
      ENDIF
C
      NDIMptr2=mxGetPr(NDIMptr1)  
      CALL mxCopyPtrToInteger4(NDIMptr2,NDIMDST,n)
      CALL mxDestroyArray(NDIMptr1)      
C       