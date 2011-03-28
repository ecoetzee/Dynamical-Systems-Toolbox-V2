classdef autoconstants < hgsetget
    
%AUTOCONSTANTS Constants used for continuation analysis. 
% 
% Similar convention used as for the constants file in AUTO. 
% 
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_NDIM'],'-helpbrowser')">Ndim </a> : Problem dimension
%    Noutx : Maximum number of auxilliary outputs 
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_IPS'],'-helpbrowser')">Ips</a>   : Problem type
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_IRS'],'-helpbrowser')">Irs</a>   : Start solution label
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_ILP'],'-helpbrowser')">Ilp</a>   : Fold detection
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_ICP'],'-helpbrowser')">Icp</a>   : Array of continuation parameters
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_NTST'],'-helpbrowser')">Ntst</a>  : Number of mesh intervals
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_NCOL'],'-helpbrowser')">Ncol</a>  : Number of collocation points
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_IAD'],'-helpbrowser')">Iad</a>   : Mesh adaptation every IAD steps
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_ISP'],'-helpbrowser')">Isp</a>   : Bifurcation detection
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_ISW'],'-helpbrowser')">Isw</a>   : Branch switching
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_IPLT'],'-helpbrowser')">Iplt</a>  : Select principal solution measure
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_NBC'],'-helpbrowser')">Nbc</a>   : Number of boundary conditions
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_NINT'],'-helpbrowser')">Nint</a>  : Number of integral conditions
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_NMX'],'-helpbrowser')">Nmx</a>   : Maximum number of steps
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_RL0'],'-helpbrowser')">Rl0</a>   : Parameter interval Rl0 =< $$\lamda$$ =< Rl1
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_RL1'],'-helpbrowser')">Rl1</a>   : 
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_A0'],'-helpbrowser')">A0</a>    : Interval of principal solution measure A0 =< ||·|| =< A1
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_A1'],'-helpbrowser')">A1</a>    : 
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_NPR'],'-helpbrowser')">Npr</a>   : Print and save restart data every Npr steps
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_MXBF'],'-helpbrowser')">Mxbf</a>  : Automatic branch switching for the fit Mxbf bifurcation points
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_IID'],'-helpbrowser')">Iid</a>   : Control diagnostic output
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_ITMX'],'-helpbrowser')">Itmx</a>  : Maximum number of iterations for locating special solutions/points
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_ITNW'],'-helpbrowser')">Itnw</a>  : Maximum number of correction steps
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_NWTN'],'-helpbrowser')">Nwtn</a>  : Corrector uses full Newton for Nwtn steps
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_JAC'],'-helpbrowser')">Jac</a>   : User defines derivatives
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_EPSL'],'-helpbrowser')">Epsl</a>  : Convergence criterion for equation parameters
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_EPSU'],'-helpbrowser')">Epsu</a>  : Convergence criterion for solution components
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_EPSS'],'-helpbrowser')">Epss</a>  : Convergence criterion for special solutions/points
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_DS'],'-helpbrowser')">Ds</a>    : Start step size
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_DSMIN'],'-helpbrowser')">Dsmin</a> : Step size interval Dsmin =< Ds =< Dsmax
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_DSMAX'],'-helpbrowser')">Dsmax</a> : 
%    <a href="matlab:web(['jar:file:///' dynasyshelproot '/help.jar!/main_constants_ref.html#CR_IADS'],'-helpbrowser')">Iads</a>  : Step size adaptation every IADS steps
%    Nthl* : This parameter has been removed. Automatically detected
%    Thl   : Nx2 vector containing list of parameter and solution weights,
%            where N is number of parameters. [Ithl,Vthl] is the vector.
%    Nthu* : This parameter has been removed. Automatically detected
%    Thu   : Nx2 vector containing list of parameter and solution weights,
%            where N is number of parameters. [Ithu,Vthu] is the vector.
%    Nuzr* : This parameter has been removed. Automatically detected.
%    Uzr   : Nx2 vector containing list of values for user defined output,
%            where N is number of parameters. [Iuz,Vuz] is the vector.
%  
% *Parameters related to the dimensions of weighted parameters are now 
%  automatically detected and do not need to be defined.

%   Created by Etienne COETZEE, James RANKIN,and Phani THOTA, University of Bristol
%
%   $Revision: 2.0.0.0 $ $Date: 2010/07/29 14:07:00$

    properties               
        %
        % Constants
        Ndim=2;             % Problem dimension
        Noutx=0;            % Maximum number of outputs written out
        Ips=1;              % Problem type
        Irs=0;              % Start solution label
        Ilp=1;              % Fold detection
        Icp=1;              % Array of continuation parameters
        Ntst=50;            % Number of mesh intervals
        Ncol=4;             % Number of collocation points
        Iad=3;              % Mesh adaptation every IAD steps
        Isp=1;              % Bifurcation detection
        Isw=1;              % Branch switching
        Iplt=0;             % Select principal solution measure
        Nbc=0;              % Number of boundary conditions
        Nint=0;             % Number of integral conditions
        Nmx=100;            % Maximum number of steps
        Rl0=0;              % Parameter interval RL0 = $$\lamda$$ = RL1
        Rl1=0.15;           %
        A0=0;               % Interval of principal solution measure A0 = ||·|| = A1
        A1=100;             %
        Npr=100;	        % Print and save restart data every NPR steps
        Mxbf=10;            % Automatic branch switching for the fit MXBF bifurcation points
        Iid=2;              % Control diagnostic output
        Itmx=8;	            % Maximum number of iterations for locating special solutions/points
        Itnw=5;             % Maximum number of correction steps
        Nwtn=3;             % Corrector uses full Newton for NWTN steps
        Jac=0;              % User defines derivatives
        Epsl=1e-6;          % Convergence criterion for equation parameters
        Epsu=1e-6;          % Convergence criterion for solution components
        Epss=1e-4;          % Convergence criterion for special solutions/points
        Ds=0.01;            % Start step size
        Dsmin=0.005;        % Step size interval DSMIN =< DS =< DSMAX
        Dsmax=0.05;         %
        Iads=1;             % Step size adaptation every IADS steps
        Thl=[];
        Thu=[];
        Uzr=[];

    end
    
    properties (SetAccess = protected,Hidden=true)
        Nicp=1;
        Nthl=0;
        Ithl=[];
        Vthl=[];
        Nthu=0;
        Ithu=[];
        Vthu=[];
        Nuzr=0;
        Iuz=[];
        Vuz=[];
    end
    
    methods
        function obj=autoconstants()            

        end

        % Problem dimension
        function obj=set.Ndim(obj,val)
             obj.Ndim=val;  
        end
        
        % Maximum number of outputs written out
        function obj=set.Noutx(obj,val)
             obj.Noutx=val;  
        end

        % Problem type
        function obj=set.Ips(obj,val)
             obj.Ips=val;  
        end        

        % Start solution label
        function obj=set.Irs(obj,val)
             obj.Irs=val;  
        end
        
        % Fold detection
        function obj=set.Ilp(obj,val)
             obj.Ilp=val;  
        end
        
        % Array of continuation parameters        
        function obj=set.Icp(obj,val)
             obj.Icp=val; 
             obj.Nicp=length(val);
        end

        % Number of mesh intervals
        function obj=set.Ntst(obj,val)
             obj.Ntst=val;  
        end
        
        % Number of collocation points
        function obj=set.Ncol(obj,val)
             obj.Ncol=val;  
        end
        
        % Mesh adaptation every IAD steps
        function obj=set.Iad(obj,val)
             obj.Iad=val;  
        end

        % Bifurcation detection
        function obj=set.Isp(obj,val)
             obj.Isp=val;  
        end
        
        % Branch switching
        function obj=set.Isw(obj,val)
             obj.Isw=val;  
        end        
        
        % Select principal solution measure
        function obj=set.Iplt(obj,val)
             obj.Iplt=val;  
        end  

        % Number of boundary conditions
        function obj=set.Nbc(obj,val)
             obj.Nbc=val;  
        end 

        % Number of integral conditions
        function obj=set.Nint(obj,val)
             obj.Nint=val;  
        end 

        % Maximum number of steps
        function obj=set.Nmx(obj,val)
             obj.Nmx=val;  
        end 

        % Parameter interval RL0 = $$\lamda$$ = RL1
        function obj=set.Rl0(obj,val)
             obj.Rl0=val;  
        end
                
        function obj=set.Rl1(obj,val)
             obj.Rl1=val;  
        end

        % Interval of principal solution measure A0 = ||·|| = A1
        function obj=set.A0(obj,val)
             obj.A0=val;  
        end
        
        function obj=set.A1(obj,val)
             obj.A1=val;  
        end

        % Print and save restart data every NPR steps
        function obj=set.Npr(obj,val)
             obj.Npr=val;  
        end

        % Automatic branch switching for the fit MXBF bifurcation points
        function obj=set.Mxbf(obj,val)
             obj.Mxbf=val;  
        end
        
        % Control diagnostic output
        function obj=set.Iid(obj,val)
             obj.Iid=val;  
        end
        
        % Maximum number of iterations for locating special
        % solutions/points
        function obj=set.Itmx(obj,val)
             obj.Itmx=val;  
        end
        
        % Maximum number of correction steps
        function obj=set.Itnw(obj,val)
             obj.Itnw=val;  
        end

        % Corrector uses full Newton for NWTN steps
        function obj=set.Nwtn(obj,val)
             obj.Nwtn=val;  
        end
        
        % User defines derivatives
        function obj=set.Jac(obj,val)
             obj.Jac=val;  
        end
        
        % Convergence criterion for equation parameters
        function obj=set.Epsl(obj,val)
             obj.Epsl=val;  
        end        
            
        % Convergence criterion for solution components
        function obj=set.Epsu(obj,val)
             obj.Epsu=val;  
        end          

        % Convergence criterion for special solutions/points
        function obj=set.Epss(obj,val)
             obj.Epss=val;  
        end            
                      
        % Start step size
        function obj=set.Ds(obj,val)
             obj.Ds=val;  
        end
        
        % Step size interval DSMIN =< DS =< DSMAX
        function obj=set.Dsmin(obj,val)
             obj.Dsmin=val; 
        end
        
        % Step size adaptation every IADS steps
        function obj=set.Dsmax(obj,val)
             obj.Dsmax=val;
        end
        
        % Weighted parameters
        function obj=set.Thl(obj,val)
            if isempty(val) || size(val,2)==2
                obj.Thl=val;
                Nthl=size(val,1);
                if Nthl > 0
                    Ithl=obj.Thl(:,1);
                    Vthl=obj.Thl(:,2);
                else
                    Ithl=[];
                    Vthl=[];
                end
                
                obj.Nthl=Nthl;
                obj.Ithl=Ithl;
                obj.Vthl=Vthl;
            else
                error('DST:ConstantsDefinition:','Make sure Thl contains two columns')
            end
        end
        
        function obj=set.Thu(obj,val)
            if isempty(val) || size(val,2)==2
                obj.Thu=val;
                Nthu=size(val,1);
                if Nthu > 0
                    Ithu=obj.Thu(:,1);
                    Vthu=obj.Thu(:,2);
                else
                    Ithu=[];
                    Vthu=[];
                end
                
                obj.Nthu=Nthu;
                obj.Ithu=Ithu;
                obj.Vthu=Vthu;
            else   
                error('DST:ConstantsDefinition:','Make sure Thu contains two columns')
            end   
        end
        
        function obj=set.Uzr(obj,val)
            if isempty(val) || size(val,2)==2
                obj.Uzr=val;
                Nuzr=size(val,1);
                if Nuzr > 0
                    Iuz=obj.Uzr(:,1);
                    Vuz=obj.Uzr(:,2);
                else
                    Iuz=[];
                    Vuz=[];
                end
                
                obj.Nuzr=Nuzr;
                obj.Iuz=Iuz;
                obj.Vuz=Vuz;
            else
                error('DST:ConstantsDefinition:','Make sure Uzr contains two columns')
            end   
        end
               
        function obj=assigntypec(obj)
            % Constants
            obj.Ndim=int32(obj.Ndim);
            obj.Noutx=int32(obj.Noutx);
            obj.Ips=int32(obj.Ips);
            obj.Irs=int32(obj.Irs);
            obj.Ilp=int32(obj.Ilp);
            obj.Icp=int32(obj.Icp);
            obj.Nicp=int32(obj.Nicp);
            obj.Ntst=int32(obj.Ntst);
            obj.Ncol=int32(obj.Ncol);
            obj.Iad=int32(obj.Iad);
            obj.Isp=int32(obj.Isp);
            obj.Isw=int32(obj.Isw);
            obj.Iplt=int32(obj.Iplt);
            obj.Nbc=int32(obj.Nbc);
            obj.Nint=int32(obj.Nint);
            obj.Nmx=int32(obj.Nmx);
            obj.Rl0=double(obj.Rl0);
            obj.Rl1=double(obj.Rl1);
            obj.A0=double(obj.A0);
            obj.A1=double(obj.A1);
            obj.Npr=int32(obj.Npr);
            obj.Mxbf=int32(obj.Mxbf);
            obj.Iid=int32(obj.Iid);
            obj.Itmx=int32(obj.Itmx);
            obj.Itnw=int32(obj.Itnw);
            obj.Nwtn=int32(obj.Nwtn);
            obj.Jac=int32(obj.Jac);
            obj.Epsl=double(obj.Epsl);
            obj.Epsu=double(obj.Epsu);
            obj.Epss=double(obj.Epss);
            obj.Ds=double(obj.Ds);
            obj.Dsmin=double(obj.Dsmin);
            obj.Dsmax=double(obj.Dsmax);
            obj.Iads=int32(obj.Iads);
            obj.Nthl=int32(obj.Nthl);
            obj.Nthu=int32(obj.Nthu);
            obj.Nuzr=int32(obj.Nuzr);
            obj.Ithl=int32(obj.Ithl);
            obj.Vthl=double(obj.Vthl);
            obj.Ithu=int32(obj.Ithu);
            obj.Vthu=double(obj.Vthu);
            obj.Iuz=int32(obj.Iuz);
            obj.Vuz=double(obj.Vuz);
        end
        
        % Make a copy of a handle object.
        function new = copy(this)
            % Instantiate new object of the same class.
            new = feval(class(this));
 
            % Copy all non-hidden properties.
            p = properties(this);
            for i = 1:length(p)
                new.(p{i}) = this.(p{i});
            end
        end
 
    end
    
end
