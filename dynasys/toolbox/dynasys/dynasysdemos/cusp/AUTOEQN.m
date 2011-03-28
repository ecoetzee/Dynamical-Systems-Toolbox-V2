function varargout=AUTOEQN(fstr,varargin)
% AUTOEQN.m - equation file for AUTO
%   
%   MATLAB version of FUNC file in AUTO
%

%   Written by ETIENNE COETZEE
%
%   $Revision: 1.0.0.0 $ $Date: 2008/12/17 16:27:00$

% Do not edit this switch yard
if strcmp(fstr,'FUNC')
    [varargout{1}]=FUNC(varargin{:});
elseif strcmp(fstr,'STPNT')
    [varargout{1:2}]=STPNT;
elseif strcmp(fstr,'BCND')
   return
elseif strcmp(fstr,'ICND')
   return
elseif strcmp(fstr,'FOPT')
   return
elseif strcmp(fstr,'PVLS')
   return 
else
  error('AUTOmatlab:eqnFileFuncCall',...
        'Incorrect function call from mex file.');
end



%--------------------------------------------------------------------------
%                  EDIT FUNCTIONS BELOW AS APPROPRIATE
%--------------------------------------------------------------------------
function F=FUNC(NDIM,U,PAR)
%   Evaluates the algebraic equations or ODE right hand side
%
%   Input arguments :
%        NDIM   :   Dimension of the ODE system 
%        U      :   State variables
%        ICP    :   Array indicating the free parameter(s)
%        PAR    :   Equation parameters
%
%   Values to be returned :
%        F      :   ODE right hand side values
%
%   Normally unused Jacobian arguments : IJAC, DFDU, DFDP (see manual)
F=[0];

x = U(1);
lambda = PAR(1);
mu = PAR(2);
F(1)= mu + lambda*x - x^3;


%--------------------------------------------------------------------------
function [PAR,U]=STPNT()

%Define the initial conditions here, where PAR are the system
%parameters, and U are the initial state conditions

% Initialize the equation parameters
  PAR(1)=1;
  PAR(2)=0;

% Initialize the solution
  U(1)=0;

%--------------------------------------------------------------------------
function []=BCND()
   return
%--------------------------------------------------------------------------
function []=ICND()
   return
%--------------------------------------------------------------------------
function []=FOPT()
   return
%--------------------------------------------------------------------------
function []=PVLS()
   return 
