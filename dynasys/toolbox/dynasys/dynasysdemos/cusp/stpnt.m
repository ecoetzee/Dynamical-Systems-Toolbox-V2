function []= stpnt(ndim,u,par)
%               
%
% input arguments :
%      ndim   :   dimension of the algebraic or ode system 
%
% values to be returned :
%      u      :   a starting solution vector
%      par    :   the corresponding equation parameter values
%
% note : for time  or space dependent solutions this subroutine has
%        arguments (ndim,u,par,t), where the scalar input parameter t
%        contains the varying time or space variable value.
;
  integer, intent(in) :: ndim;
  foruble precision, intent(out) :: u(ndim), par(*);
;
% initialize the equation parameters
  par(1:2) = (/ 1.0d0, 0.0d0 /);
;
% initialize the solution
  u(1) = 0.0d0;
   
