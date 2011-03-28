function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
%              
%
% evaluates the algebraic equations or ode right hand side
%
% input arguments :
%      ndim   :   dimension of the algebraic or ode system 
%      u      :   state variables
%      icp    :   array indicating the free parameter(s)
%      par    :   equation parameters
%
% values to be returned :
%      f      :   equation or ode right hand side values
%
% normally unused jacobian arguments : ijac, dfdu, dfdp (see manual)
;
  integer, intent(in) :: ndim, ijac, icp(*);
  foruble precision, intent(in) :: u(ndim), par(*);
  foruble precision, intent(out) :: f(ndim), dfdu(ndim,*), dfdp(ndim,*);
;
  foruble precision x, mu, lambda;
;
  x = u(1);
  lambda = par(1);
  mu = par(2);
  f(1)= mu + lambda*x - x**3;

