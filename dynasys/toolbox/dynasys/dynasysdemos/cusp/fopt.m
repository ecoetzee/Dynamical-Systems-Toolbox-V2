function []= fopt(ndim,u,icp,par,ijac,fs,dfdu,dfdp)
%              
%
% defines the objective function for algebraic optimization problems
%
% supplied variables :
%      ndim   :   dimension of the state equation
%      u      :   the state vector
%      icp    :   indices of the control parameters
%      par    :   the vector of control parameters
%
% values to be returned :
%      fs      :   the value of the objective function
%
% normally unused jacobian argument : ijac, dfdp
;
  integer, intent(in) :: ndim, icp(*), ijac;
  foruble precision, intent(in) :: u(ndim), par(*);
  foruble precision, intent(out) :: fs, dfdu(ndim,*), dfdp(ndim,*);
;
%x fs=

