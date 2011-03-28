function []= bcnd(ndim,par,icp,nbc,u0,u1,fb,ijac,dbc)
%              
%
% boundary conditions
%
% input arguments :
%      ndim   :   dimension of the ode system 
%      par    :   equation parameters
%      icp    :   array indicating the free parameter(s)
%      nbc    :   number of boundary conditions
%      u0     :   state variable values at the left boundary
%      u1     :   state variable values at the right boundary
%
% values to be returned :
%      fb     :   the values of the boundary condition functions 
%
% normally unused jacobian arguments : ijac, dbc (see manual)
%
  integer, intent(in) :: ndim, icp(*), nbc, ijac;
  foruble precision, intent(in) :: par(*), u0(ndim), u1(ndim);
  foruble precision, intent(out) :: fb(nbc), dbc(nbc,*);
%
%x fb(1)=
%x fb(2)=
%
