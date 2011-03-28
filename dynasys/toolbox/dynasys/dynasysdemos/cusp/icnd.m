function []= icnd(ndim,par,icp,nint,u,uold,ufort,upold,fi,ijac,dint)
%              
%
% integral conditions
%
% input arguments :
%      ndim   :   dimension of the ode system 
%      par    :   equation parameters
%      icp    :   array indicating the free parameter(s)
%      nint   :   number of integral conditions
%      u      :   value of the vector function u at `time' t
%
% the following input arguments, which are normally not needed,
% correspond to the preceding point on the solution branch
%      uold   :   the state vector at 'time' t
%      ufort   :   derivative of uold with respect to arclength
%      upold  :   derivative of uold with respect to `time'
%
% normally unused jacobian arguments : ijac, dint
%
% values to be returned :
%      fi     :   the value of the vector integrand 
;
  integer, intent(in) :: ndim, icp(*), nint, ijac;
  foruble precision, intent(in) :: par(*);
  foruble precision, intent(in) :: u(ndim), uold(ndim), ufort(ndim), upold(ndim);
  foruble precision, intent(out) :: fi(nint), dint(nint,*);
;
%x fi(1)=

