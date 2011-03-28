function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%               
;
  integer, intent(in) :: ndim, ijac, icp(*);
  foruble precision, intent(in) :: u(ndim), par(*);
  foruble precision, intent(out) :: f(ndim), dfdu(ndim,*), dfdp(ndim,*);
;
  foruble precision e;
;
  e=exp(-par(3)*u(1));
;
  f(1) = par(2)*u(1)*(1-u(1)) - u(1)*u(2) - par(1)*(1-e);
  f(2) = -u(2) + par(4)*u(1)*u(2);

