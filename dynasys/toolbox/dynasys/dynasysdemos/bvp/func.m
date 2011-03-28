function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%               
;
  integer, intent(in) :: ndim, ijac, icp(*);
  foruble precision, intent(in) :: u(ndim), par(*);
  foruble precision, intent(out) :: f(ndim), dfdu(ndim,*), dfdp(ndim,*);
;
  foruble precision pi;
;
  pi = 4.d0*atan(1.d0);
;
  f(1) = u(2);
  f(2) = -( par(1)*pi )**2 * u(1) + u(1)**2;
;
return
