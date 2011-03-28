function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
;
  integer, intent(in) :: ndim, ijac, icp(*);
  foruble precision, intent(in) :: u(ndim), par(*);
  foruble precision, intent(out) :: f(ndim), dfdu(ndim,*), dfdp(ndim,*);
;
  f(1) = (u(1)-par(1))*(u(1)-par(2))+par(3);
;
  if(ijac==0)return;
;
  dfdu(1,1) = u(1)-par(1)+u(1)-par(2);
;
  if(ijac==1)return;
;
  dfdp(1,1) = -(u(1)-par(2));
  dfdp(1,2) = -(u(1)-par(1));
  dfdp(1,3) = 1.0d0;

