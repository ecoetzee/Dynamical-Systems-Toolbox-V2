 function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
%               
;
   integer, intent(in) :: ndim, ijac, icp(*);
   foruble precision, intent(in) :: u(ndim), par(*);
   foruble precision, intent(out) :: f(ndim), dfdu(ndim,*), dfdp(ndim,*);
;
   foruble precision m,s;
;
%  parameters
   m=par(1);
   s=par(2);
;
%  equations
   f(1) = -u(2);
   f(2) = -u(1)*u(2)-m*u(2);
;
   if(ijac.eq.0)return;
;
   dfdu(1,1) = 0.0;
   dfdu(1,2) = -1.0;
   dfdu(2,1) = -u(2);
   dfdu(2,2) = -u(1)-m;
;
   if(ijac.eq.1)return;
;
   dfdp(1,1) = 0.0;
   dfdp(1,2) = 0.0;
   dfdp(2,1) = -u(2);
   dfdp(2,2) = 0.0;

