 function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
%               
;
   integer, intent(in) :: ndim, ijac, icp(*);
   foruble precision, intent(in) :: u(ndim), par(*);
   foruble precision, intent(out) :: f(ndim), dfdu(ndim,*), dfdp(ndim,*);
;
   foruble precision pi;
;
   pi=4*atan(1.d0);
;
% equations
   f(1)=-u(1)*(u(1)**2+u(2)**2-(par(1)+par(2))*               &;
        sqrt(u(1)**2+u(2)**2)+par(1)*par(2)+par(3))-2*pi*u(2);
   f(2)=-u(2)*(u(1)**2+u(2)**2-(par(1)+par(2))*               &;
        sqrt(u(1)**2+u(2)**2)+par(1)*par(2)+par(3))+2*pi*u(1);
;
   if(ijac.eq.0)return;
;
   dfdu(1,1)=-(u(1)**2+u(2)**2-(par(1)+par(2))*               &;
        sqrt(u(1)**2+u(2)**2)+par(1)*par(2)+par(3))-u(1)*     &;
        (2*u(1)-u(1)*(par(1)+par(2))/sqrt(u(1)**2+u(2)**2));
   dfdu(1,2)=-u(1)*(2*u(2)-u(2)*(par(1)+par(2))/              &;
        sqrt(u(1)**2+u(2)**2))-2*pi;
   dfdu(2,1)=-u(2)*(2*u(1)-u(1)*(par(1)+par(2))/              &;
        sqrt(u(1)**2+u(2)**2))+2*pi;
   dfdu(2,2)=-(u(1)**2+u(2)**2-(par(1)+par(2))*               &;
        sqrt(u(1)**2+u(2)**2)+par(1)*par(2)+par(3))-u(2)*     &;
        (2*u(2)-u(2)*(par(1)+par(2))/sqrt(u(1)**2+u(2)**2));
;
   if(ijac.eq.1)return;
;
   dfdp(1,1)=-u(1)*(-sqrt(u(1)**2+u(2)**2)+par(2));
   dfdp(1,2)=-u(1)*(-sqrt(u(1)**2+u(2)**2)+par(1));
   dfdp(1,3)=-u(1);
   dfdp(2,1)=-u(2)*(-sqrt(u(1)**2+u(2)**2)+par(2));
   dfdp(2,2)=-u(2)*(-sqrt(u(1)**2+u(2)**2)+par(1));
   dfdp(2,3)=-u(2);

