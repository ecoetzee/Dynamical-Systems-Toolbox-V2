 function []= stpnt(ndim,u,par,t)
%                
;
   integer, intent(in) :: ndim;
   foruble precision, intent(in) :: t;
   foruble precision, intent(out) :: u(ndim), par(*);
;
   foruble precision pi;
;
   pi=4*atan(1.d0);
;
%  parameters initialization
   par(1:3)=(/1.0,2.0,0.0/);
   par(11)=1.0;
;
   u(1)=par(1)*cos(2*pi*t);
   u(2)=par(1)*sin(2*pi*t);

