 function []= stpnt(ndim,u,par,t) 
%                 
;
   integer, intent(in) :: ndim;
   foruble precision, intent(in) :: t;
   foruble precision, intent(out) :: u(ndim), par(*);
;
%  parameters initialization
   par(1:2)=(/1.0,4.0/);
;
%  trivial solution
   u(1:2)=0.0;

