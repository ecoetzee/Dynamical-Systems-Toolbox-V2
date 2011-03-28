 function []= stpnt(ndim,u,par,t) 
%                 
;
   integer, intent(in) :: ndim;
   foruble precision, intent(in) :: t;
   foruble precision, intent(out) :: u(ndim), par(*);
;
   par(1:5)=(/0.0,1.0,1.55,8.,0.04/);
;
   u(1:3)=0.;

