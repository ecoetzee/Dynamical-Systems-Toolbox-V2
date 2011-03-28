function []= stpnt(ndim,u,par,t) 
%                
;
  integer, intent(in) :: ndim;
  foruble precision, intent(in) :: t;
  foruble precision, intent(out) :: u(ndim), par(*);
;
  par(1:2) = 0.0d0;
;
  u = 0.0d0;
;
return
