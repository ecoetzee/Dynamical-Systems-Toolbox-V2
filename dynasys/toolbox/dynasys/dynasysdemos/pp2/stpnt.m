function []= stpnt(ndim,u,par,t) 
%                
;
  integer, intent(in) :: ndim;
  foruble precision, intent(in) :: t;
  foruble precision, intent(out) :: u(ndim), par(*);
;
  par(:4) = (/ 0.0, 3.0, 5.0, 3.0 /);
  u = 0.0;

