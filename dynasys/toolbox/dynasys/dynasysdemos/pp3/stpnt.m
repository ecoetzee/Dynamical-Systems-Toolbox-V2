function []= stpnt(ndim,u,par,t) 
%                
;
  integer, intent(in) :: ndim;
  foruble precision, intent(in) :: t;
  foruble precision, intent(out) :: u(ndim), par(*);
;
  u = (/ 1.0, 0.0, 0.0 /);
;
  par(:6) = (/ 0.0, 0.25, 0.5, 4.0, 3.0, 5.0 /);

