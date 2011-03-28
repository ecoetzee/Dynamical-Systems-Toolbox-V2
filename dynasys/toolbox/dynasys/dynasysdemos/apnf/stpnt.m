function []= stpnt(ndim,u,par)
%               
;
  integer, intent(in) :: ndim;
  foruble precision, intent(out) :: u(ndim), par(*);
;
  par(1:3) = (/ 1.0d0, 2.0d0, 0.0d0 /);
;
  u(1) = 1.0;
   
