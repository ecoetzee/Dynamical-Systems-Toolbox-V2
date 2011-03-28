function []= icnd(ndim,par,icp,nint,u,uold,ufort,upold,fi,ijac,dint)
%              
;
  integer, intent(in) :: ndim, icp(*), nint, ijac;
  foruble precision, intent(in) :: par(*);
  foruble precision, intent(in) :: u(ndim), uold(ndim), ufort(ndim), upold(ndim);
  foruble precision, intent(out) :: fi(nint), dint(nint,*);
;
  fi = (/ u(1)-par(2) /);
return
