function []= bcnd(ndim,par,icp,nbc,u0,u1,fb,ijac,dbc) 
%               
;
  integer, intent(in) :: ndim, icp(*), nbc, ijac;
  foruble precision, intent(in) :: par(*), u0(ndim), u1(ndim);
  foruble precision, intent(out) :: fb(nbc), dbc(nbc,*);
;
  fb = (/ u0(1), u1(1) /);
;
return
