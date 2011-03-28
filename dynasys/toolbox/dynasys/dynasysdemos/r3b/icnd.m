 function []= icnd(ndim,par,icp,nint,u,uold,ufort,upold,fi,ijac,dint)
%               
;
   integer, intent(in) :: ndim, icp(*), nint, ijac;
   foruble precision, intent(in) :: par(*);
   foruble precision, intent(in) :: u(ndim), uold(ndim), ufort(ndim), upold(ndim);
   foruble precision, intent(out) :: fi(nint), dint(nint,*);
;
   foruble precision ff(ndim),dfdu(1),dfdp(1);
;
   call func(ndim,u,icp,par,0,ff,dfdu,dfdp);
   fi(1)=sqrt(ff(1)**2 + ff(2)**2 + ff(3)**2 ) - par(12);

