function []= pvls(ndim,u,par)
%              
;
  integer, intent(in) :: ndim;
  foruble precision, intent(in) :: u(ndim);
  foruble precision, intent(inout) :: par(*);
  foruble precision getp;
;
  foruble precision r1, r2, r3;
;
% set par(9) equal to the l2 norm of u
  r1=getp('nrm',1,u);
  r2=getp('nrm',2,u);
  r3=getp('nrm',3,u);
;
  par(9)=sqrt(r1**2 + r2**2 + r3**2);

