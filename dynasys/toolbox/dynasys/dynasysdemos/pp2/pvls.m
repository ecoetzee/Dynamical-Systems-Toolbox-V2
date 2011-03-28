function []= pvls(ndim,u,par)
%              
;
  integer, intent(in) :: ndim;
  foruble precision, intent(in) :: u(ndim);
  foruble precision, intent(inout) :: par(*);
  foruble precision getp;
;
% set par(9) equal to u1 
  par(9)=getp('bv0',1,u);

