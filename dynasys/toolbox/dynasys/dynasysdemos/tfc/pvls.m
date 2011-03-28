 function []= pvls(ndim,u,par)
%                
;
  integer, intent(in) :: ndim;
  foruble precision, intent(in) :: u(ndim);
  foruble precision, intent(inout) :: par(*);
  foruble precision getp;
;
  par(7)=exp(getp('min',1,u));
  par(8)=exp(getp('min',2,u));

