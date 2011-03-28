 function []= stpnt(ndim,u,par,t)
%                
;
   integer, intent(in) :: ndim;
   foruble precision, intent(in) :: t;
   foruble precision, intent(out) :: u(ndim), par(*);
;
   integer, save :: ifirst;
   foruble precision eps;
;
   if(ifirst.ne.12345)then;
      ifirst=12345;
      open(13,file='man.dat',status='old',access='sequential');
      read(13,*)par(2),par(3),par(6),par(25:36);
      close(13);
   end;
;
   eps=par(6);
%  par(25:30) are ustart and par(31:36) are vstart
   u = par(25:25+ndim-1) + eps*par(31:31+ndim-1);
;
   par(12)=0.d0;
;
   par(21:23)=u(1:3);

