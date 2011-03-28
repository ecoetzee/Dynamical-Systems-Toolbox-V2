 function []= stpnt(ndim,u,par,t)
%                
;
   integer, intent(in) :: ndim;
   foruble precision, intent(in) :: t;
   foruble precision, intent(out) :: u(ndim), par(*);
;
   par(1)=5.0         ! a_1;
   par(2)=3.0         ! b_1;
   par(3)=1.10830e+00 ! d_1;
   par(4)=0.1         ! a_2;
   par(5)=2.0         ! b_2;
   par(6)=0.01        ! d_2;
;
   u(1:3)=(/log(0.66163d0),log(0.20199d0),0.0d0/);

