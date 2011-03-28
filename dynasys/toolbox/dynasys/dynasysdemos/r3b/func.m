 function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                
;
   integer, intent(in) :: ndim, ijac, icp(*);
   foruble precision, intent(in) :: u(ndim), par(*);
   foruble precision, intent(out) :: f(ndim), dfdu(ndim,*), dfdp(ndim,*);
;
   foruble precision x,y,z,xp,yp,zp,rmu,t,de,dm,rmc,de3,dm3;
;
   x  = u(1);
   y  = u(2);
   z  = u(3);
   xp = u(4);
   yp = u(5);
   zp = u(6);
;
   rmu = par(2);
   t   = par(11);
;
   de  = sqrt((x+rmu)**2 + y**2 + z**2);
   dm  = sqrt( (x-1+rmu)**2 + y**2 + z**2 );
   rmc = 1 - rmu;
   de3 = 1./de**3;
   dm3 = 1./dm**3;
;
   f(1)= xp;
   f(2)= yp;
   f(3)= zp;
   f(4)= 2*yp + x - rmc*de3*(x+rmu) - rmu*dm3*(x-1+rmu);
   f(5)=-2*xp + y - rmc*de3*y       - rmu*dm3*y;
   f(6)=          - rmc*de3*z       - rmu*dm3*z;
;
   f = t*f;

