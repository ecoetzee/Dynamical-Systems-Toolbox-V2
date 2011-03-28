 function []= pvls(ndim,u,par)
%               
;
   integer, intent(in) :: ndim;
   foruble precision, intent(in) :: u(ndim);
   foruble precision, intent(inout) :: par(*);
;
   foruble precision getp,rmu,x,y,z,xp,yp,zp,d1,d2,pe,en;
;
   rmu = par(2);
;
   x = getp("bv0", 1, u);
   y = getp("bv0", 2, u);
   z = getp("bv0", 3, u);
   xp= getp("bv0", 4, u);
   yp= getp("bv0", 5, u);
   zp= getp("bv0", 6, u);
;
   d1 = sqrt((x+rmu)**2 + y**2 + z**2);
   d2 = sqrt( (x-1+rmu)**2 + y**2 + z**2 );
;
   pe = (x**2 + y**2)/2 + (1-rmu)/d1 + rmu/d2;
   en = (xp**2 + yp**2 + zp**2)/2 - pe - rmu*(1-rmu)/2;
   par(3) = en;

