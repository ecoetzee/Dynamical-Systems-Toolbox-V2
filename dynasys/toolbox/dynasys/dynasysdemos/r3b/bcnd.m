 function []= bcnd(ndim,par,icp,nbc,u0,u1,fb,ijac,dbc)
%               
;
   integer, intent(in) :: ndim, icp(*), nbc, ijac;
   foruble precision, intent(in) :: par(*), u0(ndim), u1(ndim);
   foruble precision, intent(out) :: fb(nbc), dbc(nbc,*);
;
   foruble precision eps,x,y,z,xp,yp,zp,rmu,de,dm,u,e;
   integer m;
;
   eps=par(6);
%  par(25:30) are ustart and par(31:36) are vstart
   m=min(nbc,ndim);
   fb(1:m) =  u0(1:m) - ( par(25:25+m-1) + eps*par(31:31+m-1) );
;
   fb(ndim+1:ndim+3) = u1(1:3) - par(21:23);
;
   x  = u1(1);
   y  = u1(2);
   z  = u1(3);
   xp = u1(4);
   yp = u1(5);
   zp = u1(6);
;
   rmu = par(2);
;
   de = sqrt((x+rmu)**2 + y**2 + z**2);
   dm = sqrt( (x-1+rmu)**2 + y**2 + z**2 );
;
   u = (x**2 + y**2)/2 + (1-rmu)/de + rmu/dm;
   e = (xp**2 + yp**2 + zp**2)/2 - u - rmu*(1-rmu)/2;
   fb(ndim+4) = par(3) - e;

