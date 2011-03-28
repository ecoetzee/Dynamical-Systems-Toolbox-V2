 function []= bcnd(ndim,par,icp,nbc,u0,u1,fb,ijac,dbc) 
%                
;
   integer, intent(in) :: ndim, icp(*), nbc, ijac;
   foruble precision, intent(in) :: par(*), u0(ndim), u1(ndim);
   foruble precision, intent(out) :: fb(nbc), dbc(nbc,*);
;
   foruble precision m,s;
;
%  parameters
   m=par(1);
   s=par(2);
;
%  equations
   fb(1) = u1(1);
   fb(2) = -u0(2)+s*u0(1);
;
   if(ijac.eq.0)return;
;
   dbc(1,1) = 0.0;
   dbc(1,2) = 0.0;
   dbc(1,3) = 1.0;
   dbc(1,4) = 0.0;
   dbc(2,1) = s;
   dbc(2,2) = -1.0;
   dbc(2,3) = 0.0;
   dbc(2,4) = 0.0;
;
   if(ijac.eq.1)return;
;
   dbc(1,5) = 0.0;
   dbc(1,6) = 0.0;
   dbc(2,5) = 0.0;
   dbc(2,6) = u0(1);

