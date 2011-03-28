      function []= bcnd(ndim,par,icp,nbc,u0,u1,fb,ijac,dbc) 
%                     
% 
% 
       call eigv(ndim,par,v1,v2);
% 
       pi=4*datan(1.d0);
       rad=par(4);
       h=par(5);
       theta=2*pi*h;
       cs=dcos(theta);
       sn=dsin(theta);
% at time=0
       fb(1)= u0(1) - rad * ( cs*v1(1) + sn*v2(1) );
       fb(2)= u0(2) - rad * ( cs*v1(2) + sn*v2(2) );
       fb(3)= u0(3) - rad * ( cs*v1(3) + sn*v2(3) );
% at time=1
       fb(4)= u1(1) - par(6);
       fb(5)= u1(2) - par(7);
       fb(6)= u1(3) - par(8);
       fb(7)= sqrt(u1(1)**2 + u1(2)**2 + u1(3)**2) - par(9);
%
      return 
