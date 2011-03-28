      function []= stpnt(ndim,u,par,t) 
%                      
% 
%
       par(1)=28.;
       par(2)=8.d0/3.d0;
       par(3)=10.;
%
       rad=5.0;
       h=0.1;
       period=1e-5;
       rlength=0.;
%
       par(4)=rad;
       par(5)=h;
%
       call eigv(ndim,par,v1,v2);
%
% set initial approximate solution (for small period)
       pi=4*datan(1.d0);
       theta=2*pi*h;
       cs=dcos(theta);
       sn=dsin(theta);
%
       u(1)= rad * ( cs*v1(1) + sn*v2(1) );
       u(2)= rad * ( cs*v1(2) + sn*v2(2) );
       u(3)= rad * ( cs*v1(3) + sn*v2(3) );
%
       par(6) = u(1);
       par(7) = u(2);
       par(8) = u(3);
       par(9) = sqrt(u(1)**2 + u(2)**2 + u(3)**2);
       par(11)=period;
       par(12)=rlength;
%
      return 
