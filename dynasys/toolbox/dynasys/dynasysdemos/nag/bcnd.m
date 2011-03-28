      function []= bcnd(ndim,par,icp,nbc,u0,u1,fb,ijac,dbc)
%                    
%
%
% local
      parameter (ndm=2);
%
      v0(1)=u0(1) - par(3)*par(7);
      v0(2)=u0(2) - par(3)*par(8);
      v1(1)=u1(1) - par(4)*par(9);
      v1(2)=u1(2) - par(4)*par(10);
%
      call ffff(ndm,v0,icp,par,1,g0,dgdu0);
      call ffff(ndm,v1,icp,par,1,g1,dgdu1);
%
      fb(1)= dgdu0(1,1)*par(7) + dgdu0(1,2)*par(8) - par(5)*par(7);
      fb(2)= dgdu0(2,1)*par(7) + dgdu0(2,2)*par(8) - par(5)*par(8);
      fb(3)= dgdu1(1,1)*par(9) + dgdu1(1,2)*par(10)- par(6)*par(9);
      fb(4)= dgdu1(2,1)*par(9) + dgdu1(2,2)*par(10)- par(6)*par(10);
      fb(5)= par(7)**2 + par(8)**2 -1;
      fb(6)= par(9)**2 + par(10)**2 -1;
      fb(7)= g0(1);
      fb(8)= g0(2);
      fb(9)= g1(1);
      fb(10)=g1(2);
%
      return
