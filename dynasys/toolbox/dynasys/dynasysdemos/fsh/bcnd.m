      function []= bcnd(ndim,par,icp,nbc,u0,u1,fb,ijac,dbc)
%                    
%
% local
      parameter (ndm=2);
%
      v1(1)=u1(1) + par(4)*par(13);
      v1(2)=u1(2) + par(4)*par(14);
%
      call ffff(ndm,v1,icp,par,1,g1,dgdu1);
%
      fb(1)= dgdu1(1,1)*par(13) + dgdu1(1,2)*par(14)- par(12)*par(13);
      fb(2)= dgdu1(2,1)*par(13) + dgdu1(2,2)*par(14)- par(12)*par(14);
      fb(3)= par(13)**2 + par(14)**2 -1;
      fb(4)= g1(1);
      fb(5)= g1(2);
%
      return
