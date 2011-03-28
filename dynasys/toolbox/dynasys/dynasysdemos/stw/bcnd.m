      function []= bcnd(ndim,par,icp,nbc,u0,u1,fb,ijac,dbc)
%                    
%
%
% local
%
      v0(1)=u0(1) - par(12)*par(16);
      v0(2)=u0(2) - par(12)*par(17);
      v1(1)=u1(1) - par(13)*par(18);
      v1(2)=u1(2) - par(13)*par(19);
%
      call ffff(ndim,v0,icp,par,1,g0,dgdu0);
      call ffff(ndim,v1,icp,par,1,g1,dgdu1);
%
% define eigenvalues and eigenvectors at t=0:
      fb(1)= dgdu0(1,1)*par(16) + dgdu0(1,2)*par(17) - par(14)*par(16);
      fb(2)= dgdu0(2,1)*par(16) + dgdu0(2,2)*par(17) - par(14)*par(17);
%
% define eigenvalues and eigenvectors at t=1:
      fb(3)= dgdu1(1,1)*par(18) + dgdu1(1,2)*par(19) - par(15)*par(18);
      fb(4)= dgdu1(2,1)*par(18) + dgdu1(2,2)*par(19) - par(15)*par(19);
%
% normalize the eigenvectors:
      fb(5)= par(16)**2 + par(17)**2 -1;
      fb(6)= par(18)**2 + par(19)**2 -1;
%
% boundary condition at t=0:
      fb(7)= g0(1);
      fb(8)= g0(2);
%
% boundary condition at t=1:
      fb(9)= g1(1);
      fb(10)=g1(2);
%
      return
