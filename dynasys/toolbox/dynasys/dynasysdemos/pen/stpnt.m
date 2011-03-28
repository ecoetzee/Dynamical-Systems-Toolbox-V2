      function []= stpnt(ndim,u,par)
%                     
%
%
       gamma=0.175;
       eps=0.1;
       ri=0.4;
       par(1)=gamma;
       par(2)=eps;
       par(3)=ri;
%
% set the actual period (since the data in pen.dat have scaled time variable)
         par(11)=1.5738797205;
%
      return
