      function []= setbc(ne,par,u0,u1) 
%                      
% set the boundary values (to be kept fixed in time)
%
%
        a=par(1);
        b=par(2);
%
        u0(1)=a;
        u0(2)=b/a;
        u1(1)=a;
        u1(2)=b/a;
%
      return 
