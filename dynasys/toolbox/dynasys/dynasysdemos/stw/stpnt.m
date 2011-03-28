      function []= stpnt(ndim,u,par,t) 
%                      
% 
% 
% homotopy parameter :
       par(1)= 0;
%
% starting period
       period=100.;
%
% c, t:
       par(10)= 1;
       par(11)= period;
%
% eps 0, eps 1:
       par(12)= 0.5*dsqrt(5.d0)/(1+exp(0.5*period));
       par(13)= par(12);
%
%  mu 0,  mu 1:
       par(14)= 1;
       par(15)= -1;
%
% v 0(1), v 0(2):
       par(16)= 2/dsqrt(5.d0);
       par(17)= 1/dsqrt(5.d0);
%
% v 1(1), v 1(2):
       par(18)= 1;
       par(19)= 0;
%
% exact solution
       tsc=period*(t-0.5);
       e=dexp(tsc);
       u(1)=1/(1+e);
       u(2)=-0.5*e/(1+e);
%
      return
