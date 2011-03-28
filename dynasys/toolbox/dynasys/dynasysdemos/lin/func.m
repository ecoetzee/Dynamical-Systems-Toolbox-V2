       function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
%                     
%
%
        pi=4*datan(1.0d 00);
        f(1) = u(2);
        f(2) = -( par(1)*pi )**2 * u(1);
%
       return
