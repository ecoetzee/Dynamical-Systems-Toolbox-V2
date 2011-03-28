      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
%                    
%
%
       ndim2=ndim/2;
       for i=1,ndim2;
         i1=2*(i-1)+1;
         i2=i1+1;
         e=fexp(u(i1));
         f(i1)=u(i2);
         f(i2)=-par(1)*e;
       endfor;
%
      return
