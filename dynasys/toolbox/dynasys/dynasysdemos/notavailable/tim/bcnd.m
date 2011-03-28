      function []= bcnd(ndim,par,icp,nbc,u0,u1,fb,ijac,dbc)
%                    
%
%
       ndim2=ndim/2;
       for i=1,ndim2;
         i1=2*(i-1)+1;
         i2=i1+1;
         fb(i1)=u0(i1);
         fb(i2)=u1(i1);
       endfor;
%
      return
