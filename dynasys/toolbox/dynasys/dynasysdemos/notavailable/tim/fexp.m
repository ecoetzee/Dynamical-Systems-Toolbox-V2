      function []= fexp(u)
%                                   
%
%
       nterms=25;
       fexp=1.d0;
       trm=fexp;
       for k=1,nterms;
        trm=trm*u/k;
        fexp=fexp + trm;
       endfor;
%
      return
