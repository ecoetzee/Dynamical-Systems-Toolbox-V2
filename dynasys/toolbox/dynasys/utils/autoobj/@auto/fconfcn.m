function obj=fconfcn(obj,dat)

objs=obj.s;
objc=obj.c;
objf8=obj.f8;

% This program converts a data file into a labeled auto solution. A fort.8
% file
%
      niap=10;
      nparx=36;
%
% initialise
%
      iap=zeros(niap,1);           
      par=zeros(nparx,1);
%
% obtain constants from constants object
%
      iap(1)=objc.Ndim;
      iap(2)=objc.Ips;
      iap(5)=objc.Ntst;
      iap(6)=objc.Ncol;
      iap(7)=objc.Iad;
      iap(9)=objc.Isp;
      iap(10)=objc.Isw;

      ndim=iap(1) ;
      ntst=iap(5);
      ncol=iap(6);
%
      [rows,cols]=size(dat);
      ndx=rows;
      nold=ndx-1;
      dtm=zeros(ndx,1);
      ups=zeros(ndx,ndim*ncol);
      vps=zeros(ndx,ndim*ncol);
      
      tm=dat(:,1);
      ups(:,1:ndim)=dat(:,2:end);
      
      period=tm(end)-tm(1); 
      for i=rows:-1:1         
         tm(i)=(tm(i)-tm(1))/period;
      end
      for i=1:rows-1
         dtm(i)=tm(i+1)-tm(i);      
      end
      
           [iap,nold,ncold,ntst,ncol,tm,dtm,ndx,ups,vps]=...
      adapt(iap,nold,1,ntst,ncol,tm,dtm,ndx,ups,vps);
%
        icp(1)=1;
        rldot=1;
        
        npar=length(objs.Par0);
        par(1:npar)=objs.Par0;
        par(11)=period;
                
        objf8=wrtbv8(objf8,iap,par,icp,rldot,ndx,ups,vps,tm,dtm,nparx);
        
        obj.f8=objf8;
        
        if isempty(obj.s.U0)
            obj.s.U0=zeros(1,ndim);
        end
%
%--------------------------------------------------------------------------
    function [iap,nold,ncold,nnew,ncnew,tm,dtm,ndx,ups,vps]=...
        adapt(iap,nold,ncold,nnew,ncnew,tm,dtm,ndx,ups,vps)

% adapts the distribution of the mesh points so that the increase of the
% monotone function eqdf becomes approximately equidistributed over the
% intervals. the functions ups and vps are interpolated on new mesh.
%
%
       ndim=iap(1);
       ips=iap(2);
       isw=iap(10);
%
       noldp1=nold+1;
       nnewp1=nnew+1;
       nrwnew=ndim*ncnew;
%
       tint=zeros(nnewp1,1);
       uint=zeros(nnewp1,nrwnew);
       tm2=zeros(nnewp1,1);
       itm=zeros(nnewp1,1);
%
% for periodic boundary conditions extrapolate by periodicity.
%
       if(ips==2 && abs(isw)~=2)
         iper=1;
       else
         iper=0;
       end
%
% generate the new mesh :
%
             [ndim,ndx,ups,nold,ncold,tm,dtm,nnew,tint,iper]=...
       newmsh(ndim,ndx,ups,nold,ncold,tm,dtm,nnew,tint,iper);
   
%
% replace ups by its interpolant on the new mesh :
%             ndim,n,nc,tm,ndx,ups,n1,nc1,tm1,ups1,tm2,itm1
             [ndim,noldp1,ncold,tm,ndx,ups,nnewp1,ncnew,tint,uint,tm2,itm]=...
       interp(ndim,noldp1,ncold,tm,ndx,ups,nnewp1,ncnew,tint,uint,tm2,itm);
       for j=1:nnewp1
         for i=1:nrwnew
           ups(j,i)=uint(j,i);
         end
       end
%
% replace vps by its interpolant on the new mesh :
%
             [ndim,noldp1,ncold,tm,ndx,vps,nnewp1,ncnew,tint,uint,tm2,itm]=...
       interp(ndim,noldp1,ncold,tm,ndx,vps,nnewp1,ncnew,tint,uint,tm2,itm);
       
       for j=1:nnewp1
         for i=1:nrwnew
           vps(j,i)=uint(j,i);
         end
       end
%
% replace old mesh :
%
       tm(1)=0;
       for j=1:nnew
         dtm(j)=tint(j+1)-tint(j);
         tm(j+1)=tint(j+1);
       end
%
%--------------------------------------------------------------------------
    function [ndim,n,nc,tm,ndx,ups,n1,nc1,tm1,ups1,tm2,itm1]=...
       interp(ndim,n,nc,tm,ndx,ups,n1,nc1,tm1,ups1,tm2,itm1)
%
% finds interpolant (tm(.) , ups(.) ) on new mesh tm1.
%
% local
      w=zeros(nc+1,1);
      x=zeros(nc+1,1);
%
       ncp1=nc+1;
       n1m1=n1-1;    
%
       for i=1:nc1
         ri=i-1;
         d=ri/nc1;
         for j1=1:n1m1
           tm2(j1)=tm1(j1)+d*( tm1(j1+1)-tm1(j1) );
         end
         
         [n,tm,n1m1,tm2,itm1]=ordr(n,tm,n1m1,tm2,itm1);
         
         for j1=1:n1m1
           j=itm1(j1);
           z=tm2(j1);
           d=( tm(j+1)-tm(j) )/nc;
           for l=1:ncp1
             x(l)=tm(j)+(l-1)*d;
           end
           
           [ncp1,z,x,w]=intwts(ncp1,z,x,w);
           
           for k=1:ndim
             k1=(i-1)*ndim+k;
             ups1(j1,k1)=w(ncp1)*ups(j+1,k);
             for l=1:nc
               l1=k+(l-1)*ndim;
               ups1(j1,k1)=ups1(j1,k1)+w(l)*ups(j,l1);
             end
           end
         end
       end
%
       for i=1:ndim
         ups1(n1,i)=ups(n,i);
       end
%
%--------------------------------------------------------------------------
    function  [ndim,ndx,ups,nold,ncold,tmold,dtmold,nnew,tmnew,iper]=...
        newmsh(ndim,ndx,ups,nold,ncold,tmold,dtmold,nnew,tmnew,iper)
     
% local
      eqf=[];
      uneq=[];
      ial=[];
      ial=zeros(nnew+1,1);
      uneq=zeros(nnew+1,1);
      eqf=zeros(nold+1,1);
%
% put the values of the monotonely increasing function eqdf in eqf.
%
           [nold,ndim,ncold,dtmold,ndx,ups,eqf,iper]=...
       eqdf(nold,ndim,ncold,dtmold,ndx,ups,eqf,iper);
%
% uniformly divide the range of eqdf :
%
       noldp1=nold+1;
       nnewp1=nnew+1;
       dal=eqf(noldp1)/nnew;
       for j=1:nnewp1
         uneq(j)=(j-1)*dal;
       end
%
       [noldp1,eqf,nnewp1,uneq,ial]=ordr(noldp1,eqf,nnewp1,uneq,ial);
%
% generate the new mesh in tmnew :
%
       for j1=1:nnew
         j=ial(j1);
         x=(uneq(j1)-eqf(j))/(eqf(j+1)-eqf(j));
         tmnew(j1)=(1.d0-x)*tmold(j)+x*tmold(j+1);
       end
%
% assign tmnew(nnewp1) explicitly because of loss of precision
% problems when eqf(noldp1) and eqf(nold) are very close
%
       tmnew(nnewp1)=tmold(noldp1);
%
%--------------------------------------------------------------------------
    function  [n,tm,n1,tm1,itm1]=ordr(n,tm,n1,tm1,itm1)

% tm and tm1 are two ascending arrays with values in [0,1]. on exit the
% value of itm1( i ) specifies the index of the tm-interval in which
% tm1(i) lies.
%
       k0=2;
       for j1=1:n1
         k1=k0;
         for j=k0:n
           k1=j;
           if(tm1(j1)<tm(j))
               break
           end
         end
         itm1(j1)=k1-1;
         k0=k1;
       end
%
%--------------------------------------------------------------------------
    function  [n,z,x,wts]=intwts(n,z,x,wts)

% generates weights for lagrange interpolation.
%
       for ib=1:n
         p=1;
         denom=1;
         for k=1:n
           if(k~=ib)
             p=p*( z-x(k) );
             denom=denom*( x(ib)-x(k) );
           end
         end
         wts(ib)=p/denom;
       end
%
%--------------------------------------------------------------------------
    function  [ntst,ndim,ncol,dtm,ndx,ups,eqf,iper]=...
          eqdf(ntst,ndim,ncol,dtm,ndx,ups,eqf,iper)

%
      hmach=1.0d-7;
      rsmall=1.0d-30;
      rlarge=1.0d+30;
%
% local
      wh=zeros(ncol+1,1);
      hd=zeros(ntst+1,ncol*ndim);
%
% compute approximation to ncol-th derivative :
       [ncol,wh]=cntdif(ncol,wh);
%
       small=true;
       for j=1:ntst
         jp1=j+1;
         sc=1/dtm(j)^ncol;
         for i=1:ndim
           hd(j,i)=wh(ncol+1)*ups(jp1,i);
           for k=1:ncol
             k1=i+(k-1)*ndim;
             hd(j,i)=hd(j,i)+wh(k)*ups(j,k1);
           end
           hd(j,i)=sc*hd(j,i);
           if(abs(hd(j,i))>hmach)
               small=false;
           end
         end
       end
%
% take care of "small derivative" case.
%
       if(small)
         for i=1:ntst+1
           eqf(i)=i-1;
         end
         return;
       end
%
       if(iper~=1)
           gotoFlag=1;
       else
           gotoFlag=0;
       end
%
% extend by periodicity :
%
       if gotoFlag==0
         for i=1:ndim
           hd(ntst+1,i)=hd(1,i);
         end
         dtm(ntst+1)=dtm(1);
         gotoFlag=2;
       end
%
% extend by extrapolation :
%
       if gotoFlag==1     
         for i=1:ndim
           hd(ntst+1,i)=2*hd(ntst,i)-hd(ntst-1,i);
         end
         dtm(ntst+1)=dtm(ntst);
         gotoFlag=2;
       end
%
% compute approximation to (ncol+1)-st derivative :
%
       if gotoFlag==2
         for j=1:ntst
           jp1=j+1;
           dtav=0.5*(dtm(j)+dtm(j+1));
           sc=1/dtav;
           for i=1:ndim
             hd(j,i)=sc*( hd(jp1,i)-hd(j,i) );
           end
         end
       end
%
% define the equidistribution function :
%
       pwr=1/(ncol+1);
       eqf(1)=0;
       for j=1:ntst
         e=0;
         for i=1:ndim
           e=e+abs( hd(j,i) )^pwr;
         end
         eqf(j+1)=eqf(j)+dtm(j)*e;
       end
%
%--------------------------------------------------------------------------
    function [n,d]=cntdif(n,d)
%
% generates the coefficients of the central difference formula for
% nth derivative at uniformly spaced points
%
      d=[];
%
       d(1)=1;
       if(n==0)
           return
       end
%
       for i=1:n
         d(i+1)=0;
         for k=1:i
           k1=i+2-k;
           d(k1)=d(k1-1)-d(k1);
         end
         d(1)=-d(1);
       end
%
% scale to [0,1]  :
%
       sc=n^n;
       np1=n+1;
       for i=1:np1
         d(i)=sc*d(i);
       end
%
%--------------------------------------------------------------------------
    function objf8=wrtbv8(objf8,iap,par,icp,rldot,ndx,ups,udotps,tm,dtm,nparx)
%
%
       ndim=iap(1);
       ntst=iap(5);
       ncol=iap(6);
       isw=iap(10);
       itp=9;
       nfpr=1;
       ibr=1;
       ntot=1;
       lab=1;
%
% write information identifying the solution :
%
       ntpl=ncol*ntst+1;
       nar=ndim+1;
       nrd=floor(2+ndim/7+(ndim-1)/7);
       nrowpr=floor(nrd*(ncol*ntst+1) + (nfpr-1)/7+1 + (nparx-1)/7+1+ (nfpr-1)/20+1);
%
       mtot=mod(ntot,10000);
%
       objf8.Ibr=ibr;
       objf8.Mtot=mtot;
       objf8.Itp=itp;
       objf8.Lab=lab;
       objf8.Nfpr=nfpr;
       objf8.Isw=isw;
       objf8.Ntpl=ntpl;
       objf8.Nar=nar;
       objf8.Nrowpr=nrowpr;
       objf8.Ntst=ntst;
       objf8.Ncol=ncol;
       objf8.Nparx=nparx;
%
% write the entire solution on unit 8 :
%
       t_=[];
       ups_=[];
       
       for j=1:ntst
         rn=1/ncol;
         for i=1:ncol
           k1=(i-1)*ndim+1;
           k2=i*ndim;
           t=tm(j)+(i-1)*rn*dtm(j);
           t_=[t_;t];
           ups_=[ups_;ups(j,k1:k2)];
         end
       end
       t_=[t_;tm(ntst+1)];
       ups_=[ups_;ups(ntst+1,1:ndim)];       

       objf8.Tm=t_;
       objf8.Ups(:,:,1)=ups_;
%
% write the free parameter indices:
%
       objf8.Ifpr=icp;
%
% write the direction of the branch:
%
       objf8.Rldot=ones(1,nfpr).*rldot;
       
       udotps_=[];
       
       for j=1:ntst
         for i=1:ncol
           k1=(i-1)*ndim+1;
           k2=i*ndim;
           udotps_=[udotps_;udotps(j,k1:k2)];
         end
       end
       udotps_=[udotps_;udotps(ntst+1,1:ndim)];

       objf8.Udotps(:,:,1)=udotps_;
%
% write the parameter values.
%
       objf8.Par=reshape(par,1,length(par));
       


