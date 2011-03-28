function [ne,u,par,f]=ff(ne,u,par,f);
%     ---------- --
%     Define the nonlinear term
%
clear global; clear functions;


par_shape=size(par);par=reshape(par,1,[]);
%
x=u(1);
y=u(2);
a=par(1);
b=par(2);
%
f(1)= x.^2.*y -(b+1).*x + a;
f(2)=-x.^2.*y + b.*x;
%
return;
end
%
function [ne,dc,par]=setdc(ne,dc,par);
%     ---------- -----
%     Set the diffusion constants (constant, or in terms of PAR)
%

par_shape=size(par);par=reshape(par,1,[]);
%
dc(1)=par(3)./par(5).^2;
dc(2)=par(4)./par(5).^2;
%
par_shape=zeros(par_shape);par_shape(:)=par(1:numel(par_shape));par=par_shape;
return;
end
%
function [ne,par,u0,u1]=setbc(ne,par,u0,u1);
%     ---------- -----
% Set the boundary values (to be kept fixed in time)
%

par_shape=size(par);par=reshape(par,1,[]);
%
a=par(1);
b=par(2);
%
u0(1)=a;
u0(2)=b./a;
u1(1)=a;
u1(2)=b./a;
%
%
par_shape=zeros(par_shape);par_shape(:)=par(1:numel(par_shape));par=par_shape;
return;
end
%
function [ndim,u,par]=stpnt(ndim,u,par);
%     ---------- -----
% Define the starting stationary solution on the spatial mesh
%
persistent ne nn nx ; 

brf
if isempty(nn), nn=nx-1 ; end;
% common :: ;
global blppde_1; if isempty(blppde_1), blppde_1=zeros(nn,nn); end;
global blppde_2; if isempty(blppde_2), blppde_2=zeros(nn,nn); end;
global blppde_3; if isempty(blppde_3), blppde_3=zeros(nn,nn); end;
global blppde_4; if isempty(blppde_4), blppde_4=zeros(nn,nn); end;
% common ::;
%% common /blppde/ d0(nn,nn),d2(nn,nn),di(nn,nn),dd(nn,nn),;
%% common /blppde/ blppde_1(nn,nn),blppde_2(nn,nn),blppde_3(nn,nn),blppde_4(nn,nn),;
.*          ri(nn,nn);
u_orig=u;u_shape=[nn,ne];u=reshape([u_orig(1:min(prod(u_shape),numel(u_orig))),zeros(1,max(0,prod(u_shape)-numel(u_orig)))],u_shape);
par_shape=size(par);par=reshape(par,1,[]);
%
% Set the parameter values
a=2.0d0;
b=5.45d0;
dx=0.008d0;
dy=0.004d0;
rl=0.4;
%
par(1)=a;
par(2)=b;
par(3)=dx;
par(4)=dy;
par(5)=rl;
%
% Set the starting solution at space-points i/NX, i=1,2,...,NX-1
for  i=1:nx-1;
u(i,1)=a;
u(i,2)=b./a;
% 1      continue;
%
u_orig(1:prod(u_shape))=u;u=u_orig;
par_shape=zeros(par_shape);par_shape(:)=par(1:numel(par_shape));par=par_shape;
return;
end  i=nx-1+1
%----------------------------------------------------------------------
%----------------------------------------------------------------------
%                Problem-independent subroutines
%----------------------------------------------------------------------
%----------------------------------------------------------------------
%
function [ndim,u,icp,par,ijac,f,dfdu,dfdp]=func(ndim,u,icp,par,ijac,f,dfdu,dfdp);
%     ---------- ----
%
persistent dc f0 f1 fw ne nn nx u0 u1 v w ; 

brf
if isempty(nn), nn=nx-1 ; end;
% common :: ;
global blppde_1; if isempty(blppde_1), blppde_1=zeros(nn,nn); end;
global blppde_2; if isempty(blppde_2), blppde_2=zeros(nn,nn); end;
global blppde_3; if isempty(blppde_3), blppde_3=zeros(nn,nn); end;
global blppde_4; if isempty(blppde_4), blppde_4=zeros(nn,nn); end;
% common ::;
%% common /blppde/ d0(nn,nn),d2(nn,nn),di(nn,nn),dd(nn,nn),;
%% common /blppde/ blppde_1(nn,nn),blppde_2(nn,nn),blppde_3(nn,nn),blppde_4(nn,nn),;
.*          ri(nn,nn);
% common :: ;
global blppfr_1; if isempty(blppfr_1), blppfr_1=0; end;
%% common /blppfr/ ifrst;
%% common /blppfr/ blppfr_1;
u_orig=u;u_shape=[nn,ne];u=reshape([u_orig(1:min(prod(u_shape),numel(u_orig))),zeros(1,max(0,prod(u_shape)-numel(u_orig)))],u_shape);
f_orig=f;f_shape=[nn,ne];f=reshape([f_orig(1:min(prod(f_shape),numel(f_orig))),zeros(1,max(0,prod(f_shape)-numel(f_orig)))],f_shape);
par_shape=size(par);par=reshape(par,1,[]);
if isempty(w), w=zeros(1,ne); end;
if isempty(fw), fw=zeros(1,ne); end;
if isempty(dc), dc=zeros(1,ne); end;
if isempty(u0), u0=zeros(1,ne); end;
if isempty(u1), u1=zeros(1,ne); end;
if isempty(f0), f0=zeros(1,ne); end;
if isempty(f1), f1=zeros(1,ne); end;
if isempty(v), v=zeros(nn,ne); end;
%
% Problem-independent initialization :
if(blppfr_1~=1234)
[par]=gencf(par);
blppfr_1=1234;
end;
%
[ne,dc,par]=setdc(ne,dc,par);
[ne,par,u0,u1]=setbc(ne,par,u0,u1);
[ne,u0,par,f0]=ff(ne,u0,par,f0);
[ne,u1,par,f1]=ff(ne,u1,par,f1);
%
for  i=1:nn;
for  j=1:ne;
v(i,j)= blppde_3(i, 1).*( dc(j).*nx.^2.*u0(j) + f0(j)./12 );
.*            + blppde_3(i,nn).*( dc(j).*nx.^2.*u1(j) + f1(j)./12 );
% 1        continue;
% 2      continue;
%
for  i=1:nn;
for  k=1:ne;
w(k)=u(i,k);
% 3        continue;
[ne,w,par,fw]=ff(ne,w,par,fw);
for  j=1:ne;
f(i,j)=v(i,j) + fw(j);
for  k=1:nn;
f(i,j)=f(i,j)+dc(j).*blppde_4(i,k).*u(k,j);
% 4          continue;
% 5        continue;
% 6      continue;
%
u_orig(1:prod(u_shape))=u;u=u_orig;
f_orig(1:prod(f_shape))=f;f=f_orig;
par_shape=zeros(par_shape);par_shape(:)=par(1:numel(par_shape));par=par_shape;
return;
end  k=nn+1
%;  j=ne+1;  k=ne+1;  i=nn+1;  j=ne+1;  i=nn+1;
function [par]=gencf(par);
%     ---------- -----
%
persistent ic ir ne nn nx ; 
ri=[];det=[];
brf
if isempty(nn), nn=nx-1 ; end;
% common :: ;
global blppde_1; if isempty(blppde_1), blppde_1=zeros(nn,nn); end;
global blppde_2; if isempty(blppde_2), blppde_2=zeros(nn,nn); end;
global blppde_3; if isempty(blppde_3), blppde_3=zeros(nn,nn); end;
global blppde_4; if isempty(blppde_4), blppde_4=zeros(nn,nn); end;
% common ::;
%% common /blppde/ d0(nn,nn),d2(nn,nn),di(nn,nn),dd(nn,nn),;
%% common /blppde/ blppde_1(nn,nn),blppde_2(nn,nn),blppde_3(nn,nn),blppde_4(nn,nn),;
.*          ri(nn,nn);
if isempty(ir), ir=zeros(1,nn); end;
if isempty(ic), ic=zeros(1,nn); end;
par_shape=size(par);par=reshape(par,1,[]);
%
for  i=1:nn;
for  j=1:nn;
blppde_1(i,j)=0;
blppde_2(i,j)=0;
blppde_3(i,j)=0;
blppde_4(i,j)=0;
ri(i,j)=0;
% 1      continue;
blppde_1(i,i)=10.0d0./12.0d0;
blppde_2(i,i)=-2.*nx.^2;
ri(i,i)=1;
% 2    continue;
%
for  i=1:nn-1;
blppde_1(i+1,i)=1.0d0./12.0d0;
blppde_1(i,i+1)=1.0d0./12.0d0;
blppde_2(i+1,i)=nx.^2;
blppde_2(i,i+1)=nx.^2;
% 3    continue;
%
[dumvar1,nn,nn,blppde_1,nn,nn,blppde_3,nn,ri,ir,ic,det]=ge(0,nn,nn,blppde_1,nn,nn,blppde_3,nn,ri,ir,ic,det);
%
for  i=1:nn;
for  j=1:nn;
s=0.0d0;
for  k=1:nn;
s=s+blppde_3(i,k).*blppde_2(k,j);
% 4        continue;
blppde_4(i,j)=s;
% 5      continue;
% 6    continue;
%
par_shape=zeros(par_shape);par_shape(:)=par(1:numel(par_shape));par=par_shape;
return;
end  k=nn+1
%;  j=nn+1;  i=nn+1;  i=nn-1+1;  j=nn+1;  i=nn+1;
function bcnd(varargin)

return;
end
%
function icnd(varargin)

return;
end
%
function fopt(varargin)

return;
end
%----------------------------------------------------------------------
%----------------------------------------------------------------------
%
function [ndim,u,par]=pvls(ndim,u,par);
%     ---------- ----
%
persistent ne nn nx ; 

brf
if isempty(nn), nn=nx-1 ; end;
par_shape=size(par);par=reshape(par,1,[]);
%
% common :: ;
global blppfr_1; if isempty(blppfr_1), blppfr_1=0; end;
%% common /blppfr/ ifrst;
%% common /blppfr/ blppfr_1;
%
% Problem-independent initialization :
if(blppfr_1~=1234)
[par]=gencf(par);
blppfr_1=1234;
end;
%
par_shape=zeros(par_shape);par_shape(:)=par(1:numel(par_shape));par=par_shape;
return;
end

