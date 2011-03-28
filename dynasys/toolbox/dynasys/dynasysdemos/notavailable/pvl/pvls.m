      function []= pvls(ndim,u,par)
%                    
%
%
%                                                                       
% note : 
% parameters set in this subroutine should be considered as ``solution 
% measures'' and be used for output purposes only.
% 
% they should never be used as `true'' continuation parameters. 
%
% they may, however, be added as ``over specified parameters'' in the 
% parameter list associated with the auto constant nicp, in order to 
% print their values on the screen and in the ``p.xxx file.
%
% they may also appear in the list associated with auto constant nuzr.
%
%                                                                       
% for algebraic problems the argument u is, as usual, the state vector.
% for differential equations the argument u represents the approximate 
% solution on the entire interval [0,1]. in this case its values must 
% be accessed indirectly by calls to getp, as illustrated below.
%                                                                       
%
% set par(2) equal to the l2 norm of u(1)
       par(2)=getp('nrm',1,u);
%
% set par(3) equal to the minimum of u(2)
       par(3)=getp('min',2,u);
%
% set par(4) equal to the value of u(2) at the left boundary.
       par(4)=getp('bv0',2,u);
%
%                                                                       
% the first argument of getp may be one of the following:
%        'nrm' (l2 norm),     'max' (maximum),
%        'int' (integral),    'bv0 (left boundary value),
%        'min' (minimum),     'bv1' (right boundary value).
%
% also available are
%   'stp' (pseudo arclength step size used).
%   'fld' (`fold function', which vanishes at folds).
%   'bif' (`bifurcation function', which vanishes at singular points).
%   'hbf' (`hopf function'; which vanishes at hopf points).
%   'spb' ( function which vanishes at secondary periodic bifurcations).
%                                                                       
%
      return
