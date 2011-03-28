function [dat,inp]=acdat()

% ACDAT data file for Mehra Aircraft B Model

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gain block values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dat.za=-3;
dat.zde=0;
dat.y_beta=-0.056;
dat.yda=0;
dat.ydr=0;
dat.i1=0.706;
dat.l_beta=-110.1;
dat.lda=-326.3;
dat.ldr=0;
dat.lp=-21.64;
dat.lq=0;
dat.lr=1.512;
dat.i2=0.960;
dat.mbar_alpha=-12.34;
dat.mbar_q=-2.205;
dat.m_alpha_dot=-0.391;
dat.mde=-33.24;
dat.i3=0.787;
dat.n_beta=3.705;
dat.n_p=0;
dat.n_r=-0.259;
dat.nda=0;
dat.ndr=0;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constant block values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dat.alpha0=0;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Inputs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
inp.da  = 40;              % Aileron input (deg)
inp.de  =  2;              % Elevator input (deg)
inp.dr  =  0;              % Rudder input (deg)
