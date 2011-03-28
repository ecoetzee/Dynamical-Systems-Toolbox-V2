function [UVW]=windv2worldv(ALPHA,BETA,PHI,THETA,PSI,Vt)

uvw=[Vt*cos(ALPHA)*cos(BETA);
                Vt*sin(BETA);
     Vt*sin(ALPHA)*cos(BETA)];



% convert to world axis
% Phillips Eq (11.2.4)
% linear velocities
C11=cos(THETA)*cos(PSI);
C12=sin(PHI)*sin(THETA)*cos(PSI)-cos(PHI)*sin(PSI);
C13=cos(PHI)*sin(THETA)*cos(PSI)+sin(PSI)*sin(PHI);
C21=cos(THETA)*sin(PSI);
C22=sin(PHI)*sin(THETA)*sin(PSI)+cos(PHI)*cos(PSI);
C23=cos(PHI)*sin(THETA)*sin(PSI)-sin(PHI)*cos(PSI);
C31=-sin(THETA);
C32=sin(PHI)*cos(THETA);
C33=cos(PHI)*cos(THETA);

UVW=[C11,C12,C13;C21,C22,C23;C31,C32,C33]*uvw';