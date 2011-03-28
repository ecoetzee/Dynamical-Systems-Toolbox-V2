function [F]=abode(t,U,ts,PAR)

P(1)=interp1(ts,PAR(1,:),t);
P(2)=interp1(ts,PAR(2,:),t);
P(3)=interp1(ts,PAR(3,:),t);

F=[0,0]';

U1=U(1);
U2=U(2);

E=double(exp(U2));

F(1)=-U1 + P(1)*(1-U1)*E;
F(2)=-U2 + P(1)*P(2)*(1-U1)*E - P(3)*U2;