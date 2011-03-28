% Numerical simulation of the shimmy equations given by somieski

% Initializing the problem parameters
parameterset

% Initializing the option of the numerical integration
options=odeset('RelTol',1e-7,'AbsTol',1e-7,'events','on');

tspan=[0 50];
inicon=[0.01 0.01 0 0 0 0 0];
%inicon=[-0,0,0,0.0006,-0.0913,-0,0;]
[T,Y,TE,YE,IE]=ode45('odefile1',tspan,inicon,options,parameters);
plot(Y(:,1),Y(:,2),'k');

        
      X(1)=0.2996714764184852
      X(3)=3.3486178600361280
	  RD= 0.021366079654148
	  RR= 0.045            
	  RE= 0.015            
	  RX0= 0.249471112870112
	  RY0= 2.472125127290171E-016
	  RESPHIY0=-RE*sin(X(3))+RY0
	  RECPHIX0=RE*cos(X(3))+RX0
      RCAMPOS= asin((RD+RR)/sqrt(RECPHIX0^2+RESPHIY0^2))-atan(-RESPHIY0/RECPHIX0)
      H=X(1)-RCAMPOS