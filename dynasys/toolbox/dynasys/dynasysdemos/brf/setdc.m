      function []= setdc(ne,dc,par) 
%                      
%     set the diffusion constants (constant, or in terms of par)
%
% 
        dc(1)=par(3)/par(5)**2;
        dc(2)=par(4)/par(5)**2;
% 
      return 
