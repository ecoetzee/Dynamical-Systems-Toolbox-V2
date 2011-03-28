function []=cusp()

%--------------------------------------------------------------------------
% Demo ABC test
%--------------------------------------------------------------------------
delete s.*

eqnfile=mfilename;

copyfile(['AUTOEQN_',eqnfile,'.m'],'AUTOEQN.m');

copyfile(['c.',eqnfile,'.1'],['c.',eqnfile]);
AUTO07run(eqnfile);


