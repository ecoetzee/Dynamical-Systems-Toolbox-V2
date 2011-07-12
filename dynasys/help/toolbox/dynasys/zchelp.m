function []=zchelp()

disp('Zipping help files into jar file...');
zip('help.jar',{'*.html','*.htm','*.jpg','*.jpeg','*.gif','*.png','*.css','*.cpt','*.txt','*.js','corel'});

helpjar=[dynasyshelproot,'\help.jar'];

if exist(helpjar)==2
    disp('Deleting original jar file...');
    delete(helpjar);
end

delstr={'*.html','*.htm','*.jpg','*.jpeg','*.gif','*.png','*.css','*.cpt','*.txt','*.js'};

for i=1:length(delstr)
  delete(delstr{i});
end

try
   rmdir('corel','s');
end
try
   rmdir('backup','s'); 
end

disp('Copying new jar file...');
movefile('help.jar.zip',helpjar);