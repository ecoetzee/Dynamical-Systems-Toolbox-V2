a=dir('*mehbrab_image*');

for i=1:length(a);
    fname=a(i).name;
    fprintf('Renaming %s...\n',fname);
    fnamenew=strrep(fname,'mehbrab_image','mehrab_image');
    movefile(fname,fnamenew);
end