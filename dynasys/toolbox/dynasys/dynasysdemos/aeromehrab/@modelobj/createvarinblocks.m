function d=createvarinblocks()

% set gain blocks
g=find_system('AircraftB','BlockType','Gain');

for i=1:length(g)
    d(i).name=get_param(g{i},'Name');
    d(i).val=get_param(g{i},'Gain');   
    
    % ignore the unit conversions
    if ~isempty(strmatch('rad_to_deg',d(i).name))||~isempty(strmatch('deg_to_rad',d(i).name))
        d(i).name=[];
        d(i).val=[];
        continue;
    end 
    d(i).name=strrep(d(i).name,'(','');
    d(i).name=strrep(d(i).name,')','');
    
    set_param(g{i},'Gain',sprintf('ac.dat.%s',d(i).name));
end

% set constant blocks
c=find_system('AircraftB','BlockType','Constant');
lng=length(g);
lnc=length(c);

for i=1:lnc
    d(lng+i).name=get_param(c{i},'Name');
    d(lng+i).val=get_param(c{i},'Value');   
    
    % ignore the unit conversions
    if ~isempty(strmatch('rad_to_deg',d(lng+i).name))||~isempty(strmatch('deg_to_rad',d(lng+i).name))
        d(lng+i).name=[];
        d(lng+i).val=[];
        continue;
    end 

    d(lng+i).name=strrep(d(lng+i).name,'(','');
    d(lng+i).name=strrep(d(lng+i).name,')','');
    
    set_param(c{i},'Value',sprintf('ac.dat.%s',d(lng+i).name));
end

% Write data to file
fid=fopen('acdat.m','w');

fprintf(fid,'function dat=acdat()\n');
fprintf(fid,'\n');
fprintf(fid,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n');
fprintf(fid,'%% Gain block values\n');
fprintf(fid,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n');
for i=1:length(d)
    if ~isempty(d(i).name)
      fprintf(fid,'dat.%s=%s;\n',d(i).name,d(i).val);
    end
end
fprintf(fid,'\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n');
fprintf(fid,'%% Constant block values\n');
fprintf(fid,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n');
for i=1:length(lnc)
    if ~isempty(d(i).name)
      fprintf(fid,'dat.%s=%s;\n',d(lng+i).name,d(lng+i).val);
    end
end

fclose(fid);
fprintf(1,'Finished!');
