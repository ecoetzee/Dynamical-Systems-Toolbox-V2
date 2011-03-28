function []=convert_auto()

dbstop if error

cdir=pwd;
dnames=dir;

for i=1:length(dnames)
    if length(dnames(i).name)>4 || strcmp(dnames(i).name,'.') || strcmp(dnames(i).name,'..')
        fprintf(1,'Skipping directory   : %s...\n',dnames(i).name);
        continue
    end
    
    fprintf(1,'Converting directory : %s...\n',dnames(i).name);
    cd(cdir);
    cd(dnames(i).name);
    mkdir('fortran');
    cnames=dir('c.*');
    fnames=dir('*.f');
    fnames90=dir('*.f90');
    
    %% Convert constants files
    for j=1:length(cnames)
        convertcc(cnames(j).name);
        movefile(cnames(j).name,['fortran\',cnames(j).name]);
    end
    
    % Convert function files
    if isempty(fnames)&&isempty(fnames90)
        continue;
    else
        if isempty(fnames)
            fnames=fnames90;
        end
        a=readfile(fnames(1).name);
    end
    
    for j=1:length(a)
    
    end
    
    isub=find(cellfun('isempty',regexp(a,'SUBROUTINE'))==0);
    
    iret=find(cellfun('isempty',regexp(a,'RETURN'))==0);
    iret=iret+1;
    itmp=find(cellfun('isempty',regexp(a,'END'))==0);
    [c,ia,ib]=intersect(iret,itmp);
    iend=itmp(ib);
    [c,ia,ib]=intersect(isub,iend);
    isub(ia)=[];
    
    if isempty(iend)
        iend=find(cellfun('isempty',regexp(a,'END SUBROUTINE'))==0);
        [c,ia,ib]=intersect(isub,iend);
        isub(ia)=[];
    end
    
    for j=1:length(isub)
        
        blkstr=a(isub(j):iend(j));
        str=blkstr{1};
        ibr=findstr(str,'(');
        str=str(1:ibr-1);
        fcnstr=strrep(str,'SUBROUTINE','');
        fcnstr=strrep(fcnstr,' ','');
        
        %remove certain lines
        blkstr(end)=[];
        t1=find(cellfun('isempty',regexp(blkstr,'DIMENSION'))==0);
        t2=find(cellfun('isempty',regexp(blkstr,'IMPLICIT'))==0);
        
        blkstr([t1,t2])=[];
        
        blkstr{1}=strrep( blkstr{1},'SUBROUTINE','function []=');
        blkstr{1}=strrep( blkstr{1},fcnstr,lower(fcnstr));
        blkstr{end}=strrep( blkstr{end},'RETURN','return');
        
        for l=1:length(blkstr)
            k1=strmatch('C',blkstr{l});
            if k1==1
                blkstr{l}(1)='%';
            end
            k2=strmatch('!',blkstr{l});
            if k2==1
                blkstr{l}(1)='%';
            end
            if ~isempty(k1) || ~isempty(k2)
                blkstr{l}=strrep( blkstr{l},'-',' ');
            end
            if isempty(k1) && isempty(k2) && l~=1 && l~=length(blkstr)
                str=blkstr{l};
                str=deblank(str);
                blkstr{l}=[str,';'];
            end
            
            blkstr{l}=strrep( blkstr{l},'DO','for');
            blkstr{l}=strrep( blkstr{l},'ENDIF','end');
            blkstr{l}=lower( blkstr{l});
        end
        
        if length(blkstr)>3
            fid=fopen([lower(fcnstr),'.m'],'w');
            
            for l=1:length(blkstr)
                fprintf(fid,'%s\n',blkstr{l});
            end
            
            fclose(fid);
        end
    end
    
    % Convert script files
    movefile(fnames(1).name,['fortran\',fnames(1).name]);
    cd(cdir);
    
end



%-------------------------------------------------------------------------
function A=readfile(fName)

fid=fopen(fName,'r');

if fid < 0
    error('Failed to open AUTO output file');
end

count=1;

while feof(fid)==0
    A{count}=fgetl(fid);
    count=count+1;
end

fclose(fid);