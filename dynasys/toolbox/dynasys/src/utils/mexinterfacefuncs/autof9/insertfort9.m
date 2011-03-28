function a=insertfort9()

% This program inserts IF statements in front of the WRITE(9 statements, to
% make sure that they are only executed when output is required.

fileName='autlib5';

% try
    
    %Parse the file and find line numbers for Subroutines, Include
    %statements, and WRITE(9 statements
    %    copyfile([fileName,'_V0.F'],[fileName,'.F'])
    copyfile([fileName,'.F'],[fileName,'_V0.F'])
    
    A=readfile([fileName,'.F']);
    
    count=0;
    a(1).func=[];
    a(1).inc=[];
    a(1).wr9=[];
    
    for i=1:length(A)
        str=A{i};
        submatch=strmatch('      SUBROUTINE ',str);
        funcmatch=strmatch('      DOUBLE PRECISION FUNCTION',str);
        modmatch=strmatch('      MODULE ',str);
        if  ~isempty(submatch) || ~isempty(funcmatch) || ~isempty(modmatch) 
            count=count+1;
            a(count).func=i;
            a(count).inc=[];
            a(count).wr9=[];
        end
        if strcmp(str,'      INCLUDE ''auto.h''')
            ln=length(a(count).inc);
            a(count).inc(ln+1)=i;
        end
        if ~isempty(findstr(str,'WRITE(9'))
            ln=length(a(count).wr9);
            a(count).wr9(ln+1)=i;
        end
    end
    
    b=[a.func]';
    count=0;
    usecount=0;
    contstr=0;
    
    % open output file
    fid=fopen([fileName,'.F'],'w');
    
    if fid < 0
        error('Could not open output file');
    end
    
    
    for i=1:length(A)
        str=A{i};
        idx=findstr(upper(str),'WRITE(9');
        
        if ~isempty(strmatch('      SUBROUTINE STDRBV(',str))
            disp('N')
        end
        
        % find any continuation characters
        if isempty(str)
            charstr=0;
        else
            cstr=strmatch('C',str);
            pstr=strmatch('     +',str);
            astr=strmatch('     &',str);
            sstr=strmatch('     *',str);
            if ~isempty(cstr)||~isempty(pstr)||~isempty(astr)||~isempty(sstr)
                charstr=1;
            else
                charstr=0;
            end
        end
        
        % set flag for writing FORT9DST if it is not declared
        if i <= b(end)
            if i==b(count+1)
                count=count+1;
                if isempty(a(count).inc) && ~isempty(a(count).wr9)
                    usecount=1;
                    charstr=1;
                end
            end
        end
        
        % write constants declaration
        if usecount==1 && charstr==0
            tstr{1}=sprintf('      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST');
            tstr{2}=str;
            usecount=0;
            contstr=1;
            % replace Include statements with constants module declarations
        elseif ~isempty(strmatch('      INCLUDE ''auto.h''',str)) && charstr==0
            tstr{1}='      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST';
            contstr=1;
            %
            % split WRITE(9 string and place IF statement in front of it
        elseif ~isempty(idx) && charstr==0
            str1=str(1:idx-1);
            str2=str(idx:length(str));
            tstr{1}=sprintf('%sIF(FORT9DST==1)%s',upper(str1),upper(str2));
            contstr=1;
        else
            tstr{1}=str;
            contstr=0;
        end
        
        for j=1:length(tstr)
            if length(tstr{j})>72 && contstr==1
                str1=tstr{j}(1:72);
                str2=tstr{j}(73:end);
                str=sprintf('%s\n     &%s',str1,str2);
                contstr=0;
            else
                str=tstr{j};
            end
            fprintf(fid,'%s\n',str);
        end
        tstr=[];
    end
    
    fclose(fid);
    
    disp('Finshed with converting WRITE(9, functionality');
% catch
%     fclose('all');
%     disp(lasterror.stack);
% end

%--------------------------------------------------------------------------
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
