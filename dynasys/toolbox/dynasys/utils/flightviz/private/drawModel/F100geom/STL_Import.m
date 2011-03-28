function varargout=STL_Import(filename,mode)
% STL_Import is a tool designed to import into MATLAB both binary and ASCII STL files.
% 
% This scprit is mainly a collage betwwen file axchange fileid 22409 and 3642, plus
% some other features that can be considered new on FEX.
% 
% SYNOPSIS:
% 
% 
% %mode 1 (default)
% [p,t,tnorm]=STL_Import(filename,mode)
% 
% %mode 2
% [v,tnorm])=STL_Import(filename,mode)
% 
% 
% INPUT:
% 
% filename: string representing the name fo the file
% 
% mode:
% 
% 
%  mode=1 (if omitted is automatically set to one)
%     
%   set the the output to:
%     
%     output=[p,t,tnorm]
%     
%     where        
%     
%     p=points (unique) of the model nx3 array

%     t=triangles indexes of the model

%     tnorm= normals of triangles
%     

%  mode=2
%       
%   set the the output to:
%     
%     output=[v,tnorm]
%     
%     where        
%     
%     v=  vertex of the model(not unique points) of the model nx3 array. Each
%         trhee points we have a triagnle in consecutive order.

%     tnorm= normals of triangles
% 
% EXAMPLES:    
%    
%    [p,t,tnorm]=STL_Import('link1.stl',1); 
%    [pv,tnorm]=STL_Import('link1.stl',2);
% 
%
% Visit:
% 
%  http://giaccariluigi.altervista.org/blog/
%  
% Author: Giaccari Luigi  (giaccariluigi@msn.com)



if nargin<2
    mode=1;%default value
end


if ~(mode==1 || mode==2)
    error('invalid mode')
end

if nargout<3 && mode==1
    error('invalid input number /mode setting')
end
if nargout>2 && mode==2
    error('invalid input number /mode setting')
end


%open file
fid=fopen(filename, 'r'); %Open the file, assumes STL ASCII format.
if fid == -1
    error('File could not be opened, check name or path.')
end


 M = fread(fid,inf,'uint8=>uint8');
    fclose(fid);    
    
if( isbinary(M) )
     [v,tnorm]=ImportSTL_binary(M);
    
else
    clear M;    
    [v,tnorm]=ImportSTL_ASCII(filename);
   
end

clear M

varargout = cell(1,nargout);
switch mode
    case 1
        [p,t]=fv2pt(v,length(v)/3);%gets points and triangles
        
        varargout{1} = p;
        varargout{2} = t;
        varargout{3} = tnorm;
    case 2
        varargout{1} = v;
        varargout{2} = tnorm;
end
end



function [v,tnorm]=ImportSTL_ASCII(filename)

%counting the number of vertex
vnum=0;
fid=fopen(filename, 'r'); %Open the file, assumes STL ASCII format.
while feof(fid) == 0                    % test for end of file, if not then do stuff
    tline = fgetl(fid);                 % reads a line of data from file.
    fword = sscanf(tline, '%s ');       % make the line a character string
    if strncmpi(fword, 'v',1) ;    % Checking if a "V"ertex line, as "V" is 1st char.
        vnum = vnum + 1;                % If a V we count the # of V's
    end
end
numt=ceil(vnum/3);%triangles number equals vertex number/3

tnorm=zeros(numt,3);%preallocate for normals
v=zeros(vnum,3);%not unique vertex

c=0;%vertex counter
fnum=0;
fid=fopen(filename, 'r'); %REOpen the file
while feof(fid) == 0                    % test for end of file, if not then do stuff
    tline = fgetl(fid);                 % reads a line of data from file.
    fword = sscanf(tline, '%s ');       % make the line a character string
    
    %% Check vertex
    if strncmpi(fword, 'v',1) ;    % Checking if a "V"ertex line, as "V" is 1st char.
        c = c + 1;                % If a V we count the # of V's
        v(c,:) = sscanf(tline, '%*s %f %f %f'); % & if a V, get the XYZ data of it.
        
        %% Check facet normal
    elseif strncmpi(fword, 'f',1) ;    % Checking if a "V"ertex line, as "V" is 1st char.
        fnum =fnum + 1;                % If a V we count the # of V's
        tnorm(fnum,:) = sscanf(tline, '%*s %*s %f %f %f'); % & if a V, get the XYZ data of it.
        
        % %% Check for color
        %     elseif strncmpi(fword, 'c',1) ;    % Checking if a "C"olor line, as "C" is 1st char.
        %         VColor = sscanf(tline, '%*s %f %f %f'); % & if a C, get the RGB color data of the face.
        %    % Keep this color, until the next color is used.
        
    end
    
end
end


function [p,t]=fv2pt(v,fnum)

%gets points and triangle indexes given vertex and facet number

c=size(v,1);

%triangles with vertex id data
t=zeros(3,fnum);
t(:)=1:c;


%now we have to keep unique points fro vertex
[p,i,j]=unique(v,'rows'); %now v=p(j) p(i)=v;
t(:)=j(t(:));
t=t';

end

% 


function tf = isbinary(A)
% ISBINARY determines if an STL file is binary or ASCII.

    % Look for the string 'endsolid' near the end of the file
    if isempty(A) || length(A) < 16
        error('MATLAB:stlread:incorrectFormat', ...
              'File does not appear to be an ASCII or binary STL file.');
    end
    
    % Read final 16 characters of M
    i2  = length(A);
    i1  = i2 - 100;%100 empirical value
    str = char( A(i1:i2)' );
    
    k = strfind(lower(str), 'endsolid');
    if ~isempty(k)
        tf = false; % ASCII
    else
        tf = true;  % Binary
    end
end


function [V,N]=ImportSTL_binary(M)

 
    
    if length(M) < 84
        error('MATLAB:stlread:incorrectFormat', ...
              'Incomplete header information in binary STL file.');
    end
    
    % Bytes 81-84 are an unsigned 32-bit integer specifying the number of faces
    % that follow.
    numFaces = typecast(M(81:84),'uint32');
    %numFaces = double(numFaces);
    if numFaces == 0
        warning('MATLAB:stlread:nodata','No data in STL file.');
        return
    end
    
    T = M(85:end);

    V = NaN(3*numFaces,3);
    N = NaN(numFaces,3);
    
    numRead = 0;
    while numRead < numFaces
        % Each facet is 50 bytes
        %  - Three single precision values specifying the face normal vector
        %  - Three single precision values specifying the first vertex (XYZ)
        %  - Three single precision values specifying the second vertex (XYZ)
        %  - Three single precision values specifying the third vertex (XYZ)
        %  - Two unused bytes
        i1    = 50 * numRead + 1;
        i2    = i1 + 50 - 1;
        facet = T(i1:i2)';
        
        n  = typecast(facet(1:12),'single');
        v1 = typecast(facet(13:24),'single');
        v2 = typecast(facet(25:36),'single');
        v3 = typecast(facet(37:48),'single');
        
        n = double(n);
        v = double([v1; v2; v3]);
        
        % Figure out where to fit these new vertices, and the face, in the
        % larger F and V collections.        
        fInd  = numRead + 1;        
        vInd1 = 3 * (fInd - 1) + 1;
        vInd2 = vInd1 + 3 - 1;
        
        V(vInd1:vInd2,:) = v;
        N(fInd,:)        = n;
        
        numRead = numRead + 1;
    end
    
end