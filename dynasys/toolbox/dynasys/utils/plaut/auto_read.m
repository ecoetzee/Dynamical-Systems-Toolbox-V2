function [b s]=auto_read(obj,varargin)
ext=obj.fName;
if nargin==1
    first_orbit_only=0;
elseif nargin==2
    first_orbit_only=varargin{1};
else
    error('Too many input arguments (<2)');
end
if ~isempty(strfind(ext,'fort'))
    b_in='fort.7';
    s_in='fort.8';
else 
    b_in=['b.',ext];
    s_in=['s.',ext];
end

if ~exist(b_in,'file') || ~exist(s_in,'file')
    error(['File(s) not found: ',b_in,' ',s_in]);
end

B=readfile(b_in);
r=strmatch('   0',B);
k=r(2:end)-r(1:end-1);
rest_idx=r(find(k~=1)+1);
rest_idx(strmatch('   0',B(rest_idx),'exact'))=[];
rest_idx=[rest_idx;length(B)];

for i=1:length(rest_idx)
    if i==1
        st_idx=1;
    else
        st_idx=rest_idx(i-1);
    end
    end_idx=rest_idx(i);
    b_strings=B(st_idx:end_idx); 
    r=strmatch('   0',b_strings);
    c_strings=b_strings(r);
    c_9=c_strings{9};
    idxst=strfind(c_9,':')+1;
    active_pars=str2num(c_9(idxst:end));
    %b.ndim=ndim;
    b(i).active_pars=active_pars;
    b_strings(r)=[];
    br_str=b_strings{1}(1:4);
    r=strmatch(br_str,b_strings);
    b_temp=b_strings(r);
    b_data=str2num(char(b_temp));
    b(i).ndim=size(b_data,2)-size(active_pars,2)-5;
    b(i).pts=b_data(:,1:4);
    b(i).par=b_data(:,5);
    b(i).l2=b_data(:,6);
    b(i).states=b_data(:,7:6+b(i).ndim);
    for i=2:length(active_pars)
        b(i).par(:,i)=b_data(:,5+b(i).ndim+i);
    end
end
if first_orbit_only
    s_strings=readfile(s_in,1);
else
    s_strings=readfile(s_in);
end
count=1;
br_count=1;
br_tmp=str2num(char(s_strings(1)));
br_new=br_tmp(1);

while ~isempty(s_strings)
while ~isempty(s_strings)
    pts(count,:)=str2num(char(s_strings(1)));
    br_old=br_new;
    br_new=pts(count,1);
    if br_old ~= br_new
        break   
    end
    s_strings(1)=[];
    num_rows=pts(count,7);
    ndim=pts(count,8)-1;
    db_size=pts(count,9);
    npar=pts(count,12);
    num_cols=obj.sfile_cols_cutoff;%% max number of cols in data block (fixed by auto)
    data_rows=ceil((ndim+1)/num_cols);
    derivs_rows=ceil(ndim/num_cols);
    par_rows=ceil(npar/num_cols);
    is_stat=data_rows*num_rows+par_rows~=db_size;
    orbit_tmp=[];
    
    for i=1:num_rows
        line_tmp=[];
        line_tmp=str2num(char(s_strings(1)));
        for j=2:data_rows
            line_tmp=[line_tmp str2num(char(s_strings(j)))];
        end
        s_strings(1:data_rows)=[];
        orbit_tmp=[orbit_tmp; line_tmp];
    end
    orbit(count,:,:)=orbit_tmp;
    orbit_tmp=[];
    if is_stat  
        s_strings(1:2)=[];
        for i=1:num_rows
            s_strings(1:derivs_rows)=[];
        end
    end
        
%%% Code to extract active pars/par derivs/derivs: not needed    
%         active_pars(count,:)=str2num(char(s_strings(1)));
%         par_derivs(count,:)=str2num(char(s_strings(2)));
%         s_strings(1:2)=[];
%         for i=1:num_rows
%             line_tmp=[];
%             line_tmp=str2num(char(s_strings(1)));
%             for j=2:derivs_rows
%                 line_tmp=[line_tmp str2num(char(s_strings(j)))];
%             end
%             s_strings(1:derivs_rows)=[];
%             orbit_tmp=[orbit_tmp; line_tmp];
%         end
%         derivs(count,:,:)=orbit_tmp;
  
   par_tmp=[];
   for j=1:par_rows
       par_tmp=[par_tmp str2num(char(s_strings(j)))];
   end
   par(count,:)=par_tmp;
   s_strings(1:j)=[];
   count=count+1;
   
end
s(br_count).pts=pts;
s(br_count).par=par;
s(br_count).orbit=orbit;
%%% Code to extract active pars/par derivs/derivs: not needed 
% if is_stat
%     s(br_count).derivs=derivs;
%     s(br_count).active_pars=active_pars;
%     s(br_count).par_derivs=par_derivs;
%     derivs=[];
%     active_pars=[];
%     par_derivs=[];
% end
pts=[];orbit=[];par=[];
br_count=br_count+1;
count=1;
if first_orbit_only
    break
end
end


function A=readfile(fname,varargin)
if ~isempty(varargin)
    first_orbit_only=varargin{1};
else
    first_orbit_only=0;
end
fid=fopen(fname,'r');
if fid < 0
    error('Failed top open AUTO output file');
end

if first_orbit_only%% Read only 1st orbit from s. file
    A{1}=fgetl(fid);
    line_tmp=str2num(char(A(1)));
    data_rows=line_tmp(9);
    for i=2:data_rows+1;
        A{i}=fgetl(fid);
    end
else%%Normal real whole file
    count=1;
    while feof(fid)==0
        A{count}=fgetl(fid);
        count=count+1;
    end
end
fclose(fid);