% GET_FNAMES - Collect all .dat file names in a dirctory
%   and return sorted name list
%
%
%
%	-----------------------------------------
%	WRITTEN BY: MOHAMMAD S. IMTIAZ.
%       Time-stamp: <2005-05-14 15:25:00 msi800>
%	-----------------------------------------
%       Collect all .dat file names in a dirctory
%       and return sorted name list
%       
%       ALSO SEE: plotxppaut3d
%	-----------------------------------------

function [f_names_sorted, f_v_sorted] = get_fnames(path_in)

a=dir([path_in '*.dat']);
for n=1:length(a),
  temp = a(n).name;
  f_v(n) = str2num(temp(1:length(temp)-4));
end;

[f_v_sorted ind_sort] = sort(f_v);

f_names_sorted = {a(ind_sort).name};
