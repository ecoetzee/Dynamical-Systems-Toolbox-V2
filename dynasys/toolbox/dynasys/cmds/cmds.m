% 5.1 Basic Commands
% 
% @r  : Type @r xxx to run AUTO. Restart data, if needed, are expected in 
%       s.xxx, and AUTO-constants in c.xxx. This is the simplest way to run
%       AUTO.
%      
% - Type @r xxx yyy to run AUTO with equations-file xxx.f and restart 
%   data-file s.yyy. AUTO-constants must be in c.xxx.
% - Type @r xxx yyy zzz to run AUTO with equations-file xxx.f, restart 
%   data-file s.yyy and constants-file c.zzz.
%   
% @R  : The command @R xxx is equivalent to the command @r xxx above.
% 
% - Type @R xxx i to run AUTO with equations-file xxx.f, constants-file 
%   c.xxx.i and, if needed, restart data-file s.xxx.
% - Type @R xxx i yyy to run AUTO with equations-file xxx.f, constants-file 
%   c.xxx.i and restart data-file s.yyy.
%   
% @sv : Type @sv xxx to save the output-files fort.7, fort.8, fort.9, as 
%       b.xxx, s.xxx, d.xxx, respectively. Existing files by these names 
%       will be deleted.
%       
% @ap : Type @ap xxx to append the output-files fort.7, fort.8, fort.9, 
%       to existing data-files b.xxx, s.xxx, d.xxx, resp.
%       
% - Type @ap xxx yyy to append b.xxx, s.xxx, d.xxx, to b.yyy, s.yyy, d.yyy, 
%   resp.
% 
% 5.2 Plotting Commands
% 
% @p  : Type @p xxx to run the graphics program PLAUT (See Chapter 7) 
%       for the graphical inspection of the data-files b.xxx and s.xxx.
%           
% - Type @p to run the graphics program PLAUT for the graphical inspection 
%   of the output-files fort.7 and fort.8.
% 
% @ps : Type @ps fig.x to convert a saved PLAUT figure fig.x from compact 
%       PLOT10 format to PostScript format. The converted file is called 
%       fig.x.ps. The original file is left unchanged.
% 
% 5.3 File-manipulation
% 
% @cp : Type @cp xxx yyy to copy the data-files b.xxx, s.xxx, d.xxx, 
%       c.xxx to b.yyy, s.yyy, d.yyy, c.yyy, respectively.
%       
% @mv : Type @mv xxx yyy to move the data-files b.xxx, s.xxx, d.xxx, 
%       c.xxx, to b.yyy, s.yyy, d.yyy, c.yyy, respectively.
%       
% @df : Type @df to delete the output-files fort.7, fort.8, fort.9.
% 
% @cl : Type @cl to clean the current directory. This command will delete 
%       all files of the form fort.*, *.o, and *.exe.
%       
% @dl : Type @dl xxx to delete the data-files b.xxx, s.xxx, d.xxx.
% 
% 5.4 Diagnostics
% 
% @lp : Type @lp to list the value of the “limit point function” in the 
%       output-file fort.9. This function vanishes at a limit point (fold).
%           
% - Type @lp xxx to list the value of the “limit point function” in the 
%   data-file d.xxx. This function vanishes at a limit point (fold).
%       
% @bp : Type @bp to list the value of the “branch-point function” in the 
%       output-file fort.9. This function vanishes at a branch point.
%           
% - Type @bp xxx to list the value of the “branch-point function” in the 
%   data-file d.xxx. This function vanishes at a branch point.
%       
% @hb : Type @hb to list the value of the “Hopf function” in the 
%       output-file fort.9. This function vanishes at a Hopf bifurcation 
%       point.
%       
% - Type @hb xxx to list the value of the “Hopf function” in the 
%   data-file d.xxx. This function vanishes at a Hopf bifurcation point.
% 
% @sp : Type @sp to list the value of the “secondary-periodic 
%       bifurcation function” in the output-file fort.9. This function 
%       vanishes at period-doubling and torus bifurcations.
%       
% - Type @sp xxx to list the value of the “secondary-periodic bifurcation 
%   function” in the data-file d.xxx. This function vanishes at 
%   period-doubling and torus bifurcations.
%   
% @it : Type @it to list the number of Newton iterations per continuation 
%       step in fort.9.
%       
% - Type @it xxx to list the number of Newton iterations per continuation 
%   step in d.xxx.
%   
% @st : Type @st to list the number of stable eigenvalues or stable 
%       Floquet multipliers per continuation step in fort.9.
%       
% @ss : Type @st to list the continuation step size for each continuation 
%       step in fort.9.
%       
% - Type @st xxx to list the continuation step size for each continuation 
%   step in d.xxx.
%   
% @ev : Type @ev to list the eigenvalues of the Jacobian in fort.9. 
%       (Algebraic problems.)
%       
% - Type @ev xxx to list the eigenvalues of the Jacobian in d.xxx. 
%   (Algebraic problems.)
%   
% @fl : Type @fl to list the Floquet multipliers in the output-file fort.9. 
%       (Differential equa-tions.)
%       
% - Type @fl xxx to list the Floquet multipliers in the data-file 
%   d.xxx. (Differential equations.)
%   
% 5.5 File-editing
% 
% @e7 : To use the vi editor to edit the output-file fort.7.
% @e8 : To use the vi editor to edit the output-file fort.8.
% @e9 : To use the vi editor to edit the output-file fort.9.
% @j7 : To use the SGI jot editor to edit the output-file fort.7.
% @j8 : To use the SGI jot editor to edit the output-file fort.8.
% @j9 : To use the SGI jot editor to edit the output-file fort.9.
% 
% 5.6 File-maintenance.
% 
% @lb : Type @lb to run an interactive utility program for listing, 
%       deleting and relabeling solutions in the output-files fort.7 and 
%       fort.8. The original files are backed up as ?fort.7 and ?fort.8.
%       
% - Type @lb xxx to list, delete and relabel solutions in the data-files 
%   b.xxx and s.xxx. The original files are backed up as ?b.xxx and ?s.xxx.
%   
% - Type @lb xxx yyy to list, delete and relabel solutions in the 
%   data-files b.xxx and s.xxx. The modified files are written as b.yyy 
%   and s.yyy.
%   
% @fc : Type @fc xxx to convert a user-supplied data file xxx.dat to 
%       AUTO format. The converted file is called s.dat. The original file 
%       is left unchanged. AUTO automatically sets the period in PAR(11). 
%       Other parameter values must be set in STPNT. (When necessary, 
%       PAR(11) may also be redefined there.) The constants-file file 
%       c.xxx must be present, as the AUTO-constants NTST and NCOL 
%       (Sections 10.3.1 and 10.3.2) are used to define the new mesh. For 
%       examples of using the @fc command see demos lor and pen.
%       
% 5.7 HomCont commands.
% 
% @h  : Use @h instead of @r when using HomCont, i.e., when IPS=9 
%       (see Chapter 20). Type @h xxx to run AUTO/HomCont. Restart data, 
%       if needed, are expected in s.xxx, AUTO-constants in c.xxx and 
%       HomCont-constants in s.xxx.
%       
% - Type @h xxx yyy to run AUTO/HomCont with equations-file xxx.f and 
%   restart data-file s.yyy. AUTO-constants must be in c.xxx and 
%   HomCont-constants in s.xxx.
% - Type @h xxx yyy zzz to run AUTO/HomCont with equations-file xxx.f, 
%   restart data-file s.yyy and constants-files c.zzz and s.zzz.
%   
% @H  : The command @H xxx is equivalent to the command @h xxx.
% 
% - Type @H xxx i in order to run AUTO/HomCont with equations-file xxx.f 
%   and constants-files c.xxx.i and s.xxx.i and, if needed, restart 
%   data-file s.xxx.
% - Type @H xxx i yyy to run AUTO/HomCont with equations-file xxx.f, 
%   constants-files c.xxx.i and s.xxx.i, and restart data-file s.yyy.
%   
% 5.8 Copying a demo.
% 
% @dm : Type @dm xxx to copy all files from auto/07p/demos/xxx to the 
%       current user directory. Here xxx denotes a demo name; e.g., 
%       abc. Note that the @dm command also copies a Makefile to the 
%       current user directory. To avoid the overwriting of existing 
%       files, always run demos in a clean work directory.
%       
% 5.9 Viewing the manual.
% 
% @mn : Use gv to view the PDF version of this manual.
