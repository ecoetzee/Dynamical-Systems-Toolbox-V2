function []= compileauto07p(varargin)

% COMPILEAUTO07 - Function to compile AUTO for the Matlab environment
%
% You can compile in three modes:
%
%  1. compileauto07p        - No input argument, calls normal mex compilation.
%  2. compileauto07p('obj') - Create object files first and then link them
%                            We found that for some setups the first option
%                            doesn't work for some or other strange reason.
%                            This way it seems to always work.
%  3. compileauto07p('dos') - Will compile Frotran files outside of Matlab as
%                            object files. Then link these using mex
%                            Function.
%
%  NOTES:
%
%  1. Remember to compile with the -g option to allow for debugging.
%  2. Single and multi-threading might be a problem, so change this option
%     in the "mexopts.bat" file if you think this could be a reason for mex
%     not working. Change the "/MD" string to "/MDs" for single threading.
%     It seems as if Intel Fotran 9.1 tries to compile as a multi-threaded
%     solution by default. Matlab is singlethreaded. The "Nompi.f90" file is
%     also used for AUTO to run as a single thread.
%  3. Check compatibility of 32 and 64 bit versions of Matlab and Fortran.
%     Use "mexext" command to see if the 32 or 64 bit version of Matlab is
%     being used.

%   Created by Etienne COETZEE, James RANKIN,and Phani THOTA, University of Bristol
%
%   $Revision: 2.0.0.0 $ $Date: 2010/07/29 14:07:00$

% Detect mex architecture configuration and throw error if not recognised

cdir=pwd;

try
    if isempty(strfind(mexext,'32')) && isempty(strfind(mexext,'64')) && isempty(strfind(mexext,'glx'))
        error('AUTOmatlab:CompileArchitectureUnknown','Cannot determine machine architecture, compilation stopped')
    end
    
    % detect path where this file is located
    dsthstr=mfilename('fullpath');
    [dsthname,fname]=fileparts(dsthstr);
    
    % switch to appropriate directory
    if ispc
        cd(fullfile(dsthname,'win32'));
        fprintf(1,'\nCompiling for Windows platform...\n');
    elseif isunix
        fprintf(1,'\nCompiling for Linux platform...\n');
        cd(fullfile(dsthname,'linux'));
    else
        error('AUTOmatlab:PlatformOSUnknown','Cannot determine machine operating system, compilation stopped')
    end
    
    if isempty(varargin)
        
        mex -g AUTO07gateway.f90 AutoConstants.f90 Autlib2.f90 Autlib3.f90 Autlib4.f90 Autlib5.f90 CopyDstObjects.f90 Autlib1.f90 Blas.f90 Eispack.f90 Func.f90 Nompi.f90 AutoStop.f90
        
    elseif strcmp(varargin,'obj')
        
        % This option came  about after some weird module dependancy errors.
        % The problem was solved by creating object files first and then
        % linking these afterwards. "Autlib1.f90" also needs to be compiled after
        % the other library files due to dependancy issues.
        %
        % Use the -c option to create object files
        %
        mex -c -g AutoConstants.f90
        mex -c -g Allocatef7.f90
        mex -c -g Allocatef8.f90
        mex -c -g Deallocatef7.f90
        mex -c -g Deallocatef8.f90
        mex -c -g Deallocatein.f90
        mex -c -g Deallocateinf7f8.f90
        mex -c -g Autlib2.f90
        mex -c -g Autlib3.f90
        mex -c -g Autlib4.f90
        mex -c -g Autlib5.f90
        mex -c -g Autlib1.f90
        mex -c -g CopyDstObjects.f90
        mex -c -g Blas.f90
        mex -c -g Eispack.f90
        mex -c -g Func.f90
        mex -c -g Nompi.f90
        mex -c -g AutoStop.f90
        
        
        if ispc
            mex -g AUTO07gateway.f90 AutoConstants.obj Allocatef7.obj Allocatef8.obj Deallocatef7.obj Deallocatef8.obj Deallocatein.obj Deallocateinf7f8.obj AutoStop.obj CopyDstObjects.obj Autlib1.obj Autlib2.obj Autlib3.obj Autlib4.obj Autlib5.obj Blas.obj Eispack.obj Func.obj Nompi.obj
        elseif isunix
            mex -g AUTO07gateway.f90 AutoConstants.o Allocatef7.o Allocatef8.o Deallocatef7.o Deallocatef8.o Deallocatein.o Deallocateinf7f8.o AutoStop.o CopyDstObjects.o Autlib1.o Autlib2.o Autlib3.o Autlib4.o Autlib5.o Blas.o Eispack.o Func.o Nompi.o
        else
            error('AUTOmatlab:CompileError','Could not determine operating system. No compilation of AUTO took place');
        end
        
    elseif strcmp(varargin,'dos')
        
        % The following lines might not all run automatically. Copy and paste
        % to the command line as appropriate. This is for Intel Visual Fortran.
        % I have never really needed this.
        
        % Compile in DOS mode first.
        !C:\WINDOWS\system32\cmd.exe /K ""C:\Program Files\Intel\Compiler\Fortran\9.1\IA32\Bin\IFortVars.bat""
        
        % Then compile each file individually
        % Use the "/MDs" option to compile for single threaded and use "-c" option
        % for creating .obj files
        ifort /MDs -c AUTO07gateway.f90
        ifort /MDs -c CopyDstObjects.f90
        ifort /MDs -c AutoConstants.f90
        ifort /MDs -c Autlib1.f90
        ifort /MDs -c Autlib2.f90
        ifort /MDs -c Autlib3.f90
        ifort /MDs -c Autlib4.f90
        ifort /MDs -c Autlib5.f90
        ifort /MDs -c Blas.f90
        ifort /MDs -c compat.f90
        ifort /MDs -c Eispack.f90
        ifort /MDs -c Func.f90
        ifort /MDs -c Nompi.f90
        ifort /MDs -c AutoStop.f90
        
        % exit the dos environment
        exit
        
        % mex the object files
        mex -g AUTO07gateway.obj CopyDstObjects.obj AutoConstants.obj Autlib1.obj Autlib2.obj Autlib3.obj Autlib4.obj Autlib5.obj Blas.obj Eispack.obj Func.obj Nompi.obj AutoStop.obj
        
    else
        error('AUTOmatlab:compileOptions','Check input argument for compilation');
    end
    
    fprintf(1,'Moving %s file one directory up...\n',['AUTO07gateway.',mexext]);
    
    % move executable one directory up
    if ispc
       movefile(fullfile(dsthname,'win32',['AUTO07gateway.',mexext]),fullfile(dsthname,['AUTO07gateway.',mexext]));
    elseif isunix
       movefile(fullfile(dsthname,'linux',['AUTO07gateway.',mexext]),fullfile(dsthname,['AUTO07gateway.',mexext])); 
    end
    
    cd(cdir);
    fprintf(1,'Compilation completed successfully.\n\n');
    
catch
    cd(cdir);
end