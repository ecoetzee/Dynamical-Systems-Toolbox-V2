#include "fintrf.h"
!========================================================================
! AUTO07GATEWAY.F - Gateway function for AUTO
!
!   This subroutine is the main gateway to MATLAB and calls AUTO07.
!   When a MEX function C	is executed MATLAB calls the MEXFUNCTION 
!   subroutine in the corresponding MEX file.  
!
!   Written by ETIENNE COETZEE
!
!   $Revision: 1.0.0.0 $ $Date: 2008/12/05 10:45:00$
!
!========================================================================
!
      SUBROUTINE MEXFUNCTION(NLHS, PLHS, NRHS, PRHS)
!     
      USE AUTO_CONSTANTS, ONLY:RUNMODEDST
!
      IMPLICIT NONE
      MWPOINTER PLHS(*), PRHS(*), mxDuplicateArray
      MWSIZE  NLHS, NRHS
      CHARACTER(LEN=63) :: mxGetClassName
      EXTERNAL AUTOSTOP
      MWSIZE  mexAtExit
      MWSIZE  k
!     
! KEEP THE ABOVE SUBROUTINE, ARGUMENT, AND FUNCTION DECLARATIONS FOR USE
! IN ALL YOUR FORTRAN MEX FILES.
!
!---------------------------------------------------------------------
!         AVOID ERRORS WHEN DIVIDING BY ZERO ON WINDOWS PLATFORM
!---------------------------------------------------------------------
! This resets the floating point exception to allow divide by zero,
! overflow and invalid numbers. 
!
#if defined MSWIND
	MWSIZE CONTROL
	CALL GETCONTROLFPQQ(CONTROL)
      CONTROL = CONTROL .OR. FPCW$ZERODIVIDE
      CONTROL = CONTROL .OR. FPCW$INVALID
      CONTROL = CONTROL .OR. FPCW$OVERFLOW
	CALL SETCONTROLFPQQ(CONTROL)
#endif

!---------------------------------------------------------------------
!   GET SIMULATION OPTIONS AND RUN IN 07P OR DST MODE
!---------------------------------------------------------------------
      if( NRHS == 0 .OR. NRHS == 2 .OR. NRHS == 3 .OR.  NRHS > 4 ) then
        call mexErrMsgTxt("Need either one or four auto input arguments"
     &)
        k=mexAtExit(AUTOSTOP)
      endif
      if( mxGetClassName(PRHS(1)) /= "autosimopts" ) then
        call mexErrMsgTxt("Make sure an AUTO simulation properties objec
     &t AUTOSIMOPTS is being passed as the first argument into the RUNAU
     &TO method of the AUTO object")
        k=mexAtExit(AUTOSTOP)
      endif

      CALL GETSIMOPTS(PRHS)

      IF( RUNMODEDST == 0)THEN
          CALL RUN07PMODE(NRHS, PRHS)
      ELSE
          CALL RUNDSTMODE(NLHS, PLHS, NRHS, PRHS)
      ENDIF

      RETURN
      END
      
!---------------------------------------------------------------------
!   RUN IN 07P MODE
!---------------------------------------------------------------------
      SUBROUTINE RUN07PMODE(NRHS, PRHS)
!
      IMPLICIT NONE
      MWPOINTER PRHS(*), mxDuplicateArray
      MWSIZE  NRHS
      CHARACTER(LEN=63) :: mxGetClassName
      EXTERNAL AUTOSTOP
      MWSIZE  mexAtExit
      MWSIZE  k
!     
!     Run the main continuation package "AUTO"
!     
      CALL AUTO()
!     
      k=mexAtExit(AUTOSTOP)
!            
      RETURN
      END
!
!---------------------------------------------------------------------
!   RUN IN DST MODE
!---------------------------------------------------------------------
      SUBROUTINE RUNDSTMODE(NLHS, PLHS, NRHS, PRHS)
!
      IMPLICIT NONE
      MWPOINTER PLHS(*), PRHS(*), mxDuplicateArray
      MWSIZE  NLHS, NRHS
      CHARACTER(LEN=63) :: mxGetClassName
      EXTERNAL AUTOSTOP
      MWSIZE  mexAtExit
      MWSIZE  k
!
!   Check for proper number of arguments and object checks
!
      if( NRHS /= 4 ) then
        call mexErrMsgTxt("Need exactly four auto input arguments")
        k=mexAtExit(AUTOSTOP)
      endif
      if( mxGetClassName(PRHS(1)) /= "autosimopts" ) then
        call mexErrMsgTxt("Make sure an AUTO simulation properties objec
     &t AUTOSIMOPTS is being passed as the first argument into the RUNAU
     &TODST function of the AUTO object")
        k=mexAtExit(AUTOSTOP)
      endif
      if( mxGetClassName(PRHS(2)) /= "autoconstants" ) then
        call mexErrMsgTxt("Make sure an AUTO constants object AUTOCONSTA
     &NTS is being passed as the second argument into the RUNAUTODST fun
     &ction of the AUTO object")
        k=mexAtExit(AUTOSTOP)
      endif
      if( mxGetClassName(PRHS(3)) /= "autof7" ) then
        call mexErrMsgTxt("Make sure an AUTO f7 object AUTOF7 is being p
     &assed as the third argument into the RUNAUTODST function of the AU
     &TO object")
        k=mexAtExit(AUTOSTOP)
      endif
      if( mxGetClassName(PRHS(4)) /= "autof8" ) then
        call mexErrMsgTxt("Make sure an AUTO f8 object AUTOF8 is being p
     &assed as the fourth argument into the RUNAUTODST function of the A
     &UTO object")
        k=mexAtExit(AUTOSTOP)
      endif
      if( NLHS > 2 ) then
        call mexErrMsgTxt("Too many outputs")
        k=mexAtExit(AUTOSTOP)
      endif
!
!
!     Assign DST objects to AUTO
!
      PLHS(1)=mxDuplicateArray(PRHS(3))
      PLHS(2)=mxDuplicateArray(PRHS(4))
!      
      CALL COPYDSTOBJECTSTOAUTO(PRHS)
!
!
!      Run the main continuation package "AUTO"
!
      CALL AUTO()    
!
!    Assign AUTO outputs back to DST object
!
      CALL COPYAUTOTODSTOBJECTS(PLHS,PRHS)
!
!    Deallocate output variables for F7 and F8
!
      CALL DEALLOCATEINF7F8() 
      
      k=mexAtExit(AUTOSTOP)
            
      RETURN
      END
