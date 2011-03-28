!========================================================================
! NOTE!!!! CHANGE ALL mwSize*4 TO mwSize*8 FOR 64-BIT MACHINES
!========================================================================
! FUNC.F - Function file "FUNC.F" called by AUTO
!
!   This subroutine contains the commands to call the derivative
!   values from the AUTOEQN.m file in the FUNC subroutine, and also 
!   loads the initial states from the STPNT subroutine. Boundary value
!   problems are analysed with the BCND subroutine. This version 
!   is thus compatible with calling routines for MATLAB functions.
!
!   Adapted by Etienne COETZEE, James RANKIN, Phani THOTA  
!   from the University of Bristol
!
!   $Revision: 2.0.0.0 $ $Date: 2010/07/29 14:02:00$
!
!========================================================================
!
!------------------------------------------------------------------------
!      MAIN FUNCTION THAT CALLS DERIVATIVE VALUES FROM "AUTOEQN" file
!------------------------------------------------------------------------
!   Evaluates the algebraic equations or ODE right hand side
!
!   Input arguments :
!        NDIM   :   Dimension of the ODE system 
!        U      :   State variables
!        ICP    :   Array indicating the free parameter(s)
!        PAR    :   Equation parameters
!
!   Values to be returned :
!        F      :   ODE right hand side values
!
!   Normally unused Jacobian arguments : IJAC, DFDU, DFDP (see manual)
!
!------------------------------------------------------------------------
#include "fintrf.h" 
!
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!
      USE AUTO_CONSTANTS, ONLY:FUNCFILENAME,ODIM,OUT,NOUTX,NPARX,NBIFX,NIAP,NRAP
!    
      IMPLICIT NONE 
!
! MATLAB MX AND MEX FUNCTIONS AND POINTERS
      mwPointer mexCallMatlabWithTrap
      mwPointer MSGptr
      mwPointer mxCreateDoubleMatrix
      mwPointer mxCreateNumericArray
      mwPointer mxGetPr
      mwPointer mxGetProperty
      mwPointer PLHS(4)
      mwPointer PRHS(3)
      mwPointer PRHSptr      
!
      mwIndex:: I = 1
!      
      mwSize    ICP(*)          
      mwSize    IJAC
      mwSize    K
      mwSize :: MSGSTRLEN=300
      mwSize    mxClassIDFromClassName
      mwSize    mxCreateString
      mwSize    mxGetString
      mwSize    mxIsDouble
      mwSize    mxGetNumberOfElements  
      mwSize    N
      mwSize    NANptr
      mwSize    NDIM
      mwSize    NUMEL
      mwSize    STATUS            
!
      REAL*8    DFDU(NDIM,NDIM)  
      REAL*8    DFDP(NDIM,NDIM)
      REAL*8    F(NDIM) 
      REAL*8    PAR(*) 
      REAL*8    U(NDIM)
!      
      CHARACTER*300 MSGSTR
!      
      LOGICAL, EXTERNAL :: NUMSTAB     
!
! Define the parameter variable in MATLAB environment 
! 
      PRHS(1) = mxCreateDoubleMatrix(NPARX,1,0) 
      CALL mxCopyReal8ToPtr(PAR,mxGetPr(PRHS(1)),NPARX) 
! 
! Trap any Not a Number occurences and throw error 
!
      NANptr = mxGetPr(PRHS(1))
      NUMEL = mxGetNumberOfElements(PLHS(1))
      IF( NUMSTAB(%VAL(NANptr),NUMEL) ) THEN
          WRITE(MSGSTR,104)TRIM(FUNCFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)  
      ENDIF
! 
! Define the state variable in MATLAB environment 
! 
      PRHS(2) = mxCreateDoubleMatrix(NDIM,1,0) 
      CALL mxCopyReal8ToPtr(U,mxGetPr(PRHS(2)),NDIM) 
! 
! Define the derivatives flag variable in MATLAB environment 
! 
      PRHS(3) = mxCreateNumericArray(1,1,mxClassIDFromClassName('int32'),0) 
      CALL mxCopyInteger4ToPtr(IJAC,mxGetPr(PRHS(3)),1) 
! 
! Call the m-file FUNC file for derivatives 
! 
      K = mexCallMatlabWithTrap(4,PLHS,3,PRHS,TRIM(FUNCFILENAME)) 
      CALL mxDestroyArray(PRHS(3)) 
      CALL mxDestroyArray(PRHS(2)) 
      CALL mxDestroyArray(PRHS(1)) 
!
!      
      IF( K /= 0 ) THEN 
          MSGptr=mxGetProperty(K,I,'message') 
          MSGSTR=''
          STATUS=mxGetString(MSGptr, MSGSTR, MSGSTRLEN); 
          CALL mxDestroyArray(MSGptr)
          CALL AUTOSTOPWITHERROR(MSGSTR)
      ENDIF
! 
!      
! Copy the results back to Fortran variable. 
! Check the F return variable first. 
!          
      IF( PLHS(1) == 0 .OR. mxIsDouble(PLHS(1)) == 0 ) THEN 
          WRITE(MSGSTR,101)TRIM(FUNCFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)         
      ENDIF
      IF( mxGetNumberOfElements(PLHS(1)) == NDIM ) THEN 
          CALL mxcopyPtrToReal8(mxGetPr(PLHS(1)),F,NDIM) 
      ELSE 
          WRITE(MSGSTR,102)TRIM(FUNCFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)  
      ENDIF 
      CALL mxDestroyArray(PLHS(1)) 
!      
! Check OUT variable
! 
      IF( PLHS(2) == 0 .OR. mxIsDouble(PLHS(2)) == 0 ) THEN 
          WRITE(MSGSTR,103)TRIM(FUNCFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)  
      ENDIF
      ODIM=mxGetNumberOfElements(PLHS(2))
      IF( ODIM > NOUTX )THEN
         ODIM=NOUTX
      ENDIF
      IF(ALLOCATED(OUT))DEALLOCATE(OUT)
      ALLOCATE(OUT(ODIM))
      CALL mxcopyPtrToReal8(mxGetPr(PLHS(2)),OUT,ODIM) 
      CALL mxDestroyArray(PLHS(2)) 
!
! Copy the DFDU result back to Fortran variable. 
!          
      NUMEL=mxGetNumberOfElements(PLHS(3))
      IF( NUMEL > 0 .AND. NUMEL .LE. NDIM**2 .AND. IJAC /= 0 )THEN
          CALL mxcopyPtrToReal8(mxGetPr(PLHS(3)),DFDU,NUMEL) 
      ELSEIF( NUMEL > 0  .AND. IJAC /= 0 ) THEN
          WRITE(MSGSTR,105)TRIM(FUNCFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)  
      ENDIF
      CALL mxDestroyArray(PLHS(3)) 
!  
! Copy the DFDP result back to Fortran variable. 
!     
      NUMEL=mxGetNumberOfElements(PLHS(4))     
      IF( NUMEL > 0 .AND. NUMEL .LE. NDIM**2  .AND. IJAC /= 0 ) THEN
          CALL mxcopyPtrToReal8(mxGetPr(PLHS(4)),DFDP,NUMEL) 
      ELSEIF( NUMEL > 0  .AND. IJAC /= 0 ) THEN
          WRITE(MSGSTR,106)TRIM(FUNCFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)  
      ENDIF
      CALL mxDestroyArray(PLHS(4))  
!         
101   FORMAT('\n\nError in definition of F in ',A,' subroutine. \n\n Che& 
      ck definition of F')         
102   FORMAT('\n\nError in definition of F in ',A,' subroutine. \n\n Cha&
           nge the size of F to match NDIM')
103   FORMAT('\n\nError in definition of OUT in ',A,' subroutine. \n\n C&
           heck definition of OUT')     
104   FORMAT('\n\nNumerical instability, NaN or Inf detected in subrouti&
           ne ',A,', adjust the tolerances of the bifurcation parameters')
105   FORMAT('\n\nError in definition of DFDU in ',A,' subroutine. \n\n& 
           Change the size of DFDU to be less or equal to NDIM^2') 
106   FORMAT('\n\nError in definition of DFDP in ',A,' subroutine. \n\n& 
           Change the size of DFDP to be less or equal to NDIM^2')           
      RETURN 
      END
!------------------------------------------------------------------------
!             INITIAL VALUES LOADED FROM STPNT SUBROUTINE   
!------------------------------------------------------------------------
!
! Input arguments :
!      NDIM   :   Dimension of the ODE system 
!
! Values to be returned :
!      U      :   A starting solution vector
!      PAR    :   The corresponding equation-parameter values
!      T      :	  Not used here
!
!------------------------------------------------------------------------
!
      SUBROUTINE STPNT(NDIM,U,PAR,T)
!
      USE AUTO_CONSTANTS,ONLY:STPNTFILENAME,PARDST0,UDST0,OUTDST0,ODIM, &
           OUT,NOUTX,NPARX,NBIFX,NIAP,NRAP,RUNMODEDST,IPS
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      mwPointer mexCallMatlabWithTrap
      mwPointer MSGptr
      mwPointer mxCreateDoubleMatrix 
      mwPointer mxGetPr
      mwPointer mxGetProperty
      mwPointer PLHS(3)
      mwPointer PRHS(1)
      mwPointer PRHSptr
!  
      mwIndex:: I = 1
!         
      mwSize    K
      mwSize :: MSGSTRLEN=300
      mwSize    mxCreateString
      mwSize    mxGetString
      mwSize    mxIsDouble
      mwSize    mxGetNumberOfElements      
      mwSize :: N      
      mwSize    NANptr
      mwSize    NUMEL
      mwSize    PDIM
      mwSize    STATUS      
      mwSize    UDIM
!      
      REAL*8    U(NDIM) 
      REAL*8    PAR(NPARX)
      REAL*8    T
!     
      CHARACTER*300 MSGSTR
!
      LOGICAL, EXTERNAL :: NUMSTAB       
!
      IF(RUNMODEDST == 0.OR.IPS==-2.OR.IPS==14.OR.IPS==16.OR.IPS==17)THEN
       GOTO 201
      ELSE
       GOTO 202
      ENDIF
!
! Define the parameter variable in MATLAB environment 
! 
201   PRHS(1) = mxCreateDoubleMatrix(1,1,0) 
      CALL mxCopyReal8ToPtr(T,mxGetPr(PRHS(1)),1) 
! 
! Call the m-file AUTOEQN for derivatives 
! 
      K = mexCallMatlabWithTrap(3,PLHS,1,PRHS,TRIM(STPNTFILENAME)) 
!
!      
      IF( K /= 0 ) THEN 
          MSGptr=mxGetProperty(K,I,'message') 
          MSGSTR=''
          STATUS=mxGetString(MSGptr, MSGSTR, MSGSTRLEN); 
          CALL mxDestroyArray(MSGptr)
!     
          CALL AUTOSTOPWITHERROR(MSGSTR)
      ENDIF
! 
! Copy the results back to Fortran variable. 
!
! Check the PAR return variable first. 
! 
      IF( PLHS(1) == 0 .OR. mxIsDouble(PLHS(1)) == 0 ) THEN 
          WRITE(MSGSTR,211)TRIM(STPNTFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)         
      ENDIF
      PDIM = mxGetNumberOfElements(PLHS(1))
      CALL mxcopyPtrToReal8(mxGetPr(PLHS(1)),PAR,PDIM) 
      CALL mxDestroyArray(PLHS(1)) 
!
! Check the U return variable. 
! 
      IF( PLHS(2) == 0 .OR. mxIsDouble(PLHS(2)) == 0 ) THEN 
          WRITE(MSGSTR,212)TRIM(STPNTFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)          
      ENDIF
      IF( mxGetNumberOfElements(PLHS(2)) == NDIM ) THEN 
          CALL mxcopyPtrToReal8(mxGetPr(PLHS(2)),U,NDIM) 
      ELSE
          WRITE(MSGSTR,213)STPNTFILENAME
          CALL AUTOSTOPWITHERROR(MSGSTR)     
      ENDIF 
      CALL mxDestroyArray(PLHS(2))       
!
! Check OUT variable
! 
      IF( PLHS(3) == 0 .OR. mxIsDouble(PLHS(3)) == 0 ) THEN 
          WRITE(MSGSTR,214)TRIM(STPNTFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)   
      ENDIF
      NOUTX=mxGetNumberOfElements(PLHS(3))
      ODIM=NOUTX
      IF(ALLOCATED(OUT))DEALLOCATE(OUT)
      ALLOCATE(OUT(ODIM))
      CALL mxcopyPtrToReal8(mxGetPr(PLHS(3)),OUT,ODIM) 
      CALL mxDestroyArray(PLHS(3)) 
!      
      GOTO 203
!
! Obtain data from variables that were passed from MATLAB object  
202   PDIM=SIZE(PARDST0)
      IF( PDIM==0 ) THEN
           CALL AUTOSTOPWITHERROR('Par0 is empty. Define correctly.') 
      ENDIF
      DO I=1,PDIM
        PAR(I)=PARDST0(I)
      ENDDO
!      
      UDIM=SIZE(UDST0)
      IF( PDIM==0 ) THEN
           CALL AUTOSTOPWITHERROR('U0 is empty. Define correctly.') 
      ENDIF
      DO I=1,UDIM
        U(I)=UDST0(I)
      ENDDO
!      
      NOUTX=SIZE(OUTDST0)
      ODIM=NOUTX
      IF(ALLOCATED(OUT))DEALLOCATE(OUT)
      ALLOCATE(OUT(NOUTX))
      IF( NOUTX > 0 )THEN  
        DO I=1,NOUTX
          OUT(I)=OUTDST0(I)
        ENDDO
      ENDIF
!
!   
211   FORMAT('\n\nError in definition of PAR in ',A,' subroutine. \n\n C&
           heck definition of PAR')
212   FORMAT('\n\nError in definition of U in ',A,' subroutine. \n\n Che&
           ck definition of U')   
213   FORMAT('\n\nError in definition of U in ',A,' subroutine. \n\n Cha&
           nge the size of U to match NDIM')
214   FORMAT('\n\nError in definition of OUT in ',A,' subroutine. \n\n C&
           heck definition of OUT')
!                   
203   RETURN
      END
!------------------------------------------------------------------------
!             BOUNDARY VALUE ANALYSIS   
!------------------------------------------------------------------------
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
!      
      USE AUTO_CONSTANTS, ONLY:BCNDFILENAME,ODIM,OUT,NOUTX,NPARX,NBIFX, &
           NIAP,NRAP  
!    
! Arguments
! 
      IMPLICIT NONE 
!
! MATLAB MX AND MEX FUNCTIONS AND POINTERS
      mwPointer mexCallMatlabWithTrap
      mwPointer MSGptr
      mwPointer mxCreateDoubleMatrix
      mwPointer mxCreateNumericArray
      mwPointer mxGetPr
      mwPointer mxGetProperty
      mwPointer PLHS(3)
      mwPointer PRHS(4)
      mwPointer PRHSptr      
!      
      mwIndex:: I = 1
!      
      mwSize    IJAC
      mwSize    K
      mwSize :: MSGSTRLEN=300
      mwSize    mxClassIDFromClassName
      mwSize    mxCreateString
      mwSize    mxGetString
      mwSize    mxGetNumberOfElements  
      mwSize    mxIsDouble
      mwSize :: N
      mwSize    NANptr
      mwSize    NBC
      mwSize    NDIM
      mwSize    NUMEL
      mwSize    STATUS
!     
      REAL*8    DBC(NBC,*)
      REAL*8    FB(NBC)
      REAL*8    ICP(*)
      REAL*8    PAR(*)
      REAL*8    U0(NDIM)
      REAL*8    U1(NDIM)      
!
      CHARACTER*300 MSGSTR
!
      LOGICAL, EXTERNAL :: NUMSTAB     
!      
! Define the parameter variable in MATLAB environment 
! 
      PRHS(1) = mxCreateDoubleMatrix(NPARX,1,0) 
      CALL mxCopyReal8ToPtr(PAR,mxGetPr(PRHS(1)),NPARX) 
!      
! Trap any Not a Number occurences and throw error 
!
      NANptr = mxGetPr(PRHS(1))
      NUMEL = mxGetNumberOfElements(PLHS(1))
      IF( NUMSTAB(%VAL(NANptr),NUMEL) ) THEN
          WRITE(MSGSTR,304)TRIM(BCNDFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)   
      ENDIF
! 
! Define the lower bounds of states in MATLAB environment 
! 
      PRHS(2) = mxCreateDoubleMatrix(NDIM,1,0) 
      CALL mxCopyReal8ToPtr(U0,mxGetPr(PRHS(2)),NDIM) 
! 
! Define the upper bounds of states in MATLAB environment 
! 
      PRHS(3) = mxCreateDoubleMatrix(NDIM,1,0) 
      CALL mxCopyReal8ToPtr(U1,mxGetPr(PRHS(3)),NDIM) 
!      
! Define the derivatives flag variable in MATLAB environment 
! 
      PRHS(4) = mxCreateNumericArray(1,1,mxClassIDFromClassName('int32'),0) 
      CALL mxCopyInteger4ToPtr(IJAC,mxGetPr(PRHS(4)),1)      
! 
! Call the m-file AUTOEQN for boundary values 
! 
      K = mexCallMatlabWithTrap(3,PLHS,4,PRHS,TRIM(BCNDFILENAME)) 
      CALL mxDestroyArray(PRHS(4))
      CALL mxDestroyArray(PRHS(3)) 
      CALL mxDestroyArray(PRHS(2)) 
      CALL mxDestroyArray(PRHS(1)) 
!
      IF( K /= 0 ) THEN 
          MSGptr=mxGetProperty(K,I,'message') 
          MSGSTR=''
          STATUS=mxGetString(MSGptr, MSGSTR, MSGSTRLEN); 
          CALL mxDestroyArray(MSGptr)
!     
          CALL AUTOSTOPWITHERROR(MSGSTR)
      ENDIF
!      
!      
! Copy the results back to Fortran variable. 
! Check the FB return variable first. 
! 
      IF( PLHS(1) == 0 .OR. mxIsDouble(PLHS(1)) == 0 ) THEN 
          WRITE(MSGSTR,301)TRIM(BCNDFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)           
      ENDIF
      IF( mxGetNumberOfElements(PLHS(1)) == NBC ) THEN 
          CALL mxcopyPtrToReal8(mxGetPr(PLHS(1)),FB,NBC) 
      ELSE 
          WRITE(MSGSTR,302)TRIM(BCNDFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)    
      ENDIF 
      CALL mxDestroyArray(PLHS(1)) 
!      
! Check OUT variable
! 
      IF( PLHS(2) == 0 .OR. mxIsDouble(PLHS(2)) == 0 ) THEN 
          WRITE(MSGSTR,303)TRIM(BCNDFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)   
      ENDIF
      ODIM=mxGetNumberOfElements(PLHS(2))
      IF( ODIM > NOUTX )THEN
         ODIM=NOUTX
      ENDIF
      IF(ALLOCATED(OUT))DEALLOCATE(OUT)
      ALLOCATE(OUT(ODIM))
      CALL mxcopyPtrToReal8(mxGetPr(PLHS(2)),OUT,ODIM) 
      CALL mxDestroyArray(PLHS(2)) 
!      
! Copy the DBC result back to Fortran variable. 
!          
      NUMEL = mxGetNumberOfElements(PLHS(3))
      IF( NUMEL < 100 ) THEN
           CALL mxcopyPtrToReal8(mxGetPr(PLHS(3)),DBC,NUMEL) 
      ENDIF
      CALL mxDestroyArray(PLHS(3))       
!      
!  
301   FORMAT('\n\nError in definition of FB in ',A,' subroutine. \n\n Ch&
           eck definition of FB')  
302   FORMAT('Mismatch between NBC and number of boundary conditions pas&
           sed from ',A)  
303   FORMAT('\n\nError in definition of OUT in ',A,' subroutine. \n\n C&
           heck definition of OUT')   
304   FORMAT('\n\nNumerical instability, NaN or Inf detected in subrouti&
           ne ',A,', adjust the tolerances of the bifurcation parameters')
!       
      RETURN
      END
! 
!------------------------------------------------------------------------
!           THE FOLLOWING SUBROUTINES NOT USED HERE
!------------------------------------------------------------------------
! See AUTO manual for its use,
      SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
!          
      USE AUTO_CONSTANTS, ONLY:ICNDFILENAME,ODIM,OUT,NOUTX,NPARX,NBIFX, &
           NIAP,NRAP     
!
! MATLAB MX AND MEX FUNCTIONS AND POINTERS
      mwPointer MSGptr
      mwPointer mexCallMatlabWithTrap
      mwPointer mxCreateDoubleMatrix
      mwPointer mxCreateNumericArray
      mwPointer mxGetPr
      mwPointer mxGetProperty
      mwPointer PLHS(3)
      mwPointer PRHS(3)
      mwPointer PRHSptr      
!      
      mwIndex:: I = 1
!      
      mwSize    ICP(*)          
      mwSize    IJAC
      mwSize    K
      mwSize :: MSGSTRLEN=300
      mwSize    mxClassIDFromClassName
      mwSize    mxCreateString
      mwSize    mxGetString
      mwSize    mxIsDouble
      mwSize    mxGetNumberOfElements  
      mwSize    N
      mwSize    NANptr
      mwSize    NDIM
      mwSize    NINT
      mwSize    NUMEL
      mwSize    STATUS
!      
      REAL*8    DINT(NINT,*)
      REAL*8    FI(NINT)
      REAL*8    PAR(*) 
      REAL*8    U(NDIM)
      REAL*8    UDOT(NDIM)
      REAL*8    UOLD(NDIM)
      REAL*8    UPOLD(NDIM) 
!      
      CHARACTER*300 MSGSTR
!      
      LOGICAL, EXTERNAL :: NUMSTAB     
!
! Define the parameter variable in MATLAB environment 
! 
      PRHS(1) = mxCreateDoubleMatrix(NPARX,1,0) 
      CALL mxCopyReal8ToPtr(PAR,mxGetPr(PRHS(1)),NPARX) 
! 
! Trap any Not a Number occurences and throw error 
!
      NANptr = mxGetPr(PRHS(1))
      NUMEL = mxGetNumberOfElements(PLHS(1))
      IF( NUMSTAB(%VAL(NANptr),NUMEL) ) THEN
          WRITE(MSGSTR,404)TRIM(ICNDFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)   
      ENDIF
! 
! Define the state variable in MATLAB environment 
! 
      PRHS(2) = mxCreateDoubleMatrix(NDIM,1,0) 
      CALL mxCopyReal8ToPtr(U,mxGetPr(PRHS(2)),NDIM) 
! 
! Define the derivatives flag variable in MATLAB environment 
! 
      PRHS(3) = mxCreateNumericArray(1,1,mxClassIDFromClassName('int32'),0) 
      CALL mxCopyInteger4ToPtr(IJAC,mxGetPr(PRHS(3)),1) 
! 
! Call the m-file ICND file for derivatives 
! 
      K = mexCallMatlabWithTrap(3,PLHS,3,PRHS,TRIM(ICNDFILENAME)) 
      CALL mxDestroyArray(PRHS(3)) 
      CALL mxDestroyArray(PRHS(2)) 
      CALL mxDestroyArray(PRHS(1)) 
!
!      
      IF( K /= 0 ) THEN 
          MSGptr=mxGetProperty(K,I,'message') 
          MSGSTR=''
          STATUS=mxGetString(MSGptr, MSGSTR, MSGSTRLEN); 
          CALL mxDestroyArray(MSGptr)
          CALL AUTOSTOPWITHERROR(MSGSTR)
      ENDIF
!        
! Copy the results back to Fortran variable. 
! Check the FB return variable first. 
! 
      IF( PLHS(1) == 0 .OR. mxIsDouble(PLHS(1)) == 0 ) THEN 
          WRITE(MSGSTR,401)TRIM(ICNDFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)           
      ENDIF
      IF( mxGetNumberOfElements(PLHS(1)) == NINT ) THEN 
          CALL mxcopyPtrToReal8(mxGetPr(PLHS(1)),FI,NINT) 
      ELSE 
          WRITE(MSGSTR,402)TRIM(ICNDFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)    
      ENDIF 
      CALL mxDestroyArray(PLHS(1)) 
!      
! Check OUT variable
! 
      IF( PLHS(2) == 0 .OR. mxIsDouble(PLHS(2)) == 0 ) THEN 
          WRITE(MSGSTR,403)TRIM(ICNDFILENAME)
          CALL AUTOSTOPWITHERROR(MSGSTR)   
      ENDIF
      ODIM=mxGetNumberOfElements(PLHS(2))
      IF( ODIM > NOUTX )THEN
         ODIM=NOUTX
      ENDIF
      IF(ALLOCATED(OUT))DEALLOCATE(OUT)
      ALLOCATE(OUT(ODIM))
      CALL mxcopyPtrToReal8(mxGetPr(PLHS(2)),OUT,ODIM) 
      CALL mxDestroyArray(PLHS(2)) 
!      
! Copy the DINT result back to Fortran variable. 
!          
      NUMEL = mxGetNumberOfElements(PLHS(3))
      IF( NUMEL < 100 ) THEN
           CALL mxcopyPtrToReal8(mxGetPr(PLHS(3)),DINT,NUMEL) 
      ENDIF
      CALL mxDestroyArray(PLHS(3))       
! 
!         
401   FORMAT('\n\nError in definition of FI in ',A,' subroutine. \n\n Ch&
           eck definition of F')         
402   FORMAT('\n\nError in definition of FI in ',A,' subroutine. \n\n Ch&
           ange the size of F to match NDIM')
403   FORMAT('\n\nError in definition of OUT in ',A,' subroutine. \n\n C&
           heck definition of OUT')     
404   FORMAT('\n\nNumerical instability, NaN or Inf detected in subrouti&
           ne ',A,', adjust the tolerances of the bifurcation parameters') 
      RETURN
      END 
!      
!------------------------------------------------------------------------
! 
      SUBROUTINE FOPT(NDIM,U,ICP,PAR,IJC,F1,DFDU,DFDP)
      CALL AUTOSTOPWITHERROR('\n\nFOPT function not implemented yet')
      END 
!
      SUBROUTINE PVLS(NDM,UPS,PAR)
      RETURN
      END
!
!------------------------------------------------------------------------
! Function to check numerical stability. Check for Not a Number (NaN) or
! Inf
!
      LOGICAL FUNCTION NUMSTAB(X,N)
!      
      mwSize  mxIsFinite
      REAL*8 X(N)

      IF( mxIsFinite(X) == 1 ) THEN
          NUMSTAB = .FALSE.
          RETURN
      ENDIF
      NUMSTAB = .TRUE.
      RETURN
 
      END FUNCTION


