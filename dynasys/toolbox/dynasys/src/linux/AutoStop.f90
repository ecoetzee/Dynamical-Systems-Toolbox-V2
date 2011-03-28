!========================================================================
! AUTOstop.F - Function file "AUTOstop.F" called by AUTO
!
!   This subroutine contains the commands to stop AUTO and return to a 
!   MATLAB command prompt.
!
!   All open files are closed and then a Matlab error message is issued
!   if needed.
!
!   Written by ETIENNE COETZEE
!
!   $Revision: 1.0.0.0 $ $Date: 2008/12/16 11:17:00$
!
!========================================================================
!
      SUBROUTINE AUTOSTOP()
!
      USE AUTO_CONSTANTS, ONLY:FORT7DST,FORT8DST,FORT9DST
!      
      LOGICAL opened       
!                 
! Check to see if files are open, and if so, close them
        INQUIRE(UNIT=2,OPENED=opened)
        IF (opened.EQV..TRUE.) THEN
          CLOSE(2, status = 'delete')
        ENDIF
        
        INQUIRE(UNIT=3,OPENED=opened)
        IF (opened.EQV..TRUE.) THEN
          CLOSE(3, status = 'delete')
        ENDIF
        
        INQUIRE(UNIT=6,OPENED=opened)
        IF (opened.EQV..TRUE.) THEN
          CLOSE(6)
        ENDIF
        
        INQUIRE(UNIT=7,OPENED=opened)
        IF (opened.EQV..TRUE.) THEN
          IF(FORT7DST==1)THEN
            CLOSE(7)
          ELSE
            CLOSE(7, status = 'delete')
          ENDIF
        ENDIF
        
        INQUIRE(UNIT=8,OPENED=opened)
        IF (opened.EQV..TRUE.) THEN
          IF(FORT8DST==1)THEN
            CLOSE(8)
          ELSE
            CLOSE(8, status = 'delete')
          ENDIF
        ENDIF
        
        INQUIRE(UNIT=9,OPENED=opened)
        IF (opened.EQV..TRUE.) THEN
          IF(FORT9DST==1)THEN
            CLOSE(9)
          ELSE
            CLOSE(9, status = 'delete')
          ENDIF
        ENDIF
           
        RETURN
       
      END
      
!---------------------------------------------------------------------
      SUBROUTINE AUTOSTOPWITHERROR(MSG)
!
      USE AUTO_CONSTANTS, ONLY:FORT7DST,FORT8DST,FORT9DST
!
! Replace integer*4 by integer*8 on 64-bit platforms
      CHARACTER (LEN=*) MSG
      INTEGER  STRL
      LOGICAL opened
!      INTEGER*4  mexSetTrapFlag
!      INTEGER*4  trapflag
!
!C Return to Matlab error function if error occurs. Needed below.
!        trapflag=0
!        k=mexSetTrapFlag(trapflag) 
      
! Determine string length
        STRL=LEN_TRIM(MSG)
        
! Check to see if files are open, and if so, close them
        INQUIRE(UNIT=2,OPENED=opened)
        IF (opened.EQV..TRUE.) THEN
          CLOSE(2, status = 'delete')
        ENDIF
        
        INQUIRE(UNIT=3,OPENED=opened)
        IF (opened.EQV..TRUE.) THEN
          CLOSE(3, status = 'delete')
        ENDIF
        
        INQUIRE(UNIT=6,OPENED=opened)
        IF (opened.EQV..TRUE.) THEN
          CLOSE(6)
        ENDIF
        
        INQUIRE(UNIT=7,OPENED=opened)
        IF (opened.EQV..TRUE.) THEN
          IF(FORT7DST==1)THEN
            CLOSE(7)
          ELSE
            CLOSE(7, status = 'delete')
          ENDIF
        ENDIF
        
        INQUIRE(UNIT=8,OPENED=opened)
        IF (opened.EQV..TRUE.) THEN
          IF(FORT8DST==1)THEN
            CLOSE(8)
          ELSE
            CLOSE(8, status = 'delete')
          ENDIF
        ENDIF
        
        INQUIRE(UNIT=9,OPENED=opened)
        IF (opened.EQV..TRUE.) THEN
          IF(FORT9DST==1)THEN
            CLOSE(9)
          ELSE
            CLOSE(9, status = 'delete')
          ENDIF
        ENDIF
        
! Deallocate F7 and F8 variables        
        CALL DEALLOCATEINF7F8()
           
! If no message just stop, otherwise throw error.
! Use "clear mex" at Matlab prompt or script to free allocated
! memory.
        IF (STRL.LT.1) THEN
          MSG='AUTO stopped due to unknown error'
          CALL mexErrMsgTxt(MSG)
        ELSE
          CALL mexErrMsgTxt(MSG)
        ENDIF

      RETURN  
      END 



