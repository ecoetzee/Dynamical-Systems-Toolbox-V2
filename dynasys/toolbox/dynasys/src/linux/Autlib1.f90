!========================================================================
! AUTLIB1.F - Main program for AUTO
!
!   This subroutine is the main program for AUTO07. This version has been
!   modified to run with Matlab. Use Examdiff to get obtain differences
!
!   Auto version          : AUTO 07-P
!   Original Source files : auto07p-0.5.tar.gz  
!   Web-site              : http://indy.cs.concordia.ca/auto/
!   MATLAB version        : 2008a
!
!   Adapted by Etienne COETZEE, James RANKIN, Phani THOTA  
!   from the University of Bristol
!
!   $Revision: 2.0.0.0 $ $Date: 2010/07/29 14:02:00$
!
!========================================================================
!
      SUBROUTINE AUTO()
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT2DST,FORT3DST, &
           FORT7DST,FORT8DST,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      LOGICAL FOUND,EOF,READ2EOF
! Local
      DIMENSION IAP(NIAP),RAP(NRAP)
      DIMENSION PAR(2*NPARX)
      CHARACTER(10000) MATLABDISPSTR 
!
! Initialization :
!
      CALL MPIINI(IAP)
!
      IF (FORT2DST == 1)THEN
       OPEN(2,FILE='fort.2',STATUS='old',ACCESS='sequential')
      ENDIF
      READ2EOF=.FALSE.
!
      IF (FORT3DST == 1)THEN  
       OPEN(3,FILE='fort.3',STATUS='unknown',ACCESS='sequential')
      ENDIF
!
      IF (FORT7DST == 1)THEN             
       OPEN(7,FILE='fort.7',STATUS='unknown',ACCESS='sequential')
      ENDIF
!
      IF (FORT8DST == 1)THEN  
       OPEN(8,FILE='fort.8',STATUS='unknown',ACCESS='sequential')
      ENDIF
!
      IF (FORT9DST == 1)THEN        
       OPEN(9,FILE='fort.9',STATUS='unknown',ACCESS='sequential')
      ENDIF
!
 1     IF(IAP(39).GT.1)THEN
         CALL MPITIM(TIME0)
       ELSE
         CALL AUTIM0(TIME0)
       ENDIF
       FOUND=.FALSE.
       CALL INIT(IAP,RAP,PAR,EOF,READ2EOF)
       IF(EOF)THEN
         CALL MPIEND()
         CALL AUTOSTOP() 
         RETURN
       ENDIF
!
! Find restart label and determine type of restart point.
!
       IRS=IAP(3)
       NFPR=IAP(29)
!
       IF(IRS.GT.0) THEN
         CALL FINDLB(IAP,IRS,NFPR,FOUND)
         IAP(29)=NFPR
         IF(.NOT.FOUND) THEN
          WRITE(MATLABDISPSTR,400)IRS
          CALL AUTOSTOPWITHERROR(MATLABDISPSTR)
         ENDIF
       ENDIF
!
       CALL MPIIAP(IAP)
       CALL AUTOI(IAP,RAP,PAR)
!-----------------------------------------------------------------------
!
      IF(IAP(39).GT.1)THEN
        CALL MPITIM(TIME1)
      ELSE
        CALL AUTIM1(TIME1)
      ENDIF
      TOTTIM=TIME1-TIME0
      CALL WRBAR("=",47)
      IF(FORT9DST==1)WRITE(9,301)TOTTIM
      CALL MEXPRINTF(achar(10))
      WRITE(MATLABDISPSTR,301)TOTTIM
      CALL MEXPRINTF(MATLABDISPSTR)
      CALL MEXPRINTF(achar(10))
      CALL MEXEVALSTRING("drawnow;")
      CALL CLEANUP()
      GOTO 1
!
 301  FORMAT(' Total Time ',E12.3)
!
! Error Message.
 400  FORMAT(' Restart label ',I4,' not found')
!
      END
!----------------------------------------------------------------
      SUBROUTINE AUTOI(IAP,RAP,PAR)
!
      USE INTERFACES
      USE AUTO_CONSTANTS
      USE HOMCONT, ONLY:FNHO,BCHO,ICHO,PVLSHO,STPNHO
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      EXTERNAL STPNUS,STPNAE,STPNBV,STPNUB
      EXTERNAL PVLSBV
!
      INTEGER IAP(*)
      CHARACTER(10000) MATLABDISPSTR
!
      ITP=IAP(27)
      NFPR=IAP(29)
!
      IF(IAP(38)==0)THEN
        CALL INIT1(IAP,RAP,ICP,PAR)
        CALL CHDIM(IAP)
      ENDIF
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  One-parameter continuations
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       IF((IPS.EQ.0.OR.IPS.EQ.1) .AND. ABS(ISW).LE.1 ) THEN
!        ** Algebraic systems.
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(IAP,RAP,PAR,ICP,FUNI,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(IAP,RAP,PAR,ICP,FUNI,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
!
       ELSE IF(IPS.EQ.11 .AND. ABS(ISW).LE.1 ) THEN
!        ** Waves : Spatially homogeneous solutions,
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(IAP,RAP,PAR,ICP,FNWS,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(IAP,RAP,PAR,ICP,FNWS,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
!
       ELSE IF((IPS.EQ.-1) .AND. ABS(ISW).LE.1 ) THEN
!        ** Discrete dynamical systems : fixed points.
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(IAP,RAP,PAR,ICP,FNDS,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(IAP,RAP,PAR,ICP,FNDS,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
!
       ELSE IF(IPS.EQ.-2) THEN
!        ** Time integration.
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(IAP,RAP,PAR,ICP,FNTI,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(IAP,RAP,PAR,ICP,FNTI,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
!
       ELSE IF(IPS.EQ.2 .AND. ABS(ISW).LE.1 ) THEN
!        ** Periodic solutions
         IF(ITP.NE.3 .AND. ABS(ITP/10).NE.3) THEN
           IF(IRS.GT.0)THEN
             CALL AUTOBV(IAP,RAP,PAR,ICP,FNPS,BCPS,ICPS,STPNBV, &
                  PVLSBV,THL,THU,IUZ,VUZ)
           ELSE
             CALL AUTOBV(IAP,RAP,PAR,ICP,FNPS,BCPS,ICPS,STPNUB, &
                  PVLSBV,THL,THU,IUZ,VUZ)
           ENDIF
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNPS,BCPS,ICPS,STPNPS, &
                PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
!
       ELSE IF(IPS.EQ.12 .AND. ABS(ISW).LE.1 ) THEN
!        ** Wave train solutions to parabolic systems.
         IF(ITP.NE.3) THEN
           IF(IRS.GT.0)THEN
             CALL AUTOBV(IAP,RAP,PAR,ICP,FNWP,BCPS,ICPS,STPNBV, &
                  PVLSBV,THL,THU,IUZ,VUZ)
           ELSE
             CALL AUTOBV(IAP,RAP,PAR,ICP,FNWP,BCPS,ICPS,STPNUB, &
                  PVLSBV,THL,THU,IUZ,VUZ)
           ENDIF
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNWP,BCPS,ICPS,STPNWP, &
                PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
!
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)<=1) THEN
!        ** Boundary value problems. (4)
!        ** Boundary value problems with Floquet multipliers. (7)
         IF(ITP.NE.3 .AND. ABS(ITP/10).NE.3) THEN
           IF(IRS.GT.0) THEN
             CALL AUTOBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNBV, &
                  PVLSBV,THL,THU,IUZ,VUZ)
           ELSE
             CALL AUTOBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNUB, &
                  PVLSBV,THL,THU,IUZ,VUZ)
           ENDIF
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNPB, &
                PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF

       ELSE IF(IPS.EQ.9 .AND. ABS(ISW).LE.1) THEN
!        ** Homoclinic bifurcation analysis.
         IF(IRS.GT.0) THEN
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNHO,BCHO,ICHO,STPNBV, &
                PVLSHO,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNHO,BCHO,ICHO,STPNHO, &
                PVLSHO,THL,THU,IUZ,VUZ)
         ENDIF
!
       ELSE IF(IPS.EQ.14) THEN
!        ** Evolution calculations for parabolic systems.
!           (Periodic boundary conditions.)
         IF(IRS.GT.0) THEN
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNPE,BCPS,ICPE,STPNBV, &
                PVLSBV,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNPE,BCPS,ICPE,STPNUB, &
                PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
!
       ELSE IF(IPS.EQ.15.AND.ABS(ISW).EQ.1) THEN
!        ** Optimization of periodic solutions.
         IF(NFPR.LT.6)THEN
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNPO,BCPO,ICPO,STPNPO, &
                PVLSBV,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNPO,BCPO,ICPO,STPNBV, &
                PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
!
       ELSE IF(IPS.EQ.16) THEN
!        ** Evolution calculations for parabolic systems.
!           (User supplied boundary conditions.)
         IF(IRS.GT.0) THEN
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNPE,BCNI,ICPE,STPNBV, &
                PVLSBV,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNPE,BCNI,ICPE,STPNUB, &
                PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
!
       ELSE IF(IPS.EQ.17) THEN
!        ** Continuation of stationary states of parabolic systems.
!           (User supplied boundary conditions.)
         IF(IRS.GT.0) THEN
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNSP,BCNI,ICPE,STPNBV, &
                PVLSBV,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNSP,BCNI,ICPE,STPNUB, &
                PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
!
       ELSE IF(IPS.EQ.5) THEN
!        ** Algebraic optimization problems.
         IF(MOD(ITP,10).EQ.2.OR.IRS.EQ.0)NFPR=NFPR+1
         IF(NFPR.EQ.2) THEN
           IF(IRS.GT.0) THEN
             CALL AUTOAE(IAP,RAP,PAR,ICP,FNC1,STPNAE,THL,THU,IUZ,VUZ)
           ELSE
             CALL AUTOAE(IAP,RAP,PAR,ICP,FNC1,STPNC1,THL,THU,IUZ,VUZ)
           ENDIF
         ELSE
           IF(MOD(ITP,10).NE.2) THEN
             CALL AUTOAE(IAP,RAP,PAR,ICP,FNC2,STPNAE,THL,THU,IUZ,VUZ)
           ELSE
             CALL AUTOAE(IAP,RAP,PAR,ICP,FNC2,STPNC2,THL,THU,IUZ,VUZ)
           ENDIF
         ENDIF
!
       ELSE 
!        **None of the above cases
         GOTO 2
       ENDIF
!
       GOTO 3
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  Two-Parameter Continuation.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
 2     IF(IPS.LE.1 .AND. ABS(ISW).EQ.2 .AND. (ITP.EQ.2) ) &
            THEN
!        ** Fold continuation (algebraic problems).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNLP,STPNLP,THL,THU,IUZ,VUZ)
!
       ELSE IF(IPS.LE.1 .AND. ABS(ISW).EQ.2  &
            .AND. ( (ABS(ITP)/10).EQ.2 ) ) &
            THEN
!        ** Fold continuation (algebraic problems, restart).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNLP,STPNAE,THL,THU,IUZ,VUZ)
!
       ELSE IF(IPS.LE.1 .AND. ABS(ISW).GE.2 .AND. (ITP.EQ.1) ) &
            THEN
!        ** BP cont (algebraic problems) (by F. Dercole).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNBP,STPNBP,THL,THU,IUZ,VUZ)
!
       ELSE IF(IPS.LE.1 .AND. ABS(ISW).GE.2  &
            .AND. ( (ABS(ITP)/10).EQ.1 ) ) &
            THEN
!        ** BP cont (algebraic problems, restart).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNBP,STPNAE,THL,THU,IUZ,VUZ)
!
       ELSE IF((IPS.EQ.0.OR.IPS.EQ.1).AND.ABS(ISW).EQ.2.AND.ITP.EQ.3 ) &
            THEN
!        ** Hopf bifurcation continuation (ODE).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHB,STPNHB,THL,THU,IUZ,VUZ)
!
       ELSE IF((IPS.EQ.0.OR.IPS.EQ.1).AND.ABS(ISW).EQ.2.AND. &
            (ABS(ITP)/10).EQ.3 ) THEN
!        ** Hopf bifurcation continuation (ODE, restart).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHB,STPNAE,THL,THU,IUZ,VUZ)
!
       ELSE IF(IPS.EQ.11.AND.ABS(ISW).EQ.2.AND.ITP.EQ.3 ) &
            THEN
!        ** Hopf bifurcation continuation (Waves).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHW,STPNHW,THL,THU,IUZ,VUZ)
!
       ELSE IF(IPS.EQ.11.AND.ABS(ISW).EQ.2.AND. &
            (ABS(ITP)/10).EQ.3 ) THEN
!        ** Hopf bifurcation continuation (Waves, restart).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHW,STPNAE,THL,THU,IUZ,VUZ)
!
       ELSE IF(IPS.EQ.-1 .AND. ABS(ISW).EQ.2 .AND. ITP.EQ.3 ) THEN
!        ** Hopf bifurcation continuation (Maps).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHD,STPNHD,THL,THU,IUZ,VUZ)
!
       ELSE IF(IPS.EQ.-1 .AND. ABS(ISW).EQ.2 .AND.(ABS(ITP)/10).EQ.3) &
            THEN
!        ** Hopf bifurcation continuation (Maps).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHD,STPNAE,THL,THU,IUZ,VUZ)
!
       ELSE IF(IPS==2 .AND. ABS(ISW)==2 .AND. ITP==5 ) THEN 
!        ** Fold continuation (Periodic solutions, start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNPL,BCPL,ICPL,STPNPL, &
              PVLSBV,THL,THU,IUZ,VUZ)
!
       ELSE IF(IPS==2 .AND. ABS(ISW)==2 .AND. (ABS(ITP)/10)==5 ) &
            THEN
!        ** Fold continuation (Periodic solutions, restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNPL,BCPL,ICPL,STPNBV, &
              PVLSBV,THL,THU,IUZ,VUZ)
!
       ELSE IF(IPS==2 .AND. ABS(ISW)>=2 .AND.  &
            (ITP==6.OR.(ABS(ITP)/10)==6) ) THEN
!        ** BP cont (Periodic sol., start and restart) (by F. Dercole).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNPBP,BCPBP,ICPBP,STPNPBP, &
              PVLSBV,THL,THU,IUZ,VUZ)
!
       ELSE IF((IPS.EQ.2 .OR. IPS.EQ.7) &
            .AND. ABS(ISW).EQ.2 .AND. ITP.EQ.7 ) THEN
!        ** Continuation of period doubling bifurcations (start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNPD,BCPD,ICPD,STPNPD, &
              PVLSBV,THL,THU,IUZ,VUZ)
!
       ELSE IF((IPS.EQ.2 .OR. IPS .EQ.7) &
            .AND. ABS(ISW).EQ.2 .AND. (ABS(ITP)/10).EQ.7) &
            THEN
!        ** Continuation of period doubling bifurcations (restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNPD,BCPD,ICPD,STPNBV, &
              PVLSBV,THL,THU,IUZ,VUZ)
!
       ELSE IF(IPS.EQ.2 .AND. ABS(ISW).EQ.2 .AND. ITP.EQ.8 ) THEN
!        ** Continuation of torus bifurcations (start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNTR,BCTR,ICTR,STPNTR, &
              PVLSBV,THL,THU,IUZ,VUZ)
!
       ELSE IF(IPS.EQ.2 .AND. ABS(ISW).EQ.2 .AND. (ABS(ITP)/10).EQ.8) &
            THEN
!        ** Continuation of torus bifurcations (restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNTR,BCTR,ICTR,STPNBV, &
              PVLSBV,THL,THU,IUZ,VUZ)
!
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)==2 .AND. ITP==5 ) THEN
!        ** Continuation of folds (BVP, start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNBL,BCBL,ICBL,STPNBL, &
              PVLSBV,THL,THU,IUZ,VUZ)
!
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)==2 .AND.  &
            (ABS(ITP)/10)==5 ) THEN
!        ** Continuation of folds (BVP, restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNBL,BCBL,ICBL,STPNBV, &
              PVLSBV,THL,THU,IUZ,VUZ)
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)>=2 .AND. &
            (ITP==6.OR.(ABS(ITP)/10)==6) ) THEN
!        ** BP cont (BVP, start and restart) (by F. Dercole).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNBBP,BCBBP,ICBBP,STPNBBP, &
              PVLSBV,THL,THU,IUZ,VUZ)
!
       ELSE
!        ** Error in INIT.
         WRITE(MATLABDISPSTR,500)
         CALL AUTOSTOPWITHERROR(MATLABDISPSTR)
       ENDIF
 3     CONTINUE
!
! Error Message.
 500  FORMAT(' Initialization Error')
!
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    Initialization
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE INIT(IAP,RAP,PAR,EOF,READ2EOF)
!
      USE AUTO_CONSTANTS    
!
      IMPLICIT NONE
!
! Reads the file of continuation constants
!
      INTEGER IAP(*)
      DOUBLE PRECISION RAP(*),PAR(*)
      LOGICAL EOF,READ2EOF
!
      INTEGER IBR,I,IUZR,ITHU,NBIF,NFPR,NDM,NNT0,NBC0
      INTEGER NINS,NIT,LAB,NTOT,ITP,IPOS,ISTOP,ITPST
      DOUBLE PRECISION AMP,BIFF,DET,DSOLD,SPBF,TIVP,HBFF,FLDF
!
      DO I=1,NPARX
        ICP(I)=I
        ICP(NPARX+I)=0
        PAR(I)=0.d0
        PAR(NPARX+I)=0.d0
      ENDDO
!
      IF (FORT2DST == 1) THEN
       READ(2,*,END=5) NDIM,IPS,IRS,ILP
!
!     we allocate THU (a pointer to the THU array in the
!     main program) here since this is the place where we 
!     know the size.  It is 8 times bigger then ndim since
!     INIT can modify THU based on the problem type,
!     but only up to making it 8 times larger.
!
       ALLOCATE(THU(8*NDIM))
       DO I=1,NDIM*8
         THU(I)=1.d0
       ENDDO
!
       READ(2,*) NICP,(ICP(NPARX+I),I=1,NICP)
       IF(NICP.GT.0)THEN
         DO I=1,NICP
           ICP(I)=ICP(NPARX+I)
         ENDDO
       ELSE
         NICP=1
         ICP(NPARX+1)=ICP(1)
       ENDIF
       READ(2,*) NTST,NCOL,IAD,ISP,ISW,IPLT,NBC,NINT
       READ(2,*) NMX,RL0,RL1,A0,A1
       READ(2,*) NPR,MXBF,IID,ITMX,ITNW,NWTN,JAC
       READ(2,*) EPSL,EPSU,EPSS
       READ(2,*) DS,DSMIN,DSMAX,IADS
       DSMIN=ABS(DSMIN)
       DSMAX=ABS(DSMAX)
       READ(2,*) NTHL
       IF(NTHL.GT.0)THEN
         ALLOCATE(ITHL(NTHL),VTHL(NTHL))
         DO I=1,NTHL
           READ(2,*)ITHL(I),VTHL(I)
         ENDDO
       ENDIF
       READ(2,*) NTHU
       IF(NTHU.GT.0)THEN
         DO I=1,NTHU
           READ(2,*)ITHU,THU(ITHU)
         ENDDO
       ENDIF
       READ(2,*)NUZR
       IF(NUZR.GT.0)THEN
         ALLOCATE(IUZ(NUZR),VUZ(NUZR))
         DO I=1,NUZR
           READ(2,*)IUZ(I),VUZ(I)
         ENDDO
       ELSE
!        Avoid uninitialized pointers
         ALLOCATE(IUZ(1),VUZ(1))
       ENDIF
!
      ELSE
       IF(READ2EOF.EQV..TRUE.)GOTO 5
       NDIM=NDIMDST
       IPS=IPSDST
       IRS=IRSDST
       ILP=ILPDST
!
!     we allocate THU (a pointer to the THU array in the
!     main program) here since this is the place where we 
!     know the size.  It is 8 times bigger then ndim since
!     INIT can modify THU based on the problem type,
!     but only up to making it 8 times larger.
!
       ALLOCATE(THU(8*NDIM))
       DO I=1,NDIM*8
         THU(I)=1.d0
       ENDDO
!
       NICP=NICPDST
       DO I=1,NICP
         ICP(NPARX+I)=ICPDST(I)
       ENDDO
       IF(NICP.GT.0)THEN
         DO I=1,NICP
           ICP(I)=ICP(NPARX+I)
         ENDDO
       ELSE
         NICP=1
         ICP(NPARX+1)=ICP(1)
       ENDIF
!      
       NTST=NTSTDST
       NCOL=NCOLDST
       IAD=IADDST
       ISP=ISPDST
       ISW=ISWDST
       IPLT=IPLTDST
       NBC=NBCDST
       NINT=NINTDST
       NMX=NMXDST
       RL0=RL0DST
       RL1=RL1DST
       A0=A0DST
       A1=A1DST
       NPR=NPRDST
       MXBF=MXBFDST
       IID=IIDDST
       ITMX=ITMXDST
       ITNW=ITNWDST
       NWTN=NWTNDST
       JAC=JACDST
       EPSL=EPSLDST
       EPSU=EPSUDST
       EPSS=EPSSDST
       DS=DSDST
       DSMIN=DSMINDST
       DSMAX=DSMAXDST
       IADS=IADSDST
       DSMIN=ABS(DSMIN)
       DSMAX=ABS(DSMAX)
       NTHL=NTHLDST
       IF(NTHL.GT.0)THEN
         ALLOCATE(ITHL(NTHL),VTHL(NTHL))
         DO I=1,NTHL
           ITHL(I)=ITHLDST(I)
           VTHL(I)=VTHLDST(I)
         ENDDO
       ENDIF
       NTHU=NTHUDST
       IF(NTHU.GT.0)THEN
         DO I=1,NTHU
           ITHU=ITHUDST(I)
           THU(ITHU)=VTHUDST(I)
         ENDDO
       ENDIF
       NUZR=NUZRDST
       IF(NUZR.GT.0)THEN
         ALLOCATE(IUZ(NUZR),VUZ(NUZR))
         DO I=1,NUZR
           IUZ(I)=IUZDST(I)
           VUZ(I)=VUZDST(I)
         ENDDO
       ELSE
!        Avoid uninitialized pointers
         ALLOCATE(IUZ(1),VUZ(1))
       ENDIF
       READ2EOF=.TRUE.
      ENDIF 
!
      IAP(1)=NDIM
      IAP(2)=IPS
      IAP(3)=IRS
      IAP(4)=ILP
      IAP(5)=NTST
      IAP(6)=NCOL
      IAP(7)=IAD
      IAP(8)=IADS
      IAP(9)=ISP
      IAP(10)=ISW
      IAP(11)=IPLT
      IAP(12)=NBC
      IAP(13)=NINT
      IAP(14)=NMX
      IAP(15)=NUZR
      IAP(16)=NPR
      IAP(17)=MXBF
      IAP(18)=IID
      IAP(19)=ITMX
      IAP(20)=ITNW
      IAP(21)=NWTN      
      IAP(22)=JAC
!
      NDM=NDIM
      IF(NBC.NE.0) THEN
        NBC0=NBC
      ELSE
        NBC0=1
      ENDIF
      IF(NINT.NE.0)THEN
        NNT0=NINT
      ELSE
        NNT0=1
      ENDIF
      IUZR=1
      ITP=0
      ITPST=0
      NFPR=1
      IBR=1
      NIT=0
      NTOT=0
      NINS=0
      ISTOP=0
      NBIF=0
      IPOS=1
      LAB=0
!
      IAP(23)=NDM
      IAP(24)=NBC0
      IAP(25)=NNT0
      IAP(26)=IUZR
      IAP(27)=ITP
      IAP(28)=ITPST
      IAP(29)=NFPR
      IAP(30)=IBR
      IAP(31)=NIT
      IAP(32)=NTOT
      IAP(33)=NINS
      IAP(34)=ISTOP
      IAP(35)=NBIF
      IAP(36)=IPOS
      IAP(37)=LAB
      IAP(41)=NICP
!
      RAP(1)=DS
      RAP(2)=DSMIN
      RAP(3)=DSMAX
      DSOLD=DS
      RAP(5)=DSOLD
      RAP(6)=RL0
      RAP(7)=RL1
      RAP(8)=A0
      RAP(9)=A1
!
      AMP=0.d0
      DET=0.d0
      TIVP=0.d0
      FLDF=0.d0
      HBFF=0.d0
      BIFF=0.d0
      SPBF=0.d0
!
      RAP(10)=AMP
      RAP(11)=EPSL
      RAP(12)=EPSU
      RAP(13)=EPSS
      RAP(14)=DET
      RAP(15)=TIVP
      RAP(16)=FLDF
      RAP(17)=HBFF
      RAP(18)=BIFF
      RAP(19)=SPBF
!
      EOF=.FALSE.
      RETURN
 5    EOF=.TRUE.
      RETURN
      END
!
!     ---------- -------
      SUBROUTINE CLEANUP()
!
!     Deallocate some globally allocated arrays.
!
      USE AUTO_CONSTANTS

      IMPLICIT NONE

      DEALLOCATE(THU,IUZ,VUZ,THL)
      
      END
!
!     ---------- -----
      SUBROUTINE CHDIM(IAP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER(10000) MATLABDISPSTR
!
! Check dimensions.
!
      DIMENSION IAP(*)
!
       NPAR=IAP(29)
!
       IF(NPAR.GT.NPARX)THEN
         WRITE(MATLABDISPSTR,101)NPAR,NPARX
         CALL AUTOSTOPWITHERROR(MATLABDISPSTR)
       ENDIF
!
 101   FORMAT(' Dimension exceeded : NPAR=',I5,'  maximum=',I5,/, &
            ' (Increase NPARX in auto.h and recompile AUTO)')
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!               The leading subroutines of AUTO
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ------
      SUBROUTINE AUTOAE(IAP,RAP,PAR,ICP,FUNI,STPNT,THL,THU,IUZ,VUZ)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! This is the entry subroutine for algebraic systems.
!
      INTEGER IAP(*)
!
      EXTERNAL FUNI,STPNT
!
       IF(IAP(38).GT.0)THEN
         CALL MPIWFI(.FALSE.,FUNI,STPNT)
         RETURN
       ENDIF
       CALL CNRLAE(IAP,RAP,PAR,ICP,FUNI,STPNT,THL,THU,IUZ,VUZ)
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE AUTOBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNT, &
           PVLI,THL,THU,IUZ,VUZ)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! THIS IS THE ENTRY ROUTINE FOR GENERAL BOUNDARY VALUE PROBLEMS.
!
      INTEGER IAP(*)
!
      EXTERNAL FUNI,BCNI,ICNI,STPNT,PVLI
!
       IF(IAP(38).GT.0)THEN
!        This is a little trick to tell MPI workers what FUNI and ICNI
!        are.
         CALL MPIWFI(.TRUE.,FUNI,ICNI)
         RETURN
       ENDIF
       CALL CNRLBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNT, &
            PVLI,THL,THU,IUZ,VUZ)
!
      RETURN
      END
!
!     ---------- -----
      SUBROUTINE INIT1(IAP,RAP,ICP,PAR)
!
      USE HOMCONT, ONLY:INHO
      USE AUTO_CONSTANTS, ONLY:ITHL,VTHL,THL,NTHL,NPARX,NBIFX,NIAP,NRAP
!      INCLUDE 'auto.h'
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER(10000) MATLABDISPSTR
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! General initialization. Redefinition of constants.
!
      DIMENSION IAP(*),RAP(*),ICP(*),PAR(*)
!
! Local
      DIMENSION ICT(NPARX)
!
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       ILP=IAP(4)
       NCOL=IAP(6)
       ISP=IAP(9)
       ISW=IAP(10)
       NBC=IAP(12)
       NINT=IAP(13)
       NMX=IAP(14)
       NUZR=IAP(15)
       JAC=IAP(22)
       ITP=IAP(27)
       NFPR=IAP(29)
       NICP=IAP(41)
!
       DS=RAP(1)
       DSMIN=RAP(2)
       DSMAX=RAP(3)
!
       IF(ISW.EQ.0)ISW=1
!
! Check and perturb pseudo arclength stepsize and steplimits.
! (Perturbed to avoid exact computation of certain singular points).
!
       IF(DS.EQ.0.d0)DS=0.1
       IF(DSMIN.EQ.0.d0)DSMIN=1.0D-4*ABS(DS)
       FC=1.d0+HMACH
       DS=FC*DS
       DSMIN=DSMIN/FC
       DSMAX=FC*DSMAX
!
! Redefinition for waves
       IF(IPS.EQ.11)THEN
         IPS=1
         IAP(2)=IPS
         NDIM=2*NDIM
         NDM=NDIM
         IAP(23)=NDM
       ELSEIF(IPS.EQ.12)THEN
         IPS=2
         IAP(2)=IPS
         NDIM=2*NDIM
         NDM=NDIM
         IAP(23)=NDM
       ENDIF
!
! General Redefinition.
!
       IF(ABS(IPS).LE.1 .AND. ISW.EQ.1 )THEN
!        ** Algebraic Systems
         NFPR=1
!
       ELSE IF(IPS.EQ.-2)THEN
!        ** Time integration
         NFPR=1
         ISP=0
         ILP=0
         ICP(1)=14
! 
       ELSE IF(IPS.EQ.2 .AND. ABS(ISW).EQ.1 )THEN
!        ** Periodic Solutions
         NBC=NDIM
         NINT=1
         NFPR=NBC+NINT-NDIM+1
!        **ISW=1 when starting from a HB
         IF(ITP.EQ.3.OR.(ABS(ITP)/10).EQ.3)ISW=1
         IF(NICP.EQ.1)THEN
!          **Variable period
           ICP(2)=11
         ENDIF
!
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)==1  ) THEN
!        ** Boundary value problems
         NFPR=NBC+NINT-NDIM+1
!
       ELSE IF( IPS.EQ.9 .AND. ABS(ISW).EQ.1  ) THEN
!        ** Homoclinic bifurcation analysis
!        Redefine AUTO constants for homoclinic orbits
         CALL INHO(IAP,ICP,PAR)
         NDIM=IAP(1)
         NBC=IAP(12)
         NINT=IAP(13)
         NUZR=IAP(15)
         NFPR=NBC+NINT-NDIM+1
!
       ELSE IF(IPS.EQ.14 .OR. IPS.EQ.16)THEN
!        **Evolution calculations for Parabolic Systems
         NDIM=2*NDIM
         NBC=NDIM
         NINT=0
         NFPR=1
         ILP=0
         ISP=0
         ICP(1)=14
!
       ELSE IF(IPS.EQ.17)THEN
!        **Stationary calculations for Parabolic Systems
         NDIM=2*NDIM
         NBC=NDIM
         NINT=0
         NFPR=1
!
         ELSE IF(IPS.EQ.15)THEN
!          ** Optimization of periodic solutions 
           DO I=1,NICP
             ICT(I)=ICP(I)
           ENDDO
           NFPR=0
           DO I=1,NICP
             IF(ICT(I).GT.0)THEN
               NFPR=NFPR+1
               ICP(NFPR)=ICT(I)
             ENDIF
           ENDDO
           ICP(NFPR+1)=10
           ICP(NFPR+2)=13
           ICP(NFPR+3)=14
           NFPR=NFPR+3
           NDIM=2*NDIM
           NBC=NDIM
           NINT=NFPR-1
! overload to define optimality integrals
           NNEG=0
           DO I=1,NICP
             IC=ICT(I)
             JC=ABS(IC)-20        
             IF(IC.LT.0.AND.JC.GT.0.AND.JC.LE.11)THEN
               NNEG=NNEG+1
               ICP(NFPR+NNEG)=JC
             ENDIF
           ENDDO
! Set indices of output parameters
           NICP=NFPR-3
           DO I=1,NICP
             ICP(NPARX+I)=ICP(I)
           ENDDO
!
       ELSE IF(IPS.EQ.5)THEN
!        ** Algebraic optimization Problems
         IF(MOD(ITP,10).EQ.2.OR.IRS.EQ.0)NFPR=NFPR+1
         IF(NFPR.EQ.2)THEN
           NDIM=NDIM+1
           ICP(1)=10
         ELSE
           NDIM=2*NDIM+NFPR
           ICP(1)=10
         ENDIF
!
       ELSE IF(IRS.GT.0 .AND. ABS(ISW).GE.2 )THEN
!        ** Continuation of singular points
!
         IF( ( ITP.EQ.2.OR.(ABS(ITP)/10).EQ.2 ) &
              .AND. ABS(IPS).LE.1)THEN
!          ** Fold continuation (Algebraic Problems)
           NDIM=2*NDIM+1
           NFPR=2
!
         ELSE IF( ( ITP.EQ.1.OR.(ABS(ITP)/10).EQ.1 ) &
              .AND. ABS(IPS).LE.1)THEN
!          ** BP cont (Algebraic Problems) (by F. Dercole)
           NDIM=2*NDIM+2
           NFPR=3
!
         ELSE IF((ITP.EQ.3.OR.(ABS(ITP)/10).EQ.3) &
              .AND. ABS(IPS).LE.1 )THEN
!          ** Hopf bifurcation continuation (Maps, ODE, Waves)
           NDIM=3*NDIM+2
           NFPR=2
!
         ELSE IF( ITP==5 .AND. IPS==2 )THEN
!          ** Fold continuation (Periodic solutions); start
           NDIM=2*NDIM
           NBC=NDIM
           NINT=3
           NFPR=NBC+NINT-NDIM+1
           IF(ICP(3).EQ.11 .OR. NICP.EQ.2)THEN
!            ** Variable period
             ICP(2)=13
             ICP(3)=11
             ICP(4)=12
           ELSE
!            ** Fixed period
             ICP(3)=13
             ICP(4)=12
           ENDIF
           ILP=0
           ISW=-2
           ISP=0
           NMX=5
           CALL MEXPRINTF(achar(10))
           WRITE(MATLABDISPSTR,101)
           CALL MEXPRINTF(MATLABDISPSTR)
!
         ELSE IF( (ABS(ITP)/10)==5 .AND. IPS==2 )THEN
!          ** Fold continuation (Periodic solutions); restart
           NDIM=2*NDIM
           NBC=NDIM
           NINT=3
           NFPR=NBC+NINT-NDIM+1
           IF(ICP(3).EQ.11 .OR. NICP.EQ.2)THEN
!            ** Variable period
             ICP(3)=11
           ENDIF
           ICP(4)=12
!
         ELSE IF( (ITP==6) .AND. IPS==2)THEN
!          ** BP cont (Periodic solutions); start (by F. Dercole)
           NDIM=4*NDIM
           NBC=NDIM
           NINT=10
           NFPR=NBC+NINT-NDIM+1
           IF(((ABS(ISW)==2).AND.(ICP(3)==11 .OR. NICP==2)).OR. &
                ((ABS(ISW)==3).AND.(ICP(4)==11 .OR. NICP==3)))THEN
!            ** Variable period
             ICP(2)=17 ! a
             ICP(3)=18 ! b
             ICP(4)=11 ! T
           ELSE
!            ** Fixed period
             ICP(3)=17 ! a
             ICP(4)=18 ! b
           ENDIF
           ICP(5)=12   ! q1
           ICP(6)=13   ! q2/beta1
           ICP(7)=14   ! r1
           ICP(8)=15   ! r2/beta2
           ICP(9)=16   ! psi^*_3
           ICP(10)=20  ! c1
           ICP(11)=21  ! c2
!
           ILP=0
           ISW=-ABS(ISW)
           ISP=0
           NMX=5
           CALL MEXPRINTF(achar(10))
           WRITE(MATLABDISPSTR,101)
           CALL MEXPRINTF(MATLABDISPSTR)
!
         ELSE IF( (ABS(ITP)/10==6) .AND. IPS==2)THEN
!          ** BP cont (Periodic solutions); restart 1 or 2
           NDIM=2*NDIM
           NBC=NDIM
           NINT=4
           NFPR=NBC+NINT-NDIM+1
           IF(ABS(ISW)==2)THEN
!            ** Non-generic case
             IF(ICP(3)==11 .OR. NICP==2)THEN
!              ** Variable period
               ICP(3)=18 ! b
               ICP(4)=11 ! T
             ELSE
!              ** Fixed period
               ICP(4)=18 ! b
             ENDIF
           ELSE
!            ** Generic case
             IF(ICP(4)==11 .OR. NICP==3)THEN
!              ** Variable period
               ICP(4)=11 ! T
             ENDIF
           ENDIF
           ICP(5)=16     ! psi^*_3
!
         ELSE IF(ITP.EQ.7 .AND. (IPS.EQ.2 .OR. IPS.EQ.7))THEN
!          ** Continuation of period doubling bifurcations; start
           NDIM=2*NDIM
           NBC=NDIM
           NINT=2
           NFPR=NBC+NINT-NDIM+1
           IF(ICP(3).EQ.11 .OR. NICP.EQ.2)THEN
!            ** Variable period
             ICP(2)=11
             ICP(3)=13
           ELSE
!            ** Fixed period
             ICP(3)=13
           ENDIF
           ILP=0
           ISW=-2
           ISP=0
           NMX=5
           CALL MEXPRINTF(achar(10))
           WRITE(MATLABDISPSTR,101)
           CALL MEXPRINTF(MATLABDISPSTR)
!
         ELSE IF(ABS(ITP)/10.EQ.7 .AND. (IPS.EQ.2 .OR. IPS.EQ.7))THEN
!          ** Continuation of period doubling bifurcations; restart
           NDIM=2*NDIM
           NBC=NDIM
           NINT=2
           NFPR=NBC+NINT-NDIM+1
           IF(ICP(3).EQ.11 .OR. NICP.EQ.2)THEN
!            ** Variable period
             ICP(3)=11
           ENDIF
!
         ELSE IF(ITP.EQ.8 .AND. IPS.EQ.2)THEN
!          ** Continuation of torus bifurcations; start
           NDIM=3*NDIM
           NBC=NDIM
           NINT=3
           NFPR=NBC+NINT-NDIM+1
           ICP(2)=11
           ICP(3)=12
           ICP(4)=13
           ILP=0
           ISP=0
           ISW=-2
           NMX=5
           CALL MEXPRINTF(achar(10))
           WRITE(MATLABDISPSTR,101)
           CALL MEXPRINTF(MATLABDISPSTR)
!
         ELSE IF(ABS(ITP)/10.EQ.8 .AND. IPS.EQ.2)THEN
!          ** Continuation of torus bifurcations; restart
           NDIM=3*NDIM
           NBC=NDIM
           NINT=3
           NFPR=NBC+NINT-NDIM+1
           ICP(3)=11
           ICP(4)=12
!
         ELSE IF( (ITP==5) .AND. (IPS==4.OR.IPS==7) ) &
              THEN
!          ** Continuation of folds (BVP; start)
           NDIM=2*NDIM
           NBC=2*NBC
           NINT=2*NINT+1
           NFPR=NBC+NINT-NDIM+1
           NXP=NFPR/2-1
           IF(NXP.GT.0)THEN
             DO I=1,NXP
               ICP(NFPR/2+I+1)=11+I
             ENDDO
           ENDIF
           ICP(NFPR/2+1)=11+NFPR/2
           ILP=0
           ISW=-2
           ISP=0
           NMX=5
           CALL MEXPRINTF(achar(10))
           WRITE(MATLABDISPSTR,101)
           CALL MEXPRINTF(MATLABDISPSTR)
!
         ELSE IF( (ABS(ITP)/10)==5 .AND. (IPS==4.OR.IPS==7))THEN
!          ** Continuation of folds (BVP; restart)
           NDIM=2*NDIM
           NBC=2*NBC
           NINT=2*NINT+1
           NFPR=NBC+NINT-NDIM+1
           NXP=NFPR/2-1
           IF(NXP.GT.0)THEN
             DO I=1,NXP
               ICP(NFPR/2+I+1)=11+I
             ENDDO
           ENDIF
!
         ELSE IF( ITP==6 .AND. (IPS==4.OR.IPS==7) )THEN
!          ** BP cont (BVP; start) (by F. Dercole)
           NXP=NBC+NINT-NDIM+1
           NDIM=4*NDIM
           NBC=3*NBC+NDIM/2+NXP
           NINT=3*NINT+NXP+5
           NFPR=NBC+NINT-NDIM+1
           ICP(NXP+1)=11+3*NXP+NDIM/4   ! a
           ICP(NXP+2)=11+3*NXP+NDIM/4+1 ! b
           DO I=1,NXP
             ICP(NXP+I+2)=11+I          ! q
             ICP(2*NXP+I+2)=11+NXP+I    ! r
             ICP(4*NXP+NDIM/4+I+3)=11+3*NXP+NDIM/4+3+I ! d
           ENDDO
           DO I=1,NXP+NDIM/4-1
             ICP(3*NXP+I+2)=11+2*NXP+I  ! psi^*_2,psi^*_3
           ENDDO
           ICP(4*NXP+NDIM/4+2)=11+3*NXP+NDIM/4+2 ! c1
           ICP(4*NXP+NDIM/4+3)=11+3*NXP+NDIM/4+3 ! c2
!
           ILP=0
           ISW=-ABS(ISW)
           ISP=0
           NMX=5
           CALL MEXPRINTF(achar(10))
           WRITE(MATLABDISPSTR,101)
           CALL MEXPRINTF(MATLABDISPSTR)
!
         ELSE IF( (ABS(ITP)/10)==6 .AND. (IPS==4.OR.IPS==7))THEN
!          ** BP cont (BVP; restart 1 or 2)
           NXP=NBC+NINT-NDIM+1
           NDIM=2*NDIM
           NBC=NBC+NDIM+NXP
           NINT=NINT+NXP+1
           NFPR=NBC+NINT-NDIM+1
           IF(ABS(ISW)==2)THEN
!            ** Non-generic case
             ICP(NXP+2)=11+3*NXP+NDIM/2+1 ! b
           ENDIF
           DO I=1,NXP+NDIM/2-1
             ICP(NXP+I+2)=11+2*NXP+I      ! psi^*_2,psi^*_3
           ENDDO
           DO I=1,NXP
             ICP(2*NXP+NDIM/2+I+1)=11+3*NXP+NDIM/2+3+I ! d
           ENDDO
!
         ENDIF
!
       ENDIF

!     redefine nthl to be nfpr sized and indexed
       ALLOCATE(THL(NFPR))
       DO I=1,NFPR
         THL(I)=1.0D0
         DO J=1,NTHL
           IF(ICP(I)==ITHL(J))THEN
             THL(I)=VTHL(J)
           ENDIF
         ENDDO
       ENDDO
       IF(NTHL>0)THEN
         DEALLOCATE(ITHL,VTHL)
       ENDIF
!
       IAP(1)=NDIM
       IAP(2)=IPS
       IAP(3)=IRS
       IAP(4)=ILP
       IAP(6)=NCOL
       IAP(9)=ISP
       IAP(10)=ISW
       IAP(12)=NBC
       IAP(13)=NINT
       IAP(14)=NMX
       IAP(15)=NUZR
       IAP(22)=JAC
       IAP(29)=NFPR
       IAP(41)=NICP
!
       RAP(1)=DS
       RAP(2)=DSMIN
       RAP(3)=DSMAX
!
 101   FORMAT(' Generating starting data  :\n Restart at EP label below &
      :\n\n')

      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    Algebraic Problems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ------
      SUBROUTINE CNRLAE(IAP,RAP,PAR,ICP,FUNI,STPNT,THL,THU,IUZ,VUZ)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Controls the bifurcation analysis of algebraic problems
!
      EXTERNAL FUNI,STPNT,FNLPAE,FNHBAE,FNBPAE,FNUZAE
!
      DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),IUZ(*)
! Local
      DIMENSION RLCUR(NPARX),RLOLD(NPARX),RLDOT(NPARX)
      ALLOCATABLE AA(:,:),RHS(:),U(:),DU(:),UDOT(:),UOLD(:),STUD(:,:)
      ALLOCATABLE STU(:,:),STLA(:),STLD(:),F(:),DFDU(:),DFDP(:),UZR(:)
!
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       ILP=IAP(4)
       IADS=IAP(8)
       ISP=IAP(9)
       NUZR=IAP(15)
       MXBF=IAP(17)
       ITPST=IAP(28)
       IBR=IAP(30)
!
       DS=RAP(1)
!
       ALLOCATE(AA(NDIM+1,NDIM+1),RHS(NDIM+1),U(NDIM),DU(NDIM+1))
       ALLOCATE(UDOT(NDIM),UOLD(NDIM),STUD(NBIFX,NDIM),STU(NBIFX,NDIM))
       ALLOCATE(STLA(NBIFX),STLD(NBIFX),F(NDIM),DFDU(NDIM**2))
       ALLOCATE(DFDP(NDIM*NPARX),UZR(NUZR))
!
       NINS=0
       IAP(33)=NINS
       RBP=0.d0
       REV=0.d0
       RLP=0.d0
       IF(NUZR.GT.0)THEN
         DO I=1,NUZR
           UZR(I)=0.d0
         ENDDO
       ENDIF
       RDS=DS
       DSOLD=DS
       RAP(5)=DSOLD
       NIT=0
       IAP(31)=NIT
       NBIF=0
       IAP(35)=NBIF
       NBFC=0
       IPOS=1
       IAP(36)=IPOS
       NTOT=0
       IAP(32)=NTOT
       LAB=0
       IAP(37)=LAB
!
       DO I=1,NDIM
         U(I)=0.d0
         DU(I)=0.d0
         UDOT(I)=0.d0
         UOLD(I)=0.d0
         F(I)=0.d0
       ENDDO
!
! Generate the starting point
!
       CALL STPNT(IAP,RAP,PAR,ICP,U)
       CALL PVLSAE(IAP,RAP,U,PAR)
!
! Determine a suitable starting label and branch number
!
       CALL NEWLAB(IAP)
!
! Write constants
!
       CALL STHD(IAP,RAP,ICP)
!
! Write plotting data for the starting point
!
       ISTOP=0
       IAP(34)=ISTOP
       IF(IRS.EQ.0) THEN
         ITP=9+10*ITPST
       ELSE
         ITP=0
       ENDIF
       IAP(27)=ITP
       RLCUR(1)=PAR(ICP(1))
       CALL STPLAE(IAP,RAP,PAR,ICP,RLCUR,U)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 6
!
! Starting procedure  (to get second point on first branch) :
!
       CALL STPRAE(IAP,RAP,PAR,ICP,FUNI,RDS,NDIM+1,AA,RHS, &
            RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 5
       ITP=0
       IAP(27)=ITP
       GOTO 3
!
! Initialize computation of the next bifurcating branch.
!
 2     CALL SWPNT(IAP,RAP,PAR,ICP,RDS,NBIFX,STUD,STU,STLA,STLD, &
            RLCUR,RLDOT,U,UDOT)
!
       IPOS=IAP(36)
       IF(IPOS.EQ.1)THEN
         NBIF=NBIF-1
         IAP(35)=NBIF
         NBFC=NBFC+1
       ENDIF
!
       RBP=0.d0
       REV=0.d0
       RLP=0.d0
       IF(NUZR.GT.0)THEN
         DO I=1,NUZR
           UZR(I)=0.d0
         ENDDO
       ENDIF
       IF(IPOS.EQ.0 .OR. MXBF.LT.0 )IBR=IBR+1
       IAP(30)=IBR
!
       NTOT=0
       IAP(32)=NTOT
       ISTOP=0
       IAP(34)=ISTOP
       ITP=0
       IAP(27)=ITP
       NIT=0
       IAP(31)=NIT
       DSOLD=RDS
       RAP(5)=DSOLD
!
! Store plotting data for first point on the bifurcating branch
!
       CALL STPLAE(IAP,RAP,PAR,ICP,RLCUR,U)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 6
!
! Determine the second point on the bifurcating branch
!
       CALL SWPRC(IAP,RAP,PAR,ICP,FUNI,NDIM+1,AA,RHS,RLCUR,RLOLD,RLDOT, &
            U,DU,UOLD,UDOT,F,DFDU,DFDP,RDS,THL,THU)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 5
!
! Store plotting data for second point :
!
       CALL STPLAE(IAP,RAP,PAR,ICP,RLCUR,U)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 6
       RBP=0.d0
       REV=0.d0
       RLP=0.d0
!
! Provide initial approximation to the next point on the branch
!
 3     CALL CONTAE(IAP,RAP,RDS,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT)
!
! Find the next solution point on the branch
!
        CALL SOLVAE(IAP,RAP,PAR,ICP,FUNI,RDS,NDIM+1,AA,RHS, &
             RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)
        ISTOP=IAP(34)
        IF(ISTOP.EQ.1)GOTO 5
!
! Check for user supplied parameter output parameter-values.
!
       IF(NUZR.GT.0)THEN
         DO IUZR=1,NUZR
           IAP(26)=IUZR
           CALL LCSPAE(IAP,RAP,PAR,ICP,FNUZAE,FUNI,NDIM+1,AA,RHS,RLCUR, &
                RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,UZR(IUZR),THL,THU, &
                IUZ,VUZ)
           ISTOP=IAP(34)
           IF(ISTOP.EQ.1)GOTO 5
           ITP=IAP(27)
           IF(ITP.EQ.-1)THEN
             IF(IUZ(IUZR).GT.0)THEN
               ITP=-4-10*ITPST
               IAP(27)=ITP
               DO K=1,NUZR
                 UZR(K)=0.d0
               ENDDO
             ELSE
               ISTOP=-1
               IAP(34)=ISTOP
! NOTE: Fix (February 2005)
               GOTO 5
             ENDIF
           ENDIF
         ENDDO
       ENDIF
!
! Check for fold
!
         IF(ABS(ILP).GT.0)THEN
           CALL LCSPAE(IAP,RAP,PAR,ICP,FNLPAE,FUNI,NDIM+1,AA,RHS,RLCUR, &
                RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,RLP,THL,THU,IUZ,VUZ)
           ITP=IAP(27)
           IF(ITP.EQ.-1) THEN
             IF(ILP.GT.0)THEN
               ITP=2+10*ITPST
               IAP(27)=ITP
               RLP=0.d0
               RBP=0.d0
               REV=0.d0
             ELSE
!            *Stop at the first found fold
               ISTOP=-1
               IAP(34)=ISTOP
               GOTO 5
             ENDIF
           ENDIF
         ENDIF
!
! Check for branch point, and if so store data :
!
         IF(ABS(ISP).GT.0)THEN
           CALL LCSPAE(IAP,RAP,PAR,ICP,FNBPAE,FUNI,NDIM+1,AA,RHS,RLCUR, &
                RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,RBP,THL,THU,IUZ,VUZ)
           ISTOP=IAP(34)
           IF(ISTOP.EQ.1)GOTO 5
           ITP=IAP(27)
           IF(ITP.EQ.-1)THEN
             IF(ISP.GT.0)THEN
               ITP=1+10*ITPST
               IAP(27)=ITP
               NBIF=NBIF+1
               IAP(35)=NBIF
               CALL STBIF(IAP,ICP,NDIM+1,AA,NBIFX,STUD,STU,STLA, &
                    STLD,RLCUR,RLDOT,U,DU,UDOT,DFDU,DFDP,THL,THU)
               RLP=0.d0
               RBP=0.d0
               REV=0.d0
             ELSE
!            *Stop at the first found BP
               ISTOP=-1
               IAP(34)=ISTOP
               GOTO 5
             ENDIF
           ENDIF
         ENDIF
!
! Check for Hopf bifurcation
!
         IF(ABS(IPS).EQ.1)THEN
           CALL LCSPAE(IAP,RAP,PAR,ICP,FNHBAE,FUNI,NDIM+1,AA,RHS,RLCUR, &
                RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,REV,THL,THU,IUZ,VUZ)
           ISTOP=IAP(34)
           IF(ISTOP.EQ.1)GOTO 5
           ITP=IAP(27)
           IF(ITP.EQ.-1)THEN
             ITP=3+10*ITPST
             IAP(27)=ITP
             REV=0.d0
           ENDIF
         ENDIF
!
! Store plotting data on unit 7 :
!
 5       CALL STPLAE(IAP,RAP,PAR,ICP,RLCUR,U)
!
! Adapt the stepsize along the branch
!
       ITP=IAP(27)
       NTOT=IAP(32)
       IF(IADS.NE.0 .AND. MOD(NTOT,IADS).EQ.0  &
            .AND. ( MOD(ITP,10).EQ.0 .OR. MOD(ITP,10).EQ.4) )THEN
         CALL ADPTDS(IAP,RAP,RDS)
       ENDIF
!
 6     ITP=0
       IAP(27)=ITP
       ISTOP=IAP(34)
       IF(ISTOP.EQ.0)GOTO 3
!
       NBIF=IAP(35)
       IF(NBIF.NE.0 .AND. NBFC.LT.ABS(MXBF))GOTO 2
!
      DEALLOCATE(AA,RHS,U,DU,UDOT,UOLD,STUD,STU,STLA,STLD,F,DFDU,DFDP)
      DEALLOCATE(UZR)
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE STPNUS(IAP,RAP,PAR,ICP,U)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Gets the starting data from user supplied STPNT
!
      DIMENSION IAP(*)
!
       NDIM=IAP(1)
!
       CALL STPNT(NDIM,U,PAR,T)
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE STPNAE(IAP,RAP,PAR,ICP,U)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      LOGICAL FOUND
!
! Gets the starting data from unit 3
!
      DIMENSION IAP(*)
!
       IRS=IAP(3)
       CALL FINDLB(IAP,IRS,NFPRS,FOUND)
       CALL READLB(IAP,U,PAR)
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE STPRAE(IAP,RAP,PAR,ICP,FUNI,RDS,M1AA,AA,RHS, &
           RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Finds the second point on the initial solution branch.
!
      EXTERNAL FUNI
!
      DIMENSION IAP(*),AA(M1AA,*),RHS(*),U(*),UOLD(*),UDOT(*),DU(*)
      DIMENSION F(*),DFDU(*),DFDP(*),THL(*),THU(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
!
! Local
      ALLOCATABLE IR(:),IC(:)
!
       NDIM=IAP(1)
       IID=IAP(18)
!
       RLOLD(1)=PAR(ICP(1))
       DO I=1,NDIM
         UOLD(I)=U(I)
       ENDDO
!
! Determine the direction of the branch at the starting point
!
       CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
       DO I=1,NDIM
         RHS(I)=F(I)
         AA(I,NDIM+1)=DFDP((ICP(1)-1)*NDIM+I)
         AA(NDIM+1,I)=0.d0
         DO K=1,NDIM
           AA(I,K)=DFDU((K-1)*NDIM+I)
         ENDDO
       ENDDO
       RHS(NDIM+1)=0.d0
       AA(NDIM+1,NDIM+1)=0.d0
!
       IF(IID.GE.3)CALL WRJAC(NDIM+1,M1AA,AA,RHS)
       ALLOCATE(IR(NDIM+1),IC(NDIM+1))
       CALL NLVC(NDIM+1,M1AA,1,AA,DU,IR,IC)
       DEALLOCATE(IR,IC)
!
! Scale and make sure that the PAR(ICP(1))-dot is positive.
!
       SS=0.d0
       DO I=1,NDIM
         SS=SS+THU(I)*DU(I)**2
       ENDDO
       SS=SS+THL(1)*DU(NDIM+1)**2
!
       SIGN=1.d0
       IF(DU(NDIM+1).LT.0.d0)SIGN=-1.d0
       SC=SIGN/DSQRT(SS)
       DO I=1,NDIM+1
         DU(I)=SC*DU(I)
       ENDDO
!
       DO I=1,NDIM
         UDOT(I)=DU(I)
       ENDDO
       RLDOT(1)=DU(NDIM+1)
!
! Set initial approximations to the second point on the branch
!
       DO I=1,NDIM
         U(I)=UOLD(I)+RDS*UDOT(I)
       ENDDO
       RLCUR(1)=RLOLD(1)+RDS*RLDOT(1)
!
       CALL SOLVAE(IAP,RAP,PAR,ICP,FUNI,RDS,M1AA,AA,RHS, &
            RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE CONTAE(IAP,RAP,RDS,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! This subroutine determines an initial approximation to the next
! solution on a branch by extrapolating from the two preceding points.
! The step used in the preceding step has been stored in DSOLD.
!
      DIMENSION IAP(*),RAP(*),UOLD(*),U(*),UDOT(*)
      DIMENSION RLCUR(*),RLOLD(*),RLDOT(*)
!
       NDIM=IAP(1)
       IPS=IAP(2)
!
       DSOLD=RAP(5)
!
       RLDOT(1)=(RLCUR(1)-RLOLD(1))/DSOLD
       DO I=1,NDIM
         UDOT(I)=(U(I)-UOLD(I))/DSOLD
       ENDDO
!
       RLOLD(1)=RLCUR(1)
       RLCUR(1)=RLCUR(1)+RDS*RLDOT(1)
       DO I=1,NDIM
         UOLD(I)=U(I)
         U(I)=U(I)+UDOT(I)*RDS
       ENDDO
!      Save old time for time integration
       IF(IPS.EQ.-2)RAP(15)=RLOLD(1)
!
      RETURN
      END
!
!     ---------- -----
      SUBROUTINE SOLVAE(IAP,RAP,PAR,ICP,FUNI,RDS,M1AA,AA,RHS, &
           RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! This is the subroutine for computing solution branches. It solves
! the equations for finding the next point on the branch at distance DS
! from the current point. An initial approximation to the new point
! ( i.e. to PAR(ICP(1)) and U ) has been supplied by CONT.
!
      EXTERNAL FUNI
!
      DIMENSION IAP(*),RAP(*)
      DIMENSION AA(M1AA,*),RHS(*),U(*),DU(*),UOLD(*),UDOT(*) 
      DIMENSION F(*),DFDU(*),DFDP(*),THL(*),THU(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
! Local
      ALLOCATABLE IR(:),IC(:)
!
       NDIM=IAP(1)
       IADS=IAP(8)
       IID=IAP(18)
       ITNW=IAP(20)
       NDM=IAP(23)
       IBR=IAP(30)
!
       DSMIN=RAP(2)
       EPSL=RAP(11)
       EPSU=RAP(12)
!
       DELREF=0
 1     DSOLD=RDS
       RAP(5)=DSOLD
       DDS=1.d0/RDS
       NIT=0
       IAP(31)=NIT
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
       IF(IID.GE.2)THEN
          IF(NIT.EQ.0)THEN
             CALL WRBAR("=",47)
             IF(FORT9DST==1)WRITE(9,100)
          ENDIF
          IF(FORT9DST==1)WRITE(9,101)IBR,NTOP+1,NIT,RLCUR(1),RNRMV(NDM,U)
       ENDIF
 100   FORMAT(/,'  BR    PT  IT         PAR',11X,'L2-NORM')
 101   FORMAT(I4,I6,I4,5X,2ES14.5)
!
! Call user-supplied FUNC to evaluate the right hand side of the
! differential equation and its derivatives :
!
       DO NIT1=1,ITNW
!
         NIT=NIT1
         IAP(31)=NIT
         PAR(ICP(1))=RLCUR(1)
         CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
!
! Set up the Jacobian matrix and the right hand side :
!
         DO I=1,NDIM
           AA(I,NDIM+1)=DFDP((ICP(1)-1)*NDIM+I)
           RHS(I)=-F(I)
           DO K=1,NDIM
             AA(I,K)=DFDU((K-1)*NDIM+I)
           ENDDO
         ENDDO
         DO K=1,NDIM
           AA(NDIM+1,K)=2.d0*THU(K)*(U(K)-UOLD(K))*DDS
         ENDDO
         AA(NDIM+1,NDIM+1)=2.d0*THL(1)*(RLCUR(1)-RLOLD(1))*DDS
         SS=0.d0
         DO I=1,NDIM
           SS=SS+THU(I)*(U(I)-UOLD(I))**2
         ENDDO
         RHS(NDIM+1)=RDS-DDS*SS-THL(1)*DDS*(RLCUR(1)-RLOLD(1))**2
!
! Use Gauss elimination with pivoting to solve the linearized system :
!
         IF(IID.GE.5)CALL WRJAC(NDIM+1,M1AA,AA,RHS)
         ALLOCATE(IR(NDIM+1),IC(NDIM+1))
         CALL GE(0,NDIM+1,M1AA,AA,1,NDIM+1,DU,NDIM+1, &
              RHS,IR,IC,DET)
         DEALLOCATE(IR,IC)
         RAP(14)=DET
         DRLM=DU(NDIM+1)
!
! Add the Newton increments :
!
         DO I=1,NDIM
           U(I)=U(I)+DU(I)
         ENDDO
         RLCUR(1)=RLCUR(1)+DRLM
         DUMX=0.d0
         UMX=0.d0
         DO I=1,NDIM
           ADU=ABS(DU(I))
           AU=ABS(U(I))
           IF(AU.GT.UMX)UMX=AU
           IF(ADU.GT.DUMX)DUMX=ADU
         ENDDO
!
         IF(IID.GE.2)THEN
            IF(FORT9DST==1)WRITE(9,101)IBR,NTOP+1, &
                 NIT,RLCUR(1),RNRMV(NDM,U)
         ENDIF
!
         RDRLM= ABS(DRLM)/(1.d0+ ABS(RLCUR(1)))
         RDUMX=DUMX/(1.d0+UMX)
         IF(RDRLM.LE.EPSL.AND.RDUMX.LE.EPSU)THEN
           CALL PVLSAE(IAP,RAP,U,PAR)
           IF(IID.GE.2)THEN
             IF(FORT9DST==1)WRITE(9,*)
           ENDIF
           RETURN
         ENDIF
!
! Check whether relative error has reached user-supplied tolerance :
!
         IF(NIT.EQ.1)THEN
           DELREF=20*DMAX1(RDRLM,RDUMX)
         ELSE
           DELMAX=DMAX1(RDRLM,RDUMX)
           IF(DELMAX.GT.DELREF)EXIT
         ENDIF
!
       ENDDO
!
! Maximum number of iterations has been reached
!
       IF(IADS.EQ.0)THEN
         IF(FORT9DST==1)WRITE(9,102)IBR,NTOP
       ENDIF
 102   FORMAT(I4,I6,' NOTE:No convergence with fixed step size')
       IF(IADS.EQ.0)GOTO 5
!
! Reduce stepsize and try again
!
       MXT=ITNW
       IAP(31)=MXT
       CALL ADPTDS(IAP,RAP,RDS)
       IF(ABS(RDS).LT.DSMIN)GOTO 4
       RLCUR(1)=RLOLD(1)+RDS*RLDOT(1)
       DO I=1,NDIM
         U(I)=UOLD(I)+RDS*UDOT(I)
       ENDDO
       IF(IID.GE.2)THEN
         IF(FORT9DST==1)WRITE(9,103)
       ENDIF  
 103   FORMAT(I4,I6,' NOTE:Retrying step')
       GOTO 1
!
! Minimum stepsize reached
!
 4     IF(FORT9DST==1)WRITE(9,104)IBR,NTOP
 104   FORMAT(I4,I6,' NOTE:No convergence using minimum step size')
 5     RLCUR(1)=RLOLD(1)
       PAR(ICP(1))=RLCUR(1)
       DO I=1,NDIM
         U(I)=UOLD(I)
       ENDDO
       ISTOP=1
       IAP(34)=ISTOP
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!               Detection of Singular Points
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ------
      SUBROUTINE LCSPAE(IAP,RAP,PAR,ICP,FNCS,FUNI,M1AA,AA,RHS,RLCUR, &
           RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,Q,THL,THU,IUZ,VUZ)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! This subroutine uses the secant method to accurately locate special
! points (branch points, folds, Hopf bifurcations, user zeroes).
! These are characterized as zeroes of the function FNCS supplied in the
! call.
! This subroutine calls CONT and SOLVAE with varying stepsize RDS.
! The special point is assumed to have been found with sufficient
! accuracy if the ratio between RDS and the user supplied value of
! DS is less than the user-supplied toler EPSS.
!
      EXTERNAL FUNI,FNCS
!
      DIMENSION IAP(*),RAP(*),RLCUR(*),RLOLD(*),RLDOT(*),PAR(*),ICP(*)
      DIMENSION F(*),DFDU(*),DFDP(*),THL(*),THU(*)
      DIMENSION AA(M1AA,*),RHS(*),U(*),DU(*),UDOT(*),UOLD(*)
!
      LOGICAL CHNG
!
       IID=IAP(18)
       ITMX=IAP(19)
       IBR=IAP(30)
!
       DS=RAP(1)
       DSMAX=RAP(3)
       DSOLD=RAP(5)
       EPSS=RAP(13)
!
! Check whether FNCS has changed sign (FNCS is EXTERNAL).
!
       Q0=Q
       Q1=FNCS(IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA, &
            RLCUR,RLOLD,RLDOT,U,UOLD,UDOT,RHS,DFDU,DFDP,IUZ,VUZ)
       PQ=Q0*Q1
       NTOT=IAP(32)
       IF(PQ.GE.0.d0 .OR. (.NOT. CHNG))THEN
         Q=Q1
         RETURN
       ENDIF
!
! Use the secant method for the first step:
!
       S0=0.d0
       S1=DSOLD
       ITLCSP=0
       DQ=Q0-Q1
       RDS=Q1/DQ*(S1-S0)
 1     RDS=(1.d0+HMACH)*RDS
       S=S1+RDS
!
! Return if relative tolerance has been met :
!
       RRDS=ABS(RDS)/(1+DSQRT(ABS(DS*DSMAX)))
       IF(RRDS.LT.EPSS)THEN
         ITP=-1
         IAP(27)=ITP
         Q=0.d0
         IF(FORT9DST==1)WRITE(9,102)RDS
         RETURN
       ENDIF
!
! If requested write additional output on unit 9 :
!
       IF(IID.GE.2)THEN
          IF(FORT9DST==1)WRITE(9,101)ITLCSP,RDS
       ENDIF
!
       CALL CONTAE(IAP,RAP,RDS,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT)
       CALL SOLVAE(IAP,RAP,PAR,ICP,FUNI,RDS,M1AA,AA,RHS, &
            RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)THEN
         Q=0.d0
         RETURN
       ENDIF
!
       Q=FNCS(IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA, &
            RLCUR,RLOLD,RLDOT,U,UOLD,UDOT,RHS,DFDU,DFDP,IUZ,VUZ)
       ITLCSP=ITLCSP+1
       IF(ITLCSP.LE.ITMX)THEN
!        Use Mueller's method with bracketing for subsequent steps
         CALL MUELLER(Q0,Q1,Q,S0,S1,S,RDS)
         GOTO 1
       ELSE
         IF(FORT9DST==1)WRITE(9,103)IBR,MOD(NTOT-1,9999)+1
         Q=0.d0
         RETURN
       ENDIF
!
 101   FORMAT(' ==> Location of special point :  Iteration ',I3, &
            '  Step size = ',ES13.5)
 102   FORMAT(' ==> Location of special point : ', &
            ' Convergence.   Step size = ',ES13.5)
 103   FORMAT(I4,I6,' NOTE:Possible special point')
      END
!
!     ---------- -------
      SUBROUTINE MUELLER(Q0,Q1,Q,S0,S1,S,RDS)
!
! Mueller's method with bracketing
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
        H0=S0-S
        H1=S1-S
        D=H0*H1*(H1-H0)
        A=( H1**2*(Q0-Q) - H0**2*(Q1-Q) ) / D
        B=(-H1*(Q0-Q)    + H0*(Q1-Q)    ) / D
        IF(ABS(B).LE.RSMALL)THEN
          RDS=-Q/A
        ELSE
          C=A/(2*B)
          R=DSQRT(C**2-Q/B)
          IF(C.LT.0.d0)THEN
            RDS=-C - R
          ELSE
            RDS=-C + R
          ENDIF
        ENDIF
!
        DQ=Q1*Q
        IF(DQ.LT.0.d0)THEN
          Q0=Q1
          S0=S1
        ENDIF
        Q1=Q
        S1=S
!
      RETURN
      END
!
!     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNBPAE &
           (IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA,RLCUR,RLOLD,RLDOT,U,UOLD, &
           UDOT,RHS,DFDU,DFDP,IUZ,VUZ)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      LOGICAL CHNG
!
      DIMENSION IAP(*),RAP(*)
!
       IID=IAP(18)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
!
       DET=RAP(14)
       FNBPAE=DET
       CHNG=.TRUE.
!
! If requested write additional output on unit 9 :
!
       IF(IID.GE.2)THEN
         IF(FORT9DST==1)WRITE(9,101)IBR,NTOP+1,FNBPAE
       ENDIF  
 101   FORMAT(I4,I6,9X,'BP   Function:',ES14.5)
!
      RETURN
      END
!
!     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNLPAE &
           (IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT, &
           RHS,DFDU,DFDP,IUZ,VUZ)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      EXTERNAL FUNI
!
      DIMENSION IAP(*),RAP(*)
      DIMENSION AA(M1AA,*),RHS(*),U(*),UOLD(*),UDOT(*),DFDU(*),DFDP(*) 
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
! Local
      ALLOCATABLE UD(:),IR(:),IC(:)
!
      LOGICAL CHNG
!
       NDIM=IAP(1)
       IID=IAP(18)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
!
       PAR(ICP(1))=RLCUR(1)
       CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,2,RHS,DFDU,DFDP)
       DO I=1,NDIM
         AA(I,NDIM+1)=DFDP((ICP(1)-1)*NDIM+I)
         DO K=1,NDIM
           AA(I,K)=DFDU((K-1)*NDIM+I)
         ENDDO
       ENDDO
       DO K=1,NDIM
         AA(NDIM+1,K)=UDOT(K)
         RHS(K)=0.d0
       ENDDO
       AA(NDIM+1,NDIM+1)=RLDOT(1)
       RHS(NDIM+1)=1.d0
!
       ALLOCATE(UD(NDIM+1),IR(NDIM+1),IC(NDIM+1))
       CALL GE(0,NDIM+1,M1AA,AA,1,NDIM+1,UD,NDIM+1,RHS,IR,IC,DET)
       RAP(14)=DET
       CALL NRMLZ(NDIM+1,UD)
       FNLPAE=UD(NDIM+1)
       DEALLOCATE(UD,IR,IC)
       RAP(16)=FNLPAE
       CHNG=.TRUE.
!
! If requested write additional output on unit 9 :
!
       IF(IID.GE.2)THEN
         IF(FORT9DST==1)WRITE(9,101)ABS(IBR),NTOP+1,FNLPAE
       ENDIF  
 101   FORMAT(I4,I6,9X,'Fold Function:',ES14.5)
!
      RETURN
      END
!
!     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNHBAE &
           (IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT, &
           RHS,DFDU,DFDP,IUZ,VUZ)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
      EXTERNAL FUNI
!
      DIMENSION AA(M1AA,*),RHS(*),U(*),UOLD(*),UDOT(*)
      DIMENSION PAR(*),ICP(*),IAP(*),RAP(*)
! Local
      COMPLEX(KIND(1.0D0)) EV, ZTMP
      ALLOCATABLE EV(:)
      LOGICAL CHNG
!
       NDIM=IAP(1)
       NDM=IAP(23)
       IPS=IAP(2)
       ISP=IAP(9)
       ISW=IAP(10)
       IID=IAP(18)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
       ALLOCATE(EV(NDIM))
!
! INITIALIZE
!
       CHNG=.FALSE.
!
! Compute the eigenvalues of the Jacobian
!
       CALL EIG(IAP,NDM,NDIM,DFDU,EV,IER)
       IF(IPS.EQ.-1)THEN
         DO I=1,NDM
           IF(REAL(EV(I)).NE.-1.d0 .OR. &
                AIMAG(EV(I)).NE. 0.d0)THEN
             EV(I)=LOG(1.d0+EV(I))
           ELSE
             EV(I)= CMPLX(-RLARGE,0.d0,KIND(1.0D0))
           ENDIF
         ENDDO
       ENDIF
!
! Order the eigenvalues by real part.
!
       DO I=1,NDM-1
         RMAX=-RLARGE
         LOC=I
         DO J=I,NDM
           RP=REAL(EV(J))
           IF(RP.GE.RMAX)THEN
             RMAX=RP
             LOC=J
           ENDIF
         ENDDO
         IF(LOC.NE.I) THEN
           ZTMP=EV(LOC)
           EV(LOC)=EV(I)
           EV(I)=ZTMP
         ENDIF
       ENDDO
!
! Compute the smallest real part.
!
       RIMHB=0.d0
       AREV=RLARGE
       REV=0.d0
       DO I=1,NDM
         IF(AIMAG(EV(I)).NE.0.d0)THEN
           AR=ABS(REAL(EV(I)))
           IF(AR.LE.AREV)THEN
             AREV=AR
             REV=REAL(EV(I))
             RIMHB=ABS(AIMAG(EV(I)))
             IF(RIMHB.NE.0.d0.AND.ABS(ISW).LE.1)PAR(11)=PI(2.d0)/RIMHB
           ENDIF
         ENDIF
       ENDDO
!
! Count the number of eigenvalues with negative real part.
!
! Set tolerance for deciding if an eigenvalue is in the positive
! half-plane. Use, for example, tol=1d-3 for conservative systems.
!
       tol=1.d-5
       NINS1=0
       DO I=1,NDM
         IF(REAL(EV(I)).LE.tol)NINS1=NINS1+1
       ENDDO
!
       IF(ISW.EQ.2 .OR. ISP.EQ.0 .OR. ISP.EQ.3)THEN
         FNHBAE=0.d0
       ELSE
         FNHBAE=REV
       ENDIF
       RAP(17)=FNHBAE
       NINS=IAP(33)
       IF(NINS1.NE.NINS)CHNG=.TRUE.
       NINS=NINS1
       IAP(33)=NINS
!
       NTOT=IAP(32)
       NTOTP1=NTOT+1
       IF(IID.GE.2)THEN
         IF(FORT9DST==1)WRITE(9,101)ABS(IBR),NTOP+1,FNHBAE
       ENDIF  
       IF(NINS1.EQ.NDM)NTOTP1=-NTOTP1
!
       IF(FORT9DST==1)WRITE(9,102)ABS(IBR),NTOP+1,NINS
       IF(IPS.EQ.-1)THEN
          DO I=1,NDM
             IF(FORT9DST==1)WRITE(9,103)ABS(IBR),NTOP+1,I,EXP(EV(I))
          ENDDO
       ELSE
          DO I=1,NDM
             IF(FORT9DST==1)WRITE(9,103)ABS(IBR),NTOP+1,I,EV(I)
          ENDDO
       ENDIF
!
 101   FORMAT(I4,I6,9X,'Hopf Function:',ES14.5)
 102   FORMAT(/,I4,I6,9X,'Eigenvalues  :   Stable:',I4)
 103   FORMAT(I4,I6,9X,'Eigenvalue',I3,":",2ES14.5)
!
      DEALLOCATE(EV)
      RETURN
      END
!
!     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNUZAE &
           (IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT, &
           RHS,DFDU,DFDP,IUZ,VUZ)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),PAR(*),IUZ(*),VUZ(*)
!
      LOGICAL CHNG
!
       IID=IAP(18)
       IUZR=IAP(26)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
!
       FNUZAE=PAR(ABS(IUZ(IUZR)))-VUZ(IUZR)
       CHNG=.TRUE.
!
       IF(IID.GE.3)THEN
         IF(FORT9DST==1)WRITE(9,101)ABS(IBR),NTOP+1,IUZR,FNUZAE
       ENDIF
 101   FORMAT(I4,I6,9X,'User Func.',I3,1X,ES14.5)
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                   Branch Switching for Algebraic Problems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- -----
      SUBROUTINE STBIF(IAP,ICP,M1AA,AA,M1SB,STUD,STU,STLA, &
           STLD,RLCUR,RLDOT,U,DU,UDOT,DFDU,DFDP,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Stores branching data in the following arrays :
!        STU    ( the solution vector U )
!        STUD   ( U-dot )
!        STLA   ( PAR(ICP(1)) )
!        STLD  ( PAR(ICP(1))-dot )
! Here the vector ( PAR(ICP(1))-dot , U-dot ) lies in the 2-d nullspace
! at branch point and is perpendicular to the direction vector of
! known branch at this point.
!
      DIMENSION IAP(*),AA(M1AA,*),U(*),DU(*),UDOT(*),DFDU(*),DFDP(*)
      DIMENSION STUD(M1SB,*),STU(M1SB,*),STLA(*),STLD(*)
      DIMENSION ICP(*),RLCUR(*),RLDOT(*),THL(*),THU(*)
      ALLOCATABLE IR(:),IC(:)
!
       NDIM=IAP(1)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
       NBIF=IAP(35)
!
! Keep track of the number of branch points stored.
!
       IF(NBIF.EQ.NBIFX)THEN
         IF(FORT9DST==1)WRITE(9,101)IBR,NTOP
       ENDIF
       IF(NBIF.GT.NBIFX)THEN
         NBIF=NBIFX
         IAP(35)=NBIF
         RETURN
       ENDIF
!
       DO I=1,NDIM
         DO J=1,NDIM
           AA(I,J)=DFDU((J-1)*NDIM+I)
         ENDDO
       ENDDO
!
       ND1=NDIM+1
       DO I=1,NDIM
         AA(I,ND1)=DFDP((ICP(1)-1)*NDIM+I)
         AA(ND1,I)=UDOT(I)
       ENDDO
       AA(ND1,ND1)=RLDOT(1)
!
       ALLOCATE(IR(NDIM+1),IC(NDIM+1))
       CALL NLVC(ND1,M1AA,1,AA,DU,IR,IC)
       DEALLOCATE(IR,IC)
!
       SS=0.d0
       DO I=1,NDIM
         SS=SS+THU(I)*DU(I)**2
       ENDDO
       SS=SS+THL(1)*DU(ND1)**2
       SC=1.d0/DSQRT(SS)
!
       DO I=1,ND1
         DU(I)=SC*DU(I)
       ENDDO
!
       NBIF=IAP(35)
       STLD(NBIF)=DU(ND1)
       DO I=1,NDIM
         STU(NBIF,I)=U(I)
         STUD(NBIF,I)=DU(I)
       ENDDO
       STLA(NBIF)=RLCUR(1)
!
      RETURN
 101   FORMAT(I4,I6,' NOTE:No more branch points can be stored')
      END
!
!     ---------- -----
      SUBROUTINE SWPNT(IAP,RAP,PAR,ICP,RDS,M1SB,STUD,STU,STLA,STLD, &
           RLCUR,RLDOT,U,UDOT)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! This subroutine retrieves the branching data U, U-dot, PAR(ICP(1)),
! PAR(ICP(1))-dot. If this initialization corresponds to the computation
! of the bifurcating branch in opposite direction, then only the sign of
!  the stepsize ( DS ) along the branch is reversed.
!
      DIMENSION IAP(*),RAP(*)
      DIMENSION U(*),UDOT(*),STUD(M1SB,*),STU(M1SB,*),STLA(*),STLD(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLDOT(*)
!
       NDIM=IAP(1)
       ISW=IAP(10)
       MXBF=IAP(17)
       NBIF=IAP(35)
       IPOS=IAP(36)
!
       DS=RAP(1)
!
       RDS=DS
       IF(IPOS.EQ.0)RDS=-DS
       RLCUR(1)=STLA(1)
       PAR(ICP(1))=RLCUR(1)
       RLDOT(1)=STLD(1)
       DO I=1,NDIM
         U(I)=STU(1,I)
         UDOT(I)=STUD(1,I)
       ENDDO
       IF(ABS(ISW).EQ.2)PAR(ICP(2))=U(NDIM)
!
       IF(MXBF.GE.0)THEN
         IPOS=1-IPOS
         IAP(36)=IPOS
       ENDIF
       IF(IPOS.EQ.0)RETURN
!
       DO I=1,NBIF
         STLA(I)=STLA(I+1)
         STLD(I)=STLD(I+1)
         DO I1=1,NDIM
           STU(I,I1)=STU(I+1,I1)
           STUD(I,I1)=STUD(I+1,I1)
         ENDDO
       ENDDO
!
      RETURN
      END
!
!     ---------- -----
      SUBROUTINE SWPRC(IAP,RAP,PAR,ICP,FUNI,M1AA,AA,RHS, &
           RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,RDS,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Controls the computation of the second point on a bifurcating branch.
! This point is required to lie in a hyper-plane at distance DS from the
! branch point. This hyper-plane is parallel to the tangent of the
! known branch at the branch point.
!
      EXTERNAL FUNI
!
      DIMENSION AA(M1AA,*),RHS(*),U(*),UOLD(*),UDOT(*),DU(*)
      DIMENSION IAP(*),RAP(*),F(*),DFDU(*),DFDP(*),THL(*),THU(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
! Local
      ALLOCATABLE IR(:),IC(:),U1(:)
!
       NDIM=IAP(1)
       IADS=IAP(8)
       IID=IAP(18)
       ITNW=IAP(20)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
!
       DSMIN=RAP(2)
       EPSL=RAP(11)
       EPSU=RAP(12)
!
! Initialize and provide initial guess :
!
       ALLOCATE(IR(NDIM+1),IC(NDIM+1),U1(NDIM+1))
       RLOLD(1)=RLCUR(1)
       RLCUR(1)=RLOLD(1)+RDS*RLDOT(1)
       DO I=1,NDIM
         UOLD(I)=U(I)
         U(I)=UOLD(I)+RDS*UDOT(I)
       ENDDO
!
 2     DSOLD=RDS
       RAP(5)=DSOLD
       NIT=0
       IAP(31)=NIT
!
! Write additional output on unit 9 if requested :
!
       NDMR=NDIM
       IF(NDMR.GT.6)NDMR=6
       IF(IID.GE.2)THEN
         IF(FORT9DST==1)THEN
           WRITE(9,101)IBR,NTOP,NIT,ICP(1),RLCUR(1),(U(I),I=1,NDMR)
         ENDIF
       ENDIF
!
       RLM1=RLCUR(1)
       DO I=1,NDIM
         U1(I)=U(I)
       ENDDO
!
       DO 3 NIT1=1,ITNW
!
         NIT=NIT1
         IAP(31)=NIT
         PAR(ICP(1))=RLCUR(1)
         CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
         DO I=1,NDIM
           AA(I,NDIM+1)=DFDP((ICP(1)-1)*NDIM+I)
           RHS(I)=-F(I)
           DO K=1,NDIM
             AA(I,K)=DFDU((K-1)*NDIM+I)
           ENDDO
         ENDDO
         DO K=1,NDIM
           AA(NDIM+1,K)=THU(K)*UDOT(K)
         ENDDO
         AA(NDIM+1,NDIM+1)=THL(1)*RLDOT(1)
         SS=0.d0
         DO I=1,NDIM
           SS=SS+THU(I)*(U(I)-U1(I))*UDOT(I)
         ENDDO
         RHS(NDIM+1)=-SS-THL(1)*(RLCUR(1)-RLM1)*RLDOT(1)
!
! Use Gauss elimination with pivoting to solve the linearized system :
!
         IF(IID.GE.5)CALL WRJAC(NDIM+1,M1AA,AA,RHS)
         CALL GE(0,NDIM+1,M1AA,AA,1,NDIM+1,DU,NDIM+1,RHS,IR,IC,DET)
         RAP(14)=DET
         DRLM=DU(NDIM+1)
!
! Add the Newton increments :
!
         DO I=1,NDIM
           U(I)=U(I)+DU(I)
         ENDDO
         RLCUR(1)=RLCUR(1)+DRLM
         DUMX=0.d0
         UMX=0.d0
         DO I=1,NDIM
           ADU=ABS(DU(I))
           IF(ADU.GT.DUMX)DUMX=ADU
           AU=ABS(U(I))
           IF(AU.GT.UMX)UMX=AU
         ENDDO
!
         IF(IID.GE.2)THEN
           IF(FORT9DST==1)THEN
             WRITE(9,101)IBR,NTOP,NIT,ICP(1),RLCUR(1),(U(I),I=1,NDMR)
           ENDIF
         ENDIF
!
! Check whether relative error has reached user-supplied tolerance :
!
         RDRLM=ABS(DRLM)/(1.d0+ABS(RLCUR(1)))
         RDUMX=DUMX/(1.d0+UMX)
         IF(RDRLM.LT.EPSL.AND.RDUMX.LT.EPSU)THEN
           DEALLOCATE(IR,IC,U1)
           RETURN
         ENDIF
 3     CONTINUE
!
! Maximum number of iterations reached. Reduce stepsize and try again.
!
       IF(IADS.EQ.0)THEN
         IF(FORT9DST==1)WRITE(9,102)IBR,NTOP
       ENDIF  
       IF(IADS.EQ.0)GOTO 5
!
       MXT=ITNW
       IAP(31)=MXT
       CALL ADPTDS(IAP,RAP,RDS)
       IF(ABS(RDS).LT.DSMIN)GOTO 4
       RLCUR(1)=RLOLD(1)+RDS*RLDOT(1)
       DO I=1,NDIM
         U(I)=UOLD(I)+RDS*UDOT(I)
       ENDDO
       IF(IID.GE.2)THEN
         IF(FORT9DST==1)WRITE(9,103)IBR,NTOP
       ENDIF
       GOTO 2
!
! Minimum stepsize reached.
!
 4     IF(FORT9DST==1)WRITE(9,104)IBR,NTOP
 5     RLCUR(1)=RLOLD(1)
       PAR(ICP(1))=RLCUR(1)
       DO I=1,NDIM
         U(I)=UOLD(I)
       ENDDO
       ISTOP=1
       IAP(34)=ISTOP
!
       DEALLOCATE(IR,IC,U1)
      RETURN
 101   FORMAT(' Branch ',I2,' N=',I5,1X,'IT=',I2,1X,'PAR(',I2,')=', &
            ES11.3,1X,'U=',7ES11.3)
 102   FORMAT(I4,I6,' NOTE:No convergence when switching branches', &
            ' with fixed step size')
 103   FORMAT(I4,I6,' NOTE:Retrying step')
 104   FORMAT(I4,I6,' NOTE:No convergence when switching branches', &
            ' with minimum step size')
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    Output (Algebraic Problems)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE STHD(IAP,RAP,ICP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT7DST, & 
           FORT8DST,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Write the values of the user defined parameters on unit 7.
! This identifying information is preceded by a '   0' on each line.
! The first line in the file contains the (generally) user-supplied
! limits of the bifurcation diagram, viz. RL0,RL1,A0 and A1.
! These are often convenient for an initial plot of the diagram.
!
       DIMENSION ICP(*),IAP(*),RAP(*)
       CHARACTER (LEN=*), PARAMETER :: D3 = "('   0',3(A8,ES11.4))"
       CHARACTER (LEN=*), PARAMETER :: I4 = "('   0',4(A8,I4))"
       CHARACTER (LEN=*), PARAMETER :: I5 = "('   0',3(A8,I4),2(A7,I4))"
!
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       ILP=IAP(4)
       NTST=IAP(5)
       NCOL=IAP(6)
       IAD=IAP(7)
       ISP=IAP(9)
       ISW=IAP(10)
       IPLT=IAP(11)
       NBC=IAP(12)
       NINT=IAP(13)
       NMX=IAP(14)
       NUZR=IAP(15)
       NPR=IAP(16)
       MXBF=IAP(17)
       IID=IAP(18)
       ITMX=IAP(19)
       ITNW=IAP(20)
       NWTN=IAP(21)
       JAC=IAP(22)
       NFPR=IAP(29)
       NICP=IAP(41)
!
       DS=RAP(1)
       DSMIN=RAP(2)
       DSMAX=RAP(3)
       RL0=RAP(6)
       RL1=RAP(7)
       A0=RAP(8)
       A1=RAP(9)
       EPSL=RAP(11)
       EPSU=RAP(12)
       EPSS=RAP(13)
!
      IF( FORT7DST == 1 )THEN 
       WRITE(7,"(I4,' ',4ES12.4)")0,RL0,RL1,A0,A1
       WRITE(7,D3)'EPSL=',EPSL,'EPSU =',EPSU, 'EPSS =',EPSS
       WRITE(7,D3)'DS  =',DS,  'DSMIN=',DSMIN,'DSMAX=',DSMAX
       WRITE(7,I4)'NDIM=',NDIM,'IPS =',IPS, 'IRS =',IRS, 'ILP =',ILP
       WRITE(7,I4)'NTST=',NTST,'NCOL=',NCOL,'IAD =',IAD, 'ISP =',ISP
       WRITE(7,I4)'ISW =',ISW, 'IPLT=',IPLT,'NBC =',NBC, 'NINT=',NINT
       WRITE(7,I4)'NMX= ',NMX, 'NPR =',NPR, 'MXBF=',MXBF,'IID =',IID
       WRITE(7,I5)'ITMX=',ITMX,'ITNW=',ITNW,'NWTN=',NWTN,'JAC=',JAC, &
            '  NUZR=',NUZR
!
       WRITE(7,"('   0   User-specified parameter')",ADVANCE="NO")
       IF(NICP.EQ.1)THEN
         WRITE(7,"(':       ',  I4)")(ICP(NPARX+I),I=1,NICP)
       ELSE
         WRITE(7,"('s:      ',24I4)")(ICP(NPARX+I),I=1,NICP)
       ENDIF
!
       WRITE(7,"('   0   Active continuation parameter')",ADVANCE="NO")
       IF(NFPR.EQ.1)THEN
         WRITE(7,"(':  ',  I4)")(ICP(I),I=1,NFPR)
       ELSE
         WRITE(7,"('s: ',24I4)")(ICP(I),I=1,NFPR)
       ENDIF
       CALL FLUSH(7)
      ENDIF
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE HEADNG(IAP,ICP,IUNIT,N1,N2)
!
      USE AUTO_CONSTANTS, ONLY:ODIM,OUT,FORT7DST,FORT8DST,FORT9DST
!
      IMPLICIT NONE
!
! Prints headings above columns on unit 6, 7, and 9.
! N1 = number of parameters to print (maximum: 7 for screen output)
! N2 = number of (max) variables to print (maximum: max(0,100-N1,100))
!
      INTEGER IAP(*),ICP(*),IUNIT,N1,N2
! Local
      INTEGER I,J,IPS,IPLT,NDM
      CHARACTER(1000) MATLABDISPSTR
!
       IPS=IAP(2)
       IPLT=IAP(11)
       NDM=IAP(23)
!
       IF(IUNIT.EQ.7)THEN
        IF( FORT7DST == 1 )THEN   
          WRITE(7,"(I4/I4,A)",ADVANCE="NO")0,0,'    PT  TY  LAB '
        ENDIF
       ELSEIF(IUNIT.EQ.6)THEN
          WRITE(MATLABDISPSTR,"(A)")'  BR    PT  TY  LAB '     
          CALL MEXPRINTF(MATLABDISPSTR)
       ELSEIF(IUNIT.EQ.8)THEN
        IF( FORT8DST == 1 )THEN  
         WRITE(IUNIT,"(1X/A)",ADVANCE="NO")'  BR    PT  TY  LAB '
        ENDIF 
       ELSEIF(IUNIT.EQ.9)THEN
        IF( FORT9DST == 1 )THEN  
         WRITE(IUNIT,"(1X/A)",ADVANCE="NO")'  BR    PT  TY  LAB '
        ENDIF 
       ELSE
         WRITE(IUNIT,"(1X/A)",ADVANCE="NO")'  BR    PT  TY  LAB ' 
       ENDIF
!
       DO J=1,N1+N2+1
          IF(J==1.OR.J>N2+2)THEN
             I=1
             IF(J>1)I=J-N2-1
             IF(ICP(I)==11.AND.IPS>0.AND.IPS/=4.AND.IPS/=7)THEN
                CALL WRITECOL(5,'  PERIOD')
             ELSEIF(ICP(I)==10.AND.(IPS==5.OR.IPS==15))THEN
                CALL WRITECOL(6,'FOPT')
             ELSEIF(ICP(I)==14.AND.(IPS==14.OR.IPS==16))THEN
                CALL WRITECOL(6,'TIME')
             ELSE
                CALL WRITECOL(4,'   PAR',ICP(I))
             ENDIF
          ELSEIF(J==2)THEN
             IF(IPLT>NDM.AND.IPLT<=2*NDM) THEN
                CALL WRITECOL(2,'INTEGRAL U',IPLT-NDM)
             ELSE IF(IPLT>2*NDM.AND.IPLT<=3*NDM) THEN
                CALL WRITECOL(2,'   L2-NORM U',IPLT-2*NDM)
             ELSE IF(IPLT>0.AND.IPLT<=NDM) THEN
                IF(ABS(IPS)<=1.OR.IPS==5.OR.IPS==11)THEN
                   CALL WRITECOL(6,'    U',IPLT)
                ELSE
                   CALL WRITECOL(4,'  MAX U',IPLT)
                ENDIF
             ELSE IF(IPLT<0.AND.IPLT>=-NDM) THEN
                IF(ABS(IPS)<=1.OR.IPS==5.OR.IPS==11)THEN
                   CALL WRITECOL(6,'    U',-IPLT)
                ELSE
                   CALL WRITECOL(4,'  MIN U',-IPLT)
                ENDIF
             ELSE
                CALL WRITECOL(4,'   L2-NORM')
             ENDIF
          ELSE !J>2 with N2>0
             IF(ABS(IPS)<=1.OR.IPS==5.OR.IPS==11)THEN
                CALL WRITECOL(6,'    U',J-2)
             ELSE
                CALL WRITECOL(4,'  MAX U',J-2)
             ENDIF
          ENDIF
!
       ENDDO
!
       IF(IUNIT==7)THEN
         DO I=1,ODIM
            CALL WRITECOL(4,'   OUT',I)      
         ENDDO
       ENDIF
!       
       WRITE(IUNIT,"()")
       CALL FLUSH(IUNIT)
      RETURN
      CONTAINS

      SUBROUTINE WRITECOL(I,S,N)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: I
      CHARACTER(*), INTENT(IN) :: S
      INTEGER, INTENT(IN), OPTIONAL :: N
! Local
      CHARACTER(10) SN
      CHARACTER(19) COL
      COL=' '
      IF(PRESENT(N))THEN
         WRITE(SN,"(I10)")N
         IF(N<10)THEN
           WRITE(COL(I:),"(A,A,A,A)") S,'(0',TRIM(ADJUSTL(SN)),')'
         ELSE
           WRITE(COL(I:),"(A,A,A,A)") S,'(',TRIM(ADJUSTL(SN)),')'
         ENDIF
      ELSE
         WRITE(COL(I:),"(A)") S
      ENDIF
      IF(IUNIT.EQ.7)THEN
        IF( FORT7DST == 1 )THEN  
         WRITE(IUNIT,"(A19)",ADVANCE="NO")COL
        ENDIF 
      ELSEIF(IUNIT.EQ.6)THEN
        WRITE(MATLABDISPSTR,"(A19)")COL
        CALL MEXPRINTF(MATLABDISPSTR)
      ELSEIF(IUNIT.EQ.8)THEN
        IF( FORT8DST == 1 )THEN  
         WRITE(IUNIT,"(A14)",ADVANCE="NO")COL       
        ENDIF
      ELSEIF(IUNIT.EQ.9)THEN
        IF( FORT9DST == 1 )THEN  
          WRITE(IUNIT,"(A14)",ADVANCE="NO")COL       
        ENDIF  
      ELSE      
         WRITE(IUNIT,"(A14)",ADVANCE="NO")COL
      ENDIF
      END SUBROUTINE WRITECOL

      END SUBROUTINE HEADNG
!
!     ---------- ------
      SUBROUTINE STPLAE(IAP,RAP,PAR,ICP,RLCUR,U)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT7DST,FORT8DST, &
           FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Stores the bifurcation diagram on unit 7 (Algebraic Problems).
! Every line written contains, in order, the following:
!
!  IBR    : The label of the branch.
!  NTOT   : The index of the point on the branch.
!           (Points are numbered consecutively along a branch).
!           If IPS=1 or -1, then the sign of NTOT indicates stability :
!            - = stable , + = unstable, unknown, or not relevant.
!  ITP    : An integer indicating the type of point :
!
!             1  (BP)  :   Branch point.
!             2  (LP)  :   Fold.
!             3  (HB)  :   Hopf bifurcation point.
!             4  (  )  :   Output point (Every NPR steps along branch).
!            -4  (UZ)  :   Output point (Zero of user function).
!             9  (EP)  :   End point of branch, normal termination.
!            -9  (MX)  :   End point of branch, abnormal termination.
!
!  LAB        : The label of a special point.
!  PAR(ICP(1)): The principal parameter.
!  A          : The L2-norm of the solution vector, or other measure of
!               the solution (see the user-supplied parameter IPLT).
!  U          : The first few components of the solution vector.
!  PAR(ICP(*)): Further free parameters (if any).
!
      DIMENSION IAP(*),ICP(*),RAP(*),RLCUR(*),U(*)
!
       NDIM=IAP(1)
       IPS=IAP(2)
       ISW=IAP(10)
       IPLT=IAP(11)
       NMX=IAP(14)
       NPR=IAP(16)
       NDM=IAP(23)
       ITP=IAP(27)
       ITPST=IAP(28)
       IBR=IAP(30)
!
       RL0=RAP(6)
       RL1=RAP(7)
       A0=RAP(8)
       A1=RAP(9)
!
       NTOT=IAP(32)
       NTOT=NTOT+1
       IAP(32)=NTOT
!
       CALL PVLSAE(IAP,RAP,U,PAR)
!
! ITP is set to 4 every NPR steps along a branch, and the entire
! solution is written on unit 8.
!
       IF(NPR.NE.0)THEN
         IF(MOD(NTOT,NPR).EQ.0 .AND. MOD(ITP,10).EQ.0)ITP=4+10*ITPST
         IAP(27)=ITP
       ENDIF
!
! CHECK WHETHER LIMITS OF THE BIFURCATION DIAGRAM HAVE BEEN REACHED :
!
       IAB=ABS(IPLT)
!
       IF(IAB.LE.NDIM .AND. IAB.GT.0)THEN
         AMP=U(IAB)
       ELSE IF(IPLT.GT.NDIM.AND.IPLT.LE.2*NDIM)THEN
         AMP=U(IPLT-NDIM)
       ELSE IF(IPLT.GT.2*NDIM.AND.IPLT.LE.3*NDIM)THEN
         AMP=U(IPLT-2*NDIM)
       ELSE
         AMP=RNRMV(NDM,U)
       ENDIF
       RAP(10)=AMP
!
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)THEN
!        Maximum number of iterations reached somewhere.
         ITP=-9-10*ITPST
         IAP(27)=ITP
       ELSEIF(ISTOP.EQ.-1)THEN
!        ** UZR endpoint
         ITP=9+10*ITPST
         IAP(27)=ITP
       ELSE
         IF(RLCUR(1).LT.RL0.OR.RLCUR(1).GT.RL1  &
              .OR. AMP.LT.A0.OR.AMP.GT.A1 &
              .OR. NTOT.EQ.NMX) THEN
           ISTOP=1
           IAP(34)=ISTOP
           ITP=9+10*ITPST
           IAP(27)=ITP
         ENDIF
       ENDIF
!
       LABW=0
       IF(MOD(ITP,10).NE.0)THEN
         LAB=IAP(37)
         LAB=LAB+1
         IAP(37)=LAB
         LABW=LAB
       ENDIF
!
! Determine stability and print output on units 6 and 7.
!
       NTOTS=NTOT
       NINS=IAP(33)
       IF(ABS(IPS).EQ.1 .AND. ABS(ISW).LE.1 .AND. NTOT.GT.1)THEN
         IF(NINS.EQ.NDIM)NTOTS=-NTOT
       ENDIF
       CALL WRLINE(IAP,PAR,ICP(NPARX+1),IBR,NTOTS,LABW,AMP,U)
!
! Write restart information for multi-parameter analysis :
!
       IF(LABW.NE.0)CALL WRTSP8(IAP,RAP,PAR,ICP,LABW,RLCUR,U)
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE WRLINE(IAP,PAR,ICU,IBR,NTOT,LAB,VAXIS,U)
!
      USE AUTO_CONSTANTS, ONLY:IBRF7,oldIBRF7,MTOTF7,oldMTOTF7,ITPF7, &
           oldITPF7,LABF7,oldLABF7,PARF7,oldPARF7,VAXISF7,oldVAXISF7,UF7,oldUF7, &
           OUTF7,oldOUTF7,ODIM,OUT,NPARX,NBIFX,NIAP,NRAP,FORT7DST,FORT8DST, &
           FORT9DST,RUNMODEDST
!      INCLUDE 'auto.h'
!
      IMPLICIT NONE
!
      INTEGER R,C,P,I,J      
!
! Write one line of output on unit 6 and 7.
!
      INTEGER IAP(*),ICU(*),IBR,NTOT,LAB
      DOUBLE PRECISION PAR(*),U(*),VAXIS
! Local
      CHARACTER*2 ATYPE
      CHARACTER*2, PARAMETER :: ATYPESP(9) = &
           (/ 'BP','LP','HB','  ','LP','BP','PD','TR','EP' /)
      CHARACTER*2, PARAMETER :: ATYPESN(9) = &
           (/ '  ','  ','  ','UZ','  ','  ','  ','  ','MX' /)
      CHARACTER(33) :: F69 ! (I4,I6,2X,A2,I5,**********ES14.5)
      CHARACTER(31) :: F7  ! (I4,I6,I4,I5,**********ES19.10)
      CHARACTER(10000) MATLABDISPSTR
      INTEGER MTOT,NDM,ITP,NICP,N1,N2
!
       NDM=IAP(23)
       ITP=IAP(27)
       NICP=IAP(41)
!
       N1=NICP
       N2=NDM
!
       IF(N1.GT.100)THEN
         N1=100
         N2=0
       ELSEIF(N1+N2.GT.100)THEN
         N2=100-N1
       ENDIF
!
! Write a heading above the first line.
!
       IF(ABS(NTOT).EQ.1)THEN
           CALL HEADNG(IAP,ICU,6,N1,N2)
           CALL MEXPRINTF(achar(10))
       ENDIF
       IF(ABS(NTOT).EQ.1)CALL HEADNG(IAP,ICU,7,NICP,N2)
       CALL HEADNG(IAP,ICU,9,N1,N2)
!
       IF(MOD(ITP,10)>0)THEN
         ATYPE=ATYPESP(MOD(ITP,10))
       ELSEIF(MOD(ITP,10)<0)THEN
         ATYPE=ATYPESN(-MOD(ITP,10))
       ELSE
         ATYPE='  '
       ENDIF
!
       MTOT=MOD(NTOT-1,9999)+1
       WRITE(F69,"(A,I10,A)") '(I4,I6,2X,A2,I5,',N1+N2+1,'ES14.5)'
       WRITE(F7,"(A,I10,A)") '(I4,I6,I4,I5,',NICP+N2+1+ODIM,'ES19.10)'
       
       IF(MOD(ITP,10).NE.0)THEN
          WRITE(MATLABDISPSTR,F69)ABS(IBR),ABS(MTOT),ATYPE,LAB, &
               PAR(ICU(1)),VAXIS, &
               (U(I),I=1,N2),(PAR(ICU(I)),I=2,N1)
          CALL MEXPRINTF(MATLABDISPSTR)
!          CALL MEXPRINTF(achar(10))
          CALL MEXPRINTF(achar(10))
          CALL MEXEVALSTRING("drawnow;") 
          CALL FLUSH(6)
       ENDIF
      IF( FORT7DST == 1 )THEN        
       WRITE(7,F7)IBR,MTOT,ITP,LAB,PAR(ICU(1)),VAXIS, &
            (U(I),I=1,N2),(PAR(ICU(I)),I=2,NICP),(OUT(I),I=1,ODIM)
       CALL FLUSH(7)
      ENDIF
       IF(FORT9DST==1)WRITE(9,F69)IBR,MTOT,ATYPE,LAB,PAR(ICU(1)),VAXIS, &
            (U(I),I=1,N2),(PAR(ICU(I)),I=2,N1)
!     
! Write values to F7 object global variables
!
      IF( RUNMODEDST == 0)GOTO 109 
! ARRAY ALLOCATION  ----------------------------------------------------
!
! IBR ------------
      R=SIZE(IBRF7,1)
      IF(ALLOCATED(oldIBRF7))DEALLOCATE(oldIBRF7)
      ALLOCATE(oldIBRF7(R))
      oldIBRF7=IBRF7
      DEALLOCATE(IBRF7)
      ALLOCATE(IBRF7(R+1))
      IF(R > 0) THEN
          DO I=1,R
             IBRF7(I)=oldIBRF7(I)
          ENDDO
      ENDIF
      IBRF7(R+1)=IBR
!
! MTOT ------------
      R=SIZE(MTOTF7,1)
      IF(ALLOCATED(oldMTOTF7))DEALLOCATE(oldMTOTF7)
      ALLOCATE(oldMTOTF7(R))
      oldMTOTF7=MTOTF7
      DEALLOCATE(MTOTF7)
      ALLOCATE(MTOTF7(R+1))
      IF(R > 0) THEN
          DO I=1,R
             MTOTF7(I)=oldMTOTF7(I)
          ENDDO
      ENDIF
      MTOTF7(R+1)=MTOT
!
! ITP ------------
      R=SIZE(ITPF7,1)
      IF(ALLOCATED(oldITPF7))DEALLOCATE(oldITPF7)
      ALLOCATE(oldITPF7(R))
      oldITPF7=ITPF7
      DEALLOCATE(ITPF7)
      ALLOCATE(ITPF7(R+1))
      IF(R > 0) THEN
          DO I=1,R
             ITPF7(I)=oldITPF7(I)
          ENDDO
      ENDIF
      ITPF7(R+1)=ITP
!
! LAB ------------
      R=SIZE(LABF7,1)
      IF(ALLOCATED(oldLABF7))DEALLOCATE(oldLABF7)      
      ALLOCATE(oldLABF7(R))
      oldLABF7=LABF7
      DEALLOCATE(LABF7)
      ALLOCATE(LABF7(R+1))
      IF(R > 0) THEN
          DO I=1,R
             LABF7(I)=oldLABF7(I)
          ENDDO
      ENDIF
      LABF7(R+1)=LAB
!
! PAR ------------
      R=SIZE(PARF7,1)
      C=SIZE(PARF7,2)
      IF(ALLOCATED(oldPARF7))DEALLOCATE(oldPARF7)      
      ALLOCATE(oldPARF7(R,C))
      oldPARF7=PARF7
      DEALLOCATE(PARF7)
      ALLOCATE(PARF7(R+1,NICP))
      IF(R > 0) THEN
          DO I=1,R
            DO J=1,NICP
             PARF7(I,J)=oldPARF7(I,J)
            ENDDO
          ENDDO
      ENDIF
      DO J=1,NICP
        PARF7(R+1,J)=PAR(ICU(J))
      ENDDO
!
! VAXIS ------------
      R=SIZE(VAXISF7,1)
      IF(ALLOCATED(oldVAXISF7))DEALLOCATE(oldVAXISF7)      
      ALLOCATE(oldVAXISF7(R))
      oldVAXISF7=VAXISF7
      DEALLOCATE(VAXISF7)
      ALLOCATE(VAXISF7(R+1))
      IF(R > 0) THEN
          DO I=1,R
             VAXISF7(I)=oldVAXISF7(I)
          ENDDO
      ENDIF
      VAXISF7(R+1)=VAXIS
!
! U ------------
      R=SIZE(UF7,1)
      C=SIZE(UF7,2)
      IF(ALLOCATED(oldUF7))DEALLOCATE(oldUF7)      
      ALLOCATE(oldUF7(R,N2))
      oldUF7=UF7
      DEALLOCATE(UF7)
      ALLOCATE(UF7(R+1,N2))
      IF(R > 0) THEN
          DO I=1,R
            DO J=1,N2
             UF7(I,J)=oldUF7(I,J)
            ENDDO
          ENDDO
      ENDIF
      DO J=1,N2
        UF7(R+1,J)=U(J)
      ENDDO
!
! OUT ------------
      R=SIZE(OUTF7,1)
      C=SIZE(OUTF7,2)
      IF(ALLOCATED(oldOUTF7))DEALLOCATE(oldOUTF7)      
      ALLOCATE(oldOUTF7(R,ODIM))
      oldOUTF7=OUTF7
      DEALLOCATE(OUTF7)
      ALLOCATE(OUTF7(R+1,ODIM))
      IF(R > 0) THEN
          DO I=1,R
            DO J=1,ODIM
             OUTF7(I,J)=oldOUTF7(I,J)
            ENDDO
          ENDDO
      ENDIF
      DO J=1,ODIM
        OUTF7(R+1,J)=OUT(J)
      ENDDO
!
!      
109   RETURN
      END
!
!     ---------- -----
      SUBROUTINE WRBAR(C,N)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      CHARACTER*1 C
        IF(FORT9DST==1)WRITE(9,101)(C,I=1,N)
 101    FORMAT(80A1)
 102    RETURN
      END
!
!     ---------- ------
      SUBROUTINE WRTSP8(IAP,RAP,PAR,ICP,LAB,RLCUR,U)
!  
      USE AUTO_CONSTANTS, ONLY:IBRF8,oldIBRF8,MTOTF8,oldMTOTF8,ITPF8, & 
           oldITPF8,LABF8,oldLABF8,NFPRF8,oldNFPRF8,ISWF8,oldISWF8,NTPLF8, &
           oldNTPLF8,NARF8,oldNARF8,NROWPRF8,oldNROWPRF8,NTSTF8,oldNTSTF8,NCOLF8, & 
           oldNCOLF8,NPARXF8,oldNPARXF8,IFPRF8,oldIFPRF8,TF8,oldTF8,TMF8,oldTMF8, &
           PARF8,oldPARF8,RLDOTF8,oldRLDOTF8,UF8,oldUF8,UPSF8,oldUPSF8,UDOTPSF8, &
           oldUDOTPSF8,NPARX,NBIFX,NIAP,NRAP,FORT7DST,FORT8DST,FORT9DST, &
           RUNMODEDST
!      INCLUDE 'auto.h'  
!
!      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IMPLICIT NONE
!
      INTEGER R,C,P,I,J 
      INTEGER IAP(*),ICP(*),ISW,ITP,IBR,LAB
      INTEGER NAR,NDIM,NFPR,NTOT,MTOT,NTPL,NROWPR
      DOUBLE PRECISION RAP(*),PAR(*),RLCUR(*),U(*),T,AMP
!
! Write restart information on singular points, plotting points, etc.,
! on unit 8.
!
       NDIM=IAP(1)
       ISW=IAP(10)
       ITP=IAP(27)      
       IBR=IAP(30)
       NFPR=IAP(29)
       NTOT=IAP(32)
!
       NTPL=1
       NAR=NDIM+1
       NROWPR=NDIM/7+1 + (NPARX-1)/7+1
       PAR(ICP(1))=RLCUR(1)
       T=0.d0
       AMP=0.d0
       RAP(10)=AMP
!       
       MTOT=MOD(NTOT-1,9999)+1
!      
      IF( FORT8DST == 1 )THEN
       WRITE(8,101)IBR,MTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,0,0,NPARX
       WRITE(8,102)T,(U(I),I=1,NDIM)
       WRITE(8,102)(PAR(I),I=1,NPARX)
      ENDIF
!
!
! ARRAY ALLOCATION  ----------------------------------------------------
!
      IF( RUNMODEDST == 0 )GOTO 809
!
! IBR ------------
      R=SIZE(IBRF8,1)
      IF(ALLOCATED(oldIBRF8))DEALLOCATE(oldIBRF8)
      ALLOCATE(oldIBRF8(R))
      oldIBRF8=IBRF8
      DEALLOCATE(IBRF8)
      ALLOCATE(IBRF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            IBRF8(I)=oldIBRF8(I)
          ENDDO
      ENDIF
      IBRF8(R+1)=IBR
!
! MTOT ------------
      R=SIZE(MTOTF8,1)
      IF(ALLOCATED(oldMTOTF8))DEALLOCATE(oldMTOTF8)
      ALLOCATE(oldMTOTF8(R))
      oldMTOTF8=MTOTF8
      DEALLOCATE(MTOTF8)
      ALLOCATE(MTOTF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            MTOTF8(I)=oldMTOTF8(I)
          ENDDO
      ENDIF
      MTOTF8(R+1)=MTOT
!
! ITP ------------
      R=SIZE(ITPF8,1)
      IF(ALLOCATED(oldITPF8))DEALLOCATE(oldITPF8)
      ALLOCATE(oldITPF8(R))
      oldITPF8=ITPF8
      DEALLOCATE(ITPF8)
      ALLOCATE(ITPF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            ITPF8(I)=oldITPF8(I)
          ENDDO
      ENDIF
      ITPF8(R+1)=ITP
!
! LAB ------------
      R=SIZE(LABF8,1)
      IF(ALLOCATED(oldLABF8))DEALLOCATE(oldLABF8)
      ALLOCATE(oldLABF8(R))
      oldLABF8=LABF8
      DEALLOCATE(LABF8)
      ALLOCATE(LABF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            LABF8(I)=oldLABF8(I)
          ENDDO
      ENDIF
      LABF8(R+1)=LAB
!
! NFPR ------------
      R=SIZE(NFPRF8,1)
      IF(ALLOCATED(oldNFPRF8))DEALLOCATE(oldNFPRF8)
      ALLOCATE(oldNFPRF8(R))
      oldNFPRF8=NFPRF8
      DEALLOCATE(NFPRF8)
      ALLOCATE(NFPRF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            NFPRF8(I)=oldNFPRF8(I)
          ENDDO
      ENDIF
      NFPRF8(R+1)=NFPR
!
! ISW ------------
      R=SIZE(ISWF8,1)
      IF(ALLOCATED(oldISWF8))DEALLOCATE(oldISWF8)
      ALLOCATE(oldISWF8(R))
      oldISWF8=ISWF8
      DEALLOCATE(ISWF8)
      ALLOCATE(ISWF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            ISWF8(I)=oldISWF8(I)
          ENDDO
      ENDIF
      ISWF8(R+1)=ISW
!
! NTPL ------------
      R=SIZE(NTPLF8,1)
      IF(ALLOCATED(oldNTPLF8))DEALLOCATE(oldNTPLF8)
      ALLOCATE(oldNTPLF8(R))
      oldNTPLF8=NTPLF8
      DEALLOCATE(NTPLF8)
      ALLOCATE(NTPLF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            NTPLF8(I)=oldNTPLF8(I)
          ENDDO
      ENDIF
      NTPLF8(R+1)=NTPL
!
! NAR ------------
      R=SIZE(NARF8,1)
      IF(ALLOCATED(oldNARF8))DEALLOCATE(oldNARF8)
      ALLOCATE(oldNARF8(R))
      oldNARF8=NARF8
      DEALLOCATE(NARF8)
      ALLOCATE(NARF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            NARF8(I)=oldNARF8(I)
          ENDDO
      ENDIF
      NARF8(R+1)=NAR
!
! NROWPR ------------
      R=SIZE(NROWPRF8,1)
      IF(ALLOCATED(oldNROWPRF8))DEALLOCATE(oldNROWPRF8)
      ALLOCATE(oldNROWPRF8(R))
      oldNROWPRF8=NROWPRF8
      DEALLOCATE(NROWPRF8)
      ALLOCATE(NROWPRF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            NROWPRF8(I)=oldNROWPRF8(I)
          ENDDO
      ENDIF
      NROWPRF8(R+1)=NROWPR
!
! NTST ------------
      R=SIZE(NTSTF8,1)
      IF(ALLOCATED(oldNTSTF8))DEALLOCATE(oldNTSTF8)
      ALLOCATE(oldNTSTF8(R))
      oldNTSTF8=NTSTF8
      DEALLOCATE(NTSTF8)
      ALLOCATE(NTSTF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            NTSTF8(I)=oldNTSTF8(I)
          ENDDO
      ENDIF
      NTSTF8(R+1)=0
!
! NCOL ------------
      R=SIZE(NCOLF8,1)
      IF(ALLOCATED(oldNCOLF8))DEALLOCATE(oldNCOLF8)
      ALLOCATE(oldNCOLF8(R))
      oldNCOLF8=NCOLF8
      DEALLOCATE(NCOLF8)
      ALLOCATE(NCOLF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            NCOLF8(I)=oldNCOLF8(I)
          ENDDO
      ENDIF
      NCOLF8(R+1)=0
      DEALLOCATE(oldNCOLF8)
!
! NPARX ------------
      R=SIZE(NPARXF8,1)
      IF(ALLOCATED(oldNPARXF8))DEALLOCATE(oldNPARXF8)
      ALLOCATE(oldNPARXF8(R))
      oldNPARXF8=NPARXF8
      DEALLOCATE(NPARXF8)
      ALLOCATE(NPARXF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            NPARXF8(I)=oldNPARXF8(I)
          ENDDO
      ENDIF
      NPARXF8(R+1)=NPARX
!
! T ------------
      R=SIZE(TF8,1)
      IF(ALLOCATED(oldTF8))DEALLOCATE(oldTF8)
      ALLOCATE(oldTF8(R))
      oldTF8=TF8
      DEALLOCATE(TF8)
      ALLOCATE(TF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            TF8(I)=oldTF8(I)
          ENDDO
      ENDIF
      TF8(R+1)=T
!
! PAR ------------
      R=SIZE(PARF8,1)
      C=SIZE(PARF8,2)
      IF(ALLOCATED(oldPARF8))DEALLOCATE(oldPARF8)
      ALLOCATE(oldPARF8(R,NPARX))
      oldPARF8=PARF8
      DEALLOCATE(PARF8)
      ALLOCATE(PARF8(R+1,NPARX))
      IF(R > 0) THEN
          DO I=1,R
            DO J=1,NPARX
              PARF8(I,J)=oldPARF8(I,J)
            ENDDO
          ENDDO
      ENDIF
      DO J=1,NPARX
        PARF8(R+1,J)=PAR(J)
      ENDDO
!
! U ------------
      R=SIZE(UF8,1)
      C=SIZE(UF8,2)
      IF(ALLOCATED(oldUF8))DEALLOCATE(oldUF8)
      ALLOCATE(oldUF8(R,NDIM))
      oldUF8=UF8
      DEALLOCATE(UF8)
      ALLOCATE(UF8(R+1,NDIM))
      IF(R > 0) THEN
          DO I=1,R
            DO J=1,NDIM
              UF8(I,J)=oldUF8(I,J)
            ENDDO
          ENDDO
      ENDIF
      DO J=1,NDIM
        UF8(R+1,J)=U(J)
      ENDDO     
!
101   FORMAT(6I6,I8,I6,I8,3I5)
102   FORMAT(4X,7ES19.10)
!
809   IF( FORT8DST == 1 )THEN
       CALL FLUSH(8)
      ENDIF
       RETURN
       END
!
!     ---------- ------
      SUBROUTINE WRJAC(N,M1AA,AA,RHS)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION AA(M1AA,*),RHS(*)
!
       IF(FORT9DST==1)WRITE(9,101)
       IF(FORT9DST==1)WRITE(9,100)(RHS(I),I=1,N)
       IF(FORT9DST==1)WRITE(9,102)
       DO I=1,N
         IF(FORT9DST==1)WRITE(9,100)(AA(I,J),J=1,N)
       ENDDO
!
 100   FORMAT(1X,12E10.3)
 101   FORMAT(/,' Residual vector :')
 102   FORMAT(/,' Jacobian matrix :')
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    Mesh and Weight Generation
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ---
      SUBROUTINE MSH(NTST,TM)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates a uniform mesh on [0,1].
!
      DIMENSION TM(*)
!
       TM(1)=0.d0
       DT=1.d0/NTST
       DO J=1,NTST
         TM(J+1)=J*DT
       ENDDO
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE GENWTS(NCOL,N1,WT,WP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates weights of the collocation method. The user selected
! number of collocation points (ncol) must be one of { 2,...,7 }.
!
! The following weights are generated :
!
!         WT : for the function value,
!         WP : for the first derivative,
!
      DIMENSION WT(N1,*),WP(N1,*)   
! Local
      DIMENSION ZM(NCOL),XM(NCOL+1)
!
! Generate the collocation points :
       CALL CPNTS(NCOL,ZM)
!
       NCP1=NCOL+1
       D=1.d0/NCOL
       DO I=1,NCP1
         XM(I)=(I-1)*D
       ENDDO
!
! Generate weights :
!
       DO IB=1,NCP1
         DENOM=1.d0
         DO K=1,NCP1
           IF(K.NE.IB)DENOM=DENOM*( XM(IB)-XM(K) )
         ENDDO
         DO IC=1,NCOL
! Weights for the function values :
           P=1.d0
           DO K=1,NCP1
             IF(K.NE.IB)P=P*( ZM(IC)-XM(K) )
           ENDDO
           WT(IB,IC)=P/DENOM
! Weights for derivatives :
           SUM=0.d0
           DO L=1,NCP1
             IF(L.NE.IB)THEN
               P=1.d0
               DO K=1,NCP1
                 IF(K.NE.IB.AND.K.NE.L)P=P*( ZM(IC)-XM(K) )
               ENDDO
               SUM=SUM+P
             ENDIF
           ENDDO
           WP(IB,IC)=SUM/DENOM
         ENDDO
       ENDDO
!
      RETURN
      END
!
!     ---------- -----
      SUBROUTINE CPNTS(NCOL,ZM)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates the collocation points with respect to [0,1].
!
      DIMENSION ZM(*)
!
       GOTO (2,3,4,5,6,7)NCOL-1
!
 2     C=.5d0/DSQRT(3.0d0)
       ZM(1)=.5d0-C
       ZM(2)=.5d0+C
      RETURN
!
 3     C=.5d0*DSQRT(0.6d0)
       ZM(1)=.5d0-C
       ZM(2)=.5d0
       ZM(3)=.5d0+C
      RETURN
!
 4     R=6.0d0/7.0d0
       C=.5d0*DSQRT(R**2-12.0d0/35.0d0)
       C1=.5d0*DSQRT(3.0d0/7.0d0+C)
       C2=.5d0*DSQRT(3.0d0/7.0d0-C)
       ZM(1)=.5d0-C1
       ZM(2)=.5d0-C2
       ZM(3)=.5d0+C2
       ZM(4)=.5d0+C1
      RETURN
!
 5     C1=.5d0*0.90617984593866399280d0
       C2=.5d0*0.53846931010568309104d0
       ZM(1)=.5d0-C1
       ZM(2)=.5d0-C2
       ZM(3)=.5d0
       ZM(4)=.5d0+C2
       ZM(5)=.5d0+C1
      RETURN
!
 6     C1=.5d0*0.93246951420315202781d0
       C2=.5d0*0.66120938646626451366d0
       C3=.5d0*0.23861918608319690863d0
       ZM(1)=.5d0-C1
       ZM(2)=.5d0-C2
       ZM(3)=.5d0-C3
       ZM(4)=.5d0+C3
       ZM(5)=.5d0+C2
       ZM(6)=.5d0+C1
      RETURN
!
 7     C1=.5d0*0.949107991234275852452d0
       C2=.5d0*0.74153118559939443986d0
       C3=.5d0*0.40584515137739716690d0
       ZM(1)=.5d0-C1
       ZM(2)=.5d0-C2
       ZM(3)=.5d0-C3
       ZM(4)=.5d0
       ZM(5)=.5d0+C3
       ZM(6)=.5d0+C2
       ZM(7)=.5d0+C1
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE CNTDIF(N,D)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates the coefficients of the central difference formula for
! Nth derivative at uniformly spaced points
!              0 = x  < x  < ... < x  = 1.
!                   0    1          N
!
      DIMENSION D(*)
!
       D(1)=1.d0
       IF(N.EQ.0)RETURN
!
       DO I=1,N
         D(I+1)=0.d0
         DO K=1,I
           K1=I+2-K
           D(K1)=D(K1-1)-D(K1)
         ENDDO
         D(1)=-D(1)
       ENDDO
!
! Scale to [0,1]  :
!
       SC=N**N
       NP1=N+1
       DO I=1,NP1
         D(I)=SC*D(I)
       ENDDO
!
      RETURN
      END
!
!     ---------- ----
      SUBROUTINE WINT(N,WI)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates the weights for the integration formula based on polynomial
! interpolation at N equally spaced points in [0,1].
!
      DIMENSION WI(*)
!
       GOTO (3,4,5,6,7,8)N-2
!
 3     C=1.d0/6.0d0
       WI(1)=C
       WI(2)=4.0d0*C
       WI(3)=C
      RETURN
!
 4     C=1.d0/8.0d0
       WI(1)=C
       WI(2)=3.0d0*C
       WI(3)=WI(2)
       WI(4)=C
      RETURN
!
 5     C=1.d0/90.0d0
       WI(1)=7.0d0*C
       WI(2)=32.0d0*C
       WI(3)=12.0d0*C
       WI(4)=WI(2)
       WI(5)=WI(1)
      RETURN
!
 6     WI(1)=19.0d0/288.0d0
       WI(2)=25.0d0/96.0d0
       WI(3)=25.0d0/144.0d0
       WI(4)=WI(3)
       WI(5)=WI(2)
       WI(6)=WI(1)
      RETURN
!
 7     WI(1)=41.0d0/840.0d0
       WI(2)=9.0d0/35.0d0
       WI(3)=9.0d0/280.0d0
       WI(4)=34.0d0/105.0d0
       WI(5)=WI(3)
       WI(6)=WI(2)
       WI(7)=WI(1)
      RETURN
!
 8     WI(1)=751.0d0/17280.0d0
       WI(2)=3577.0d0/17280.0d0
       WI(3)=49.0d0/640.0d0
       WI(4)=2989.0d0/17280.0d0
       WI(5)=WI(4)
       WI(6)=WI(3)
       WI(7)=WI(2)
       WI(8)=WI(1)
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!          Stepsize and Mesh Adaption
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ------
      SUBROUTINE ADPTDS(IAP,RAP,RDS)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! The stepsize along the branch of solutions is adapted depending on the
! number of Newton iterations in the previous step (called if IADS > 0).
!
      DIMENSION IAP(*),RAP(*)
!
       DSMAX=RAP(3)
       ITNW=IAP(20)
       IBR=IAP(30)
       NIT=IAP(31)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
!
       IF(ITNW.LE.3) THEN
         ITNW=3
         N1=2
       ELSE
         N1=ITNW/2
       ENDIF
!
       IF(NIT.LE.1) THEN
         RDS= 2.d0*RDS
       ELSE IF(NIT.EQ.2) THEN
         RDS= 1.5*RDS
       ELSE IF(NIT.GT.2 .AND. NIT.LE.N1) THEN
         RDS= 1.1*RDS
       ELSE IF(NIT.GE.ITNW) THEN
         RDS=.5d0*RDS
       ENDIF
!
       ARDS= ABS(RDS)
       IF(ARDS.GT.DSMAX)RDS=RDS*DSMAX/ARDS
!
       IF(FORT9DST==1)WRITE(9,101)ABS(IBR),NTOP,NIT
       IF(FORT9DST==1)WRITE(9,102)ABS(IBR),NTOP,RDS
 101   FORMAT(/,I4,I6,8X,' Iterations   : ',I3)
 102   FORMAT(I4,I6,8X,' Next Step    : ',ES13.5)
!
      RETURN
      END
!
!     ---------- -----
      SUBROUTINE ADAPT(IAP,NOLD,NCOLD,NNEW,NCNEW,TM,DTM,NDX,UPS,VPS)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Adapts the distribution of the mesh points so that the increase of the
! monotone function EQDF becomes approximately equidistributed over the
! intervals. The functions UPS and VPS are interpolated on new mesh.
!
      DIMENSION IAP(*),UPS(NDX,*),VPS(NDX,*),TM(*),DTM(*)
! Local
      ALLOCATABLE TINT(:),UINT(:,:),TM2(:),ITM(:)
!
       NDIM=IAP(1)
       IPS=IAP(2)
       ISW=IAP(10)
!
       NOLDP1=NOLD+1
       NNEWP1=NNEW+1
       NRWNEW=NDIM*NCNEW
       ALLOCATE(TINT(NNEWP1),UINT(NRWNEW,NNEWP1))
       ALLOCATE(TM2(NNEWP1),ITM(NNEWP1))
!
       DO J=1,NNEWP1
         DO I=1,NRWNEW
           UINT(I,J)=0.d0
         ENDDO
       ENDDO
!
! For periodic boundary conditions extrapolate by periodicity.
!
       IF(IPS.EQ.2 .AND. ABS(ISW).LE.1) THEN
         IPER=1
       ELSE
         IPER=0
       ENDIF
!
! Generate the new mesh :
!
       CALL NEWMSH(NDIM,NDX,UPS,NOLD,NCOLD,TM,DTM,NNEW,TINT,IPER)
!
! Replace UPS by its interpolant on the new mesh :
!
       CALL INTERP(NDIM,NOLDP1,NCOLD,TM,NDX,UPS,NNEWP1,NCNEW, &
            TINT,UINT,TM2,ITM)
       DO J=1,NNEWP1
         DO I=1,NRWNEW
           UPS(I,J)=UINT(I,J)
         ENDDO
       ENDDO
!
! Replace VPS by its interpolant on the new mesh :
!
       CALL INTERP(NDIM,NOLDP1,NCOLD,TM,NDX,VPS,NNEWP1,NCNEW, &
            TINT,UINT,TM2,ITM)
       DO J=1,NNEWP1
         DO I=1,NRWNEW
           VPS(I,J)=UINT(I,J)
         ENDDO
       ENDDO
!
! Replace old mesh :
!
       TM(1)=0.d0
       DO J=1,NNEW
         DTM(J)=TINT(J+1)-TINT(J)
         TM(J+1)=TINT(J+1)
       ENDDO
!
      DEALLOCATE(TINT,UINT,TM2,ITM)
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE INTERP(NDIM,N,NC,TM,NDX,UPS,N1,NC1,TM1,UPS1, &
           TM2,ITM1)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Finds interpolant (TM(.) , UPS(.) ) on new mesh TM1.
!
      DIMENSION TM(*),TM1(*),TM2(*),ITM1(*),UPS(NDX,*),UPS1(NC1*NDIM,*)
! Local
      DIMENSION X(NC+1),W(NC+1)
!
       NCP1=NC+1
       N1M1=N1-1
!
       DO I=1,NC1
         RI=I-1
         D=RI/NC1
         DO J1=1,N1M1
           TM2(J1)=TM1(J1)+D*( TM1(J1+1)-TM1(J1) )
         ENDDO
         CALL ORDR(N,TM,N1M1,TM2,ITM1)
         DO J1=1,N1M1
           J=ITM1(J1)
           Z=TM2(J1)
           D=( TM(J+1)-TM(J) )/NC
           DO L=1,NCP1
             X(L)=TM(J)+(L-1)*D
           ENDDO
           CALL INTWTS(NCP1,Z,X,W)
           DO K=1,NDIM
             K1=(I-1)*NDIM+K
             UPS1(K1,J1)=W(NCP1)*UPS(K,J+1)
             DO L=1,NC
               L1=K+(L-1)*NDIM
               UPS1(K1,J1)=UPS1(K1,J1)+W(L)*UPS(L1,J)
             ENDDO
           ENDDO
         ENDDO
       ENDDO
!
       DO I=1,NDIM
         UPS1(I,N1)=UPS(I,N)
       ENDDO
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE NEWMSH(NDIM,NDX,UPS,NOLD,NCOLD,TMOLD,DTMOLD, &
           NNEW,TMNEW,IPER)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Redistributes the mesh according to the function EQDF.
!
      DIMENSION TMOLD(*),DTMOLD(*),TMNEW(*)
! Local
      ALLOCATABLE EQF(:),UNEQ(:),IAL(:)
      ALLOCATE(EQF(NOLD+1),UNEQ(NNEW+1),IAL(NNEW+1))
!
! Put the values of the monotonely increasing function EQDF in EQF.
!
       CALL EQDF(NOLD,NDIM,NCOLD,DTMOLD,NDX,UPS,EQF,IPER)
!
! Uniformly divide the range of EQDF :
!
       NOLDP1=NOLD+1
       NNEWP1=NNEW+1
       DAL=EQF(NOLDP1)/NNEW
       DO J=1,NNEWP1
         UNEQ(J)=(J-1)*DAL
       ENDDO
!
       CALL ORDR(NOLDP1,EQF,NNEWP1,UNEQ,IAL)
!
! Generate the new mesh in TMNEW :
!
       DO J1=1,NNEW
         J=IAL(J1)
         X=(UNEQ(J1)-EQF(J))/(EQF(J+1)-EQF(J))
         TMNEW(J1)=(1.d0-X)*TMOLD(J)+X*TMOLD(J+1)
       ENDDO
!
! Assign TMNEW(NNEWP1) explicitly because of loss of precision
! problems when EQF(NOLDP1) and EQF(NOLD) are very close
!
       TMNEW(NNEWP1)=TMOLD(NOLDP1)
!
      DEALLOCATE(EQF,UNEQ,IAL)
      RETURN
      END
!
!     ---------- ----
      SUBROUTINE ORDR(N,TM,N1,TM1,ITM1)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! TM and TM1 are two ascending arrays with values in [0,1]. On exit the
! value of ITM1( i ) specifies the index of the TM-interval in which
! TM1(i) lies.
!
      DIMENSION TM(N),TM1(N1),ITM1(N1)
!
       K0=2
       DO J1=1,N1
         K1=K0
         DO J=K0,N
           K1=J
           IF(TM1(J1).LT.TM(J))GOTO 1
         ENDDO
 1       ITM1(J1)=K1-1
         K0=K1
       ENDDO
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE INTWTS(N,Z,X,WTS)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates weights for Lagrange interpolation.
!
      DIMENSION X(*),WTS(*)
!
       DO IB=1,N
         P=1.d0
         DENOM=1.d0
         DO K=1,N
           IF(K.NE.IB)THEN
             P=P*( Z-X(K) )
             DENOM=DENOM*( X(IB)-X(K) )
            ENDIF
         ENDDO
         WTS(IB)=P/DENOM
       ENDDO
!
      RETURN
      END
!
!     ---------- ----
      SUBROUTINE EQDF(NTST,NDIM,NCOL,DTM,NDX,UPS,EQF,IPER)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
      DIMENSION UPS(NDX,*),EQF(*),DTM(*)
      LOGICAL SMALL
! Local
      DIMENSION WH(NCOL+1)
      ALLOCATABLE HD(:,:)
      ALLOCATE(HD(NDIM*NCOL,NTST+1))
!
! Compute approximation to NCOL-th derivative :
       CALL CNTDIF(NCOL,WH)
!
       SMALL=.TRUE.
       DO J=1,NTST
         JP1=J+1
         SC=1.d0/DTM(J)**NCOL
         DO I=1,NDIM
           HD(I,J)=WH(NCOL+1)*UPS(I,JP1)
           DO K=1,NCOL
             K1=I+(K-1)*NDIM
             HD(I,J)=HD(I,J)+WH(K)*UPS(K1,J)
           ENDDO
           HD(I,J)=SC*HD(I,J)
           IF(ABS(HD(I,J)).GT.HMACH)SMALL=.FALSE.
         ENDDO
       ENDDO
!
! Take care of "small derivative" case.
!
       IF(SMALL)THEN
         DO I=1,NTST+1
           EQF(I)=I-1
         ENDDO
         DEALLOCATE(HD)
         RETURN
       ENDIF
!
       IF(IPER.EQ.1)THEN
!        *Extend by periodicity :
         DO I=1,NDIM
           HD(I,NTST+1)=HD(I,1)
         ENDDO
         DTM(NTST+1)=DTM(1)
       ELSE
!        *Extend by extrapolation :
         DO I=1,NDIM
           HD(I,NTST+1)=2*HD(I,NTST)-HD(I,NTST-1)
         ENDDO
         DTM(NTST+1)=DTM(NTST)
       ENDIF
!
! Compute approximation to (NCOL+1)-st derivative :
!
       DO J=1,NTST
         JP1=J+1
         DTAV=.5d0*(DTM(J)+DTM(J+1))
         SC=1.d0/DTAV
         DO I=1,NDIM
           HD(I,J)=SC*( HD(I,JP1)-HD(I,J) )
         ENDDO
       ENDDO
!
! Define the equidistribution function :
!
       PWR=1.d0/(NCOL+1.d0)
       EQF(1)=0.d0
       DO J=1,NTST
         E=0.d0
         DO I=1,NDIM
           E=E+ABS( HD(I,J) )**PWR
         ENDDO
         EQF(J+1)=EQF(J)+DTM(J)*E
       ENDDO
!
!
       DEALLOCATE(HD)
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    General Support Routines
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ---
      SUBROUTINE EIG(IAP,NDIM,M1A,A,EV,IER)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! This subroutine uses the EISPACK subroutine RG to compute the
! eigenvalues of the general real matrix A.
! NDIM is the dimension of A.
! M1A is the first dimension of A as in the DIMENSION statement.
! The eigenvalues are to be returned in the complex vector EV.
!
      DIMENSION A(M1A,*),IAP(*)
!
      COMPLEX(KIND(1.0D0)) EV(*)
! Local
      ALLOCATABLE WR(:),WI(:),Z(:),FV1(:),IV1(:)
      ALLOCATE(WR(NDIM),WI(NDIM),Z(M1A*NDIM),FV1(NDIM),IV1(NDIM))
!
       IID=IAP(18)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
!
       IER=0
       IF(IID.GE.4)THEN 
         MATZ=1
       ELSE
         MATZ=0
       ENDIF
!
       CALL RG(M1A,NDIM,A,WR,WI,MATZ,Z,IV1,FV1,IER)
       IF(IER.NE.0)IER=1
       IF(IER.EQ.1)THEN
         IF(FORT9DST==1)WRITE(9,101)IBR,NTOP
       ENDIF
!
       IF(MATZ.NE.0)THEN
         IF(FORT9DST==1)WRITE(9,102)
         DO I=1,NDIM
            IF(FORT9DST==1)WRITE(9,104)WR(I),WI(I)
         ENDDO
         IF(FORT9DST==1)WRITE(9,103)
         DO I=1,NDIM
            IF(FORT9DST==1)WRITE(9,104)(Z((I-1)*M1A+J),J=1,NDIM)
         ENDDO
       ENDIF
!
       DO I=1,NDIM
          EV(I) = CMPLX(WR(I),WI(I),KIND(1.0D0))
       ENDDO
!

 101   FORMAT(I4,I6,' NOTE:Error return from EISPACK routine RG')
 102   FORMAT(/,' Eigenvalues:')
 103   FORMAT(/,' Eigenvectors (by row):')
 104   FORMAT(4X,7ES19.10)
!
      DEALLOCATE(WR,WI,Z,FV1,IV1)
      RETURN
      END
!
!     ---------- ----
      SUBROUTINE NLVC(N,M,K,A,U,IR,IC)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Finds a null-vector of a singular matrix A.
! The null space of A is assumed to be K-dimensional.
!
! Parameters :
!
!     N : number of equations,
!     M : first dimension of A from DIMENSION statement,
!     K : dimension of nullspace,
!     A : N * N matrix of coefficients,
!     U : on exit U contains the null vector,
! IR,IC : integer arrays of dimension at least N.
!
!
      DIMENSION IR(*),IC(*),A(M,*),U(*)
!
       DO I=1,N
         IC(I)=I
         IR(I)=I
       ENDDO
!
!   Elimination.
!
       NMK=N-K
!
       DO JJ=1,NMK
         IPIV=JJ
         JPIV=JJ
         PIV=0.d0
         DO I=JJ,N
           DO J=JJ,N
             P=ABS(A(IR(I),IC(J)))
             IF(P.GT.PIV)THEN
               PIV=P
               IPIV=I
               JPIV=J
             ENDIF
           ENDDO
         ENDDO
         IF(PIV.LT.RSMALL)THEN
           IF(FORT9DST==1)WRITE(9,101)JJ,RSMALL
         ENDIF
!
         KK=IR(JJ)
         IR(JJ)=IR(IPIV)
         IR(IPIV)=KK
!
         KK=IC(JJ)
         IC(JJ)=IC(JPIV)
         IC(JPIV)=KK
!
         JJP1=JJ+1
         DO L=JJP1,N
           RM=A(IR(L),IC(JJ))/A(IR(JJ),IC(JJ))
           IF(RM.NE.0.d0)THEN
             DO I=JJP1,N
               A(IR(L),IC(I))=A(IR(L),IC(I))-RM*A(IR(JJ),IC(I))
             ENDDO
           ENDIF
         ENDDO
       ENDDO
!
!   Backsubstitution :
!
       DO I=1,K
         U(IC(N+1-I))=1.d0
       ENDDO
!
       DO I1=1,NMK
         I=NMK+1-I1
         SM=0.d0
         IP1=I+1
         DO J=IP1,N
           SM=SM+A(IR(I),IC(J))*U(IC(J))
         ENDDO
         U(IC(I))=-SM/A(IR(I),IC(I))
       ENDDO
!
 101     FORMAT(8x,' NOTE:Pivot ',I3,' < ',E10.3,' in NLVC : ', &
              /,'        A null space may be multi-dimensional')
!
      RETURN
      END
!
!     ------ --------- -------- -----
      DOUBLE PRECISION FUNCTION RNRMV(N,V)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION V(*)
!
! Returns the L2-norm of the vector V.
!
       RNRMV = 0.d0
       DO I=1,N
         RNRMV=RNRMV+V(I)**2
       ENDDO
       RNRMV=DSQRT(RNRMV)
!
      RETURN
      END
!
!     ---------- -----
      SUBROUTINE NRMLZ(NDIM,V)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION V(*)
!
! Scale the vector V so that its discrete L2-norm becomes 1.
!
       SS=0.d0
       DO I=1,NDIM
         SS=SS+V(I)*V(I)
       ENDDO
       C=1.d0/DSQRT(SS)
       DO I=1,NDIM
         V(I)=V(I)*C
       ENDDO
!
      RETURN
      END
!
!     ------ --------- --------
      DOUBLE PRECISION FUNCTION PI(R)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
       PI=R*4.0d0*DATAN(1.d0)
!
      RETURN
      END
!
!     ---------- --
      SUBROUTINE GE(IAM,N,M1A,A,NRHS,NDX,U,M1F,F,IR,IC,DET)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Solves the linear system  A U = F by Gauss elimination
! with complete pivoting.
!
! Parameters :
!
!   N   : number of equations,
!   M1A : first dimension of A from DIMENSION statement,
!   A   : N * N matrix of coefficients,
!   NRHS: 0   if no right hand sides (determinant only),
!         >0   if there are NRHS right hand sides,
!   NDX : first dimension of U from DIMENSION statement,
!   U   : on exit U contains the solution vector(s),
!   M1F : first dimension of F from DIMENSION statement,
!   F   : right hand side vector(s),
!  IR,IC: integer vectors of dimension at least N.
!
! The input matrix A is overwritten.
!
      DIMENSION IR(*),IC(*),A(M1A,*),U(NDX,*),F(M1F,*)
!
       DO I=1,N
         IC(I)=I
         IR(I)=I
       ENDDO
!
!   Elimination.
!
       DET=1.d0
       NM1=N-1
!
       DO JJ=1,NM1
         IPIV=JJ
         JPIV=JJ
         PIV=0.d0
         DO I=JJ,N
           DO J=JJ,N
             P=ABS(A(IR(I),IC(J)))
             IF(P.GT.PIV)THEN
               PIV=P
               IPIV=I
               JPIV=J
              ENDIF
           ENDDO
         ENDDO
!
         DET=DET*A(IR(IPIV),IC(JPIV))
         IF(IPIV.NE.JJ)DET=-DET
         IF(JPIV.NE.JJ)DET=-DET
!
         IF(PIV.LT.RSMALL)THEN
           IF(FORT9DST==1)WRITE(9,101)JJ,RSMALL
         ENDIF
!
         K=IR(JJ)
         IR(JJ)=IR(IPIV)
         IR(IPIV)=K
!
         K=IC(JJ)
         IC(JJ)=IC(JPIV)
         IC(JPIV)=K
!
         JJP1=JJ+1
         DO L=JJP1,N
           RM=A(IR(L),IC(JJ))/A(IR(JJ),IC(JJ))
           IF(RM.NE.0.d0)THEN
             DO I=JJP1,N
               A(IR(L),IC(I))=A(IR(L),IC(I))-RM*A(IR(JJ),IC(I))
             ENDDO
             IF(NRHS.NE.0)THEN
               DO IRH=1,NRHS
                 F(IR(L),IRH)=F(IR(L),IRH)-RM*F(IR(JJ),IRH)
               ENDDO
             ENDIF
           ENDIF
         ENDDO
       ENDDO
       DET=DET*A(IR(N),IC(N))
!
       IF(NRHS.EQ.0)RETURN
!
!   Backsubstitution :
!
       DO IRH=1,NRHS
         U(IC(N),IRH)=F(IR(N),IRH)/A(IR(N),IC(N))
         DO I1=1,NM1
           I=N-I1
           SM=0.d0
           IP1=I+1
           DO J=IP1,N
             SM=SM+A(IR(I),IC(J))*U(IC(J),IRH)
           ENDDO
           U(IC(I),IRH)=(F(IR(I),IRH)-SM)/A(IR(I),IC(I))
         ENDDO
       ENDDO
!
 101   FORMAT(8x,' NOTE:Pivot ',I3,' < ',D10.3,' in GE')
!
      RETURN
      END
!
!     ---------- ----
      SUBROUTINE GESC(IAM,N,M1A,A,NRHS,NDX,U,M1F,F,IR,IC,DET)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Solves the linear system  A U = F by Gauss elimination
! with complete pivoting. Returns a scaled determinant.
!
! Parameters :
!
!   N   : number of equations,
!   M1A : first dimension of A from DIMENSION statement,
!   A   : N * N matrix of coefficients,
!   NRHS: 0   if no right hand sides (determinant only),
!         >0   if there are NRHS right hand sides,
!   NDX : first dimension of U from DIMENSION statement,
!   U   : on exit U contains the solution vector(s),
!   M1F : first dimension of F from DIMENSION statement,
!   F   : right hand side vector(s),
!  IR,IC: integer vectors of dimension at least N.
!
! The input matrix A is overwritten.
!
      DIMENSION IR(*),IC(*),A(M1A,*),U(NDX,*),F(M1F,*)
!
       DO I=1,N
         IC(I)=I
         IR(I)=I
       ENDDO
!
!   Elimination.
!
       DET=1.d0
       NM1=N-1
!
       DO JJ=1,NM1
         IPIV=JJ
         JPIV=JJ
         PIV=0.d0
         DO I=JJ,N
           DO J=JJ,N
             P=ABS(A(IR(I),IC(J)))
             IF(P.GT.PIV)THEN
               PIV=P
               IPIV=I
               JPIV=J
              ENDIF
           ENDDO
         ENDDO
!
         AP=A(IR(IPIV),IC(JPIV)) 
         DET=DET*LOG10(10+ABS(AP)) * atan(AP)
         IF(IPIV.NE.JJ)DET=-DET
         IF(JPIV.NE.JJ)DET=-DET
!
         IF(PIV.LT.RSMALL)THEN
           IF(FORT9DST==1)WRITE(9,101)JJ,RSMALL
         ENDIF
!
         K=IR(JJ)
         IR(JJ)=IR(IPIV)
         IR(IPIV)=K
!
         K=IC(JJ)
         IC(JJ)=IC(JPIV)
         IC(JPIV)=K
!
         JJP1=JJ+1
         DO L=JJP1,N
           RM=A(IR(L),IC(JJ))/A(IR(JJ),IC(JJ))
           IF(RM.NE.0.d0)THEN
             DO I=JJP1,N
               A(IR(L),IC(I))=A(IR(L),IC(I))-RM*A(IR(JJ),IC(I))
             ENDDO
             IF(NRHS.NE.0)THEN
               DO IRH=1,NRHS
                 F(IR(L),IRH)=F(IR(L),IRH)-RM*F(IR(JJ),IRH)
               ENDDO
             ENDIF
           ENDIF
         ENDDO
       ENDDO
       AP=A(IR(N),IC(N)) 
       DET=DET*LOG10(10+ABS(AP)) * atan(AP)
!
       IF(NRHS.EQ.0)RETURN
!
!   Backsubstitution :
!
       DO IRH=1,NRHS
         U(IC(N),IRH)=F(IR(N),IRH)/A(IR(N),IC(N))
         DO I1=1,NM1
           I=N-I1
           SM=0.d0
           IP1=I+1
           DO J=IP1,N
             SM=SM+A(IR(I),IC(J))*U(IC(J),IRH)
           ENDDO
           U(IC(I),IRH)=(F(IR(I),IRH)-SM)/A(IR(I),IC(I))
         ENDDO
       ENDDO
!
 101   FORMAT(8x,' NOTE:Pivot ',I3,' < ',D10.3,' in GE')
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE NEWLAB(IAP)
!
      USE AUTO_CONSTANTS, ONLY:IBRF8,MTOTF8,ITPF8,LABF8,NFPRF8,ISWF8,NTPLF8, &
           NARF8,NROWPRF8,ILAB,FORT3DST   
!      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER I,M
!
! Determine a suitable label when restarting.
!
      LOGICAL EOF3
      DIMENSION IAP(*)
!
       IPS=IAP(2)
       IRS=IAP(3)
       ISW=IAP(10)
       ITP=IAP(27)
       MBR=0
       MLAB=0
!
      IF (FORT3DST == 1) THEN
       REWIND 3
!
       IF(IRS>0)THEN
          DO
             READ(3,*,END=2)IBRS,NTOTRS,ITPRS,LABRS,NFPRS,ISWRS,NTPLRS, &
                  NARS,NSKIP
             IF(IBRS>MBR)MBR=IBRS
             IF(LABRS>MLAB)MLAB=LABRS
             CALL SKIP3(NSKIP,EOF3)
             IF(EOF3)EXIT
          ENDDO
       ENDIF
!      
      ELSE        
       IF(IRS>0)THEN
          M=SIZE(IBRF8)   
          DO I=1,M
            IBRS=IBRF8(I)
            NTOTRS=MTOTF8(I)
            ITPRS=ITPF8(I)
            LABRS=LABF8(I)
            NFPRS=NFPRF8(I)
            ISWRS=ISWF8(I)
            NTPLRS=NTPLF8(I)
            NARS=NARF8(I)
            NSKIP=NROWPRF8(I)          
            IF(IBRS>MBR)MBR=IBRS
            IF(LABRS>MLAB)MLAB=LABRS
          ENDDO
          EOF3=.FALSE.
       ENDIF
      ENDIF
!
 2     LAB=MLAB
       IAP(37)=LAB
       IF(ISW.LT.0.OR.IRS.EQ.0)THEN
         IBR=MBR+1
         IAP(30)=IBR
       ELSEIF( (ABS(ITP).LT.10.AND.ABS(ISW).EQ.2) &
            .OR. (IPS.EQ.2.AND.ITP.EQ.3) &
            .OR. (IPS.EQ.4.AND.ISW.EQ.2.AND.ABS(ITP).LT.10) &
            .OR. (IPS.EQ.5.AND.MOD(ITP,10).EQ.2) )THEN
         IBR=IRS
         IAP(30)=IBR
       ENDIF
!
      CALL DEALLOCATEF8()
      CALL ALLOCATEF8()
!      
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE FINDLB(IAP,IRS,NFPR,FOUND)
!
      USE AUTO_CONSTANTS, ONLY:IBRF8,MTOTF8,ITPF8,LABF8,NFPRF8,ISWF8,NTPLF8, &
           NARF8,NROWPRF8,ILAB,FORT3DST 
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER I,M
!
      LOGICAL FOUND,EOF3
!
      DIMENSION IAP(*)
!
! Locates restart point with label IRS and determines type.
! If the label can not be located on unit 3 then FOUND will be .FALSE.
!
       FOUND=.FALSE.
       ISW=IAP(10)
!
      IF(FORT3DST==1)THEN
       REWIND 3
!
       DO
         READ(3,*,END=2)IBR,NTOTRS,ITP,LABRS,NFPR,ISWRS,NTPLRS, &
              NARS,NSKIP
         IAP(27)=ITP
         IAP(30)=IBR
         IF(LABRS.EQ.IRS)THEN
           FOUND=.TRUE.
           IF(ABS(ISW).GE.2)THEN
             IF(ABS(ITP).LT.10)THEN
               ITPST=ABS(ITP)
               IAP(28)=ITPST
             ELSE
               ITPST=ABS(ITP/10)
               IAP(28)=ITPST
             ENDIF
           ELSE
             ITPST=0
             IAP(28)=ITPST
           ENDIF
           BACKSPACE 3
           RETURN
         ELSE
           CALL SKIP3(NSKIP,EOF3)
           IF(EOF3)GOTO 2
         ENDIF
       ENDDO
      ELSE
       M=SIZE(LABF8)
       DO I=1,M
         IBR=IBRF8(I)
         NTOTRS=MTOTF8(I)
         ITP=ITPF8(I)
         LABRS=LABF8(I)
         NFPR=NFPRF8(I)
         ISWRS=ISWF8(I)
         NTPLRS=NTPLF8(I)
         NARS=NARF8(I)
         NSKIP=NROWPRF8(I)
         IAP(27)=ITP
         IAP(30)=IBR
         IF(LABRS.EQ.IRS)THEN
           FOUND=.TRUE.
           ILAB=I
           IF(ABS(ISW).GE.2)THEN
             IF(ABS(ITP).LT.10)THEN
               ITPST=ABS(ITP)
               IAP(28)=ITPST
             ELSE
               ITPST=ABS(ITP/10)
               IAP(28)=ITPST
             ENDIF
           ELSE
             ITPST=0
             IAP(28)=ITPST
           ENDIF
           EOF3=.TRUE.
           GOTO 2          
         ENDIF
       ENDDO
      ENDIF
!
 2    CONTINUE
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE READLB(IAP,U,PAR)
!
      USE AUTO_CONSTANTS, ONLY:ILAB,IBRF8,MTOTF8,ITPF8,LABF8,NFPRF8,ISWF8, &
           NTPLF8,NARF8,NROWPRF8,NTSTF8,NCOLF8,NPARXF8,TF8,UF8,PARF8,NPARX, &
           NBIFX,NIAP,NRAP,FORT3DST
!      INCLUDE 'auto.h'
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER(10000) MATLABDISPSTR
!
      DIMENSION IAP(*),U(*),PAR(*)
!
! Reads the restart data for algebraic problems.
!
      IF(FORT3DST==1)THEN
       READ(3,*)IBRR,NTOTR,ITPR,LABR,NFPRR,ISWR,NTPLRS,NAR, &
            NSKIPR,N1,N2,NPARR
       NDIM=NAR-1
       IF(NDIM.LE.IAP(1))THEN
         READ(3,*)T,(U(I),I=1,NDIM)
       ELSE
         READ(3,*)T,(U(I),I=1,IAP(1)),(DUM,I=1,NDIM-IAP(1))
       ENDIF
      ELSE
       IBRR=IBRF8(ILAB)
       NTOTR=MTOTF8(ILAB)
       ITPR=ITPF8(ILAB)
       LABR=LABF8(ILAB)
       NFPRR=NFPRF8(ILAB)
       ISWR=ISWF8(ILAB)
       NTPLRS=NTPLF8(ILAB)
       NAR=NARF8(ILAB)
       NSKIPR=NROWPRF8(ILAB)
       N1=NTSTF8(ILAB)
       N2=NCOLF8(ILAB)
       NPARR=NPARXF8(ILAB)              
       NDIM=NAR-1      
       IF(NDIM.LE.IAP(1))THEN
         T=TF8(ILAB)
         DO I=1,NDIM
           U(I)=UF8(ILAB,I)
         ENDDO
       ELSE
         T=TF8(ILAB)
         DO I=1,IAP(1)
           U(I)=UF8(ILAB,I)
         ENDDO
       ENDIF
      ENDIF
!       
       IF(NPARR.GT.NPARX)THEN
         NPARR=NPARX
         WRITE(MATLABDISPSTR,100)NPARR
         CALL MEXPRINTF(MATLABDISPSTR)
         CALL MEXPRINTF(achar(10))
 100     FORMAT(' Warning : NPARX too small for restart data :', &
              ' restart PAR(i) skipped for i > ',I3)
       ENDIF
!
      IF(FORT3DST==1)THEN
       READ(3,*)(PAR(I),I=1,NPARR)
      ELSE       
       DO I=1,NPARR
         PAR(I)=PARF8(ILAB,I)
       ENDDO
      ENDIF
!
      RETURN
      END
!
!     ---------- -----
      SUBROUTINE SKIP3(NSKIP,EOF3)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Skips the specified number of lines on unit 3.
!
      LOGICAL EOF3
!
       EOF3=.FALSE.
       DO I=1,NSKIP
         READ(3,*,END=2)
       ENDDO
       RETURN
 2     EOF3=.TRUE.
       RETURN
      END
!
!     ------ --------- -------- -----
      DOUBLE PRECISION FUNCTION RINPR(IAP,NDIM1,NDX,UPS,VPS,DTM,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Computes the L2 inner product of UPS and VPS.
! (Using the first NDIM1 components only.)
!
      DIMENSION IAP(*),UPS(NDX,*),VPS(NDX,*),DTM(*),THU(*)
! Local
      DIMENSION WI(IAP(6)+1)
!
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
!
! Weights for the integration formulae :
       CALL WINT(NCOL+1,WI)
!
       S=0.d0
       DO J=1,NTST
         JP1=J+1
         SJ=0.d0
         DO I=1,NDIM1
           DO K=1,NCOL
             K1=(K-1)*NDIM+I
             SJ=SJ+WI(K)*THU(I)*UPS(K1,J)*VPS(K1,J)
           ENDDO
           SJ=SJ+WI(NCOL+1)*THU(I)*UPS(I,JP1)*VPS(I,JP1)
         ENDDO
         S=S+DTM(J)*SJ
       ENDDO
!
       RINPR=S
!
      RETURN
      END
!
!     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION RNRMSQ(IAP,NDIM1,NDX,UPS,DTM,THU)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION THU(*),IAP(*),UPS(*),DTM(*)
!
! Finds the norm-squared of UPS (first NDIM1 components are included only).
!
       RNRMSQ=RINPR(IAP,NDIM1,NDX,UPS,UPS,DTM,THU)
!
      RETURN
      END
!
!     ------ --------- -------- -----
      DOUBLE PRECISION FUNCTION RINTG(IAP,NDX,IC,UPS,DTM)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Computes the integral of the IC'th component of UPS.
!
      DIMENSION IAP(*),UPS(NDX,*),DTM(*)
! Local
      DIMENSION WI(IAP(6)+1)
!
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
!
! Weights for the integration formulae :
       CALL WINT(NCOL+1,WI)
!
       S=0.d0
       DO J=1,NTST
         JP1=J+1
         SJ=0.d0
           DO K=1,NCOL
             K1=(K-1)*NDIM+IC
             SJ=SJ+WI(K)*UPS(K1,J)
           ENDDO
           SJ=SJ+WI(NCOL+1)*UPS(IC,JP1)
         S=S+DTM(J)*SJ
       ENDDO
!
       RINTG=S
!
      RETURN
      END
!
!     ------ --------- -------- -----
      DOUBLE PRECISION FUNCTION RNRM2(IAP,NDX,IC,UPS,DTM)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Computes the L2-norm of the IC'th component of UPS.
! 
      DIMENSION IAP(*),UPS(NDX,*),DTM(*)
! Local
      DIMENSION WI(IAP(6)+1)
!
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
!
! Weights for the integration formulae :
       CALL WINT(NCOL+1,WI)
!
       S=0.d0
       DO J=1,NTST
         JP1=J+1
         SJ=0.d0
           DO K=1,NCOL
             K1=(K-1)*NDIM+IC
             SJ=SJ+WI(K)*UPS(K1,J)**2
           ENDDO
           SJ=SJ+WI(NCOL+1)*UPS(IC,JP1)**2
         S=S+DTM(J)*SJ
       ENDDO
!
       RNRM2=DSQRT(S)
!
      RETURN
      END
!
!     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION RMXUPS(IAP,NDX,I,UPS)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Computes the maximum of the I'th component of UPS.
!
      DIMENSION IAP(*),UPS(NDX,*)
!
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
!
       RMXUPS=UPS(I,1)
!
       DO J=1,NTST
         DO K=1,NCOL
           K1=(K-1)*NDIM+I
           IF(UPS(K1,J).GT.RMXUPS)RMXUPS=UPS(K1,J)
         ENDDO
       ENDDO
       IF(UPS(I,NTST+1).GT.RMXUPS)RMXUPS=UPS(I,NTST+1)
!
      RETURN
      END
!
!     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION RMNUPS(IAP,NDX,I,UPS)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Computes the minimum of the I'th component of UPS.
!
      DIMENSION IAP(*),UPS(NDX,*)
!
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
!
       RMNUPS=UPS(I,1)
!
       DO J=1,NTST
         DO K=1,NCOL
           K1=(K-1)*NDIM+I
           IF(UPS(K1,J).LT.RMNUPS)RMNUPS=UPS(K1,J)
         ENDDO
       ENDDO
       IF(UPS(I,NTST+1).LT.RMNUPS)RMNUPS=UPS(I,NTST+1)
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE SCALEB(IAP,NDIM1,NDX,DVPS,RLD,DTM,THL,THU)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Scales the vector (DVPS,RLD) so its norm becomes 1.
!
      DIMENSION IAP(*),DVPS(NDX,*),DTM(*),RLD(*),THL(*),THU(*)
!
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
!
       SS=RNRMSQ(IAP,NDIM1,NDX,DVPS,DTM,THU)
!
       DO I=1,NFPR
         SS=SS+THL(I)*RLD(I)**2
       ENDDO
!
       SC=1.d0/DSQRT(SS)
!
       DO J=1,NTST
         DO I=1,NCOL
           K1=(I-1)*NDIM
           DO K=K1+1,K1+NDIM1
             DVPS(K,J)=DVPS(K,J)*SC
           ENDDO
         ENDDO
       ENDDO
!
       DO I=1,NDIM1
         DVPS(I,NTST+1)=DVPS(I,NTST+1)*SC
       ENDDO
!
       DO I=1,NFPR
         RLD(I)=SC*RLD(I)
       ENDDO
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    General Boundary Value Problems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ------
      SUBROUTINE CNRLBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNT, &
           PVLI,THL,THU,IUZ,VUZ)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Controls the computation of solution branches.
!
      EXTERNAL FUNI,BCNI,ICNI,STPNT,FNLPBV,FNUZBV,PVLI,FNBPBV,FNSPBV
!
      DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),IUZ(*),THL(*),THU(*)
! Local
      DIMENSION RLCUR(NPARX),RLOLD(NPARX),RLDOT(NPARX) 
      COMPLEX(KIND(1.0D0)) EV
      ALLOCATABLE EV(:),UPS(:,:),UOLDPS(:,:),UPOLDP(:,:)
      ALLOCATABLE DUPS(:,:),UDOTPS(:,:),FA(:,:),FC(:),TM(:),DTM(:)
      ALLOCATABLE P0(:,:),P1(:,:),UZR(:)
!
! INITIALIZE COMPUTATION OF BRANCH
!
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       ILP=IAP(4)
       NTST=IAP(5)
       NCOL=IAP(6)
       IAD=IAP(7)
       IADS=IAP(8)
       ISP=IAP(9)
       ISW=IAP(10)
       NBC=IAP(12)
       NINT=IAP(13)
       NUZR=IAP(15)
       ITPST=IAP(28)
       NDX=NDIM*NCOL
!
       ALLOCATE(UPS(NDX,NTST+1),UOLDPS(NDX,NTST+1))
       ALLOCATE(UPOLDP(NDX,NTST+1),DUPS(NDX,NTST+1))
       ALLOCATE(UDOTPS(NDX,NTST+1),FA(NDX,NTST+1))
       ALLOCATE(FC(NBC+NINT+1),TM(NTST+1),DTM(NTST+1))
       ALLOCATE(P0(NDIM,NDIM),P1(NDIM,NDIM),UZR(NUZR),EV(NDIM))
!
       DS=RAP(1)
!
       RDS=DS
       DSOLD=RDS
       RAP(5)=DSOLD
       IF(ISP.LT.0)THEN
         ISP=-ISP
         IAP(9)=ISP
       ENDIF
       SP1=0.d0
       BP1=0.d0
       RLP=0.d0
       IF(NUZR.GT.0)THEN
         DO I=1,NUZR
           UZR(I)=0.d0
         ENDDO
       ENDIF
       NITPS=0
       IAP(31)=NITPS
       NTOT=0
       IAP(32)=NTOT
       ISTOP=0
       IAP(34)=ISTOP
!
      DO I=1,NPARX
        RLCUR(I)=0.d0
        RLOLD(I)=0.d0
        RLDOT(I)=0.d0
      ENDDO
!
       DO J=1,NTST+1
         DO I=1,NDX
           UPS(I,J)=0.d0
           UOLDPS(I,J)=0.d0
           UPOLDP(I,J)=0.d0
           DUPS(I,J)=0.d0
           UDOTPS(I,J)=0.d0
           FA(I,J)=0.d0
         ENDDO
       ENDDO
!
      NODIR=0
       CALL RSPTBV(IAP,RAP,PAR,ICP,FUNI,STPNT,RLCUR,RLOLD,RLDOT, &
            NDX,UPS,UOLDPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
       CALL PVLI(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
!      
!     don't set global rotations here for homoclinics, but in autlib5.c
       IF(IPS.NE.9)CALL SETRTN(IAP(23),NTST,NDX,UPS,PAR)
!
       IF(NODIR.EQ.1 .AND. ISW.GT.0)THEN
         CALL STDRBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RLCUR,RLOLD,RLDOT, &
              NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DTM,0,P0,P1,THL,THU)
       ELSEIF(IRS.NE.0 .AND. ISW.LT.0)THEN
         CALL STDRBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RLCUR,RLOLD,RLDOT, &
              NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DTM,1,P0,P1,THL,THU)
       ENDIF
!
! Store plotting data for restart point :
!
       CALL STHD(IAP,RAP,ICP)
       IF(IRS.EQ.0) THEN
         ITP=9+10*ITPST
       ELSE
         ITP=0
       ENDIF
       IAP(27)=ITP
       ISTOP=0
       IAP(34)=ISTOP
       CALL PVLI(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
       CALL STPLBV(IAP,RAP,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,TM,DTM,THL,THU)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)RETURN
!
       CALL EXTRBV(IAP,FUNI,RDS,RLCUR,RLOLD,RLDOT,NDX,UPS,UOLDPS, &
            UDOTPS)
!
       ITP=0
       IAP(27)=ITP
       GOTO 2
!
 1     ITP=0
       IAP(27)=ITP
       NTOT=IAP(32)
!
! Adapt the mesh to the solution.
!
       IF(IAD.NE.0)THEN
         IF(MOD(NTOT,IAD).EQ.0) &
              CALL ADAPT(IAP,NTST,NCOL,NTST,NCOL,TM,DTM,NDX,UPS,UOLDPS)
       ENDIF
!
! Adapt the stepsize along the branch.
!
       IF(IADS.NE.0)THEN
         IF(MOD(NTOT,IADS).EQ.0)CALL ADPTDS(IAP,RAP,RDS)
       ENDIF
!
! Provide initial approximation and determine next point.
!
       CALL CONTBV(IAP,RAP,PAR,ICP,FUNI,RDS,RLCUR,RLOLD,RLDOT, &
            NDX,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)
2      CALL STEPBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS, &
            RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC, &
            TM,DTM,P0,P1,THL,THU)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 3
!
! Check for user supplied parameter output parameter-values.
!
       IF(NUZR.GT.0)THEN
         DO IUZR=1,NUZR
           IAP(26)=IUZR 
           CALL LCSPBV(IAP,RAP,PAR,ICP,FNUZBV,FUNI,BCNI,ICNI,PVLI, &
                UZR(IUZR),RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS, &
                UDOTPS,UPOLDP,FA,FC,TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ)
           ISTOP=IAP(34)
           IF(ISTOP.EQ.1)GOTO 3
           ITP=IAP(27)
           IF(ITP.EQ.-1)THEN
             IF(IUZ(IUZR).GT.0)THEN
               ITP=-4-10*ITPST
               IAP(27)=ITP
               DO K=1,NUZR
                 UZR(K)=0.d0
               ENDDO
             ELSE
               ISTOP=-1
               IAP(34)=ISTOP
             ENDIF
           ENDIF
         ENDDO
       ENDIF
!
! Check for fold.
!
       IF(ABS(ILP).GT.0)THEN
         CALL LCSPBV(IAP,RAP,PAR,ICP,FNLPBV,FUNI,BCNI,ICNI,PVLI,RLP, &
              RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP, &
              FA,FC,TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ)
         ISTOP=IAP(34)
         IF(ISTOP.EQ.1)GOTO 3
         ITP=IAP(27)
         IF(ITP.EQ.-1)THEN
           IF(ILP.GT.0)THEN
             ITP=5+10*ITPST
             IAP(27)=ITP
             RLP=0.d0
             BP1=0.d0
             SP1=0.d0
           ELSE
!            *Stop at the first found fold
             ISTOP=-1
             IAP(34)=ISTOP
             GOTO 3
           ENDIF
         ENDIF
       ENDIF
!
! Check for branch point.
!
       IF(ABS(ISP).GE.2)THEN
         CALL LCSPBV(IAP,RAP,PAR,ICP,FNBPBV,FUNI,BCNI,ICNI,PVLI,BP1, &
              RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP, &
              FA,FC,TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ)
         ISTOP=IAP(34)
         IF(ISTOP.EQ.1)GOTO 3
         ITP=IAP(27)
         IF(ITP.EQ.-1)THEN
           IF(ISP.GT.0)THEN
             ITP=6+10*ITPST
             IAP(27)=ITP
             RLP=0.d0
             BP1=0.d0
             SP1=0.d0
           ELSE
!            *Stop at the first found BP
             ISTOP=-1
             IAP(34)=ISTOP
             GOTO 3
           ENDIF
         ENDIF
       ENDIF
!
! Check for period-doubling and torus bifurcation.
!
       IF(ABS(ISP).GT.0 .AND. &
            (IPS.EQ.2.OR.IPS.EQ.7.OR.IPS.EQ.12) )THEN
         CALL LCSPBV(IAP,RAP,PAR,ICP,FNSPBV,FUNI,BCNI,ICNI,PVLI,SP1, &
              RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP, &
              FA,FC,TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ)
         ISTOP=IAP(34)
         IF(ISTOP.EQ.1)GOTO 3
         ITP=IAP(27)
         IF(ITP.EQ.-1)THEN
           IF(ISP.GT.0)THEN
!            **Secondary periodic bifurcation: determine type
             CALL TPSPBV(IAP,RAP,PAR,EV)
             RLP=0.d0
             BP1=0.d0
             SP1=0.d0
           ELSE
!            *Stop at the first found SPB
             ISTOP=-1
             IAP(34)=ISTOP
             GOTO 3
           ENDIF
         ENDIF
       ENDIF
!
! Store plotting data.
!
 3     CALL PVLI(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
       CALL STPLBV(IAP,RAP,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,TM,DTM,THL,THU)
!
       ISTOP=IAP(34)
       IF(ISTOP.EQ.0)THEN
         GOTO 1
       ENDIF
       DEALLOCATE(EV,UPS,UOLDPS,UPOLDP,DUPS,UDOTPS,FA,FC,TM,DTM,P0,P1)
       DEALLOCATE(UZR)
       RETURN
!
      END
!
!     ---------- ------
      SUBROUTINE CONTBV(IAP,RAP,PAR,ICP,FUNI,RDS,RLCUR,RLOLD,RLDOT, &
           NDX,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Determines an initial approximation to the next solution point,
! by extrapolating from the two preceding points.
! The stepsize used in the preceding step has been stored in DSOLD.

      EXTERNAL FUNI
!
      DIMENSION IAP(*),RAP(*),PAR(*),ICP(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UOLDPS(NDX,*),UPOLDP(*),DTM(*)
      DIMENSION RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
!
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
!
       DSOLD=RAP(5)
!
! Compute rate of change (along branch) of PAR(ICP(1)) and U :
!
       DDS=1.d0/DSOLD
       NROW=NDIM*NCOL
       DO J=1,NTST+1
         DO I=1,NROW
           UDOTPS(I,J)=(UPS(I,J)-UOLDPS(I,J))*DDS
         ENDDO
       ENDDO
       DO I=1,NFPR
         RLDOT(I)=(RLCUR(I)-RLOLD(I))*DDS
       ENDDO
!        Rescale, to set the norm of (UDOTPS,RLDOT) equal to 1.
       CALL SCALEB(IAP,NDIM,NDX,UDOTPS,RLDOT,DTM,THL,THU)
!
! Extrapolate to get initial approximation to next solution point.
!
       CALL EXTRBV(IAP,FUNI,RDS,RLCUR,RLOLD,RLDOT,NDX,UPS,UOLDPS, &
            UDOTPS)
!
! Store time-derivative.
!
       CALL STUPBV(IAP,RAP,PAR,ICP,FUNI,RLCUR,RLOLD,NDX,UPS, &
            UOLDPS,UPOLDP)
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE EXTRBV(IAP,FUNI,RDS,RLCUR,RLOLD,RLDOT, &
           NDX,UPS,UOLDPS,UDOTPS)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Determines an initial approximation to the next solution by
! extrapolating from the two preceding points.
! The stepsize used in the preceding step has been stored in DSOLD.
!
      EXTERNAL FUNI
!
      DIMENSION IAP(*),UPS(NDX,*),UDOTPS(NDX,*),UOLDPS(NDX,*)
      DIMENSION RLCUR(*),RLOLD(*),RLDOT(*)
!
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
!
       NROW=NDIM*NCOL
       DO I=1,NFPR
         RLOLD(I)=RLCUR(I)
         RLCUR(I)=RLCUR(I)+RDS*RLDOT(I)
       ENDDO
       DO J=1,NTST+1
         DO I=1,NROW
           UOLDPS(I,J)=UPS(I,J)
           UPS(I,J)=UPS(I,J)+RDS*UDOTPS(I,J)
         ENDDO
       ENDDO
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE STUPBV(IAP,RAP,PAR,ICP,FUNI,RLCUR,RLOLD, &
           NDX,UPS,UOLDPS,UPOLDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Stores U-prime (derivative with respect to T) in UPOLDP.
!
      EXTERNAL FUNI
!
      DIMENSION UPS(NDX,*),UOLDPS(NDX,*),UPOLDP(NDX,*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),IAP(*),RAP(*)
! Local
      ALLOCATABLE U(:),UOLD(:),F(:),DFDU(:),DFDP(:)
!
       NDIM=IAP(1)
       IPS=IAP(2)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
!
       DO I=1,NFPR
         PAR(ICP(I))=RLOLD(I)
       ENDDO
!
       ALLOCATE(U(NDIM),UOLD(NDIM),F(NDIM))
       ALLOCATE(DFDU(NDIM**2),DFDP(NDIM*NPARX))
!
       DO J=1,NTST+1
         DO I=1,NDIM
           U(I)=UOLDPS(I,J)
           IF(IPS.EQ.14 .OR. IPS.EQ.16)THEN
             UOLD(I)=2*UOLDPS(I,J)-UPS(I,J)
           ELSE
             UOLD(I)=UOLDPS(I,J)
           ENDIF
         ENDDO
         CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,0,F,DFDU,DFDP)
         DO I=1,NDIM
           UPOLDP(I,J)=F(I)
         ENDDO
       ENDDO
!
       NC1=NCOL-1
       DO K=1,NC1
         N1=K*NDIM
         DO J=1,NTST
           DO I=1,NDIM
             U(I)=UOLDPS(N1+I,J)
             IF(IPS.EQ.14 .OR. IPS.EQ.16)THEN
               UOLD(I)=2*UOLDPS(N1+I,J)-UPS(N1+I,J)
             ELSE
               UOLD(I)=UOLDPS(N1+I,J)
             ENDIF
           ENDDO
           CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,0,F,DFDU,DFDP)
           DO I=1,NDIM
             UPOLDP(N1+I,J)=F(I)
           ENDDO
         ENDDO
       ENDDO
!
       DO I=1,NFPR
         PAR(ICP(I))=RLCUR(I)
       ENDDO
!
      DEALLOCATE(U,UOLD,F,DFDU,DFDP)
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE STEPBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS, &
           RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC, &
           TM,DTM,P0,P1,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      USE SOLVEBV
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Controls the solution of the nonlinear equations (by Newton's method)
! for the next solution (PAR(ICP(*)) , U) on a branch of solutions.
!
      EXTERNAL FUNI,BCNI,ICNI,PVLI
!
      DIMENSION IAP(*),RAP(*),UPS(NDX,*),UOLDPS(NDX,*),UDOTPS(NDX,*)
      DIMENSION UPOLDP(NDX,*),DUPS(NDX,*),FA(NDX,*),FC(*),TM(*),DTM(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
      DIMENSION P0(*),P1(*)
      LOGICAL DONE
!
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       NROW=NDIM*NCOL
       IADS=IAP(8)
       IID=IAP(18)
       ITNW=IAP(20)
       NWTN=IAP(21)
       NFPR=IAP(29)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
!
       DSMIN=RAP(2)
       EPSL=RAP(11)
       EPSU=RAP(12)
!
       DELREF=0
 1     DSOLD=RDS
       RAP(5)=DSOLD
       NITPS=0
       IAP(31)=NITPS
!
! Write additional output on unit 9 if requested.
!
       CALL WRTBV9(IAP,RAP,RLCUR,NDX,UPS,TM,DTM,THU)
!
! Generate the Jacobian matrix and the right hand side.
!
       DO NIT1=1,ITNW
!
         NITPS=NIT1
         IAP(31)=NITPS
         NLLV=0
!
         IFST=0
         IF(NITPS.LE.NWTN)IFST=1
!
         CALL SOLVBV(IFST,IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RDS,NLLV, &
              RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,DTM,FA,FC, &
              P0,P1,THL,THU)
!
! Add Newton increments.
!
         DO I=1,NDIM
           UPS(I,NTST+1)=UPS(I,NTST+1)+FC(I)
         ENDDO
         DO I=1,NFPR
           RLCUR(I)=RLCUR(I)+FC(NDIM+I)
           PAR(ICP(I))=RLCUR(I)
         ENDDO
!
         DUMX=0.d0
         UMX=0.d0
         DO J=1,NTST
           DO I=1,NROW
             ADU=ABS(FA(I,J))
             IF(ADU.GT.DUMX)DUMX=ADU
             AU=ABS(UPS(I,J))
             IF(AU.GT.UMX)UMX=AU
             UPS(I,J)=UPS(I,J)+FA(I,J)
           ENDDO
         ENDDO
!
         CALL WRTBV9(IAP,RAP,RLCUR,NDX,UPS,TM,DTM,THU)
!
! Check whether user-supplied error tolerances have been met :
!
         DONE=.TRUE.
         RDRL=0.d0
         DO I=1,NFPR
           ADRL=ABS(FC(NDIM+I))/(1.d0+ABS(RLCUR(I)))
           IF(ADRL.GT.EPSL)DONE=.FALSE.
           IF(ADRL.GT.RDRL)RDRL=ADRL
         ENDDO
         RDUMX=DUMX/(1.d0+UMX)
         IF(DONE.AND.RDUMX.LT.EPSU)THEN
	   CALL PVLI(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
           IF(IID.GE.2)THEN
             IF(FORT9DST==1)WRITE(9,*)  
           ENDIF
           RETURN
         ENDIF
!
         IF(NITPS.EQ.1)THEN
           DELREF=20*DMAX1(RDRL,RDUMX)
         ELSE
           DELMAX=DMAX1(RDRL,RDUMX)
           IF(DELMAX.GT.DELREF)EXIT
         ENDIF
!
       ENDDO
!
! Maximum number of iterations reached.
!
       IF(IADS.EQ.0)THEN
         IF(FORT9DST==1)WRITE(9,101)IBR,NTOP
       ENDIF
       IF(IADS.EQ.0)GOTO 13
!
! Reduce stepsize and try again.
!
       MXT=ITNW
       IAP(31)=MXT
       CALL ADPTDS(IAP,RAP,RDS)
       IF(ABS(RDS).LT.DSMIN)GOTO 12
       DO I=1,NFPR
         RLCUR(I)=RLOLD(I)+RDS*RLDOT(I)
       ENDDO
       DO J=1,NTST+1
         DO I=1,NROW
           UPS(I,J)=UOLDPS(I,J)+RDS*UDOTPS(I,J)
         ENDDO
       ENDDO
       IF(IID.GE.2)THEN
         IF(FORT9DST==1)WRITE(9,102)IBR,NTOP
       ENDIF  
       GOTO 1
!
! Minimum stepsize reached.
!
 12    IF(FORT9DST==1)WRITE(9,103)IBR,NTOP
 13    DO I=1,NFPR
         RLCUR(I)=RLOLD(I)
         PAR(ICP(I))=RLCUR(I)
       ENDDO
       DO J=1,NTST+1
         DO I=1,NROW
           UPS(I,J)=UOLDPS(I,J)
         ENDDO
       ENDDO
       ISTOP=1
       IAP(34)=ISTOP
!
 101   FORMAT(I4,I6,' NOTE:No convergence with fixed step size')
 102   FORMAT(I4,I6,' NOTE:Retrying step')
 103   FORMAT(I4,I6,' NOTE:No convergence using minimum step size')
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!      Restart of Solution Branches ( Differential Equations )
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ------
      SUBROUTINE RSPTBV(IAP,RAP,PAR,ICP,FUNI,STPNT,RLCUR,RLOLD, &
           RLDOT,NDX,UPS,UOLDPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:ILAB,NTSTF8,NCOLF8,UF8,FORT3DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Restarts computation of a branch of solutions at point labelled IRS.
! The output written on unit 8 by a previous run is now expected as
! input on unit 3. The label IRS, where computation is to resume, must
! be specified in the user-supplied subroutine INIT.
! If IRS=0 then the starting point must be provided analytically in the
! user-supplied subroutine STPNT.
!
      EXTERNAL FUNI, STPNT
!
      DIMENSION IAP(*),RAP(*)
      DIMENSION UPS(NDX,*),UOLDPS(NDX,*),UPOLDP(NDX,*),UDOTPS(NDX,*)
      DIMENSION TM(*),DTM(*),PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
!
      LOGICAL FOUND
      ALLOCATABLE UPSN(:,:),UPOLDN(:,:),UDOTPN(:,:),TMN(:),DTMN(:)
!
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       NTST=IAP(5)
       NCOL=IAP(6)
       NDM=IAP(23)
       NFPR=IAP(29)
!
! Get restart data :
!
!     First take a peek at the file to see if ntst, ndim and
!     ncol are different then the values found in
!     the parameter file fort.2.
!
       IF(IRS.GT.0)THEN
        CALL FINDLB(IAP,IRS,NFPRS,FOUND)
        IF(FORT3DST==1)THEN
         READ(3,*)IBR,NTOTRS,ITPRS,LAB,NFPRS,ISWRS,NTPLRS,NARS,NSKIP, &
              NTSRS,NCOLRS,NPARR
         NTST3=NTSRS
         NCOL3=NCOLRS
         NDIM3=NARS-1
        ELSE
          NTST3=NTSTF8(ILAB)
          NCOL3=NCOLF8(ILAB)
          NDIM3=SIZE(UF8,2)
        ENDIF
       ELSE
         NTST3=NTST
         NCOL3=NCOL
         NDIM3=NDIM
       ENDIF
       
! use the bigger of the size defined in fort.2 and the one defined in fort.8
       NTSTU=MAX(NTST,NTST3)
       NCOLU=MAX(NCOL,NCOL3)
       NDIMU=NDIM
       NDXLOC=NDIMU*NCOLU
       NTSTCU=(NTSTU+1)*NCOLU
!
! Autodetect special case when homoclinic branch switching is
! completed and the orbit's representation has to be
! changed.
!
       IF(IPS.EQ.9.AND.NDIM3.GT.(NDM*2).AND.NDIM3.GT.NDIM)THEN
         NTSTCU=(NTSTU+1)*(NDIM3/NDM)
         NDIMU=NDIM3
         NDXLOC=NDIMU*NCOLU
         IAP(1)=NDIMU
       ENDIF
       ALLOCATE(UPSN(NDXLOC,NTSTCU),UPOLDN(NDXLOC,NTSTCU))
       ALLOCATE(UDOTPN(NDXLOC,NTSTCU),TMN(NTSTCU),DTMN(NTSTCU))
! initialize arrays
       DO I=1,NTSTCU
         DO J=1,NDXLOC
           UPSN(J,I)=0.0d0
           UPOLDN(J,I)=0.0d0
           UDOTPN(J,I)=0.0d0
         ENDDO
       ENDDO
!
       CALL STPNT(IAP,RAP,PAR,ICP,NTSRS,NCOLRS,RLCUR,RLDOT, &
            NDXLOC,UPSN,UDOTPN,UPOLDN,TMN,DTMN,NODIR,THL,THU)
       IAP(1)=NDIM
!
! Determine a suitable starting label and branch number.
!
       CALL NEWLAB(IAP)
!
       DO J=1,NTSRS
         DTMN(J)=TMN(J+1)-TMN(J)
       ENDDO
!
! Adapt mesh if necessary :
!
       IF( NTST.NE.NTSRS .OR. NCOL.NE.NCOLRS)THEN
         CALL ADAPT(IAP,NTSRS,NCOLRS,NTST,NCOL,TMN,DTMN,NDXLOC, &
              UPSN,UDOTPN)
       ENDIF
! Copy from the temporary large arrays into the normal arrays.
       DO I=1,NTST+1
         DTM(I)=DTMN(I)
         TM(I)=TMN(I)
         DO J=1,NDIM*NCOL
           UPS(J,I)=UPSN(J,I)
           UPOLDP(J,I)=UPOLDN(J,I)
           UDOTPS(J,I)=UDOTPN(J,I)
         ENDDO
       ENDDO
       DEALLOCATE(DTMN,TMN,UPSN,UPOLDN,UDOTPN)
!
! Set UOLDPS, RLOLD.
!
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
         RLOLD(I)=RLCUR(I)
       ENDDO
!
       NROW=NDIM*NCOL
       DO J=1,NTST+1
         DO I=1,NROW
           UOLDPS(I,J)=UPS(I,J)
         ENDDO
       ENDDO
!
! Store U-prime (derivative with respect to time or space variable).
!
       IF(NODIR.EQ.-1)THEN
!        ** Restart from a Hopf bifurcation.
         NODIR=0
         ISW=1
       ELSE
!        ** Restart from orbit.
          CALL STUPBV(IAP,RAP,PAR,ICP,FUNI,RLCUR,RLOLD, &
               NDX,UPS,UOLDPS,UPOLDP)
        ENDIF
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE READBV(IAP,PAR,ICPRS,NTSRS,NCOLRS,NDIMRD,RLDOTRS,UPS, &
           UDOTPS,TM,ITPRS,NDX)
!
      USE AUTO_CONSTANTS, ONLY:IBRF8,MTOTF8,ITPF8,LABF8,NFPRF8,ISWF8,NTPLF8, &
           NARF8,NROWPRF8,ILAB,NTSTF8,NCOLF8,NPARXF8,TMF8,UPSF8,IFPRF8,RLDOTF8, &
           UDOTPSF8,PARF8,NPARX,NBIFX,NIAP,NRAP         
!      INCLUDE 'auto.h'
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER(10000) MATLABDISPSTR
!
      DIMENSION IAP(*),PAR(*),ICPRS(*)
      DIMENSION RLDOTRS(*),UPS(NDX,*),UDOTPS(NDX,*),TM(*)
! Local
      DIMENSION TEMP(7)
      LOGICAL EOF3
      INTEGER R,C,I,J,K,CTR
!
      NDIM=IAP(1)
      IF(FORT3DST==1)THEN
       READ(3,*)IBR,NTOT,ITPRS,LAB,NFPR,ISW,NTPL,NARS,NSKIP, &
            NTSRS,NCOLRS,NPARR
      ELSE
        IBR=IBRF8(ILAB)
        NTOT=MTOTF8(ILAB)
        ITPRS=ITPF8(ILAB)
        LAB=LABF8(ILAB)
        NFPR=NFPRF8(ILAB)
        ISW=ISWF8(ILAB)
        NTPL=NTPLF8(ILAB)
        NARS=NARF8(ILAB)
        NSKIP=NROWPRF8(ILAB)
        NTSRS=NTSTF8(ILAB)
        NCOLRS=NCOLF8(ILAB)
        NPARR=NPARXF8(ILAB)
      ENDIF
!       
       IAP(30)=IBR
       IAP(37)=LAB
       NRSP1=NTSRS+1
!
       NDIMRS=NARS-1
       NSKIP1=(NDIMRS+1)/8 - NDIM/7
       NSKIP2=(NDIMRS+1)/9 - NDIM/8
       IF(NDIM.LE.NDIMRS)THEN
         NDIMRD=NDIM
       ELSE
         NDIMRD=NDIMRS
       ENDIF
!
      IF (FORT3DST == 1) THEN
       DO J=1,NTSRS
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=K1+NDIMRD-1
           READ(3,*)TEMP(I),(UPS(K,J),K=K1,K2)
           IF(NSKIP1.GT.0)CALL SKIP3(NSKIP1,EOF3)
         ENDDO
         TM(J)=TEMP(1)
       ENDDO
       READ(3,*)TM(NRSP1),(UPS(K,NRSP1),K=1,NDIMRD)
       IF(NSKIP1.GT.0)CALL SKIP3(NSKIP1,EOF3)
      ELSE
       R=SIZE(UPSF8,1)
       C=SIZE(UPSF8,2)       
       DO J=1,NTSRS
         DO I=1,NCOLRS
           DO K=1,NDIM
             K1=(I-1)*NDIM+K
             K2=(J-1)*NCOLRS+I
             UPS(K1,J)=UPSF8(K2,K,ILAB)
           ENDDO
           TEMP(I)=TMF8(K2,ILAB)
         ENDDO
         TM(J)=TEMP(1)
       ENDDO
       K2=K2+1
       TM(NRSP1)=TMF8(K2,ILAB)
       DO I=1,NDIM
         UPS(I,NRSP1)=UPSF8(K2,I,ILAB)
       ENDDO  
      ENDIF
!
      IF (FORT3DST == 1) THEN
       READ(3,*)(ICPRS(K),K=1,NFPR)       
       READ(3,*)(RLDOTRS(K),K=1,NFPR)
      ELSE        
       R=SIZE(RLDOTF8,1)
       IF( R == 0 ) THEN
         CALL AUTOSTOPWITHERROR("Property Rldot in object F8 is not defined")
       ENDIF
       DO I=1,NFPRF8(ILAB)
         ICPRS(I)=IFPRF8(ILAB,I)
         RLDOTRS(I)=RLDOTF8(ILAB,I)
       ENDDO
      ENDIF
!
! Read U-dot (derivative with respect to arclength).
!
      IF (FORT3DST == 1) THEN
       DO J=1,NTSRS
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=K1+NDIMRD-1
           READ(3,*)(UDOTPS(K,J),K=K1,K2)
           IF(NSKIP2.GT.0)CALL SKIP3(NSKIP2,EOF3)
         ENDDO
       ENDDO
       READ(3,*)(UDOTPS(K,NRSP1),K=1,NDIMRD)
       IF(NSKIP2.GT.0)CALL SKIP3(NSKIP2,EOF3)
      ELSE
       EOF3=.FALSE.          
       DO J=1,NTSRS
         DO I=1,NCOLRS
           DO K=1,NDIM
             K1=(I-1)*NDIM+K
             K2=(J-1)*NCOLRS+I
             UDOTPS(K1,J)=UDOTPSF8(K2,K,ILAB)
           ENDDO
         ENDDO
       ENDDO
       K2=K2+1
       DO I=1,NDIM
         UDOTPS(I,NRSP1)=UDOTPSF8(K2,I,ILAB)
       ENDDO 
      ENDIF       
!
! Read the parameter values.
!
       IF(NPARR.GT.NPARX)THEN
         NPARR=NPARX
         WRITE(MATLABDISPSTR,100)NPARR
         CALL MEXPRINTF(MATLABDISPSTR)
         CALL MEXPRINTF(achar(10))
 100     FORMAT(' Warning : NPARX too small for restart data : ',/, &
              ' PAR(i) set to zero, for i > ',I3)
       ENDIF
!
      IF (FORT3DST == 1) THEN
       READ(3,*)(PAR(I),I=1,NPARR)
      ELSE
       DO I=1,NPARXF8(ILAB)
         PAR(I)=PARF8(ILAB,I)
       ENDDO
      ENDIF
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE STPNBV(IAP,RAP,PAR,ICP,NTSRS,NCOLRS,RLCUR,RLDOT, &
           NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
!
      USE INTERFACES, ONLY:PDBLE
      USE HOMCONT, ONLY:PREHO
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! This subroutine locates and retrieves the information required to
! restart computation at the point with label IRS.
! This information is expected on unit 3.
!
      DIMENSION IAP(*),RAP(*),UPS(NDX,*),UDOTPS(NDX,*),TM(*),DTM(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLDOT(*)
! Local
      DIMENSION ICPRS(NPARX)
!
      LOGICAL FOUND
!
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       ISW=IAP(10)
       NFPR=IAP(29)
!
       CALL FINDLB(IAP,IRS,NFPRS,FOUND)
       CALL READBV(IAP,PAR,ICPRS,NTSRS,NCOLRS,NDIMRD,RLDOT,UPS, &
            UDOTPS,TM,ITPRS,NDX)
!
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
!
! Special case : Preprocess restart data in case of homoclinic
! continuation
!
       IF(IPS.EQ.9)THEN
         CALL PREHO(IAP,PAR,ICP,NDX,NTSRS,NDIMRD,NCOLRS,UPS, &
              UDOTPS,TM,DTM)
!
! Special case : Preprocess restart data in case of branch switching
! at a period doubling bifurcation.
!
       ELSE IF((IPS.EQ.2.OR.IPS.EQ.7).AND.ISW.EQ.-1.AND.ITPRS.EQ.7) THEN
         CALL PDBLE(NDIM,NTSRS,NCOLRS,NDX,UPS,UDOTPS,TM,PAR)
         RETURN
       ENDIF
!
! Take care of the case where the free parameters have been changed at
! the restart point.
!
       NODIR=0
       IF(NFPRS.NE.NFPR)THEN
         NODIR=1
         RETURN
       ENDIF
       DO I=1,NFPR
         IF(ICPRS(I).NE.ICP(I)) THEN
           NODIR=1
           RETURN
         ENDIF
       ENDDO
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE STPNUB(IAP,RAP,PAR,ICP,NTSRS,NCOLRS,RLCUR,RLDOT, &
           NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates a starting point for the continuation of a branch of
! of solutions to general boundary value problems by calling the user
! supplied subroutine STPNT where an analytical solution is given.
!
      DIMENSION IAP(*),UPS(NDX,*),UDOTPS(NDX,*),TM(*),DTM(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLDOT(*)
! Local
      ALLOCATABLE U(:)
!
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
       ALLOCATE(U(NDIM))
!
! Generate the (initially uniform) mesh.
!
       CALL MSH(NTST,TM)
       DT=1.d0/(NTST*NCOL)
!
       DO J=1,NTST+1
         IF(J.EQ.(NTST+1)) THEN
           NCOL1=1
         ELSE
           NCOL1=NCOL
         ENDIF
         DO I=1,NCOL1
           T=TM(J)+(I-1)*DT
           K1=(I-1)*NDIM+1
           K2=I*NDIM
           CALL STPNT(NDIM,U,PAR,T)
           DO K=K1,K2
             UPS(K,J)=U(K-K1+1)
           ENDDO
         ENDDO
       ENDDO
!
       NTSRS=NTST
       NCOLRS=NCOL
       IBR=1
       IAP(30)=IBR
       LAB=0
       IAP(37)=LAB
!
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
!
       NODIR=1
!
      DEALLOCATE(U)
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE SETRTN(NDM,NTST,NDX,UPS,PAR)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Initialization for rotations
!
      POINTER NRTN(:)
      COMMON /BLRTN/ NRTN,IRTN
      DIMENSION UPS(NDX,*),PAR(*)
!
        ALLOCATE(NRTN(NDM))
        IRTN=0
        DO I=1,NDM
          NRTN(I)=NINT( (UPS(I,NTST+1)-UPS(I,1)) / PI(2.d0) )
          IF(NRTN(I).NE.0)THEN
             PAR(19)=PI(2.d0)
             IRTN=1
          ENDIF
        ENDDO
        IF(IRTN.EQ.0)DEALLOCATE(NRTN)
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE STDRBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RLCUR,RLOLD, &
           RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DTM,IPERP, &
           P0,P1,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      USE SOLVEBV
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates a direction vector (UDOTPS,RLDOT) that is needed to start
! the computation of a branch when no direction vector is given.
!
      EXTERNAL FUNI,BCNI,ICNI
!
      DIMENSION IAP(*),RAP(*),UDOTPS(NDX,*),FA(NDX,*),FC(*),DTM(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
      DOUBLE PRECISION UPS(NDX,*),DUPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION UPOLDP(NDX,*),P0(*),P1(*)
!
! Generate the Jacobian matrix with zero direction vector.
! (Then the last row of the Jacobian will be zero)
! in case the starting direction is to be determined.
!
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       IID=IAP(18)
       NFPR=IAP(29)
!
       NROW=NDIM*NCOL
       IF(IPERP.EQ.0)THEN
         DO J=1,NTST+1
           DO I=1,NROW
             UDOTPS(I,J)=0.d0
           ENDDO
         ENDDO
         DO I=1,NFPR
           RLDOT(I)=0.d0
         ENDDO
       ENDIF
!
        RDSZ=0.d0
        NLLV=1
        IFST=1
        CALL SOLVBV(IFST,IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RDSZ,NLLV, &
             RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,DTM,FA,FC, &
             P0,P1,THL,THU)
!
! Compute the starting direction.
!
         DO I=1,NDIM
           UDOTPS(I,NTST+1)=FC(I)
         ENDDO
         DO I=1,NFPR
           RLDOT(I)=FC(NDIM+I)
           PAR(ICP(I))=RLCUR(I)
         ENDDO
!
         DO J=1,NTST
           DO I=1,NROW
             UDOTPS(I,J)=FA(I,J)
           ENDDO
         ENDDO
!
! Scale the starting direction.
!
         CALL SCALEB(IAP,NDIM,NDX,UDOTPS,RLDOT,DTM,THL,THU)
!
! Make sure that RLDOT(1) is positive (unless zero).
!
         IF(RLDOT(1).LT.0.d0)THEN
           DO I=1,NFPR
             RLDOT(I)=-RLDOT(I)
           ENDDO
           DO J=1,NTST+1
             DO I=1,NROW
               UDOTPS(I,J)=-UDOTPS(I,J)
             ENDDO
           ENDDO
         ENDIF
!
         IF(IID.GE.2)THEN
           IF(FORT9DST==1)WRITE(9,101)
            DO I=1,NFPR 
              IF(FORT9DST==1)WRITE(9,102)ICP(I),RLDOT(I)
            ENDDO
         ENDIF
!
 101     FORMAT(/,' Starting direction of the free parameter(s) : ')
 102     FORMAT(' PAR(',I3,') :',E11.3)
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  Detection and Location of Branch Points in Boundary Value Problems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ------
      SUBROUTINE LCSPBV(IAP,RAP,PAR,ICP,FNCS,FUNI,BCNI,ICNI,PVLI,Q, &
           RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC, &
           TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! This subroutine uses the Secant method to accurately locate folds
! branch points, and zero(es) of user parameter values.
! Such points are located as points on a solution branch where the
! EXTERNAL function FNCS changes sign.
! It involves calling the basic solution subroutines CONTBV and STEP
! with decreasing values of RDS (stepsize along branch).
! The point is assumed to have been found with sufficient accuracy if
! the ratio between RDS and the user supplied value of DS is less than
! the user-supplied tolerance EPSS.
! This subroutine is called from CNRLB, which controls the computation
! of branches of solutions to general boundary value problems.
!
      EXTERNAL FNCS,FUNI,BCNI,ICNI,PVLI
!
      COMPLEX(KIND(1.0D0)) EV(*)
!
      LOGICAL CHNG
!
      DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),TM(*),DTM(*),FA(*),FC(*)
      DIMENSION UPS(*),UDOTPS(*),UOLDPS(*),UPOLDP(*),DUPS(*)
      DIMENSION RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*),P0(*),P1(*)
!
       IID=IAP(18)
       ITMX=IAP(19)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
!
       DS=RAP(1)
       DSMAX=RAP(3)
       DSOLD=RAP(5)
       EPSS=RAP(13)
!
! Check for zero.
!
       Q0=Q
       Q1=FNCS(IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV, &
            RLCUR,RLOLD,RLDOT,NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS, &
            TM,DTM,THL,THU,IUZ,VUZ)
!
       PQ=Q0*Q1
       IF(PQ.GE.0.d0 .OR. (.NOT. CHNG))THEN
         Q=Q1
         RETURN
       ENDIF
!
! Use the secant method for the first step:
!
       S0=0.d0
       S1=DSOLD
       NITSP1=0
       DQ=Q0-Q1
       RDS=Q1/DQ*(S1-S0)
 1     RDS=(1.d0+HMACH)*RDS
       S=S1+RDS
!
! Return if tolerance has been met :
!
       RRDS=ABS(RDS)/(1+DSQRT(ABS(DS*DSMAX)))
       IF(RRDS.LT.EPSS) THEN
         ITP=-1
         IAP(27)=ITP
         Q=0.d0
         IF(FORT9DST==1)WRITE(9,102)RDS
         RETURN
       ENDIF
!
! If requested write additional output on unit 9 :
!
       IF(IID.GE.2)THEN
         IF(FORT9DST==1)WRITE(9,101)NITSP1,RDS
       ENDIF
!
       CALL CONTBV(IAP,RAP,PAR,ICP,FUNI,RDS,RLCUR,RLOLD,RLDOT, &
            NDX,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)
       CALL STEPBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS, &
            RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP, &
            FA,FC,TM,DTM,P0,P1,THL,THU)
       ISTOP=IAP(34)
       IF(ISTOP.NE.0)THEN
         Q=0.d0
         RETURN
       ENDIF
!
! Check for zero.
!
       Q=FNCS(IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV, &
            RLCUR,RLOLD,RLDOT,NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS, &
            TM,DTM,THL,THU,IUZ,VUZ)
!
       NITSP1=NITSP1+1
       IF(NITSP1.LE.ITMX)THEN
!        Use Mueller's method with bracketing for subsequent steps
         CALL MUELLER(Q0,Q1,Q,S0,S1,S,RDS)
         GOTO 1
       ENDIF
!
       IF(FORT9DST==1)WRITE(9,103)IBR,NTOP+1
       Q=0.d0
 101   FORMAT(' ==> Location of special point :  Iteration ',I3, &
            '  Step size = ',ES13.5)
 102   FORMAT(' ==> Location of special point : ', &
            ' Convergence.   Step size = ',ES13.5)
 103    FORMAT(I4,I6,' NOTE:Possible special point')
!
      RETURN
      END
!
!     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNLPBV &
           (IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV,RLCUR,RLOLD,RLDOT, &
          NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS,TM,DTM,THL,THU,IUZ,VUZ)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      USE SOLVEBV
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! RETURNS A QUANTITY THAT CHANGES SIGN AT A LIMIT POINT (BVP)
!
      COMPLEX(KIND(1.0D0)) EV(*)
!
      LOGICAL CHNG
!
      EXTERNAL FUNI,BCNI,ICNI
!
      DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),UDOTPS(NDX,*),FA(NDX,*)
      DIMENSION FC(*)
      DIMENSION RLCUR(*),RLOLD(*),RLDOT(*),TM(*),DTM(*),THL(*),THU(*)
      DOUBLE PRECISION UPS(NDX,*),DUPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION UPOLDP(NDX,*),P0(*),P1(*)
!
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       IID=IAP(18)
       NFPR=IAP(29)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
!
! Find the direction vector.
!
         NLLV=-1
         IFST=0
         RDSZ=0.d0
!
         CALL SOLVBV(IFST,IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RDSZ,NLLV, &
              RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,DTM,FA,FC, &
              P0,P1,THL,THU)
!
         DO I=1,NDIM
           UDOTPS(I,NTST+1)=FC(I)
         ENDDO
!
         DO I=1,NFPR
           RLDOT(I)=FC(NDIM+I)
         ENDDO
!
         NROW=NDIM*NCOL
         DO J=1,NTST
           DO I=1,NROW
             UDOTPS(I,J)=FA(I,J)
          ENDDO
         ENDDO
!
! Scale the direction vector.
!
         CALL SCALEB(IAP,NDIM,NDX,UDOTPS,RLDOT,DTM,THL,THU)
         IF(IID.GE.2)THEN
           IF(FORT9DST==1)WRITE(9,101)ABS(IBR),NTOP+1,RLDOT(1)
         ENDIF
!
! Set the quantity to be returned.
!
         FNLPBV=RLDOT(1)
         CHNG=.TRUE.
         RAP(16)=FNLPBV
!
 101     FORMAT(I4,I6,9X,'Fold Function ',ES14.5)
!
      RETURN
      END
!
!     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNBPBV &
           (IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV,RLCUR,RLOLD,RLDOT, &
           NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS,TM,DTM,THL,THU,IUZ,VUZ)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMPLEX(KIND(1.0D0)) EV(*)
!
      LOGICAL CHNG
!
      EXTERNAL FUNI,BCNI,ICNI
!
      DIMENSION IAP(*),RAP(*),P1(*)
! Local
      ALLOCATABLE IR(:),IC(:),PP(:)
      DOUBLE PRECISION U(1),F(1)
!
       NDIM=IAP(1)
       IID=IAP(18)
!
! Save the determinant of the reduced system.
!
      DET=RAP(14)
      DET0=DET
      IBR=IAP(30)
      NTOT=IAP(32)
      NTOP=MOD(NTOT-1,9999)+1
!
! Compute the determinant of P1.
!
      ALLOCATE(IR(NDIM),IC(NDIM),PP(NDIM**2))
      DO I=1,NDIM**2
        PP(I)=P1(I)
      ENDDO                               
      CALL GE(0,NDIM,NDIM,PP,0,1,U,1,F,IR,IC,DET)
      DEALLOCATE(IR,IC,PP)
      RAP(14)=DET
!
! Set the determinant of the normalized reduced system.
!
      IF(ABS(DET0)/HUGE(DET).LT.ABS(DET))THEN
        FNBPBV=DET0/DET
        CHNG=.TRUE.
      ELSE
        FNBPBV=0.d0
        CHNG=.FALSE.
      ENDIF
      RAP(18)=FNBPBV
!
      IF(IID.GE.2)THEN
        IF(FORT9DST==1)WRITE(9,101)ABS(IBR),NTOP+1,FNBPBV
      ENDIF
 101  FORMAT(I4,I6,9X,'BP   Function ',ES14.5)
!
      RETURN
      END
!
!     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNSPBV &
           (IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV, RLCUR,RLOLD,RLDOT, &
           NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS,TM,DTM,THL,THU,IUZ,VUZ)
!
      USE FLOQUET
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! This function returns a quantity that changes sign when a complex
! pair of eigenvalues of the linearized Poincare map moves in or out
! of the unit circle or when a real eigenvalues passes through -1.

      COMPLEX(KIND(1.0D0)) EV(*),ZTMP
      DIMENSION IAP(*),RAP(*),P0(*),P1(*)
! Local
      LOGICAL CHNG
!
      EXTERNAL FUNI,BCNI,ICNI
!
       NDIM=IAP(1)
       ISP=IAP(9)
       ISW=IAP(10)
       IID=IAP(18)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
!
! Initialize.
!
       FNSPBV=0.d0
       RAP(19)=FNSPBV
       D=0.d0
       CHNG=.FALSE.
!
       IF(IID.GE.4)THEN
         CALL EVECS(NDIM,P0,P1)
       ENDIF
!
!  Compute the Floquet multipliers
      CALL FLOWKM(NDIM, P0, P1, IID, EV)
!
! Find the multiplier closest to z=1.
!
       AMIN=RLARGE
       LOC=1
       DO J=1,NDIM
         AZM1= ABS( EV(J) - 1.d0 )
         IF(AZM1.LE.AMIN)THEN
           AMIN=AZM1
           LOC=J
         ENDIF
       ENDDO
       IF(LOC.NE.1) THEN
         ZTMP=EV(LOC)
         EV(LOC)=EV(1)
         EV(1)=ZTMP
       ENDIF
!
! Order the remaining Floquet multipliers by distance from |z|=1.
!
       IF(NDIM.GE.3)THEN
         DO I=2,NDIM-1
           AMIN=RLARGE
           DO J=I,NDIM
             AZM1= ABS(EV(J)) - 1.d0 
             AZM1=ABS(AZM1)
             IF(AZM1.LE.AMIN)THEN
               AMIN=AZM1
               LOC=J
             ENDIF
           ENDDO
           IF(LOC.NE.I) THEN
             ZTMP=EV(LOC)
             EV(LOC)=EV(I)
             EV(I)=ZTMP
           ENDIF
         ENDDO
       ENDIF
!
! Print error message if the Floquet multiplier at z=1 is inaccurate.
! (ISP is set to negative and detection of bifurations is discontinued)
!
       AMIN= ABS( EV(1) - 1.d0 )
       IF(AMIN.GT.5.0E-2 .AND. ISP.EQ.2) THEN
         IF(IID.GE.2)THEN
           IF(FORT9DST==1)WRITE(9,101)ABS(IBR),NTOP+1
         ENDIF  
         DO I=1,NDIM
            IF(FORT9DST==1)WRITE(9,105)ABS(IBR),NTOP+1,I,EV(I)
         ENDDO
         NINS=0
         IAP(33)=NINS
         IF(FORT9DST==1)WRITE(9,104)ABS(IBR),NTOP+1,NINS
         ISP=-ISP
         IAP(9)=ISP
         RETURN
       ENDIF
!
! Restart automatic detection if the Floquet multiplier at z=1 is
! sufficiently accurate again.
!
       IF(ISP.LT.0)THEN
         IF(AMIN.LT.1.0E-2)THEN
           IF(FORT9DST==1)WRITE(9,102)ABS(IBR),NTOP+1
           ISP=-ISP
           IAP(9)=ISP
         ELSE
           DO I=1,NDIM
              IF(FORT9DST==1)WRITE(9,105)ABS(IBR),NTOP+1,I,EV(I)
           ENDDO
           RETURN
         ENDIF
       ENDIF
!
! Count the number of Floquet multipliers inside the unit circle.
!
! Set tolerance for deciding if a multiplier is outside |z=1|.
! Use, for example, tol=1d-3 for conservative systems.
       tol=1.d-5
!
       NINS1=1
       IF(NDIM.EQ.1) THEN
         D=0.d0
         FNSPBV=D
         RAP(19)=FNSPBV
       ELSE
         DO I=2,NDIM
           IF( ABS(EV(I)).LE.(1.d0+tol))NINS1=NINS1+1
         ENDDO
         IF(ISP.EQ.2) THEN
           IF(AIMAG(EV(2)).EQ.0.d0 .AND. REAL(EV(2)).GT.0.d0)THEN
!            *Ignore if second multiplier is real positive
             D=0.d0
           ELSE
             D= ABS(EV(2)) - 1.d0
           ENDIF
           IF(ISW.EQ.2)THEN
             FNSPBV=0.d0
           ELSE
             FNSPBV=D
           ENDIF
           RAP(19)=FNSPBV
           NINS=IAP(33)
           IF(NINS1.NE.NINS)CHNG=.TRUE.
         ENDIF
       ENDIF
!
       NINS=NINS1
       IAP(33)=NINS
       IF( IID.GE.2 .AND. (ISP.EQ.1 .OR. ISP.EQ.2))THEN
          IF(FORT9DST==1)WRITE(9,103)ABS(IBR),NTOP+1,D
       ENDIF
!
! Print the Floquet multipliers.
!
       NINS=IAP(33)
       IF(FORT9DST==1)WRITE(9,104)ABS(IBR),NTOP+1,NINS
       DO I=1,NDIM
          IF(FORT9DST==1)WRITE(9,105)ABS(IBR),NTOP+1,I,EV(I),ABS(EV(I))
       ENDDO
!
 101   FORMAT(I4,I6,' NOTE:Multiplier inaccurate')
 102   FORMAT(I4,I6,' NOTE:Multiplier accurate again')
 103   FORMAT(I4,I6,9X,'SPB  Function ',ES14.5)
 104   FORMAT(I4,I6,9X,'Multipliers:     Stable:',I4)
 105   FORMAT(I4,I6,9X,'Multiplier',I3,1X,2ES14.5, &
            '  Abs. Val.',ES14.5)
!
      RETURN
      END
!
!     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNUZBV &
           (IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV,RLCUR,RLOLD,RLDOT, &
           NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS,TM,DTM,THL,THU,IUZ,VUZ)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      COMPLEX(KIND(1.0D0)) EV(*)
!
      LOGICAL CHNG
!
      EXTERNAL FUNI,BCNI,ICNI
!
      DIMENSION IAP(*),PAR(*),IUZ(*),VUZ(1)
!
       IID=IAP(18)
       IUZR=IAP(26)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
!
       FNUZBV=PAR(ABS(IUZ(IUZR)))-VUZ(IUZR)
       CHNG=.TRUE.
!
       IF(IID.GE.3)THEN
         IF(FORT9DST==1)WRITE(9,101)ABS(IBR),NTOP+1,IUZR,FNUZBV
       ENDIF  
 101   FORMAT(I4,I6,9X,'User Func.',I3,1X,ES14.5)
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE TPSPBV(IAP,RAP,PAR,EV)
!
! Determines type of secondary periodic bifurcation.
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
      COMPLEX(KIND(1.0D0)) EV(*)
!
      DIMENSION PAR(*),IAP(*),RAP(*)
!
       NDIM=IAP(1)
!
       EPSS=RAP(13)
       ITPST=IAP(28)
!
! Find the eigenvalue closest to z=1.
!
       LOC=1
       AMIN=RLARGE
       DO I=1,NDIM
         AZM1= ABS( EV(I) - 1.d0 )
         IF(AZM1.LE.AMIN)THEN
           AMIN=AZM1
           LOC=I
         ENDIF
       ENDDO
!
! Find the eigenvalue closest to the unit circle
! (excluding the eigenvalue at z=1).
!
       LOC1=1
       AMIN=RLARGE
       DO I=1,NDIM
         IF(I.NE.LOC)THEN
           D= ABS(EV(I)) - 1.d0
           AD=ABS(D)
           IF(AD.LE.AMIN)THEN
             AMIN=AD
             LOC1=I
           ENDIF
         ENDIF
       ENDDO
!
      IF(ABS(AIMAG(EV(LOC1))).GT.SQRT(EPSS))THEN
!       ** torus bifurcation
        ITP=8+10*ITPST
        IAP(27)=ITP
        PAR(12)=ASIN(AIMAG(EV(LOC1)))
      ELSE IF(REAL(EV(LOC1)).LT.-.5d0)THEN
!       ** period doubling
        ITP=7+10*ITPST
        IAP(27)=ITP
      ELSE
!       ** something else...
        ITP=0
        IAP(27)=ITP
      ENDIF
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    Output (Boundary Value Problems)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ------
      SUBROUTINE STPLBV(IAP,RAP,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,TM,DTM, &
           THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Writes the bifurcation diagram on unit 7 (Differential Equations)
! (Also controls the writing of complete solutions on unit 8).
! Every line written contains, in order, the following:
!
!  IBR    : The label of the branch.
!  NTOT   : The index of the point on the branch.
!           (Points are numbered consecutively along a branch).
!           If IPS=2 or 3, then the sign of NTOT indicates stability :
!            - = stable , + = unstable, or unknown.
!  ITP    : An integer indicating the type of point :
!
!             4  (  )  :   Output point (Every NPR steps along branch).
!            -4  (UZ)  :   Output point (Zero of user function).
!             5  (LP)  :   Fold (fold).
!             6  (BP)  :   Branch point.
!             7  (PD)  :   Period doubling bifurcation.
!             8  (TR)  :   Bifurcation to an invariant torus.
!             9  (EP)  :   End point of branch, normal termination.
!            -9  (MX)  :   End point of branch, abnormal termination.
!
!  LAB        : The label of a special point.
!  PAR(ICP(1)): The principal parameter.
!  A          : The L2-norm of the solution vector, or other measure of
!               the solution (see the user-supplied parameter IPLT).
!  MAX U(*)   : The maxima of the first few solution components.
!  PAR(ICP(*)): Further free parameters (if any).
!
      DIMENSION PAR(*),ICP(*),IAP(*),RAP(*),TM(*),DTM(*),UPS(*),THU(*)
! Local
      DIMENSION UMX(IAP(23))
!
       NDIM=IAP(1)
       IPS=IAP(2)
       ISW=IAP(10)
       IPLT=IAP(11)
       NMX=IAP(14)
       NPR=IAP(16)
       NDM=IAP(23)
       ITP=IAP(27)
       ITPST=IAP(28)
       IBR=IAP(30)
!
       RL0=RAP(6)
       RL1=RAP(7)
       A0=RAP(8)
       A1=RAP(9)
!
       NTOT=IAP(32)
       NTOT=NTOT+1
       IAP(32)=NTOT
!
! ITP is set to 4 every NPR steps along a branch of solns and the entire
! solution is written on unit 8.
!
       IF(NPR.NE.0)THEN
         IF(MOD(NTOT,NPR).EQ.0 .AND. MOD(ITP,10).EQ.0)ITP=4+10*ITPST
         IAP(27)=ITP
       ENDIF
!
! Check whether limits of the bifurcation diagram have been reached :
!
       IAB=ABS(IPLT)
       IF(IAB.EQ.0.OR.IAB.GT.3*NDM) &
            AMP=DSQRT(RNRMSQ(IAP,NDM,NDX,UPS,DTM,THU))
       IF(IPLT.GT.0.AND.IAB.LE.NDM)AMP=RMXUPS(IAP,NDX,IAB,UPS)
       IF(IPLT.GT.NDM.AND.IAB.LE.2*NDM) &
            AMP=RINTG(IAP,NDX,IAB-NDM,UPS,DTM)
       IF(IPLT.GT.2*NDM.AND.IAB.LE.3*NDM) &
            AMP=RNRM2(IAP,NDX,IAB-2*NDM,UPS,DTM)
       IF(IPLT.LT.0.AND.IAB.LE.NDM)AMP=RMNUPS(IAP,NDX,IAB,UPS)
!
       RAP(10)=AMP
!
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)THEN
!        ** Maximum number of iterations reached somewhere.
         ITP=-9-10*ITPST
         IAP(27)=ITP
       ELSEIF(ISTOP.EQ.-1)THEN
!        ** UZR endpoint
         ITP=9+10*ITPST
         IAP(27)=ITP
       ELSE
         IF(PAR(ICP(1)).LT.RL0.OR.PAR(ICP(1)).GT.RL1 &
              .OR. AMP.LT.A0.OR.AMP.GT.A1 .OR. NTOT.GE.NMX)THEN
           ISTOP=1
           IAP(34)=ISTOP
           ITP=9+10*ITPST
           IAP(27)=ITP
         ENDIF
       ENDIF
!
! All special points receive label:
!
       LABW=0
       IF(MOD(ITP,10).NE.0) THEN
         LAB=IAP(37)
         LAB=LAB+1
         IAP(37)=LAB
         LABW=LAB
       ENDIF
!
! Compute maxima of solution components.
!
       N2=NDM
       IF(N2.GT.100)N2=100
       DO I=1,N2
         ITMP=I
         UMX(I)=RMXUPS(IAP,NDX,ITMP,UPS)
       ENDDO
!
! Branch number is negative for periodic solutions
!
       IF(IPS.EQ.2)THEN
         IBRS=-IBR
       ELSE
         IBRS=IBR
       ENDIF
!
! Determine stability, and write output on units 7 and 8.
!
       NTOTS=NTOT
       IF(ABS(ISW).LE.1 .AND. (IPS.EQ.2.OR.IPS.EQ.7))THEN
         NINS=IAP(33)
         IF(NINS.EQ.NDIM)NTOTS=-NTOT
       ENDIF
       CALL WRLINE(IAP,PAR,ICP(NPARX+1),IBRS,NTOTS,LABW,AMP,UMX)
!
! Write plotting and restart data on unit 8.
!
       IF(MOD(ITP,10).NE.0) &
            CALL WRTBV8(IAP,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,TM,DTM)
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE WRTBV8(IAP,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,TM,DTM)
!
      USE AUTO_CONSTANTS, ONLY:IBRF8,oldIBRF8,MTOTF8,oldMTOTF8,ITPF8,oldITPF8, &
           LABF8,oldLABF8,NFPRF8,oldNFPRF8,ISWF8,oldISWF8,NTPLF8,oldNTPLF8, &
           NARF8,oldNARF8,NROWPRF8,oldNROWPRF8,NTSTF8,oldNTSTF8,NCOLF8, & 
           oldNCOLF8,NPARXF8,oldNPARXF8,IFPRF8,oldIFPRF8,TF8,oldTF8,TMF8,oldTMF8, &
           PARF8,oldPARF8,RLDOTF8,oldRLDOTF8,UF8,oldUF8,UPSF8,oldUPSF8,UDOTPSF8, &
           oldUDOTPSF8,UPSF8TMP,TMF8TMP,UDOTPSF8TMP,NPARX,NBIFX,NIAP,NRAP, &
           FORT7DST,FORT8DST,FORT9DST,RUNMODEDST  
!      INCLUDE 'auto.h'
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      INTEGER R,C,P,I,J,K      
!
! Writes plotting and restart data on unit 8, viz.:
! (1) data identifying the corresponding point on unit 7,
! (2) the complete solution,
! (3) the direction of the branch.
!
! Specifically the following is written:
!
!  IBR   : The index of the branch.
!  NTOT  : The index of the point.
!  ITP   : The type of point (see STPLBV above).
!  LAB   : The label of the point.
!  NFPR : The number of free parameters used in the computation.
!  ISW   : The value of ISW used in the computation.
!  NTPL  : The number of points in the time interval [0,1] for which
!          solution values are written.
!  NAR   : The number of values written per point.
!          (NAR=NDIM+1, since T and U(i), i=1,..,NDIM are written).
!  NROWPR: The number of lines printed following the identifying line
!          and before the next data set or the end of the file.
!          (Used for quickly skipping a data set when searching).
!  NTST  : The number of time intervals used in the discretization.
!  NCOL  : The number of collocation points used.
!  NPARX : The dimension of the array PAR.
!
!  Following the above described identifying line there are NTPL lines
! containing :
!     T , U-1(T) , U-2(T) , ... , U-NDIM(T),
! where NDIM is the dimension of the system of differential equations.
!
! Following this is a line containing the indices of the free parameters
!    ICP(I),I=1,NFPR,
!
! followed by a line containing the values
!    RL-dot(i) , i=1,NFPR,
!
! and following this are NTPL lines each containing
!    U-dot-1(T), U-dot-2(T), ... , U-dot-NDIM(T).
!
! Finally the parameter values PAR(i) , i=1,NPARX, are written.
!
!  Above, RL-dot(.) and U-dot(.) specify the direction of the branch.
!
      DIMENSION IAP(*),UPS(NDX,*),UDOTPS(NDX,*),TM(*),DTM(*)
      DIMENSION PAR(*),ICP(*),RLDOT(*)
!xxx====================================================================
!xxx Test problem: compute the error
      err(x,t)=x - 2*DATAN(1.d0)*PAR(2)*DSIN(4*DATAN(1.d0)*t)
!xxx====================================================================
!
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       ISW=IAP(10)
       ITP=IAP(27)
       NFPR=IAP(29)
       IBR=IAP(30)
       NTOT=IAP(32)
       LAB=IAP(37)
!
! Write information identifying the solution :
!
       NTPL=NCOL*NTST+1
       NAR=NDIM+1
       NRD=2+NDIM/7+(NDIM-1)/7
       NROWPR=NRD*(NCOL*NTST+1) + (NFPR-1)/7+1 + (NPARX-1)/7+1 &
            + (NFPR-1)/20+1
!
       MTOT=MOD(NTOT-1,9999)+1
!
      IF( FORT8DST == 1 )THEN
       WRITE(8,101)IBR,MTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR, &
            NTST,NCOL,NPARX
      ENDIF
!
!
      IF(ALLOCATED(UPSF8TMP))DEALLOCATE(UPSF8TMP)
      IF(ALLOCATED(TMF8TMP))DEALLOCATE(TMF8TMP)
      IF(ALLOCATED(UDOTPSF8TMP))DEALLOCATE(UDOTPSF8TMP)  
      ALLOCATE(UPSF8TMP(NTPL,NDIM))
      ALLOCATE(TMF8TMP(NTPL))
      ALLOCATE(UDOTPSF8TMP(NTPL,NDIM))       
!
! Write the entire solution on unit 8 :
!
!xxx====================================================================
!xxx Test problem
       eg=0.d0
       em=0.d0
!xxx====================================================================
       DO J=1,NTST
         RN=1.d0/NCOL
         DO I=1,NCOL
           K1=(I-1)*NDIM+1
           K2=I*NDIM
           T=TM(J)+(I-1)*RN*DTM(J)
!
           IF( FORT8DST == 1 )THEN
            WRITE(8,102)T,(UPS(K,J),K=K1,K2)
           ENDIF
!
           TMF8TMP(NCOL*(J-1)+I)=T
           DO K=K1,K2
             UPSF8TMP(NCOL*(J-1)+I,K-K1+1)=UPS(K,J)
           ENDDO
!xxx====================================================================
!xxx Test problem
           er = err(ups(k1,j),T)
           if(dabs(er).gt.eg)eg=dabs(er)
           if(i.eq.1 .and. dabs(er).gt.em)em=dabs(er)
!xxx====================================================================
         ENDDO
       ENDDO
!xxx====================================================================
!xxx Test problem
! Write global error and mesh error
!xxx       write(10,100)ncol,ntst,eg,em
!xxx 100   FORMAT(4X,I2,I4,7ES11.3)
!xxx====================================================================
      IF( FORT8DST == 1 )THEN
       WRITE(8,102)TM(NTST+1),(UPS(I,NTST+1),I=1,NDIM)
      ENDIF
      TMF8TMP(NTPL)=TM(NTST+1)
      DO J=1,NDIM
        UPSF8TMP(NTPL,J)=UPS(J,NTST+1)
      ENDDO
!
! Write the free parameter indices:
!
      IF( FORT8DST == 1 )THEN
       WRITE(8,103)(ICP(I),I=1,NFPR)
      ENDIF
!
! Write the direction of the branch:
!
      IF( FORT8DST == 1 )THEN
       WRITE(8,102)(RLDOT(I),I=1,NFPR)
      ENDIF
!       
       DO J=1,NTST
         DO I=1,NCOL
           K1=(I-1)*NDIM+1
           K2=I*NDIM
!
           IF( FORT8DST == 1 )THEN
            WRITE(8,102)(UDOTPS(K,J),K=K1,K2)
           ENDIF
! 
           DO K=K1,K2
             UDOTPSF8TMP(NCOL*(J-1)+I,K-K1+1)=UDOTPS(K,J)
           ENDDO
         ENDDO
       ENDDO
       IF( FORT8DST == 1 )THEN 
        WRITE(8,102)(UDOTPS(K,NTST+1),K=1,NDIM)
       ENDIF
       DO J=1,NDIM
         UDOTPSF8TMP(NTPL,J)=UDOTPS(J,NTST+1)
       ENDDO
!       
!
! Write the parameter values.
!
      IF( FORT8DST == 1 )THEN
       WRITE(8,102)(PAR(I),I=1,NPARX)
      ENDIF
!
!
! ARRAY ALLOCATION  ----------------------------------------------------------
!
      IF( RUNMODEDST == 0 )GOTO 109
!
! IBR ------------
      R=SIZE(IBRF8,1)
      ALLOCATE(oldIBRF8(R))
      oldIBRF8=IBRF8
      DEALLOCATE(IBRF8)
      ALLOCATE(IBRF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            IBRF8(I)=oldIBRF8(I)
          ENDDO
      ENDIF
      IBRF8(R+1)=IBR
      DEALLOCATE(oldIBRF8)
!
! MTOT ------------
      R=SIZE(MTOTF8,1)
      ALLOCATE(oldMTOTF8(R))
      oldMTOTF8=MTOTF8
      DEALLOCATE(MTOTF8)
      ALLOCATE(MTOTF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            MTOTF8(I)=oldMTOTF8(I)
          ENDDO
      ENDIF
      MTOTF8(R+1)=MTOT
      DEALLOCATE(oldMTOTF8)
!
! ITP ------------
      R=SIZE(ITPF8,1)
      ALLOCATE(oldITPF8(R))
      oldITPF8=ITPF8
      DEALLOCATE(ITPF8)
      ALLOCATE(ITPF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            ITPF8(I)=oldITPF8(I)
          ENDDO
      ENDIF
      ITPF8(R+1)=ITP
      DEALLOCATE(oldITPF8)
!
! LAB ------------
      R=SIZE(LABF8,1)
      ALLOCATE(oldLABF8(R))
      oldLABF8=LABF8
      DEALLOCATE(LABF8)
      ALLOCATE(LABF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            LABF8(I)=oldLABF8(I)
          ENDDO
      ENDIF
      LABF8(R+1)=LAB
      DEALLOCATE(oldLABF8)
!
! NFPR ------------
      R=SIZE(NFPRF8,1)
      ALLOCATE(oldNFPRF8(R))
      oldNFPRF8=NFPRF8
      DEALLOCATE(NFPRF8)
      ALLOCATE(NFPRF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            NFPRF8(I)=oldNFPRF8(I)
          ENDDO
      ENDIF
      NFPRF8(R+1)=NFPR
      DEALLOCATE(oldNFPRF8)
!
! ISW ------------
      R=SIZE(ISWF8,1)
      ALLOCATE(oldISWF8(R))
      oldISWF8=ISWF8
      DEALLOCATE(ISWF8)
      ALLOCATE(ISWF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            ISWF8(I)=oldISWF8(I)
          ENDDO
      ENDIF
      ISWF8(R+1)=ISW
      DEALLOCATE(oldISWF8)
!
! NTPL ------------
      R=SIZE(NTPLF8,1)
      ALLOCATE(oldNTPLF8(R))
      oldNTPLF8=NTPLF8
      DEALLOCATE(NTPLF8)
      ALLOCATE(NTPLF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            NTPLF8(I)=oldNTPLF8(I)
          ENDDO
      ENDIF
      NTPLF8(R+1)=NTPL
      DEALLOCATE(oldNTPLF8)
!
! NAR ------------
      R=SIZE(NARF8,1)
      ALLOCATE(oldNARF8(R))
      oldNARF8=NARF8
      DEALLOCATE(NARF8)
      ALLOCATE(NARF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            NARF8(I)=oldNARF8(I)
          ENDDO
      ENDIF
      NARF8(R+1)=NAR
      DEALLOCATE(oldNARF8)
!
! NROWPR ------------
      R=SIZE(NROWPRF8,1)
      ALLOCATE(oldNROWPRF8(R))
      oldNROWPRF8=NROWPRF8
      DEALLOCATE(NROWPRF8)
      ALLOCATE(NROWPRF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            NROWPRF8(I)=oldNROWPRF8(I)
          ENDDO
      ENDIF
      NROWPRF8(R+1)=NROWPR
      DEALLOCATE(oldNROWPRF8)
!
! NTST ------------
      R=SIZE(NTSTF8,1)
      ALLOCATE(oldNTSTF8(R))
      oldNTSTF8=NTSTF8
      DEALLOCATE(NTSTF8)
      ALLOCATE(NTSTF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            NTSTF8(I)=oldNTSTF8(I)
          ENDDO
      ENDIF
      NTSTF8(R+1)=NTST
      DEALLOCATE(oldNTSTF8)
!
! NCOL ------------
      R=SIZE(NCOLF8,1)
      ALLOCATE(oldNCOLF8(R))
      oldNCOLF8=NCOLF8
      DEALLOCATE(NCOLF8)
      ALLOCATE(NCOLF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            NCOLF8(I)=oldNCOLF8(I)
          ENDDO
      ENDIF
      NCOLF8(R+1)=NCOL
      DEALLOCATE(oldNCOLF8)
!
! NPARX ------------
      R=SIZE(NPARXF8,1)
      ALLOCATE(oldNPARXF8(R))
      oldNPARXF8=NPARXF8
      DEALLOCATE(NPARXF8)
      ALLOCATE(NPARXF8(R+1))
      IF(R > 0) THEN
          DO I=1,R
            NPARXF8(I)=oldNPARXF8(I)
          ENDDO
      ENDIF
      NPARXF8(R+1)=NPARX
      DEALLOCATE(oldNPARXF8)
!
! IFPR ------------
      R=SIZE(IFPRF8,1)
      C=SIZE(IFPRF8,2)
      ALLOCATE(oldIFPRF8(R,NFPR))
      oldIFPRF8=IFPRF8
      DEALLOCATE(IFPRF8)
      ALLOCATE(IFPRF8(R+1,NFPR))
      IF(R > 0) THEN
          DO I=1,R
            DO J=1,NFPR
              IFPRF8(I,J)=oldIFPRF8(I,J)
            ENDDO
          ENDDO
      ENDIF
      DO J=1,NFPR
        IFPRF8(R+1,J)=ICP(J)
      ENDDO
      DEALLOCATE(oldIFPRF8)
!
! TM ------------
      R=SIZE(TMF8,1)
      C=SIZE(TMF8,2)
      ALLOCATE(oldTMF8(R,C))
      oldTMF8=TMF8
      DEALLOCATE(TMF8)
      ALLOCATE(TMF8(NTPL,C+1))
      IF(R > 0) THEN
          DO I=1,NTPL
            DO J=1,C
              TMF8(I,J)=oldTMF8(I,J)
            ENDDO
          ENDDO
      ENDIF
      DO I=1,NTPL
        TMF8(I,C+1)=TMF8TMP(I)
      ENDDO
      DEALLOCATE(oldTMF8)
!
! PAR ------------
      R=SIZE(PARF8,1)
      C=SIZE(PARF8,2)
      ALLOCATE(oldPARF8(R,NPARX))
      oldPARF8=PARF8
      DEALLOCATE(PARF8)
      ALLOCATE(PARF8(R+1,NPARX))
      IF(R > 0) THEN
          DO I=1,R
            DO J=1,NPARX
              PARF8(I,J)=oldPARF8(I,J)
            ENDDO
          ENDDO
      ENDIF
      DO J=1,NPARX
        PARF8(R+1,J)=PAR(J)
      ENDDO
      DEALLOCATE(oldPARF8)
!
! RLDOT ------------
      R=SIZE(RLDOTF8,1)
      C=SIZE(RLDOTF8,2)
      ALLOCATE(oldRLDOTF8(R,NFPR))
      oldRLDOTF8=RLDOTF8
      DEALLOCATE(RLDOTF8)
      ALLOCATE(RLDOTF8(R+1,NFPR))
      IF(R > 0) THEN
          DO I=1,R
            DO J=1,NFPR
              RLDOTF8(I,J)=oldRLDOTF8(I,J)
            ENDDO
          ENDDO
      ENDIF
      DO J=1,NFPR
        RLDOTF8(R+1,J)=RLDOT(J)
      ENDDO
      DEALLOCATE(oldRLDOTF8)
!
! UPS ------------
      R=SIZE(UPSF8,1)
      C=SIZE(UPSF8,2)
      P=SIZE(UPSF8,3)
      ALLOCATE(oldUPSF8(NTPL,NDIM,P))
      oldUPSF8=UPSF8
      DEALLOCATE(UPSF8)
      ALLOCATE(UPSF8(NTPL,NDIM,P+1))
      IF(R > 0) THEN
        DO I=1,NTPL
          DO J=1,NDIM
            DO K=1,P
              UPSF8(I,J,K)=oldUPSF8(I,J,K)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      DO I=1,NTPL  
        DO J=1,NDIM  
          UPSF8(I,J,P+1)=UPSF8TMP(I,J)
        ENDDO
      ENDDO
      DEALLOCATE(oldUPSF8)
!
! UDOTPS ------------
      R=SIZE(UDOTPSF8,1)
      C=SIZE(UDOTPSF8,2)
      P=SIZE(UDOTPSF8,3)
      ALLOCATE(oldUDOTPSF8(NTPL,NDIM,P))
      oldUDOTPSF8=UDOTPSF8
      DEALLOCATE(UDOTPSF8)
      ALLOCATE(UDOTPSF8(NTPL,NDIM,P+1))
      IF(R > 0) THEN
        DO I=1,NTPL
          DO J=1,NDIM
            DO K=1,P
              UDOTPSF8(I,J,K)=oldUDOTPSF8(I,J,K)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      DO I=1,NTPL
        DO J=1,NDIM  
          UDOTPSF8(I,J,P+1)=UDOTPSF8TMP(I,J)
        ENDDO
      ENDDO
      DEALLOCATE(oldUDOTPSF8)
!
!
      IF(ALLOCATED(UPSF8TMP))DEALLOCATE(UPSF8TMP)
      IF(ALLOCATED(TMF8TMP))DEALLOCATE(TMF8TMP)
      IF(ALLOCATED(UDOTPSF8TMP))DEALLOCATE(UDOTPSF8TMP)  
!     
!
101   FORMAT(6I6,I8,I6,I8,3I5)
102   FORMAT(4X,7ES19.10)
103   FORMAT(20I5)
!
109   IF( FORT8DST == 1 )THEN
      CALL FLUSH(8)
      ENDIF
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE WRTBV9(IAP,RAP,RLCUR,NDX,UPS,TM,DTM,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Writes additional output on unit 9.
!
      DIMENSION IAP(*),RAP(*)
      DIMENSION DTM(*),UPS(NDX,*),TM(*),RLCUR(*),THU(*)
!
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       IPLT=IAP(11)
       IID=IAP(18)
       NDM=IAP(23)
       IBR=IAP(30)
       NITPS=IAP(31)
       NTOT=IAP(32)
!
       IAB=ABS(IPLT)
       IF(IAB.EQ.0.OR.IAB.GT.NDIM) &
            AMP=DSQRT(RNRMSQ(IAP,NDM,NDX,UPS,DTM,THU))
       IF(IPLT.GT.0.AND.IAB.LE.NDIM)AMP=RMXUPS(IAP,NDX,IAB,UPS)
       IF(IPLT.LT.0.AND.IAB.LE.NDIM)AMP=RMNUPS(IAP,NDX,IAB,UPS)
       RAP(10)=AMP
       IF(IID.GE.2)THEN
         IF(NITPS.EQ.0)CALL WRBAR("=",47)
         IF(NITPS.EQ.0 .OR. IID.GE.3)THEN
            IF(FORT9DST==1)WRITE(9,102)
         ENDIF
         MTOT=MOD(NTOT-1,9999)+1
         IF(FORT9DST==1)WRITE(9,103)IBR,MTOT+1,NITPS,RLCUR(1),AMP
       ENDIF
!
       IF(IID.GE.5)THEN
         IF(FORT9DST==1)WRITE(9,104)
         DO J=1,NTST
           RN=1.d0/NCOL
           DO I=1,NCOL
             T=TM(J)+(I-1)*RN*DTM(J)
             K1=(I-1)*NDIM+1
             K2=I*NDIM
             IF(FORT9DST==1)WRITE(9,105)T,(UPS(K,J),K=K1,K2)
           ENDDO
         ENDDO
         IF(FORT9DST==1)WRITE(9,105)TM(NTST+1),(UPS(I,NTST+1),I=1,NDIM)
       ENDIF
!
 102   FORMAT(/,'  BR    PT  IT         PAR',11X,'L2-NORM')
 103   FORMAT(I4,I6,I4,5X,6ES14.5)
 104   FORMAT(' UPS :')
 105   FORMAT(1X,7ES14.5)
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE PVLSAE(IAP,RAP,U,PAR)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),RAP(*),U(*),PAR(*)
!
      INTERFACE
        SUBROUTINE SETPAE(IAP,RAP)
          INCLUDE 'auto.h'
          IMPLICIT DOUBLE PRECISION (A-H,O-Z)
          TARGET IAP(NIAP),RAP(NRAP)
        END SUBROUTINE SETPAE
      END INTERFACE
!
        CALL SETPAE(IAP,RAP)
        NDM=IAP(23)
        CALL PVLS(NDM,U,PAR)
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE PVLSBV(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),RAP(*),ICP(*),DTM(*),UPS(NDX,*),PAR(*)
      DIMENSION P0(NDIM,*),P1(NDIM,*)
!
      INTERFACE
        SUBROUTINE SETPBV(IAP,RAP,DTM,NDIM,P0,P1)
          INCLUDE 'auto.h'
          IMPLICIT DOUBLE PRECISION (A-H,O-Z)
          TARGET IAP(NIAP),RAP(NRAP),DTM(IAP(5)+1)
          TARGET P0(NDIM,NDIM),P1(NDIM,NDIM)
        END SUBROUTINE SETPBV
      END INTERFACE
!
        CALL SETPBV(IAP,RAP,DTM,NDIM,P0,P1)
        NDM=IAP(23)
        CALL PVLS(NDM,UPS,PAR)
!
      RETURN
      END
!
!     ---------- -----
      SUBROUTINE EVECS(NDIM,P0,P1)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION P0(NDIM,*),P1(NDIM,*)
!
! Local
      ALLOCATABLE Q0(:,:), Q1(:,:), P(:,:), Z(:,:), WR(:), WI(:)
      ALLOCATABLE IR(:), IC(:), IV1(:), FV1(:)
!
      ALLOCATE(Q0(NDIM,NDIM), Q1(NDIM,NDIM), P(NDIM,NDIM))
      ALLOCATE(Z(NDIM,NDIM), WR(NDIM), WI(NDIM))
      ALLOCATE(IR(NDIM), IC(NDIM))
      ALLOCATE(IV1(NDIM), FV1(NDIM))
!
        DO I=1,NDIM
          DO J=1,NDIM
             Q0(I,J)=-P0(I,J)
             Q1(I,J)= P1(I,J)
          ENDDO
        ENDDO
!
        CALL GE(0,NDIM,NDIM,Q1,NDIM,NDIM,P,NDIM,Q0,IR,IC,DET)
        CALL RG(NDIM,NDIM,P,WR,WI,1,Z,IV1,FV1,IERR)
!
        IF(FORT9DST==1)WRITE(9,100)
        IF(FORT9DST==1)WRITE(9,101)
        DO I=1,NDIM
          IF(FORT9DST==1)WRITE(9,102)WR(I),WI(I),(Z(I,J),J=1,NDIM)
        ENDDO
        IF(FORT9DST==1)WRITE(9,101)
100     FORMAT(" Multipliers + eigenvectors obtained from - P0^-1 P1 :")
        IF(FORT9DST==1)WRITE(9,112)WR(1)*WR(2)
112     format(" Product = ",ES16.7)       
101     FORMAT(" ")
102     FORMAT(2ES14.5," | ",8ES14.5)
        !
      DEALLOCATE(Q0,Q1,P,Z,WR,WI,IR,IC,IV1,FV1)
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE SETPAE(IAP,RAP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      POINTER DTV(:),RAV(:),IAV(:),P0V(:,:),P1V(:,:)
      COMMON /BLPV/ DTV,RAV,IAV,P0V,P1V
      TARGET IAP(NIAP),RAP(NRAP)
!
      IAV=>IAP
      RAV=>RAP
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE SETPBV(IAP,RAP,DTM,NDIM,P0,P1)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      POINTER DTV(:),RAV(:),IAV(:),P0V(:,:),P1V(:,:)
      COMMON /BLPV/ DTV,RAV,IAV,P0V,P1V
      TARGET IAP(NIAP),RAP(NRAP),DTM(IAP(5)+1)
      TARGET P0(NDIM,NDIM),P1(NDIM,NDIM)
!
      IAV=>IAP
      RAV=>RAP
      DTV=>DTM
      P0V=>P0
      P1V=>P1
!
      RETURN
      END
!
!     ------ --------- -------- ----
      DOUBLE PRECISION FUNCTION GETP(CODE,IC,UPS)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      POINTER DTV(:),RAV(:),IAV(:),P0V(:,:),P1V(:,:)
      COMMON /BLPV/ DTV,RAV,IAV,P0V,P1V
      DIMENSION UPS(IAV(1)*IAV(6),*)
      CHARACTER*3 CODE
!
        IPS=IAV(2)
        NX=IAV(1)*IAV(6)
        GETP=0
!
        IF( ABS(IPS).LE.1 .OR. IPS.EQ.5)THEN
          IF(CODE.EQ.'NRM'.OR.CODE.EQ.'nrm')THEN
            GETP=ABS(UPS(IC,1))    
          ELSEIF(CODE.EQ.'INT'.OR.CODE.EQ.'int')THEN
            GETP=UPS(IC,1)
          ELSEIF(CODE.EQ.'MAX'.OR.CODE.EQ.'max')THEN
            GETP=UPS(IC,1)
          ELSEIF(CODE.EQ.'MIN'.OR.CODE.EQ.'min')THEN
            GETP=UPS(IC,1)
          ELSEIF(CODE.EQ.'BV0'.OR.CODE.EQ.'bv0')THEN
            GETP=UPS(IC,1)
          ELSEIF(CODE.EQ.'BV1'.OR.CODE.EQ.'bv1')THEN
            GETP=UPS(IC,1)
          ELSEIF(CODE.EQ.'STP'.OR.CODE.EQ.'stp')THEN
            GETP=RAV(5)
          ELSEIF(CODE.EQ.'FLD'.OR.CODE.EQ.'fld')THEN
            GETP=RAV(16)
          ELSEIF(CODE.EQ.'HBF'.OR.CODE.EQ.'hbf')THEN
            GETP=RAV(17)
          ELSEIF(CODE.EQ.'BIF'.OR.CODE.EQ.'bif')THEN
            GETP=RAV(14)
          ELSEIF(CODE.EQ.'SPB'.OR.CODE.EQ.'spb')THEN
            GETP=0.
          ELSEIF(CODE.EQ.'STA'.OR.CODE.EQ.'sta')THEN
            GETP=IAV(33)
          ENDIF
        ELSE
          IF(CODE.EQ.'NRM'.OR.CODE.EQ.'nrm')THEN
            GETP=RNRM2(IAV,NX,IC,UPS,DTV)    
          ELSEIF(CODE.EQ.'INT'.OR.CODE.EQ.'int')THEN
            GETP=RINTG(IAV,NX,IC,UPS,DTV)
          ELSEIF(CODE.EQ.'MAX'.OR.CODE.EQ.'max')THEN
            GETP=RMXUPS(IAV,NX,IC,UPS)
          ELSEIF(CODE.EQ.'MIN'.OR.CODE.EQ.'min')THEN
            GETP=RMNUPS(IAV,NX,IC,UPS)
          ELSEIF(CODE.EQ.'BV0'.OR.CODE.EQ.'bv0')THEN
            GETP=UPS(IC,1)
          ELSEIF(CODE.EQ.'BV1'.OR.CODE.EQ.'bv1')THEN
            GETP=UPS(IC,IAV(5)+1)
          ELSEIF(CODE.EQ.'STP'.OR.CODE.EQ.'stp')THEN
            GETP=RAV(5)
          ELSEIF(CODE.EQ.'FLD'.OR.CODE.EQ.'fld')THEN
            GETP=RAV(16)
          ELSEIF(CODE.EQ.'HBF'.OR.CODE.EQ.'hbf')THEN
            GETP=0.
          ELSEIF(CODE.EQ.'BIF'.OR.CODE.EQ.'bif')THEN
            GETP=RAV(18)
          ELSEIF(CODE.EQ.'SPB'.OR.CODE.EQ.'spb')THEN
            GETP=RAV(19)
          ELSEIF(CODE.EQ.'STA'.OR.CODE.EQ.'sta')THEN
            GETP=IAV(33)
          ENDIF
        ENDIF
!
      RETURN
      END
!
!     ---------- -------
      SUBROUTINE GETMDMX(NDIM1,P0,P1,NMM)
      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(OUT) :: P0(NDIM1,NDIM1),P1(NDIM1,NDIM1)
      INTEGER, INTENT(IN) :: NDIM1
      LOGICAL, INTENT(OUT) :: NMM

      DOUBLE PRECISION, POINTER :: DTV(:),RAV(:),P0V(:,:),P1V(:,:)
      INTEGER, POINTER :: IAV(:)
      COMMON /BLPV/ DTV,RAV,IAV,P0V,P1V

      INTEGER NDIM,IPS,ISP,NTOT

        NDIM=IAV(1)
        IPS=IAV(2)
        ISP=IAV(9)
        NTOT=IAV(32)
        NMM=.FALSE.
        IF(NDIM==NDIM1.AND.NTOT>0.AND.ABS(ISP)>0.AND. &
             (IPS==2.OR.IPS==7.OR.IPS==12))THEN
          P0=P0V
          P1=P1V
          NMM=.TRUE.
        ENDIF

      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!          System Dependent Subroutines for Timing AUTO
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ------
      SUBROUTINE AUTIM0(T)
!
!$    USE OMP_LIB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL etime
      REAL timaray(2)
!
! Set initial time for measuring CPU time used.
!
      T=etime(timaray)
!$    T=omp_get_wtime()
!
      RETURN
      END
!
!     ---------- ------
      SUBROUTINE AUTIM1(T)
!
!$    USE OMP_LIB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL etime
      REAL timaray(2)
!
! Set final time for measuring CPU time used.
!
      T=etime(timaray)
!$    T=omp_get_wtime()
!
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
