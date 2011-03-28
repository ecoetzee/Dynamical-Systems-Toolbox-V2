!========================================================================
! AUTLIB2.F - Subroutine for AUTO
!
!   This subroutine is a subroutine of the main program for AUTO07. 
!   This version has been modified to run with Matlab. Use Examdiff 
!   to obtain exact differences from original source file.
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
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!           Setting up of the Jacobian and right hand side
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      MODULE SOLVEBV

      PRIVATE

      PUBLIC ::SOLVBV,PARTITION

      CONTAINS
!
!     ---------- ------
      SUBROUTINE SOLVBV(IFST,IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RDS,
     * NLLV,RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,DTM,
     * FA,FC,P0,P1,THL,THU)
!
!$    USE OMP_LIB
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!
! Sets up and solves the linear equations for one Newton/Chord iteration
!
      EXTERNAL FUNI,BCNI,ICNI
      DIMENSION IAP(*),RAP(*),FC(*),PAR(*),ICP(*),RLOLD(*),RLCUR(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UOLDPS(NDX,*),UPOLDP(NDX,*)
      DIMENSION FA(IAP(1)*IAP(6),*),DTM(*),RLDOT(*),DUPS(NDX,*)
      DOUBLE PRECISION P0(*),P1(*),THL(*),THU(*)
!
! Local
      ALLOCATABLE A(:,:,:),B(:,:,:),C(:,:,:),D(:,:),A1(:,:,:),A2(:,:,:)
      ALLOCATABLE S1(:,:,:),S2(:,:,:),BB(:,:,:),CC(:,:,:)
      ALLOCATABLE CCLO(:,:,:),CCBC(:,:,:)
      ALLOCATABLE DDBC(:,:),DD(:,:,:),FCFC(:,:),FAA(:,:),SOL(:,:)
      ALLOCATABLE ICF(:,:),IRF(:,:),IPR(:,:),IPC(:,:),NP(:)
      SAVE A,B,C,D,A1,A2,S1,S2,BB,CC,CCBC,DDBC,ICF,IRF,IPR,IPC
!
! Most of the required memory is allocated below
!
! This is an interesting section of code.  The main point
! is that setubv and conpar only get called when ifst
! is 1.  This is a optimization since you can solve
! the system using the previously factored jacobian.
! One thing to watch out for is that two seperate calls
! of solvbv_ talk to each other through these arrays,
! so it is only safe to get rid of them when ifst is
! 1 (since their entries will then be recreated in conpar
!  and setubv).
!
!
      IAM=IAP(38)
      KWT=IAP(39)
!
      NDIM=IAP(1)
      NTST=IAP(5)
      NCOL=IAP(6)
      NBC=IAP(12)
      NINT=IAP(13)
      IID=IAP(18)
      NFPR=IAP(29)
      NRC=NINT+1
      NFC=NRC+NBC
      NROW=NDIM*NCOL
      NCLM=NROW+NDIM
!
      ALLOCATE(NP(KWT))
      IF(KWT.GT.NTST)THEN
        PRINT*,'NTST is less than the number of nodes'
        CALL AUTOSTOPWITHERROR('NTST is less than the number of nodes')
      ELSE
        CALL PARTITION(NTST,KWT,NP)
      ENDIF
!
!     NTST is the global one, NA is the local one.
!     The value of NTST may be different in different nodes.
      NA=NP(IAM+1)
!
      NTSTNA=NA
      IF(IAM.EQ.0)NTSTNA=NTST
      MNT = 1
!$    MNT = OMP_GET_MAX_THREADS()
      IF(MNT.GT.NA)THEN
         MNT=NA
!$       CALL OMP_SET_NUM_THREADS(NA)
      ENDIF
      IF(IFST.EQ.1)THEN
         IF(ALLOCATED(A))THEN
!           Free floating point arrays
            DEALLOCATE(A,B,C,D,A1,A2,S1,S2,BB,CC,CCBC,DDBC)
!           Free integer arrays
            DEALLOCATE(ICF,IRF,IPR,IPC)
         ENDIF
!
         ALLOCATE(A(NCLM,NROW,NA+1),B(NFPR,NROW,NA+1))
         ALLOCATE(C(NCLM,NRC,NA+1),D(NFPR,NRC))
         ALLOCATE(A1(NDIM,NDIM,NTSTNA+1),A2(NDIM,NDIM,NTSTNA+1))
         ALLOCATE(S1(NDIM,NDIM,NTSTNA+1),S2(NDIM,NDIM,NTSTNA+1))
         ALLOCATE(BB(NFPR,NDIM,NTSTNA+1),CC(NDIM,NRC,NTSTNA+1))
         ALLOCATE(CCBC(NDIM,NBC,2),DDBC(NFPR,NBC))
!
         ALLOCATE(ICF(NCLM,NA),IRF(NROW,NA),IPR(NDIM,NTSTNA-1))
         ALLOCATE(IPC(NDIM,NTSTNA-1))
      ENDIF
      IF(IAM.EQ.0)THEN
!
         DO I=1,NFPR
            PAR(ICP(I))=RLCUR(I)
         ENDDO
!
!     ** Time evolution computations (parabolic systems)
         IPS=IAP(2)
         IF(IPS.EQ.14 .OR. IPS.EQ.16)RAP(15)=RLOLD(1)
         CALL SUBVBC(NDIM,NTST,NBC,NFPR,BCNI,NDX,
     +        IAP,RAP,PAR,ICP,CCBC,DDBC,FC,UPS,IFST)
         CALL SUBVPSA(NDIM,NTST,NFPR,NDX,IAP,RDS,D(1,NRC),FC(NFC),
     +        RLCUR,RLOLD,RLDOT,UPS,UOLDPS,UDOTPS,DUPS,DTM,THL,THU,IFST)
         IF(KWT.GT.1)THEN
            CALL MPISBV(IAP,RAP,PAR,ICP,RLDOT,NDX,UPS,UOLDPS,UDOTPS,
     +           UPOLDP,DTM,THU,IFST,NLLV)
         ENDIF
      ELSE
!     The matrix D and FC are set to zero for all nodes except the first.
!     zero pseudo-arclength part of matrices, rest is done in setubv()
         CALL SETFCDD(IFST,D(1,NRC),FC(NFC),NFPR,1)
      ENDIF
!
      IF(MNT.GT.1)THEN
!        CCLO avoids write overlap between threads
         ALLOCATE(DD(NFPR,NRC,MNT-1),FCFC(NRC,MNT-1),
     +        CCLO(NDIM,NRC,MNT-1))
      ENDIF
      ALLOCATE(FAA(NDIM,NTSTNA+1),SOL(NDIM,NTSTNA+1))
!
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(I,IT,NT)
!
      IT = 0
!$    IT = OMP_GET_THREAD_NUM()
      NT = 1
!$    NT = OMP_GET_NUM_THREADS()
      CALL SETUBV(NDIM,NA,NCOL,NINT,NFPR,NRC,NROW,NCLM,
     +   FUNI,ICNI,NDX,IAP,RAP,PAR,ICP,A,B,C,D,DD,FA,
     +   FC(NBC+1),FCFC,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,IFST,IT,NT)
!
      I = IT*NA/NT+1
      CALL BRBD(A(1,1,I),B(1,1,I),C(1,1,I),D,DD,FA(1,I),FAA,FC,
     +  FCFC,P0,P1,IFST,IID,NLLV,DET,NDIM,NTST,NA,NBC,NROW,NCLM,
     +  NFPR,NFC,A1,A2,BB,CC,CCLO,CCBC,DDBC,
     +  SOL,S1,S2,IPR,IPC,IRF(1,I),ICF(1,I),IAM,KWT,IT,NT)
!
!$OMP END PARALLEL
!
      IF(MNT.GT.1)DEALLOCATE(DD,FCFC,CCLO)
      DEALLOCATE(FAA,SOL)
!
      IF(KWT.GT.1)THEN
!        Global concatenation of the solution from each node.
        CALL MPIGAT(FA,NDX,NTST)
      ENDIF
!
      IF(IAM.EQ.0)RAP(14)=DET
!
      DEALLOCATE(NP)
      RETURN
      END SUBROUTINE SOLVBV
!
!     ---------- -------
      SUBROUTINE SETFCDD(IFST,DD,FC,NCB,NRC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION FC(*),DD(NCB,*)
!
      DO I=1,NRC
        IF(IFST.EQ.1)THEN
          DO J=1,NCB
            DD(J,I)=0.0D0
          ENDDO
        ENDIF
        FC(I)=0.0D0
      ENDDO
!
      RETURN
      END SUBROUTINE SETFCDD
!
!     ---------- ---------
      SUBROUTINE PARTITION(N,KWT,M)
!
!     Linear distribution of NTST over all nodes
      IMPLICIT NONE
      INTEGER N,KWT,M(KWT)
      INTEGER I
!     
        DO I=1,KWT
          M(I) = I*N/KWT - (I-1)*N/KWT
        ENDDO
!     
      RETURN
      END SUBROUTINE PARTITION
!
!     ---------- ---------
      SUBROUTINE SUBVBC(NDIM,NTST,NBC,NCB,BCNI,NDX,
     + IAP,RAP,PAR,ICP,CCBC,DDBC,FC,UPS,IFST)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST      
      IMPLICIT NONE
!
!     This subroutine handles a non-parallel part of SETUBV, that is,
!     * the boundary conditions (not much to parallelize here and
!       HomCont relies on non-parallel execution): the arrays CCBC,
!       DDBC, and parts of FC.
!
      EXTERNAL BCNI
!
      INTEGER NDIM,NTST,NBC,NCB,NDX,IAP(*),ICP(*),IFST
      DOUBLE PRECISION CCBC(NDIM,NBC,*),DDBC(NCB,*)
      DOUBLE PRECISION RAP(*),UPS(NDX,*),FC(*),PAR(*)
!
! Local
      DOUBLE PRECISION, ALLOCATABLE :: UBC0(:),UBC1(:),FBC(:),DBC(:,:)
      INTEGER I,K
!
      ALLOCATE(UBC0(NDIM),UBC1(NDIM),FBC(NBC),DBC(NBC,2*NDIM+NPARX))
!
!     Boundary conditions :
!     
       IF(NBC.GT.0)THEN
         DO I=1,NDIM
            UBC0(I)=UPS(I,1)
            UBC1(I)=UPS(I,NTST+1)
         ENDDO
         CALL BCNI(IAP,RAP,NDIM,PAR,ICP,NBC,UBC0,UBC1,FBC,IFST*2,DBC)
         DO I=1,NBC
            FC(I)=-FBC(I)
            IF(IFST.EQ.1)THEN
               DO K=1,NDIM
                  CCBC(K,I,1)=DBC(I,K)
                  CCBC(K,I,2)=DBC(I,NDIM+K)
               ENDDO
               DO K=1,NCB
                  DDBC(K,I)=DBC(I,2*NDIM+ICP(K))
               ENDDO
            ENDIF
         ENDDO    
       ENDIF
       DEALLOCATE(UBC0,UBC1,FBC,DBC)
       RETURN
       END SUBROUTINE SUBVBC
!
!     ---------- -------
      SUBROUTINE SUBVPSA(NDIM,NTST,NCB,NDX,IAP,RDS,DDPA,FCPA,
     + RLCUR,RLOLD,RLDOT,UPS,UOLDPS,UDOTPS,DUPS,DTM,THL,THU,IFST)
!
      IMPLICIT NONE
!
!     This subroutine handles a non-parallel part of SETUBV, that is,
!     * creating the pseudo-arclength parts of FC and D: (the bottom
!       element FCPA and row DDPA)
!
      DOUBLE PRECISION RINPR
!
      INTEGER NDIM,NTST,NCB,NDX,IAP(*),IFST
      DOUBLE PRECISION RDS,DDPA(*),FCPA,DTM(*),UPS(NDX,*),DUPS(NDX,*)
      DOUBLE PRECISION UOLDPS(NDX,*),UDOTPS(NDX,*)
      DOUBLE PRECISION RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
!
! Local
      INTEGER I,J
      DOUBLE PRECISION RLSUM
!
!       Save difference :
!
       DO J=1,NTST+1
         DO I=1,NDX
            DUPS(I,J)=UPS(I,J)-UOLDPS(I,J)
          ENDDO
       ENDDO
!
!     Pseudo-arclength equation :
!
       RLSUM=0.d0
       DO I=1,NCB
          IF(IFST.EQ.1)THEN
             DDPA(I)=THL(I)*RLDOT(I)
          ENDIF
          RLSUM=RLSUM+THL(I)*(RLCUR(I)-RLOLD(I))*RLDOT(I)
       ENDDO
!
       FCPA=RDS-RINPR(IAP,NDIM,NDX,UDOTPS,DUPS,DTM,THU)-RLSUM
!
       RETURN
       END SUBROUTINE SUBVPSA
!
!     ---------- ------
      SUBROUTINE SETUBV(NDIM,NA,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,
     + ICNI,NDX,IAP,RAP,PAR,ICP,AA,BB,CC,DD,DDD,FA,FC,FCFC,
     + UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,IFST,IAM,NT)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT NONE
!
      INTEGER NDIM,NA,NCOL,NINT,NCB,NRC,NRA,NCA,NDX,IFST,IAM,NT
      INTEGER IAP(*),ICP(*)
      DOUBLE PRECISION AA(NCA,NRA,*),BB(NCB,NRA,*),CC(NCA,NRC,*)
      DOUBLE PRECISION DD(NCB,*),RAP(*),UPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION UDOTPS(NDX,*),UPOLDP(NDX,*),FA(NRA,*),FC(*)
      DOUBLE PRECISION DTM(*),PAR(*),THU(*)
      DOUBLE PRECISION DDD(NCB,NRC,*),FCFC(NRC,*)
!
      EXTERNAL FUNI,ICNI
!
! Local
      DOUBLE PRECISION WI(NCOL+1),WP(NCOL+1,NCOL),WT(NCOL+1,NCOL)
      INTEGER I,N
!
!
      CALL WINT(NCOL+1,WI)
      CALL GENWTS(NCOL,NCOL+1,WT,WP)
!
      I = IAM*NA/NT+1
      N = (IAM+1)*NA/NT+1-I
      IF(IFST.EQ.1)THEN
         IF(IAM.EQ.0)THEN
            CALL SUBVPA(NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,ICNI,NDX,
     +           IAP,RAP,PAR,ICP,AA,BB,CC,DD,FA,FC,
     +           UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,WI,WP,WT)
         ELSE
            CALL SETFCDD(IFST,DDD(1,NRC,IAM),FCFC(NRC,IAM),NCB,1)
            CALL SUBVPA(NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,ICNI,NDX,
     +           IAP,RAP,PAR,ICP,AA(1,1,I),BB(1,1,I),CC(1,1,I),
     +           DDD(1,1,IAM),FA(1,I),FCFC(1,IAM),UPS(1,I),UOLDPS(1,I),
     +           UDOTPS(1,I),UPOLDP(1,I),DTM(I),THU,WI,WP,WT)
         ENDIF
      ELSE
         IF (IAM.EQ.0)THEN
            CALL SETRHS(NDIM,N,NCOL,NINT,NRA,FUNI,ICNI,NDX,IAP,
     +           RAP,PAR,ICP,FA,FC,UPS,UOLDPS,
     +           UDOTPS,UPOLDP,DTM,WI,WP,WT)
         ELSE
            CALL SETFCDD(IFST,DDD(1,NRC,IAM),FCFC(NRC,IAM),NCB,1)
            CALL SETRHS(NDIM,N,NCOL,NINT,NRA,FUNI,ICNI,NDX,IAP,
     +           RAP,PAR,ICP,FA(1,I),FCFC(1,IAM),UPS(1,I),UOLDPS(1,I),
     +           UDOTPS(1,I),UPOLDP(1,I),DTM(I),WI,WP,WT)
         ENDIF
      ENDIF
!
      CONTAINS

!     ---------- ---------
      SUBROUTINE SUBVPA(NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,
     + ICNI,NDX,IAP,RAP,PAR,ICP,AA,BB,CC,DD,FA,FC,
     + UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,WI,WP,WT)
!
!     This is the per-CPU parallelized part of SETUBV
!
      EXTERNAL FUNI,ICNI
!
      INTEGER NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,IAP(*),ICP(*),NDX
      DOUBLE PRECISION AA(NCA,NRA,*),BB(NCB,NRA,*),CC(NCA,NRC,*)
      DOUBLE PRECISION DD(NCB,*),RAP(*),UPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION UDOTPS(NDX,*),UPOLDP(NDX,*),FA(NRA,*),FC(*)
      DOUBLE PRECISION DTM(*),PAR(*),THU(*)
      DOUBLE PRECISION WI(*),WP(NCOL+1,*),WT(NCOL+1,*)
!
! Local
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: DFDU,DFDP,UOLD,U,
     +  F,FICD,DICD,UIC,UIO,UID,UIP
      INTEGER I,J,K,IC,IC1,NCP1,I1,J1,JP1,K1
!
      ALLOCATE(DFDU(NDIM*NDIM),DFDP(NDIM*NPARX),UOLD(NDIM),U(NDIM))
      ALLOCATE(F(NDIM),FICD(NINT))
      ALLOCATE(DICD(NINT*(NDIM+NPARX)),UIC(NDIM),UIO(NDIM))
      ALLOCATE(UID(NDIM),UIP(NDIM))
!
! Generate AA , BB and FA :
!
       DO J=1,N
          DO IC=1,NCOL
             IC1=(IC-1)*NDIM+1
             CALL SBVFUN(NDIM,NCOL,NCB,NCA,FUNI,NDX,IAP,RAP,PAR,ICP,
     +            AA(1,IC1,J),BB(1,IC1,J),FA(IC1,J),UPS(1,J),
     +            UOLDPS(1,J),DTM(J),WP(1,IC),WT(1,IC),DFDU,DFDP,
     +            U,UOLD,F)
          ENDDO
       ENDDO
!     
!     Generate CC, DD and FC :
!
! Initialize to zero.
!
       DO I=1,NINT
         FC(I)=0.d0
         DO K=1,NCB
           DD(K,I)=0.d0
         ENDDO
       ENDDO
!
       NCP1=NCOL+1
       DO J=1,N
         JP1=J+1
         DO K=1,NCP1
            J1=J
            K1=(K-1)*NDIM+1
            I1=K1
            IF(K.EQ.NCP1)THEN
               J1=JP1
               I1=1
            ENDIF
!     
!     Integral constraints+pseudo-arclength equation :
!     
            CALL SBVICN(NDIM,NINT,NCB,NCA,ICNI,IAP,RAP,PAR,ICP,
     +           CC(K1,1,J),DD,FC,UPS(I1,J1),UOLDPS(I1,J1),
     +           UDOTPS(I1,J1),UPOLDP(I1,J1),DTM(J),THU,WI(K),FICD,DICD,
     +           UIC,UIO,UID,UIP)
          ENDDO
       ENDDO
!     
       DEALLOCATE(DFDU,DFDP,UOLD,U,F,FICD,DICD)
       DEALLOCATE(UIC,UIO,UID,UIP)
       RETURN
      END SUBROUTINE SUBVPA
!
!     ---------- ---------
      SUBROUTINE SBVFUN(NDIM,NCOL,NCB,NCA,FUNI,NDX,IAP,RAP,PAR,ICP,
     + AA,BB,FA,UPS,UOLDPS,DTM,WP,WT,DFDU,DFDP,U,UOLD,F)
!
!     Does one call to FUNI and stores the result in AA, BB, and FA.
!
      EXTERNAL FUNI
!
      INTEGER, INTENT(IN) :: NDIM,NCOL,NCB,NCA,NDX,IAP(*),ICP(*)
      DOUBLE PRECISION, INTENT(IN) :: DTM,PAR(*),WT(*),WP(*)
      DOUBLE PRECISION, INTENT(IN) :: UPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION, INTENT(OUT) :: AA(NCA,*),BB(NCB,*),FA(*),U(*)
      DOUBLE PRECISION, INTENT(OUT) :: UOLD(*),DFDU(NDIM,*),DFDP(NDIM,*)
      DOUBLE PRECISION, INTENT(OUT) :: F(*),RAP(*)
!
! Local
      DOUBLE PRECISION PRM(NPARX),WPLOC(NCOL+1),WTTMP,TMP
      INTEGER I,IB,IB1,II,JJ,K,L,L1,NCP1
!
      NCP1=NCOL+1
      DO K=1,NDIM
         U(K)=   WT(NCP1)*   UPS(K,2)
         UOLD(K)=WT(NCP1)*UOLDPS(K,2)
         DO L=1,NCOL
            L1=(L-1)*NDIM+K
            U(K)=U(K)        +WT(L)*   UPS(L1,1)
            UOLD(K) =UOLD(K) +WT(L)*UOLDPS(L1,1)
         ENDDO
      ENDDO
      DO I=1,NPARX
         PRM(I)=PAR(I)
      ENDDO
      CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PRM,2,F,DFDU,DFDP)
!     transpose DFDU for optimal access
      DO II=1,NDIM
         DO JJ=1,II-1
            TMP=DFDU(II,JJ)
            DFDU(II,JJ)=DFDU(JJ,II)
            DFDU(JJ,II)=TMP
         ENDDO
      ENDDO
      DO IB=1,NCP1
         WPLOC(IB)=WP(IB)/DTM
      ENDDO
      DO I=1,NDIM
         DO IB=1,NCP1
            WTTMP=-WT(IB)
            IB1=(IB-1)*NDIM
            DO K=1,NDIM
               AA(IB1+K,I)=WTTMP*DFDU(K,I)
            ENDDO
            AA(IB1+I,I)=AA(IB1+I,I)+WPLOC(IB)
         ENDDO
         DO K=1,NCB
            BB(K,I)=-DFDP(I,ICP(K))
         ENDDO
         FA(I)=F(I)-WPLOC(NCP1)*UPS(I,2)
         DO K=1,NCOL
            FA(I)=FA(I)-WPLOC(K)*UPS((K-1)*NDIM+I,1)
         ENDDO
      ENDDO
      END SUBROUTINE SBVFUN
!
!     ---------- ------
      SUBROUTINE SBVICN(NDIM,NINT,NCB,NCA,ICNI,IAP,RAP,PAR,ICP,CC,DD,FC,
     + UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,WI,FICD,DICD,UIC,UIO,UID,UIP)
!
      IMPLICIT NONE
!
!     Does one call to ICNI (integral constraints) and stores the
!     result in CC, DD and FC; and stores the pseudo-arclength
!     result too.
!
      EXTERNAL ICNI
      INTEGER, INTENT(IN) :: NDIM,NINT,NCB,NCA,IAP(*),ICP(*)
      DOUBLE PRECISION, INTENT(IN) :: RAP(*),PAR(*),UPS(*),UDOTPS(*)
      DOUBLE PRECISION, INTENT(IN) :: UOLDPS(*),UPOLDP(*),DTM,WI,THU(*)
      DOUBLE PRECISION, INTENT(OUT) :: CC(NCA,*),FICD(*),DICD(NINT,*)
      DOUBLE PRECISION, INTENT(OUT) :: UIC(*),UIO(*),UID(*),UIP(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DD(NCB,*),FC(*)
!
! Local
      INTEGER I,M
      DOUBLE PRECISION PRM(NPARX)
!
      IF(NINT.GT.0)THEN
         DO I=1,NDIM
            UIC(I)=UPS(I)
            UIO(I)=UOLDPS(I)
            UID(I)=UDOTPS(I)
            UIP(I)=UPOLDP(I)
         ENDDO
         DO I=1,NPARX
            PRM(I)=PAR(I)
         ENDDO
         CALL ICNI(IAP,RAP,NDIM,PRM,ICP,NINT,UIC,UIO,UID,UIP,
     +        FICD,2,DICD)
         DO M=1,NINT
            DO I=1,NDIM
               CC(I,M)=DTM*WI*DICD(M,I)
            ENDDO
            DO I=1,NCB
               DD(I,M)=DD(I,M)+DTM*WI*DICD(M,NDIM+ICP(I))
            ENDDO
            FC(M)=FC(M)-DTM*WI*FICD(M)
         ENDDO
      ENDIF
!     
!     Pseudo-arclength equation :
!     
      DO I=1,NDIM
         CC(I,NINT+1)=DTM*THU(I)*WI*UDOTPS(I)
      ENDDO
      END SUBROUTINE SBVICN
!
!     ---------- ------
      SUBROUTINE SETRHS(NDIM,N,NCOL,NINT,NRA,FUNI,ICNI,
     + NDX,IAP,RAP,PAR,ICP,FA,FC,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,WI,WP,WT)
!
      EXTERNAL FUNI,ICNI
!
      INTEGER NDIM,N,NCOL,NINT,NRA,IAP(*),ICP(*),NDX
      DOUBLE PRECISION RAP(*),UPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION UDOTPS(NDX,*),UPOLDP(NDX,*),FA(NRA,*),FC(*)
      DOUBLE PRECISION DTM(*),PAR(*),WI(*),WP(NCOL+1,*),WT(NCOL+1,*)
!
! Local
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: DFDU,DFDP,UOLD,U,
     +  F,FICD,DICD,UIC,UIO,UID,UIP
      INTEGER I,J,K,L,M,IB,IC,IC1,NCP1,I1,J1,JP1,K1,L1
      DOUBLE PRECISION WPLOC(NCOL+1,NCOL),PRM(NPARX),DDT
!
      ALLOCATE(DFDU(NDIM*NDIM),DFDP(NDIM*NPARX),UOLD(NDIM),U(NDIM))
      ALLOCATE(F(NDIM),FICD(NINT),DICD(NINT*(NDIM+NPARX)))
      ALLOCATE(UIC(NDIM),UIO(NDIM),UID(NDIM),UIP(NDIM))
!
! Initialize to zero.
       DO I=1,NINT
         FC(I)=0.d0
       ENDDO
       NCP1=NCOL+1
!
! Generate FA :
!
       DO J=1,N
          JP1=J+1
          DDT=1.d0/DTM(J)
          DO IC=1,NCOL
             DO IB=1,NCP1
                WPLOC(IB,IC)=DDT*WP(IB,IC)
             ENDDO
          ENDDO
          DO IC=1,NCOL
             DO K=1,NDIM
                U(K)   =WT(NCP1,IC)*UPS(K,JP1)
                UOLD(K)=WT(NCP1,IC)*UOLDPS(K,JP1)
                DO L=1,NCOL
                   L1=(L-1)*NDIM+K
                   U(K)   =U(K)   +WT(L,IC)*UPS(L1,J)
                   UOLD(K)=UOLD(K)+WT(L,IC)*UOLDPS(L1,J)
                ENDDO
             ENDDO
             DO I=1,NPARX
                PRM(I)=PAR(I)
             ENDDO
             CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PRM,0,F,DFDU,DFDP)
             IC1=(IC-1)*NDIM
             DO I=1,NDIM
                FA(IC1+I,J)=F(I)-WPLOC(NCP1,IC)*UPS(I,JP1)
                DO K=1,NCOL
                   K1=(K-1)*NDIM+I
                   FA(IC1+I,J)=FA(IC1+I,J)-WPLOC(K,IC)*UPS(K1,J)
                ENDDO
             ENDDO
          ENDDO
       ENDDO
!     
!     Generate FC :
!
!     Integral constraints :     
       IF(NINT.GT.0)THEN
         DO J=1,N
            JP1=J+1
            DO K=1,NCP1
               DO I=1,NDIM
                  I1=(K-1)*NDIM+I
                  J1=J
                  IF(K.EQ.NCP1)I1=I
                  IF(K.EQ.NCP1)J1=JP1
                  UIC(I)=UPS(I1,J1)
                  UIO(I)=UOLDPS(I1,J1)
                  UID(I)=UDOTPS(I1,J1)
                  UIP(I)=UPOLDP(I1,J1)
               ENDDO
               DO I=1,NPARX
                  PRM(I)=PAR(I)
               ENDDO
               CALL ICNI(IAP,RAP,NDIM,PRM,ICP,NINT,UIC,UIO,UID,UIP,
     *              FICD,0,DICD)
               DO M=1,NINT
                  FC(M)=FC(M)-DTM(J)*WI(K)*FICD(M)
               ENDDO
            ENDDO
         ENDDO
       ENDIF
!     
       DEALLOCATE(DFDU,DFDP,UOLD,U,F,FICD,DICD,UIC,UIO,UID,UIP)
       RETURN
       END SUBROUTINE SETRHS
!
      END SUBROUTINE SETUBV
!
!     ---------- ----
      SUBROUTINE BRBD(A,B,C,D,DD,FA,FAA,FC,FCFC,P0,P1,IFST,
     +  IDB,NLLV,DET,NOV,NTST,NA,NBC,NRA,NCA,
     +  NCB,NFC,A1,A2,BB,CC,CCLO,CCBC,DDBC,
     +  SOL,S1,S2,IPR,IPC,IRF,ICF,IAM,KWT,IT,NT)
!
      IMPLICIT NONE
!
! Arguments
      INTEGER   IFST,IDB,NLLV,NOV,NTST,NA,NBC,NRA
      INTEGER   NCA,NCB,NFC,IAM,KWT,IT,NT
      DOUBLE PRECISION, INTENT(OUT) :: DET
      DOUBLE PRECISION A(*),B(*),C(NCA,NFC-NBC,*),D(NCB,*)
      DOUBLE PRECISION DD(NCB,NFC-NBC,*)
      DOUBLE PRECISION FA(*),FAA(NOV,*),FC(*),FCFC(NFC-NBC,*),P0(*)
      DOUBLE PRECISION P1(*),A1(NOV,NOV,*),A2(NOV,NOV,*)
      DOUBLE PRECISION BB(NCB,NOV,*),CC(NOV,NFC-NBC,*)
      DOUBLE PRECISION CCLO(NOV,NFC-NBC,*)
      DOUBLE PRECISION CCBC(*),DDBC(*),SOL(NOV,*),S1(*),S2(*)
      INTEGER   IPR(*),IPC(*),IRF(*),ICF(*)
!
! Local
      DOUBLE PRECISION FCC,E,X
      INTEGER I,II,N,IR,IC,NRC
      ALLOCATABLE FCC(:),E(:,:),IR(:),IC(:),X(:)
!
      NRC=NFC-NBC
!
      IF(IDB.GT.4.and.IAM.EQ.0.and.IT.EQ.0)
     +     CALL PRINT1(NA,NRA,NCA,NCB,NFC,NBC,A,B,C,CCBC,D,DD,DDBC,
     +     FA,FC,FCFC,NT)
!
      I = IT*NA/NT+1
      N = (IT+1)*NA/NT+1-I
      IF(IFST.EQ.1)THEN
         IF(IT.EQ.0)THEN
            CALL CONPAR(NOV,N,NRA,NCA,A,NCB,B,NRC,C,D,IRF,ICF)
         ELSE
            CALL CONPAR(NOV,N,NRA,NCA,A,NCB,B,NRC,C,DD(1,1,IT),IRF,ICF)
         ENDIF
         CALL COPYCP(N,NOV,NRA,NCA,A,NCB,B,NRC,C,A1(1,1,I),A2(1,1,I),
     +       BB(1,1,I),CC(1,1,I),CCLO,IRF,IT)
      ENDIF
!
      IF(NLLV.EQ.0)THEN
         IF(IT.EQ.0)THEN
            CALL CONRHS(NOV,N,NRA,NCA,A,NRC,C,FA,FC(NBC+1),IRF)
         ELSE
            CALL CONRHS(NOV,N,NRA,NCA,A,NRC,C,FA,FCFC(1,IT),IRF)
         ENDIF
      ELSE
         IF(IT.EQ.0)THEN
            CALL SETZERO(FA,FC,N,NRA,NFC)
         ELSE
            CALL SETZERO(FA,FCFC(1,IT),N,NRA,NRC)
         ENDIF
      ENDIF
      CALL CPYRHS(N,NOV,NRA,FAA(1,I),FA,IRF)
!
      CALL REDUCE(A1,A2,BB,CC,CCLO,D,DD,FAA,FC(NBC+1),FCFC,
     +     NTST,NOV,NCB,NRC,S1,S2,IPC,IPR,IFST,NLLV,IT,NT,IAM,KWT)
!
! REDUCE already has a barrier.
!$OMP MASTER
!
      IF(IAM.EQ.0)THEN
         ALLOCATE(FCC(2*NOV+NFC+2*NOV*NOV+1),E(NOV+NFC,2*NOV+NFC))
         ALLOCATE(IR(2*NOV+NFC+2*NOV*NOV+1),IC(2*NOV+NFC+2*NOV*NOV+1))
!
         CALL DIMRGE(E,CC,CCBC,D,DDBC,FC,IR,IC,
     +     NTST,NFC,NBC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,S1,A2,FAA,BB)
         DO II=1,NOV
            SOL(II,1)=FCC(II)
         ENDDO
!
         DEALLOCATE(FCC,E,IR,IC)
      ENDIF
!
!$OMP END MASTER
!
      CALL BCKSUB(S1,A2,S2,BB,FAA,SOL,FC,NTST,NOV,NCB,IPC,IT,NT,IAM,KWT)
!
      ALLOCATE(X(NRA))
      CALL INFPAR(A,B,FA,SOL(1,I),FC,N,NOV,NRA,NCA,NCB,IRF,ICF,X)
      DEALLOCATE(X)
!
      RETURN
      END SUBROUTINE BRBD
!
!     ---------- -------
      SUBROUTINE SETZERO(FA,FC,NA,NRA,NFC)
!
      IMPLICIT NONE
!
! Arguments
      INTEGER   NA,NRA,NFC
      DOUBLE PRECISION FA(NRA,*),FC(*)
!
! Local
      INTEGER    I,J
!
      DO I=1,NA
        DO J=1,NRA
          FA(J,I)=0.D0
        ENDDO
      ENDDO
!
      DO I=1,NFC
        FC(I)=0.D0
      ENDDO
!
      RETURN
      END SUBROUTINE SETZERO
!
!     This is the per-CPU, per-element process function of CONPAR
!     ---------- ------
      SUBROUTINE CONPAP(NOV,NRA,NCA,A,NCB,B,NRC,C,D,IRF,ICF,IAMAX)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Arguments
      INTEGER   NOV,NRA,NCA
      INTEGER   NCB,NRC,ICF(*),IRF(*)
      DIMENSION A(NCA,*),B(NCB,*),C(NCA,*),D(NCB,*),IAMAX(*)
!
! Note that the summation of the adjacent overlapped part of C
! is delayed until REDUCE, in order to merge it with other communications.
!
      NEX=NCA-2*NOV
!
! Condensation of parameters (Elimination of local variables).
!
      M1    = NOV+1
      M2    = NOV+NEX
!     
      ZERO = 0.D0
!
         DO J=1,NRA
            IRF(J)=J
            IAMAX(J)=NOV+IDAMAX(NEX,A(NOV+1,J),1)
         ENDDO
         DO IC=M1,M2
            IRP=IC-NOV
            IR1=IRP+1
            ICP1=IC+1
!           **Search for pivot (Complete pivoting)
            PIV = ZERO
            IPIV = IRP
            JPIV = IC
            DO K1=IRP,NRA
               IROW=IRF(K1)
               TPIV = DABS(A(IAMAX(IROW),IROW))
               IF(PIV.LT.TPIV)THEN
                  PIV = TPIV
                  IPIV = K1
                  JPIV = IAMAX(IROW)
               ENDIF
            ENDDO
!           **Move indices
            ICF(IC)=JPIV
            IF(IC.NE.JPIV)THEN
!              **Physically swap columns
               DO IR=1,NRA
                  TMP=A(JPIV,IR)
                  A(JPIV,IR)=A(IC,IR)
                  A(IC,IR)=TMP
               ENDDO
            ENDIF
            IRFIRP      = IRF(IPIV)
            IRF(IPIV)   = IRF(IRP)
            IRF(IRP)    = IRFIRP
!           **End of pivoting; elimination starts here
            PIV=A(IC,IRFIRP)
            DO IR=IR1,NRA
               IRFIR=IRF(IR)
               RM=A(IC,IRFIR)/PIV
               A(IC,IRFIR)=RM
               IF(RM.NE.0.0)THEN
                  CALL IMSBRA(NOV,NCA,NRA,A(1,IRFIR),A(1,IRFIRP),
     +                 ICP1,IAMAX(IRFIR),RM)
                  DO L=1,NCB
                     B(L,IRFIR)=B(L,IRFIR)-RM*B(L,IRFIRP)
                  ENDDO
               ELSEIF(IAMAX(IRFIR).EQ.JPIV)THEN
                  IAMAX(IRFIR)=IC+IDAMAX(NRA-IC,A(ICP1,IRFIR),1)
               ELSEIF(IAMAX(IRFIR).EQ.IC)THEN
                  IAMAX(IRFIR)=JPIV
               ENDIF
            ENDDO
            DO IR=1,NRC
!              **Swap columns of C physically
               RM=C(JPIV,IR)/PIV
               C(JPIV,IR)=C(IC,IR)
               C(IC,IR)=RM
               IF(RM.NE.0.0)THEN
                  CALL SUBRAC(NOV,NCA,C(1,IR),A(1,IRFIRP),ICP1,RM)
                  DO L=1,NCB
                     D(L,IR)=D(L,IR)-RM*B(L,IRFIRP)
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
      RETURN
!
      CONTAINS
!
!     ---------- ------
      SUBROUTINE IMSBRA(NOV,NCA,NRA,A,AP,ICP1,IAMAX,RM)
      IMPLICIT NONE
! Arguments
      DOUBLE PRECISION, INTENT(IN) :: AP(*),RM
      DOUBLE PRECISION, INTENT(INOUT) :: A(*)
      INTEGER, INTENT(IN) :: NOV,NRA,NCA,ICP1
      INTEGER, INTENT(OUT) :: IAMAX
! Local
      INTEGER L
      DOUBLE PRECISION PPIV,TPIV,V
!
      DO L=1,NOV
         A(L)=A(L)-RM*AP(L)
      ENDDO
      PPIV=0d0
      IAMAX=ICP1
      DO L=ICP1,NRA
         V=A(L)-RM*AP(L)
!     Also recalculate absolute maximum for current row
         A(L)=V
         TPIV=DABS(V)
         IF(PPIV.LT.TPIV)THEN
            PPIV=TPIV
            IAMAX=L
         ENDIF
      ENDDO
      DO L=NCA-NOV+1,NCA
         A(L)=A(L)-RM*AP(L)
      ENDDO
      END SUBROUTINE IMSBRA
!
!     ---------- ------
      SUBROUTINE SUBRAC(NOV,NCA,C,AP,ICP1,RM)
      IMPLICIT NONE
! Arguments
      DOUBLE PRECISION, INTENT(IN) :: AP(*),RM
      DOUBLE PRECISION, INTENT(INOUT) :: C(*)
      INTEGER, INTENT(IN) :: NOV,NCA,ICP1
! Local
      INTEGER L
!
      DO L=1,NOV
         C(L)=C(L)-RM*AP(L)
      ENDDO
      DO L=ICP1,NCA
         C(L)=C(L)-RM*AP(L)
      ENDDO
      END SUBROUTINE SUBRAC
!
      END SUBROUTINE CONPAP
!
!     ---------- ------
      SUBROUTINE CONPAR(NOV,NA,NRA,NCA,A,NCB,B,NRC,C,D,IRF,ICF)
!
      IMPLICIT NONE
!
! Arguments
      INTEGER   NOV,NA,NRA,NCA
      INTEGER   NCB,NRC,ICF(NCA,*),IRF(NRA,*)
      DOUBLE PRECISION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NRC,*)
      DOUBLE PRECISION D(NCB,*)
! Local
      INTEGER J
      INTEGER, ALLOCATABLE :: IAMAX(:)
!
! Condensation of parameters (Elimination of local variables).
! NA is the local NTST.
!
      IF(NCA.EQ.2*NOV)RETURN
      ALLOCATE(IAMAX(NRA))
      DO J=1,NA
         CALL CONPAP(NOV,NRA,NCA,A(1,1,J),NCB,B(1,1,J),NRC,C(1,1,J),
     +        D,IRF(1,J),ICF(1,J),IAMAX)
      ENDDO
      DEALLOCATE(IAMAX)
!
      RETURN
      END SUBROUTINE CONPAR
!
!     ---------- ------
      SUBROUTINE CONRHS(NOV,NA,NRA,NCA,A,NRC,C,FA,FC,IRF)
!
      IMPLICIT NONE
!
! Arguments
      INTEGER   NOV,NA,NRA,NCA
      INTEGER   NRC,IRF(NRA,*)
      DOUBLE PRECISION A(NCA,NRA,*),C(NCA,NRC,*)
      DOUBLE PRECISION FA(NRA,*),FC(*)
!
! Local
      INTEGER   M1,M2,NEX,I,IC,IR1,IR,IRFIR
      DOUBLE PRECISION RM
!
      NEX=NCA-2*NOV
      IF(NEX.EQ.0)RETURN
!
! Condensation of right hand side.
!
      M1    = NOV+1
      M2    = NOV+NEX
!
      DO I=1,NA
         DO IC=M1,M2
            IR1=IC-NOV+1
            RM=FA(IRF(IR1-1,I),I)
            IF(RM.NE.0.0)THEN
               DO IR=IR1,NRA
                  IRFIR=IRF(IR,I)
                  FA(IRFIR,I)=FA(IRFIR,I)-RM*A(IC,IRFIR,I)
               ENDDO
               DO IR=1,NRC
                  FC(IR)=FC(IR)-RM*C(IC,IR,I)
               ENDDO
            ENDIF
         ENDDO
      ENDDO
!
      RETURN
      END SUBROUTINE CONRHS
!
!     ---------- ------
      SUBROUTINE COPYCP(NA,NOV,NRA,NCA,A,
     +  NCB,B,NRC,C,A1,A2,BB,CC,CCLO,IRF,IT)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Arguments
      INTEGER   NA,NOV,NRA,NCA
      INTEGER   NCB,NRC,IRF(NRA,*),IT
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NRC,*)
      DIMENSION A1(NOV,NOV,*),A2(NOV,NOV,*)
      DIMENSION BB(NCB,NOV,*),CC(NOV,NRC,*),CCLO(NOV,NRC,*)
!
!     DIMENSION FA(NRA,*),FAA(NOV,*)
!
! Local
      INTEGER   I,IR,IC
!
! Copies the condensed sytem generated by CONPAR into workspace.
!
      DO I=1,NA
         DO IR=1,NOV
            IRFIR=IRF(NRA-NOV+IR,I)
            DO IC=1,NOV
               IC1=NCA-NOV+IC
               A1(IC,IR,I)=A(IC,IRFIR,I)
               A2(IC,IR,I)=A(IC1,IRFIR,I)
            ENDDO     
            DO IC=1,NCB
               BB(IC,IR,I)=B(IC,IRFIR,I)
            ENDDO
         ENDDO
      ENDDO
!
      NAP1=NA+1
      DO I=1,NAP1
         DO IR=1,NRC
            DO IC=1,NOV
               IF(I.EQ.1)THEN
                  IF(IT.EQ.0)THEN
                     CC(IC,IR,I)=C(IC,IR,I)
                  ELSE
                     CCLO(IC,IR,IT)=C(IC,IR,I)
                  ENDIF
               ELSEIF(I.EQ.NAP1)THEN
                  CC(IC,IR,I)=C(NRA+IC,IR,I-1)
               ELSE
                  CC(IC,IR,I)=C(IC,IR,I)+C(NRA+IC,IR,I-1)
               ENDIF
            ENDDO
         ENDDO
      ENDDO
! 
      RETURN
      END SUBROUTINE COPYCP
!
!     ---------- ------
      SUBROUTINE CPYRHS(NA,NOV,NRA,FAA,FA,IRF)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Arguments
      INTEGER   NA,NOV,NRA
      INTEGER   IRF(NRA,*)

      DIMENSION FA(NRA,*),FAA(NOV,*)
!
! Local
      INTEGER   I,IR
!
!     **Copy the RHS
      DO I=1,NA
         DO IR=1,NOV
            IRFIR=IRF(NRA-NOV+IR,I)
            FAA(IR,I)=FA(IRFIR,I)
         ENDDO         
      ENDDO
!     
      RETURN
      END SUBROUTINE CPYRHS
!
!     ---------- ------
      SUBROUTINE REDUCE(A1,A2,BB,CC,CCLO,DD,DDD,FAA,FC,FCFC,
     +     NTST,NOV,NCB,NRC,S1,S2,IPC,IPR,IFST,NLLV,IT,NT,IAM,KWT)
!
      IMPLICIT NONE
!
! Arguments
      INTEGER   NTST,NOV,NCB,NRC,IFST,NLLV,IT,NT,IAM,KWT
      INTEGER   IPC(NOV,*),IPR(NOV,*)
      DOUBLE PRECISION A1(NOV,NOV,*),A2(NOV,NOV,*)
      DOUBLE PRECISION S1(NOV,NOV,*),S2(NOV,NOV,*)
      DOUBLE PRECISION BB(NCB,NOV,*),CC(NOV,NRC,*),CCLO(NOV,NRC,*)
      DOUBLE PRECISION DD(NCB,*),DDD(NCB,NRC,*)
      DOUBLE PRECISION FAA(NOV,*),FC(*),FCFC(NRC,*)
!
! Local 
      INTEGER IAMAX,I,II,J,K,PLO,PHI,BASE,NA
      ALLOCATABLE IAMAX(:)
!
      ALLOCATE(IAMAX(2*NOV))
!
      BASE=IAM*NTST/KWT
      NA=(IAM+1)*NTST/KWT-BASE
      IF(IT.EQ.0)THEN
!     Reduce non-overlapping 1st piece
         CALL REDUCER(1,NTST,BASE+1,BASE+NA/NT,CC,DD,FC)
      ELSE
         PLO = BASE+IT*NA/NT+1
         PHI = BASE+(IT+1)*NA/NT
!     Reduce non-overlapping pieces
         CALL REDUCER(1,NTST,PLO,PHI,
     +        CCLO(1,1,IT),DDD(1,1,IT),FCFC(1,IT))
      ENDIF
!
!$OMP BARRIER
!$OMP MASTER
!
!     Fix up boundaries between CC parts from COPYCP
!
      IF(IFST.EQ.1)THEN
         DO I=1,NT-1
            II=I*NA/NT+1
            DO J=1,NRC
               DO K=1,NOV
                  CC(K,J,II)=CC(K,J,II)+CCLO(K,J,I)
               ENDDO
            ENDDO
         ENDDO
      ENDIF
!
!     Reduce overlapping pieces
      IF(NT.GT.1)
     +   CALL REDUCER(1,NTST,BASE+1,BASE+NA,CC,DD,FC)
!
!     This is where we sum into the global copy of the d array
      DO I=1,NT-1
         DO J=1,NRC
            IF(IFST.EQ.1)THEN
               DO K=1,NCB
                  DD(K,J)=DD(K,J)+DDD(K,J,I)
               ENDDO
            ENDIF
            IF(NLLV.EQ.0)THEN
               FC(J)=FC(J)+FCFC(J,I)
            ENDIF
         ENDDO
      ENDDO
!
      IF(KWT.GT.1)THEN
         CALL MPICON(S1,A1,A2,BB,CC,DD,FAA,FC,NTST,NOV,NCB,NRC,IFST)
         IF(IAM.EQ.0)
     +        CALL REDUCER(1,NTST,0,NTST,CC,DD,FC)
      ENDIF
!$OMP END MASTER
!
      DEALLOCATE(IAMAX)
      RETURN
!
      CONTAINS
!
!      --------- ---------- -------
       RECURSIVE SUBROUTINE REDUCER(LO,HI,PLO,PHI,CCLO,DD,FC)
!
! Arguments
       INTEGER   LO,HI,PLO,PHI
       DOUBLE PRECISION CCLO(NOV,NRC),DD(NCB,*),FC(*)
!
! Local 
       INTEGER IR,IC,I0,I1,I2,MID
!
       IF(HI.LT.PLO.OR.LO.GT.PHI)RETURN
! This is a check for the master reduction so it will stop as soon
! as there is no more overlap (already handled by workers).
       IF(PLO.EQ.0)THEN
          IF((LO*KWT-1)/NTST.EQ.(HI*KWT-1)/NTST)RETURN
       ELSEIF(NT.GT.1.AND.PHI-PLO.EQ.NA-1.AND.LO.GT.BASE)THEN
          IF(((LO-BASE)*NT-1)/NA.EQ.((HI-BASE)*NT-1)/NA)RETURN
       ENDIF
!
! Use nested dissection for reduction; this is naturally a recursive
! procedure.
!
       MID=(LO+HI)/2
!
       IF(LO.LT.MID)
     +    CALL REDUCER(LO,MID,PLO,PHI,CCLO,DD,FC)
!
       IF(MID+1.LT.HI)
     +    CALL REDUCER(MID+1,HI,PLO,PHI,CCLO,DD,FC)
!
! Thread is not in the [PLO,PHI] range: return
       IF(LO.LT.PLO.OR.HI.GT.PHI)RETURN
!
! Initialization
!
       I0=LO-BASE
       I1=MID-BASE
       I2=HI-BASE
       IF(IFST.EQ.1)THEN
          IF(LO.EQ.MID)THEN
             DO IR=1,NOV
                DO IC=1,NOV
                   S1(IC,IR,I1)=A1(IC,IR,I1)
                ENDDO
             ENDDO
          ENDIF
          IF(MID+1.LT.HI)THEN
             DO IR=1,NOV
                DO IC=1,NOV
                   A1(IC,IR,I1+1)=S1(IC,IR,I2)
                ENDDO
             ENDDO
          ENDIF
!
          IF(LO.EQ.PLO)THEN
             CALL REDBLK(S1(1,1,I1),A2(1,1,I1),S2(1,1,I1),BB(1,1,I1),
     +                   S1(1,1,I2),A1(1,1,I1+1),A2(1,1,I2),BB(1,1,I2),
     +                   CCLO,      CC(1,1,I1+1),CC(1,1,I2+1),DD,
     +                   IPC(1,I1),IPR(1,I1),IAMAX,NOV,NCB,NRC)
          ELSE
             CALL REDBLK(S1(1,1,I1),A2(1,1,I1),S2(1,1,I1),BB(1,1,I1),
     +                   S1(1,1,I2),A1(1,1,I1+1),A2(1,1,I2),BB(1,1,I2),
     +                   CC(1,1,I0),CC(1,1,I1+1),CC(1,1,I2+1),DD,
     +                   IPC(1,I1),IPR(1,I1),IAMAX,NOV,NCB,NRC)
          ENDIF
       ENDIF
       IF(NLLV.EQ.0)THEN
          CALL REDRHSBLK(A2(1,1,I1),FAA(1,I1),
     +         A1(1,1,I1+1),FAA(1,I2),
     +         CC(1,1,I1+1),FC,NOV,NRC,IPR(1,I1))
       ENDIF
!
       RETURN
       END SUBROUTINE REDUCER
!
!      ---------- ------
       SUBROUTINE REDBLK(S11,A21,S21,BB1,
     +                   S12,A12,A22,BB2,
     +                   CC1,CC2,CC3,DD,
     +     IPC,IPR,IAMAX,NOV,NCB,NRC)
!
! Arguments
       INTEGER   NOV,NCB,NRC
       INTEGER   IPC(NOV),IPR(NOV),IAMAX(NOV*2)
       DOUBLE PRECISION S11(NOV,NOV),A21(NOV,NOV),S21(NOV,NOV)
       DOUBLE PRECISION S12(NOV,NOV),A12(NOV,NOV),A22(NOV,NOV)
       DOUBLE PRECISION CC1(NOV,NRC),CC2(NOV,NRC),CC3(NOV,NRC)
       DOUBLE PRECISION BB1(NCB,NOV),BB2(NCB,NOV), DD(NCB,NRC)
!
! Local
       INTEGER K1,K2,IR,IC,ICP1,IPIV1,IPIV2,JPIV,JPIV1,JPIV2
       INTEGER IDAMAX
       DOUBLE PRECISION PIV1,PIV2,TPIV,TMP
!
         DO K1=1,NOV
            DO K2=1,NOV
               S21(K2,K1) = 0.0D0
               S12(K2,K1) = 0.0D0
            ENDDO
         ENDDO
!
         DO K1=1,NOV
            IAMAX(K1)=IDAMAX(NOV,A21(1,K1),1)
         ENDDO
         DO K1=1,NOV
            IAMAX(NOV+K1)=IDAMAX(NOV,A12(1,K1),1)
         ENDDO
!
         DO IC=1,NOV
            ICP1=IC+1
!
! Complete pivoting; rows are swapped physically, columns also
            PIV1 = 0.d0
            IPIV1 = IC
            DO K1=IC,NOV
               TPIV=DABS(A21(IAMAX(K1),K1))
               IF(PIV1.LT.TPIV)THEN
                  PIV1   = TPIV
                  IPIV1  = K1
               ENDIF
            ENDDO
            JPIV1=IAMAX(IPIV1)
!
            PIV2 = 0.d0
            IPIV2 = 1
            DO K1=1,NOV
               TPIV=DABS(A12(IAMAX(NOV+K1),K1))
               IF(PIV2.LT.TPIV)THEN
                  PIV2   = TPIV
                  IPIV2  = K1
               ENDIF
            ENDDO
            JPIV2=IAMAX(NOV+IPIV2)
!
! rows and columns are swapped physically
!
            IF(PIV1.GE.PIV2)THEN
               JPIV        = JPIV1
               IPR(IC)     = IPIV1
               IF(IC.NE.IPIV1)
     +            CALL REDSWP(IC,NOV,NCB,
     +              S11(1,IC),S11(1,IPIV1),A21(1,IC),A21(1,IPIV1),
     +              S21(1,IC),S21(1,IPIV1),BB1(1,IC),BB1(1,IPIV1))
            ELSE
               JPIV        = JPIV2
               IPR(IC)     = NOV+IPIV2
               CALL REDSWP(IC,NOV,NCB,
     +              S11(1,IC),S12(1,IPIV2),A21(1,IC),A12(1,IPIV2),
     +              S21(1,IC),A22(1,IPIV2),BB1(1,IC),BB2(1,IPIV2))
            ENDIF
            IAMAX(IPR(IC)) = IAMAX(IC)
            IPC(IC) = JPIV
            IF(JPIV.NE.IC)THEN
               DO IR=1,NOV
                  TMP          = A12(IC,IR)
                  A12(IC,IR)   = A12(JPIV,IR)
                  A12(JPIV,IR) = TMP
                  TMP          = A21(IC,IR)
                  A21(IC,IR)   = A21(JPIV,IR)
                  A21(JPIV,IR) = TMP
               ENDDO
            ENDIF
!
! End of pivoting; Elimination starts here
!
            DO IR=ICP1,NOV
               CALL REDELIM(IC,NOV,NCB,IAMAX(IR),JPIV,
     +              A21(1,IR),A21(1,IC),S11(1,IR),S11(1,IC),
     +              S21(1,IR),S21(1,IC),BB1(1,IR),BB1(1,IC))
            ENDDO
!     
            DO IR=1,NOV
               CALL REDELIM(IC,NOV,NCB,IAMAX(NOV+IR),JPIV,
     +              A12(1,IR),A21(1,IC),S12(1,IR),S11(1,IC),
     +              A22(1,IR),S21(1,IC),BB2(1,IR),BB1(1,IC))
            ENDDO
!     
            DO IR=1,NRC
               TMP          = CC2(IC,IR)
               CC2(IC,IR)   = CC2(JPIV,IR)
               CC2(JPIV,IR) = TMP
               CALL REDELIM(IC,NOV,NCB,0,JPIV,
     +              CC2(1,IR),A21(1,IC),CC1(1,IR),S11(1,IC),
     +              CC3(1,IR),S21(1,IC),DD(1,IR),BB1(1,IC))
            ENDDO
         ENDDO
!
       END SUBROUTINE REDBLK

!      ---------- ------
       SUBROUTINE REDSWP(IC,NOV,NCB,
     +     S11,S12,A12,A21,S21,A22,BB1,BB2)
!
       IMPLICIT NONE
!
       INTEGER IC,NOV,NCB
       DOUBLE PRECISION S11(NOV),S12(NOV),A12(NOV),A21(NOV)
       DOUBLE PRECISION S21(NOV),A22(NOV),BB1(NCB),BB2(NCB)
!
       INTEGER L
       DOUBLE PRECISION TMP
! Swapping
       DO L=1,NOV
          TMP    = S11(L)
          S11(L) = S12(L)
          S12(L) = TMP                  
          IF(L.GE.IC)THEN
             TMP    = A21(L)
             A21(L) = A12(L)
             A12(L) = TMP
          ENDIF
          TMP    = S21(L)
          S21(L) = A22(L)
          A22(L) = TMP
       ENDDO
       DO L=1,NCB
          TMP    = BB1(L)
          BB1(L) = BB2(L)
          BB2(L) = TMP
       ENDDO
       END SUBROUTINE REDSWP
!
!      ---------- -------
       SUBROUTINE REDELIM(IC,NOV,NCB,IAMAX,JPIV,
     +     A12,A21,S12,S11,A22,S21,BB2,BB1)
!
       IMPLICIT NONE
!
       INTEGER IC,NOV,NCB,IAMAX,JPIV
       DOUBLE PRECISION A12(NOV),A21(NOV),S12(NOV),S11(NOV)
       DOUBLE PRECISION A22(NOV),S21(NOV),BB1(NOV),BB2(NOV)
!
       INTEGER IDAMAX
       INTEGER L
       DOUBLE PRECISION RM,V,PPIV,TPIV
!
       RM = A12(IC)/A21(IC)
       A12(IC) = RM
!
       IF(RM.NE.0.0)THEN
          IF(IAMAX.EQ.0)THEN
             DO L=IC+1,NOV
                A12(L)=A12(L)-RM*A21(L)
             ENDDO
          ELSE
             PPIV=0d0
             IAMAX=IC+1
             DO L=IC+1,NOV
                V=A12(L)-RM*A21(L)
!     Also recalculate absolute maximum for current row
                A12(L)=V
                TPIV=DABS(V)
                IF(PPIV.LT.TPIV)THEN
                   PPIV=TPIV
                   IAMAX=L
                ENDIF
             ENDDO
          ENDIF
          DO L=1,NOV
             S12(L) = S12(L)-RM*S11(L)
             A22(L) = A22(L)-RM*S21(L)
          ENDDO
          DO L=1,NCB
             BB2(L) = BB2(L)-RM*BB1(L)
          ENDDO
       ELSEIF(IAMAX.EQ.JPIV)THEN
!     recalculate absolute maximum for current row
          IAMAX = IC+IDAMAX(NOV-IC,A12(IC+1),1)
       ELSEIF(IAMAX.EQ.IC)THEN
          IAMAX = JPIV
       ENDIF
!
       END SUBROUTINE REDELIM
!
      END SUBROUTINE REDUCE
!
!     ---------- ---------
      SUBROUTINE REDRHSBLK(A21,FAA1,A12,FAA2,CC,FC,NOV,NRC,IPR)
!
      IMPLICIT NONE
!
! Arguments
      INTEGER   NOV,NRC,IPR(NOV)
      DOUBLE PRECISION A12(NOV,NOV),A21(NOV,NOV)
      DOUBLE PRECISION CC(NOV,NRC)
      DOUBLE PRECISION FAA1(NOV),FAA2(NOV),FC(*)
!
! Local
      INTEGER IC,IR,IPIV1,L1
      DOUBLE PRECISION RM
!
! Reduce with the right hand side for one block
      DO IC=1,NOV
         IPIV1 = IPR(IC)
         IF(IPIV1.LE.NOV)THEN
            RM          = FAA1(IPIV1)
            FAA1(IPIV1) = FAA1(IC)
         ELSE
            L1       = IPIV1-NOV
            RM       = FAA2(L1)
            FAA2(L1) = FAA1(IC)
         ENDIF
         FAA1(IC) = RM
         DO IR=IC+1,NOV
            FAA1(IR) = FAA1(IR)-A21(IC,IR)*RM
         ENDDO
         DO IR=1,NOV
            FAA2(IR) = FAA2(IR)-A12(IC,IR)*RM
         ENDDO
         DO IR=1,NRC
            FC(IR)= FC(IR)-CC(IC,IR)*RM
         ENDDO
      ENDDO
!
      RETURN
      END SUBROUTINE REDRHSBLK
!
!     ---------- ------
      SUBROUTINE DIMRGE(E,CC,CCBC,D,DDBC,FC,IR,IC,
     +  NA,NFC,NBC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,S,A2,FAA,BB)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT NONE
!
! Arguments
      INTEGER   NA,NFC,NBC,NOV,NCB,IDB,NLLV
      INTEGER   IR(*),IC(*)
      DOUBLE PRECISION E(NOV+NFC,*),CC(NOV,NFC-NBC,*),CCBC(NOV,NBC,*)
      DOUBLE PRECISION D(NCB,*),DDBC(NCB,*),P0(NOV,*),P1(NOV,*)
      DOUBLE PRECISION S(NOV,NOV,*),FAA(NOV,*),A2(NOV,NOV,*)
      DOUBLE PRECISION BB(NCB,NOV,*),FC(*),FCC(*)
      DOUBLE PRECISION, INTENT(OUT) :: DET
!
! Local
      INTEGER  I,J,K,K1,K2,KC,KR,NAP1,NCR,NRC,NOVPI,NOVPJ,NOVPJ2
      DOUBLE PRECISION XE
      ALLOCATABLE XE(:)
      ALLOCATE(XE(NOV+NFC))
!
      NAP1    = NA+1
      NCR     = NFC+NOV
      NRC     = NFC-NBC
!     
! Copy
      DO I=1,NOV
         DO J=1,NOV
            NOVPJ      = NOV+J
            E(I,J)     = S(J,I,NA)
            P0(I,J)    = S(J,I,NA)
            E(I,NOVPJ) = A2(J,I,NA)
            P1(I,J)    = A2(J,I,NA)
         ENDDO
         DO J=1,NCB
            NOVPJ2      = 2*NOV+J
            E(I,NOVPJ2) = BB(J,I,NA)
         ENDDO
      ENDDO
!     
      DO I=1,NBC
         NOVPI=NOV+I
         DO J=1,NOV
            NOVPJ          = NOV+J
            E(NOVPI,J)     = CCBC(J,I,1)
            E(NOVPI,NOVPJ) = CCBC(J,I,2)
         ENDDO
         DO J=1,NCB
            NOVPJ2          = 2*NOV+J
            E(NOVPI,NOVPJ2) = DDBC(J,I)
         ENDDO
      ENDDO
      DO I=1,NRC
         NOVPI=NOV+NBC+I
         DO J=1,NOV
            NOVPJ          = NOV+J
            E(NOVPI,J)     = CC(J,I,1)
            E(NOVPI,NOVPJ) = CC(J,I,NAP1)
         ENDDO
         DO J=1,NCB
            NOVPJ2          = 2*NOV+J
            E(NOVPI,NOVPJ2) = D(J,I)
         ENDDO
      ENDDO
!
      DO I=1,NOV
         XE(I)=FAA(I,NA)
      ENDDO
!
      DO I=1,NFC
         NOVPI     = NOV+I
         XE(NOVPI) = FC(I)
      ENDDO
!     
      IF(IDB.GE.3)THEN
         IF(FORT9DST==1)WRITE(9,101)
         IF(FORT9DST==1)WRITE(9,100)(XE(I),I=1,NCR)
      ENDIF
!     
      IF(IDB.GE.4)THEN
         IF(FORT9DST==1)WRITE(9,102)
         DO I=1,NCR
            IF(FORT9DST==1)WRITE(9,100)(E(I,J),J=1,NCR)
         ENDDO
      ENDIF
!
! Solve for FCC
      IF(NLLV.EQ.0)THEN
         CALL GE(0,NCR,NCR,E,1,NCR,FCC,NCR,XE,IR,IC,DET)
      ELSEIF(NLLV.GT.0)THEN
         CALL NLVC(NCR,NCR,NLLV,E,FCC,IR,IC)
      ELSE
         DO I=1,NCR-1
            XE(I)=0.D0
         ENDDO
         XE(NCR)=1.D0
         CALL GE(0,NCR,NCR,E,1,NCR,FCC,NCR,XE,IR,IC,DET)
      ENDIF

      IF(IDB.GE.4)THEN
         IF(FORT9DST==1)WRITE(9,103)
         IF(FORT9DST==1)WRITE(9,100)(FCC(I),I=1,NCR)
      ENDIF
!
      K1=NCR
      K2=K1+NOV**2
      DO KR=1,NOV
         DO KC=1,NOV
            K=(KR-1)*NOV+KC
            FCC(K1+K)=P0(KR,KC)
            FCC(K2+K)=P1(KR,KC)
         ENDDO
      ENDDO
      FCC(NCR+2*NOV**2+1)=DET
!

 100  FORMAT(1X,10E11.3)
 101  FORMAT(/,1X,'Residuals of reduced system:')
 102  FORMAT(/,1X,'Reduced Jacobian matrix:')
 103  FORMAT(/,1X,'Solution vector:')
!     
      DO I=1,NFC
         FC(I)=FCC(NOV+I)
      ENDDO
!
      K1=NCR
      K2=K1+NOV**2
      DO KR=1,NOV
         DO KC=1,NOV
            K=(KR-1)*NOV+KC
            P0(KR,KC) = FCC(K1+K)
            P1(KR,KC) = FCC(K2+K)
         ENDDO
      ENDDO
      DET=FCC(NCR+2*NOV**2+1)
!
      DEALLOCATE(XE)
      RETURN
      END SUBROUTINE DIMRGE
!
!     ---------- -------
      SUBROUTINE BCKSUB1(S1,A2,S2,BB,FAA,FCC,SOL1,SOL2,FC,NOV,NCB,IPC)
!
      IMPLICIT NONE
!
! Arguments
      INTEGER   NOV,NCB,IPC(NOV)
      DOUBLE PRECISION S1(NOV,NOV),S2(NOV,NOV)
      DOUBLE PRECISION A2(NOV,NOV),BB(NCB,NOV)
      DOUBLE PRECISION SOL1(NOV),SOL2(NOV),FAA(NOV),FC(*),FCC(*)
!
! Local
      INTEGER K,L
      DOUBLE PRECISION SM,TMP
!
!
! Backsubstitution process for 1 block row
      DO K=NOV,1,-1
         SM=FAA(K)
         DO L=1,NOV
            SM=SM-FCC(L)*S1(L,K)
            SM=SM-SOL2(L)*S2(L,K)
         ENDDO
         DO L=1,NCB
            SM=SM-FC(L)*BB(L,K)
         ENDDO
         DO L=K+1,NOV
            SM=SM-SOL1(L)*A2(L,K)
         ENDDO
         SOL1(K)=SM/A2(K,K)
      ENDDO
!     Revert column pivoting on SOL1
      DO K=NOV,1,-1
         TMP=SOL1(K)
         SOL1(K)=SOL1(IPC(K))
         SOL1(IPC(K))=TMP
      ENDDO
!
      RETURN
      END SUBROUTINE BCKSUB1
!
!     ---------- ------
      SUBROUTINE BCKSUB(S1,A2,S2,BB,FAA,SOL,FC,NTST,NOV,NCB,IPC,
     +     IT,NT,IAM,KWT)
!
      IMPLICIT NONE
!
! Arguments
      INTEGER   NTST,NOV,NCB,IPC(NOV,*),IT,NT,IAM,KWT
      DOUBLE PRECISION S1(NOV,NOV,*),S2(NOV,NOV,*)
      DOUBLE PRECISION A2(NOV,NOV,*),BB(NCB,NOV,*)
      DOUBLE PRECISION SOL(NOV,*),FAA(NOV,*),FC(*)
!
! Local
      INTEGER   I,PLO,PHI,BASE,NA
!
      BASE=IAM*NTST/KWT
      NA=(IAM+1)*NTST/KWT-BASE
!$OMP MASTER
! do global backsubsitution until there is no overlap left
      IF(IAM.EQ.0)THEN
         DO I=1,NOV
            SOL(I,NTST+1) = FC(I)
         ENDDO
      ENDIF
      IF(KWT.GT.1)THEN
         IF(IAM.EQ.0)
     +        CALL BCKSUBR(1,NTST,0,NTST)
         CALL MPIBCAST(FC,NOV+NCB)
         CALL MPISCAT(SOL,NOV,NTST,1)
      ENDIF
      IF(NT.GT.1)
     +     CALL BCKSUBR(1,NTST,BASE+1,BASE+NA)
!$OMP END MASTER
!$OMP BARRIER
      PLO=BASE+IT*NA/NT+1
      PHI=BASE+(IT+1)*NA/NT
      CALL BCKSUBR(1,NTST,PLO,PHI)
!
      RETURN
      CONTAINS

!      Back substitution within the interval [PLO,PHI]; no overlap
!      --------- ---------- -------
       RECURSIVE SUBROUTINE BCKSUBR(LO,HI,PLO,PHI)
!
! Arguments
       INTEGER   LO,HI,PLO,PHI
!
! Local
       INTEGER MID,I,I0,I1
!
       IF(LO.GE.HI.OR.HI.LT.PLO.OR.LO.GT.PHI)RETURN
       IF(PLO.EQ.0)THEN
          IF((LO*KWT-1)/NTST.EQ.(HI*KWT-1)/NTST)RETURN
       ELSEIF(NT.GT.1.AND.PHI-PLO.EQ.NA-1.AND.LO.GT.BASE)THEN
          IF(((LO-BASE)*NT-1)/NA.EQ.((HI-BASE)*NT-1)/NA)RETURN
       ENDIF
       MID=(LO+HI)/2
       I=MID-BASE
       I0=LO-BASE
       I1=HI-BASE
       IF(PLO.LE.LO.AND.HI.LE.PHI)THEN
          CALL BCKSUB1(S1(1,1,I),A2(1,1,I),S2(1,1,I),BB(1,1,I),
     +         FAA(1,I),SOL(1,I0),SOL(1,I+1),SOL(1,I1+1),FC(NOV+1),
     +         NOV,NCB,IPC(1,I))
       ENDIF
       CALL BCKSUBR(MID+1,HI,PLO,PHI)
       CALL BCKSUBR(LO,MID,PLO,PHI)
!     
       RETURN
       END SUBROUTINE BCKSUBR
!
      END SUBROUTINE BCKSUB
!
!     ---------- ------
      SUBROUTINE INFPAR(A,B,FA,SOL,FC,NA,NOV,NRA,NCA,NCB,IRF,ICF,X)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!  Arguments
      INTEGER   NA,NOV,NRA,NCA,NCB,IRF(NRA,*),ICF(NCA,*)
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),FA(NRA,*),FC(*)
      DIMENSION SOL(NOV,*),X(*)
!
! Local
      DOUBLE PRECISION SM
!
! Determine the local varables by backsubstitition.
!
      NRAM=NRA-NOV
!
! Backsubstitution in the condensation of parameters; no communication.
      DO I=1,NA
         DO IR=NRAM,1,-1
            IRP1=IR+1
            IRFIR=IRF(IR,I)
            SM=FA(IRFIR,I)
            DO J=1,NOV
               NRAPJ=NRA+J
               SM=SM-A(J,IRFIR,I)*SOL(J,I)
               SM=SM-A(NRAPJ,IRFIR,I)*SOL(J,I+1)
            ENDDO
            DO J=1,NCB
               SM=SM-B(J,IRFIR,I)*FC(NOV+J)
            ENDDO
            DO J=IRP1,NRAM
               J1=J+NOV
               SM=SM-A(J1,IRFIR,I)*X(J1)
            ENDDO
            NOVPIR=NOV+IR
            X(NOVPIR)=SM/A(NOVPIR,IRFIR,I)
         ENDDO    
!        **Copy SOL into FA 
         DO J=1,NOV
            FA(J,I)=SOL(J,I)
         ENDDO
!        **Undo pivots and copy X into FA
         DO J=NRA,NOV+1,-1
            TMP=X(ICF(J,I))
            X(ICF(J,I))=X(J)
            X(J)=TMP
         ENDDO
         DO J=NOV+1,NRA
            FA(J,I)=X(J)
         ENDDO
      ENDDO
!
!     
      RETURN
      END SUBROUTINE INFPAR
!           
!     ---------- ------
      SUBROUTINE PRINT1(NA,NRA,NCA,NCB,NFC,NBC,A,B,C,CCBC,D,DD,DDBC,FA,
     + FC,FCFC,NT)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NFC-NBC,*)
      DIMENSION CCBC(NCA-NRA,NBC,*),D(NCB,*),DD(NCB,NFC-NBC,*)
      DIMENSION DDBC(NCB,*),FA(NRA,*),FC(*),FCFC(NFC-NBC,*)
!
       IF(FORT9DST==1)WRITE(9,101)
       DO I=1,NA
         IF(FORT9DST==1)WRITE(9,102)I
         DO IR=1,NRA
           IF(FORT9DST==1)WRITE(9,103)(A(IC,IR,I),IC=1,NCA),(B(IC,IR,I),
     &IC=1,NCB)
     *     ,FA(IR,I)
         ENDDO
       ENDDO
!
       IF(FORT9DST==1)WRITE(9,104)
       DO I=1,NA
         IF(FORT9DST==1)WRITE(9,102)I
         DO IR=1,NFC
           IF(IR.GT.NBC)THEN
             IF(FORT9DST==1)WRITE(9,103)(C(IC,IR-NBC,I),IC=1,NCA)
           ELSEIF(I.EQ.1)THEN
             IF(FORT9DST==1)WRITE(9,103)(CCBC(IC,IR,1),IC=1,NCA-NRA)
           ELSEIF(I.EQ.NA)THEN
             IF(FORT9DST==1)WRITE(9,103)(CCBC(IC,IR,2),IC=1,NCA-NRA)
           ENDIF
         ENDDO
       ENDDO
!
       IF(FORT9DST==1)WRITE(9,105)
       DO IR=1,NBC
         IF(FORT9DST==1)WRITE(9,103)(DDBC(IC,IR),IC=1,NCB),FC(IR)
       ENDDO
       DO IR=1,NFC-NBC
         DO IC=1,NCB
           D1=D(IC,IR)
           IF(NT.GT.1)THEN
             DO I=1,NT-1
               D1=D1+DD(IC,IR,I)
             ENDDO
           ENDIF
           IF(FORT9DST==1)WRITE(9,103)D1
         ENDDO
         FC1=FC(NBC+IR)
         IF(NT.GT.1)THEN
           DO I=1,NT-1
             FC1=FC1+FCFC(IR,I)
           ENDDO
         ENDIF
         IF(FORT9DST==1)WRITE(9,103)FC1
       ENDDO
!
 101   FORMAT(' AA , BB , FA (Full dimension) :')
 102   FORMAT(' I=',I3)
 103   FORMAT(1X,12E10.3)
 104   FORMAT(' CC (Full dimension) :')
 105   FORMAT(' DD , FC')
!
      RETURN
      END SUBROUTINE PRINT1

      END MODULE SOLVEBV
