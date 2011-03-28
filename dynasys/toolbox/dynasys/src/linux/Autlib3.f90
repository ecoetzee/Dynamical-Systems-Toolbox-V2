!========================================================================
! AUTLIB3.F - Subroutine for AUTO
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
      MODULE INTERFACES

      PRIVATE

      PUBLIC :: FNLP,STPNLP ! Folds (Algebraic Problems)
      PUBLIC :: FNBP,STPNBP ! BP (Algebraic Problems)
      PUBLIC :: FNC1,STPNC1 ! Optimizations (Algebraic,NFPR=2)
      PUBLIC :: FNC2,STPNC2 ! Optimizations (Algebraic,otherwise)
      PUBLIC :: FNDS        ! Discrete systems
      PUBLIC :: FNTI        ! Time integration
      PUBLIC :: FNHD,STPNHD ! Hopf bifs (maps)
      PUBLIC :: FNHB,STPNHB ! Hopf bifs (ODEs)
      PUBLIC :: FNHW,STPNHW ! Hopf bifs (waves)
      PUBLIC :: FNPS,BCPS,ICPS,PDBLE,STPNPS ! Periodic solutions
      PUBLIC :: STPNPB      ! Periodic solutions from Hopf
      PUBLIC :: FNWS        ! Spatially uniform sols (parabolic PDEs)
      PUBLIC :: FNWP,STPNWP ! Travelling waves (parabolic PDEs)
      PUBLIC :: FNSP        ! Stationary states (parabolic PDEs)
      PUBLIC :: FNPE,ICPE   ! Time evolution (parabolic PDEs)
      PUBLIC :: FNPL,BCPL,ICPL,STPNPL ! Fold cont of periodic sol
      PUBLIC :: FNPBP,BCPBP,ICPBP,STPNPBP ! BP cont of periodic sol
      PUBLIC :: FNPD,BCPD,ICPD,STPNPD ! PD cont of periodic sol
      PUBLIC :: FNTR,BCTR,ICTR,STPNTR ! Torus cont of periodic sol
      PUBLIC :: FNPO,BCPO,ICPO,STPNPO ! Optimization of periodic sol
      PUBLIC :: FNBL,BCBL,ICBL,STPNBL ! Fold cont of BVPs
      PUBLIC :: FNBBP,BCBBP,ICBBP,STPNBBP ! BP cont of BVPs

      PUBLIC :: FUNI,BCNI,ICNI ! Interface subroutines

      CONTAINS

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  Subroutines for the Continuation of Folds (Algebraic Problems)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNLP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates the equations for the 2-par continuation of folds.
!
      DIMENSION IAP(*),U(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*),ICP(*)
      DOUBLE PRECISION RAP(*),UOLD(*)
! Local
      ALLOCATABLE DFU(:),FF1(:),FF2(:)
!
       NDM=IAP(23)
!
! Generate the function.
!
       ALLOCATE(DFU(NDM*NDM))
       CALL FFLP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU)
!
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU)
         RETURN
       ENDIF
       ALLOCATE(FF1(NDIM),FF2(NDIM))
!
! Generate the Jacobian.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         UU=U(I)
         U(I)=UU-EP
         CALL FFLP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU)
         U(I)=UU+EP
         CALL FFLP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF2,NDM,DFU)
         U(I)=UU
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(FF2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF1,DFU)
         RETURN
       ENDIF
       PAR(ICP(1))=PAR(ICP(1))+EP
!
       CALL FFLP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU)
!
       DO J=1,NDIM
         DFDP(J,ICP(1))=(FF1(J)-F(J))/EP
       ENDDO
!
       PAR(ICP(1))=PAR(ICP(1))-EP
       DEALLOCATE(FF1,DFU)
!
      RETURN
      END SUBROUTINE FNLP
!
!     ---------- ----
      SUBROUTINE FFLP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION PAR(*),ICP(*),IAP(*),U(*),F(*),DFDU(NDM,*)
      DOUBLE PRECISION RAP(*),UOLD(*)
! Local
      DOUBLE PRECISION DUMDP(1)
!
       IPS=IAP(2)
!
       PAR(ICP(2))=U(NDIM)
       IF(IPS.EQ.-1) THEN
         CALL FNDS(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DUMDP)
       ELSE
         CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DUMDP)
       ENDIF
!
       DO I=1,NDM
         F(NDM+I)=0.d0
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
         ENDDO
       ENDDO
!
       F(NDIM)=-1
!
       DO I=1,NDM
         F(NDIM)=F(NDIM)+U(NDM+I)*U(NDM+I)
       ENDDO
!
      RETURN
      END SUBROUTINE FFLP
!
!     ---------- ------
      SUBROUTINE STPNLP(IAP,RAP,PAR,ICP,U)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!
      LOGICAL FOUND
!
! Generates starting data for the continuation of folds.
!
      DIMENSION U(*),PAR(*),ICP(*),IAP(*),RAP(*)
! Local
      ALLOCATABLE DFU(:),IR(:),IC(:),V(:),F(:)
      DOUBLE PRECISION DUMDFP(1),UOLD(1)
!
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       NDM=IAP(23)
!
       CALL FINDLB(IAP,IRS,NFPR1,FOUND)
       CALL READLB(IAP,U,PAR)
!
       ALLOCATE(DFU(NDM*NDM),IR(NDM),IC(NDM),V(NDM),F(NDM))
       IF(IPS.EQ.-1)THEN
         CALL FNDS(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFU,DUMDFP)
       ELSE
         CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFU,DUMDFP)
       ENDIF
       CALL NLVC(NDM,NDM,1,DFU,V,IR,IC)
       CALL NRMLZ(NDM,V)
       DO I=1,NDM
         U(NDM+I)=V(I)
       ENDDO
       DEALLOCATE(DFU,IR,IC,V,F)
       U(NDIM)=PAR(ICP(2))
!
      RETURN
      END SUBROUTINE STPNLP
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!   Subroutines for BP cont (Algebraic Problems) (by F. Dercole)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNBP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates the equations for the 2-par continuation of BP.
!
      DIMENSION IAP(*),U(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*),ICP(*)
      DOUBLE PRECISION RAP(*),UOLD(*)
! Local
      ALLOCATABLE DFU(:),DFP(:),FF1(:),FF2(:)
!
       NDM=IAP(23)
!
! Generate the function.
!
       ALLOCATE(DFU(NDM*NDM),DFP(NDM*NPARX))
       CALL FFBP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
!
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU,DFP)
         RETURN
       ENDIF
       ALLOCATE(FF1(NDIM),FF2(NDIM))
!
! Generate the Jacobian.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         UU=U(I)
         U(I)=UU-EP
         CALL FFBP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         U(I)=UU+EP
         CALL FFBP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
         U(I)=UU
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(FF2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF1,DFU,DFP)
         RETURN
       ENDIF
       PAR(ICP(1))=PAR(ICP(1))+EP
!
       CALL FFBP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
!
       DO J=1,NDIM
         DFDP(J,ICP(1))=(FF1(J)-F(J))/EP
       ENDDO
!
       PAR(ICP(1))=PAR(ICP(1))-EP
       DEALLOCATE(FF1,DFU,DFP)
!
      RETURN
      END SUBROUTINE FNBP
!
!     ---------- ----
      SUBROUTINE FFBP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION PAR(*),ICP(*),IAP(*),U(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
      DOUBLE PRECISION RAP(*),UOLD(*)
!
       IPS=IAP(2)
       ISW=IAP(10)
!
       IF(ISW.EQ.3) THEN
!        ** Generic case
         PAR(ICP(3))=U(NDIM)
       ENDIF
       PAR(ICP(2))=U(NDIM-1)
!
       IF(IPS.EQ.-1) THEN
         CALL FNDS(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
       ELSE
         CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
       ENDIF
!
       IF(ISW.EQ.2) THEN
!        ** Non-generic case
         DO I=1,NDM
           F(I)=F(I)+U(NDIM)*U(NDM+I)
         ENDDO
       ENDIF
!
       DO I=1,NDM
         F(NDM+I)=0.d0
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(J,I)*U(NDM+J)
         ENDDO
       ENDDO
!
       F(NDIM-1)=0.d0
       DO I=1,NDM
         F(NDIM-1)=F(NDIM-1)+DFDP(I,ICP(1))*U(NDM+I)
       ENDDO
!
       F(NDIM)=-1
       DO I=1,NDM
         F(NDIM)=F(NDIM)+U(NDM+I)*U(NDM+I)
       ENDDO
!
      RETURN
      END SUBROUTINE FFBP
!
!     ---------- ------
      SUBROUTINE STPNBP(IAP,RAP,PAR,ICP,U)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      LOGICAL FOUND
!
! Generates starting data for the continuation of BP.
!
      DIMENSION U(*),PAR(*),ICP(*),IAP(*),RAP(*)
! Local
      ALLOCATABLE DFU(:),DFP(:),A(:,:),IR(:),IC(:),V(:),F(:)
      DOUBLE PRECISION UOLD(1)
!
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       ISW=IAP(10)
       NDM=IAP(23)
!
       CALL FINDLB(IAP,IRS,NFPR1,FOUND)
       CALL READLB(IAP,U,PAR)
!
       ALLOCATE(DFU(NDM*NDM),DFP(NDM*NPARX),A(NDM+1,NDM+1))
       ALLOCATE(IR(NDM+1),IC(NDM+1),V(NDM+1),F(NDM))
       IF(IPS.EQ.-1)THEN
         CALL FNDS(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFU,DFP)
       ELSE
         CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFU,DFP)
       ENDIF
       DO I=1,NDM
         DO J=1,NDM
           A(I,J)=DFU((J-1)*NDM+I)
         ENDDO
         A(I,NDM+1)=DFP((ICP(1)-1)*NDM+I)
         A(NDM+1,I)=0.d0
       ENDDO
       CALL NLVC(NDM+1,NDM+1,2,A,V,IR,IC)
       DO I=1,NDM
         DO J=1,NDM
           A(I,J)=DFU((I-1)*NDM+J)
         ENDDO
         A(I,NDM+1)=V(I)
         A(NDM+1,I)=DFP((ICP(1)-1)*NDM+I)
       ENDDO
       A(NDM+1,NDM+1)=V(NDM+1)
       CALL NLVC(NDM+1,NDM+1,1,A,V,IR,IC)
       CALL NRMLZ(NDM,V)
       DO I=1,NDM
         U(NDM+I)=V(I)
       ENDDO
       DEALLOCATE(DFU,DFP,A,IR,IC,V,F)
       U(NDIM-1)=PAR(ICP(2))
       IF(ISW.EQ.3) THEN
!        ** Generic case
         U(NDIM)=PAR(ICP(3))
       ELSE
!        ** Non-generic case
         U(NDIM)=0.d0
       ENDIF
!
      RETURN
      END SUBROUTINE STPNBP
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     Subroutines for the Optimization of Algebraic Systems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNC1(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!
! Generate the equations for the continuation scheme used for
! the optimization of algebraic systems (one parameter).
!
      DIMENSION IAP(*)
      DIMENSION U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
      DOUBLE PRECISION RAP(*),UOLD(*)
! Local
      DIMENSION DDP(NPARX)
      ALLOCATABLE DDU(:)
      ALLOCATE(DDU(NDIM))
!
       NDM=IAP(23)
!
       PAR(ICP(2))=U(NDIM)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
! Rearrange (Since dimensions in FNC1 and FUNI differ).
!
       IF(IJAC.NE.0)THEN
         DO J=NDM,1,-1
           DO I=NDM,1,-1
             DFDU(I,J)=DFDU( (J-1)*NDM+I ,1 )
           ENDDO
         ENDDO
!
         DO J=NPARX,1,-1
           DO I=NDM,1,-1
             DFDP(I,J)=DFDP( (J-1)*NDM+I , 1 )
           ENDDO
         ENDDO
       ENDIF
!
       CALL FOPI(IAP,RAP,NDM,U,ICP,PAR,IJAC,F(NDIM),DDU,DDP)
       F(NDIM)=PAR(ICP(1))-F(NDIM)
!
       IF(IJAC.NE.0)THEN
         DO I=1,NDM
           DFDU(NDIM,I)=-DDU(I)
           DFDU(I,NDIM)=DFDP(I,ICP(2))
           DFDP(I,ICP(1))=0
         ENDDO
         DFDU(NDIM,NDIM)=-DDP(ICP(2))
         DFDP(NDIM,ICP(1))=1
       ENDIF
!
      DEALLOCATE(DDU)
      RETURN
      END SUBROUTINE FNC1
!
!     ---------- ------
      SUBROUTINE STPNC1(IAP,RAP,PAR,ICP,U)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generate starting data for optimization problems (one parameter).
!
      DIMENSION U(*),PAR(*),ICP(*),IAP(*),RAP(*)
! Local
      DOUBLE PRECISION DUM(1)
!
       NDIM=IAP(1)
       NDM=IAP(23)
!
       CALL STPNT(NDIM,U,PAR,T)
       NFPR=2
       IAP(29)=NFPR
       CALL FOPI(IAP,RAP,NDM,U,ICP,PAR,0,FOP,DUM,DUM)
       PAR(ICP(1))=FOP
       U(NDIM)=PAR(ICP(2))
!
      RETURN
      END SUBROUTINE STPNC1
!
!     ---------- ----
      SUBROUTINE FNC2(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generate the equations for the continuation scheme used for the
! optimization of algebraic systems (more than one parameter).
!
      INTEGER IAP(*),ICP(*)
      DOUBLE PRECISION RAP(*),U(*),UOLD(*),PAR(*),F(*)
      DOUBLE PRECISION DFDU(NDIM,*),DFDP(NDIM,*)
! Local
      ALLOCATABLE DFU(:),DFP(:),UU1(:),UU2(:),FF1(:),FF2(:)
!
       NDM=IAP(23)
!
! Generate the function.
!
       ALLOCATE(DFU(NDM*NDM),DFP(NDM*NPARX))
       CALL FFC2(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
!
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU,DFP)
         RETURN
       ENDIF
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NDIM),FF2(NDIM))
!
! Generate the Jacobian.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U(J)
           UU2(J)=U(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FFC2(IAP,RAP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         CALL FFC2(IAP,RAP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(DFU,DFP,UU1,UU2,FF1,FF2)
       IF (IJAC.EQ.1)RETURN
!
       DO I=1,NDIM
         DFDP(I,ICP(1))=0.d0
       ENDDO
       DFDP(NDIM,ICP(1))=1.d0
!
      RETURN
      END SUBROUTINE FNC2
!
!     ---------- ----
      SUBROUTINE FFC2(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
      DOUBLE PRECISION RAP(*),UOLD(*)
! Local
      DIMENSION DDP(NPARX)
      ALLOCATABLE DDU(:)
      ALLOCATE(DDU(NDM))
!
       NFPR=IAP(29)
!
       DO I=2,NFPR
         PAR(ICP(I))=U(2*NDM+I)
       ENDDO
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
       CALL FOPI(IAP,RAP,NDM,U,ICP,PAR,2,FOP,DDU,DDP)
!
       DO I=1,NDM
         F(NDM+I)=DDU(I)*U(2*NDM+1)
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(J,I)*U(NDM+J)
         ENDDO
       ENDDO
!
       NDM2=2*NDM
       ICPM=NFPR-2
       DO I=1,ICPM
         F(NDM2+I)=DDP(ICP(I+1))*U(NDM2+1)
       ENDDO
!
       DO I=1,ICPM
         DO J=1,NDM
           F(NDM2+I)=F(NDM2+I)+U(NDM+J)*DFDP(J,ICP(I+1))
         ENDDO
       ENDDO
!
       F(NDIM-1)=U(NDM2+1)*U(NDM2+1)-1
       DO J=1,NDM
         F(NDIM-1)=F(NDIM-1)+U(NDM+J)*U(NDM+J)
       ENDDO
       F(NDIM)=PAR(ICP(1))-FOP
!
      DEALLOCATE(DDU)
      RETURN
      END SUBROUTINE FFC2
!
!     ---------- ------
      SUBROUTINE STPNC2(IAP,RAP,PAR,ICP,U)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      LOGICAL FOUND
!
! Generates starting data for the continuation equations for
! optimization of algebraic systems (More than one parameter).
!
      DIMENSION U(*),PAR(*),ICP(*),IAP(*),RAP(*)
! Local
      ALLOCATABLE DFU(:),DFP(:),DD(:,:),DU(:),V(:),F(:),IR(:),IC(:)
      DIMENSION DP(NPARX),UOLD(1)
!
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
!
       CALL FINDLB(IAP,IRS,NFPR,FOUND)
       NFPR=NFPR+1
       IAP(29)=NFPR
       CALL READLB(IAP,U,PAR)
!
       IF(NFPR.EQ.3)THEN
         ALLOCATE(DFU(NDM*NDM),DFP(NDM*NPARX),F(NDM),V(NDM+1))
         ALLOCATE(DD(NDM+1,NDM+1),DU(NDM),IR(NDM+1),IC(NDM+1))
         CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFU,DFP)
         CALL FOPI(IAP,RAP,NDM,U,ICP,PAR,2,FOP,DU,DP)
!       TRANSPOSE
         DO I=1,NDM
           DO J=1,NDM
             DD(I,J)=DFU((I-1)*NDM+J)
           ENDDO
         ENDDO
         DO I=1,NDM
           DD(I,NDM+1)=DU(I)
           DD(NDM+1,I)=DFP((ICP(2)-1)*NDM+I)
         ENDDO
         DD(NDM+1,NDM+1)=DP(ICP(2))
         CALL NLVC(NDM+1,NDM+1,1,DD,V,IR,IC)
         CALL NRMLZ(NDM+1,V)
         DO I=1,NDM+1
           U(NDM+I)=V(I)
         ENDDO
         PAR(ICP(1))=FOP
         DEALLOCATE(DFU,DFP,F,V,DD,DU,IR,IC)
       ENDIF
!
       DO I=1,NFPR-1
         U(NDIM-NFPR+1+I)=PAR(ICP(I+1))
       ENDDO
!
      RETURN
      END SUBROUTINE STPNC2
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        Subroutines for Discrete Dynamical Systems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNDS(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generate the equations for continuing fixed points.
!
      DIMENSION U(*),IAP(*),ICP(*),PAR(*)
      DOUBLE PRECISION RAP(*),UOLD(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(*),DFDU(NDIM,*),DFDP(NDIM,*)
!
       CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
       DO I=1,NDIM
         F(I)=F(I)-U(I)
       ENDDO
!
       IF(IJAC.EQ.0)RETURN
!
       DO I=1,NDIM
         DFDU(I,I)=DFDU(I,I)-1
       ENDDO
!
      RETURN
      END SUBROUTINE FNDS
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        Subroutines for Time Integration of ODEs
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNTI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generate the equations for continuing fixed points.
!
      DIMENSION U(*),UOLD(*),ICP(*),PAR(*),F(*),IAP(*),RAP(*)
      DIMENSION DFDU(NDIM,*),DFDP(NDIM,*)
!
       CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
       TOLD=RAP(15)
       DT=PAR(ICP(1))-TOLD
!
       DO I=1,NDIM
         DFDP(I,ICP(1))=F(I)
         F(I)= DT*F(I) - U(I) + UOLD(I)
       ENDDO
!
       IF(IJAC.EQ.0)RETURN
!
       DO I=1,NDIM
         DO J=1,NDIM
           DFDU(I,J)= DT*DFDU(I,J)
         ENDDO
         DFDU(I,I)= DFDU(I,I) - 1.d0
       ENDDO
!
      RETURN
      END SUBROUTINE FNTI
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     Subroutines for the Continuation of Hopf Bifurcation Points (Maps)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNHD(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates the equations for the 2-parameter continuation of Hopf
! bifurcation points for maps.
!
      DIMENSION IAP(*),RAP(*),U(*),UOLD(*),ICP(*),PAR(*)
      DIMENSION F(*),DFDU(NDIM,*),DFDP(NDIM,*)
! Local
      ALLOCATABLE DFU(:),UU1(:),UU2(:),FF1(:),FF2(:)
!
       NDM=IAP(23)
!
! Generate the function.
!
       ALLOCATE(DFU(NDIM*NDIM))
       CALL FFHD(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU)
!
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU)
         RETURN
       ENDIF
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NDIM),FF2(NDIM))
!
! Generate the Jacobian.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U(J)
           UU2(J)=U(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FFHD(IAP,RAP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU)
         CALL FFHD(IAP,RAP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(UU1,UU2,FF2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF1,DFU)
         RETURN
       ENDIF
!
       PAR(ICP(1))=PAR(ICP(1))+EP
!
       CALL FFHD(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU)
!
       DO J=1,NDIM
         DFDP(J,ICP(1))=(FF1(J)-F(J))/EP
       ENDDO
!
       PAR(ICP(1))=PAR(ICP(1))-EP
!
       DEALLOCATE(FF1,DFU)
      RETURN
      END SUBROUTINE FNHD
!
!     ---------- ----
      SUBROUTINE FFHD(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      INTEGER IAP(*),ICP(*)
      DOUBLE PRECISION RAP(*),U(*),UOLD(*),PAR(*),F(*),DFDU(NDM,*)
! Local
      DOUBLE PRECISION DUMDP(1)
!
       NDM2=2*NDM
!
       THTA=U(NDIM-1)
       S1=DSIN(THTA)
       C1=DCOS(THTA)
       PAR(ICP(2))=U(NDIM)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DUMDP)
       DO I=1,NDM
         F(I)=F(I)-U(I)
         DFDU(I,I)=DFDU(I,I)-C1
       ENDDO
!
       DO I=1,NDM
         F(NDM+I)=S1*U(NDM2+I)
         F(NDM2+I)=-S1*U(NDM+I)
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
           F(NDM2+I)=F(NDM2+I)+DFDU(I,J)*U(NDM2+J)
         ENDDO
       ENDDO
!
       F(NDIM-1)=-1
!
       DO I=1,NDM
         F(NDIM-1)=F(NDIM-1)+U(NDM+I)*U(NDM+I)+U(NDM2+I)*U(NDM2+I)
       ENDDO
!
       F(NDIM)=0.d0
!
       DO I=1,NDM
         F(NDIM)=F(NDIM)+UOLD(NDM2+I)*U(NDM+I)-UOLD(NDM+I)*U(NDM2+I)
       ENDDO
!
      RETURN
      END SUBROUTINE FFHD
!
!     ---------- ------
      SUBROUTINE STPNHD(IAP,RAP,PAR,ICP,U)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      LOGICAL FOUND
!
! Generates starting data for the continuation of Hopf bifurcation
! points for maps.
!
      DIMENSION U(*),PAR(*),ICP(*),IAP(*),RAP(*)
! Local
      ALLOCATABLE DFU(:),SMAT(:,:),IR(:),IC(:),V(:),F(:)
      DOUBLE PRECISION UOLD(1),DUMDFP(1)
!
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
       ALLOCATE(DFU(NDIM*NDIM),F(NDIM),V(NDIM),SMAT(2*NDIM,2*NDIM))
!
       CALL FINDLB(IAP,IRS,NFPR1,FOUND)
       CALL READLB(IAP,U,PAR)
!
       THTA=PI(2.d0)/PAR(11)
       S1=DSIN(THTA)
       C1=DCOS(THTA)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFU,DUMDFP)
!
       NDM2=2*NDM
       DO I=1,NDM2
         DO J=1,NDM2
           SMAT(I,J)=0.d0
         ENDDO
       ENDDO
!
       DO I=1,NDM
         SMAT(I,NDM+I)=S1
       ENDDO
!
       DO I=1,NDM
         SMAT(NDM+I,I)=-S1
       ENDDO
!
       DO I=1,NDM
         DO J=1,NDM
           SMAT(I,J)=DFU((J-1)*NDM+I)
           SMAT(NDM+I,NDM+J)=DFU((J-1)*NDM+I)
         ENDDO
         SMAT(I,I)=SMAT(I,I)-C1
         SMAT(NDM+I,NDM+I)=SMAT(NDM+I,NDM+I)-C1
       ENDDO
       ALLOCATE(IR(2*NDIM),IC(2*NDIM+1))
       CALL NLVC(NDM2,2*NDIM,2,SMAT,V,IR,IC)
       CALL NRMLZ(NDM2,V)
!
       DO I=1,NDM2
         U(NDM+I)=V(I)
       ENDDO
!
       U(NDIM-1)=THTA
       U(NDIM)=PAR(ICP(2))
       DEALLOCATE(DFU,SMAT,F,V,IR,IC)
!
      RETURN
      END SUBROUTINE STPNHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     Subroutines for the Continuation of Hopf Bifurcation Points (ODE)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNHB(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates the equations for the 2-parameter continuation of Hopf
! bifurcation points in ODE.
!
      DIMENSION IAP(*),RAP(*),U(*),UOLD(*),ICP(*),PAR(*)
      DIMENSION F(*),DFDU(NDIM,*),DFDP(NDIM,*)
! Local
      ALLOCATABLE DFU(:),UU1(:),UU2(:),FF1(:),FF2(:)
!
       NDM=IAP(23)
!
! Generate the function.
!
       ALLOCATE(DFU(NDIM*NDIM))
       CALL FFHB(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU)
!
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU)
         RETURN
       ENDIF
!
! Generate the Jacobian.
!
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NDIM),FF2(NDIM))
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U(J)
           UU2(J)=U(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FFHB(IAP,RAP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU)
         CALL FFHB(IAP,RAP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(UU1,UU2,FF2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF1,DFU)
         RETURN
       ENDIF
!
       PAR(ICP(1))=PAR(ICP(1))+EP
!
       CALL FFHB(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU)
!
       DO J=1,NDIM
         DFDP(J,ICP(1))=(FF1(J)-F(J))/EP
       ENDDO
!
       PAR(ICP(1))=PAR(ICP(1))-EP
       DEALLOCATE(FF1,DFU)
!
      RETURN
      END SUBROUTINE FNHB
!
!     ---------- ----
      SUBROUTINE FFHB(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      INTEGER IAP(*),ICP(*)
      DOUBLE PRECISION RAP(*),U(*),UOLD(*),PAR(*),F(*),DFDU(NDM,*)
! Local
      DOUBLE PRECISION DUMDP(1)
!
       NDM2=2*NDM
!
       ROM=U(NDIM-1)
       PAR(11)=ROM*PI(2.d0)
       PAR(ICP(2))=U(NDIM)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DUMDP)
!
       DO I=1,NDM
         F(NDM+I)=U(NDM2+I)
         F(NDM2+I)=-U(NDM+I)
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+ROM*DFDU(I,J)*U(NDM+J)
           F(NDM2+I)=F(NDM2+I)+ROM*DFDU(I,J)*U(NDM2+J)
         ENDDO
       ENDDO
!
       F(NDIM-1)=-1
!
       DO I=1,NDM
         F(NDIM-1)=F(NDIM-1)+U(NDM+I)*U(NDM+I)+U(NDM2+I)*U(NDM2+I)
       ENDDO
!
       F(NDIM)=0.d0
!
       DO I=1,NDM
         F(NDIM)=F(NDIM)+UOLD(NDM2+I)*(U(NDM+I)-UOLD(NDM+I)) - &
              UOLD(NDM+I)*(U(NDM2+I)-UOLD(NDM2+I))
       ENDDO
!
      RETURN
      END SUBROUTINE FFHB
!
!     ---------- ------
      SUBROUTINE STPNHB(IAP,RAP,PAR,ICP,U)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!
      LOGICAL FOUND
!
! Generates starting data for the 2-parameter continuation of
! Hopf bifurcation point (ODE).
!
      DIMENSION U(*),PAR(*),ICP(*),IAP(*),RAP(*)
! Local
      ALLOCATABLE DFU(:),SMAT(:,:),IR(:),IC(:),V(:),F(:)
      DOUBLE PRECISION UOLD(1),DFP(1)
!
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
       ALLOCATE(DFU(NDIM*NDIM),F(NDIM),V(NDIM),SMAT(2*NDIM,2*NDIM))
!
       CALL FINDLB(IAP,IRS,NFPR1,FOUND)
       CALL READLB(IAP,U,PAR)
!
       PERIOD=PAR(11)
       ROM=PERIOD/PI(2.d0)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFU,DFP)
!
       NDM2=2*NDM
       DO I=1,NDM2
         DO J=1,NDM2
           SMAT(I,J)=0.d0
         ENDDO
       ENDDO
!
       DO I=1,NDM
         SMAT(I,NDM+I)=1
       ENDDO
!
       DO I=1,NDM
         SMAT(NDM+I,I)=-1
       ENDDO
!
       DO I=1,NDM
         DO J=1,NDM
           SMAT(I,J)=ROM*DFU((J-1)*NDM+I)
           SMAT(NDM+I,NDM+J)=ROM*DFU((J-1)*NDM+I)
         ENDDO
       ENDDO
       ALLOCATE(IR(2*NDIM),IC(2*NDIM))
       CALL NLVC(NDM2,2*NDIM,2,SMAT,V,IR,IC)
       CALL NRMLZ(NDM2,V)
!
       DO I=1,NDM2
         U(NDM+I)=V(I)
       ENDDO
!
       U(NDIM-1)=ROM
       U(NDIM)=PAR(ICP(2))
!
       DEALLOCATE(DFU,F,V,SMAT,IR,IC)
      RETURN
      END SUBROUTINE STPNHB
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!   Subroutines for the Continuation of Hopf Bifurcation Points (Waves)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNHW(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates the equations for the 2-parameter continuation of a
! bifurcation to a traveling wave.
!
      DIMENSION IAP(*),RAP(*),U(*),UOLD(*),ICP(*),PAR(*)
      DIMENSION F(*),DFDU(NDIM,*),DFDP(NDIM,*)
! Local
      ALLOCATABLE DFU(:),UU1(:),UU2(:),FF1(:),FF2(:)
!
       NDM=IAP(23)
!
! Generate the function.
!
       ALLOCATE(DFU(NDIM*NDIM))
       CALL FFHW(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU)
!
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU)
         RETURN
       ENDIF
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NDIM),FF2(NDIM))
!
! Generate the Jacobian.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U(J)
           UU2(J)=U(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FFHW(IAP,RAP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU)
         CALL FFHW(IAP,RAP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(UU1,UU2,FF2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF1,DFU)
         RETURN
       ENDIF
!
       PAR(ICP(1))=PAR(ICP(1))+EP
!
       CALL FFHW(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU)
!
       DO J=1,NDIM
         DFDP(J,ICP(1))=(FF1(J)-F(J))/EP
       ENDDO
!
       PAR(ICP(1))=PAR(ICP(1))-EP
!
      DEALLOCATE(FF1,DFU)
      RETURN
      END SUBROUTINE FNHW
!
!     ---------- ----
      SUBROUTINE FFHW(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),RAP(*)
      DIMENSION U(*),UOLD(*),ICP(*),PAR(*),F(*),DFDU(NDM,*)
! Local
      DOUBLE PRECISION DUMDP(1)
!
       NDM2=2*NDM
!
       ROM=U(NDIM-1)
       PAR(ICP(2))=U(NDIM)
       IJAC=1
       CALL FNWS(IAP,RAP,NDM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DUMDP)
!
       DO I=1,NDM
         F(NDM+I)=U(NDM2+I)
         F(NDM2+I)=-U(NDM+I)
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+ROM*DFDU(I,J)*U(NDM+J)
           F(NDM2+I)=F(NDM2+I)+ROM*DFDU(I,J)*U(NDM2+J)
         ENDDO
       ENDDO
!
       F(NDIM-1)=-1
!
       DO I=1,NDM
         F(NDIM-1)=F(NDIM-1)+U(NDM+I)*U(NDM+I)+U(NDM2+I)*U(NDM2+I)
       ENDDO
!
       F(NDIM)=0.d0
!
       DO I=1,NDM
         F(NDIM)=F(NDIM)+UOLD(NDM2+I)*(U(NDM+I)-UOLD(NDM+I)) - &
              UOLD(NDM+I)*(U(NDM2+I)-UOLD(NDM2+I))
       ENDDO
!
      RETURN
      END SUBROUTINE FFHW
!
!     ---------- ------
      SUBROUTINE STPNHW(IAP,RAP,PAR,ICP,U)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      LOGICAL FOUND
!
! Generates starting data for the continuation of a bifurcation to a
! traveling wave.
!
      DIMENSION U(*),PAR(*),ICP(*),IAP(*),RAP(*)
! Local (Cannot use BLLOC here.)
      ALLOCATABLE DFU(:),SMAT(:,:),IR(:),IC(:),V(:),F(:)
      DOUBLE PRECISION DUMDFP(1),UOLD(1)
!
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
       ALLOCATE(DFU(NDIM*NDIM),F(NDIM),V(NDIM),SMAT(2*NDIM,2*NDIM))
!
       CALL FINDLB(IAP,IRS,NFPR1,FOUND)
       CALL READLB(IAP,U,PAR)
!
       IJAC=1
       PERIOD=PAR(11)
       ROM=PERIOD/PI(2.d0)
       CALL FNWS(IAP,RAP,NDM,U,UOLD,ICP,PAR,IJAC,F,DFU,DUMDFP)
!
       NDM2=2*NDM
       DO I=1,NDM2
         DO J=1,NDM2
           SMAT(I,J)=0.d0
         ENDDO
       ENDDO
!
       DO I=1,NDM
         SMAT(I,NDM+I)=1
       ENDDO
!
       DO I=1,NDM
         SMAT(NDM+I,I)=-1
       ENDDO
!
       DO I=1,NDM
         DO J=1,NDM
           SMAT(I,J)=ROM*DFU((J-1)*NDM+I)
           SMAT(NDM+I,NDM+J)=ROM*DFU((J-1)*NDM+I)
         ENDDO
       ENDDO
       ALLOCATE(IR(2*NDIM),IC(2*NDIM))
       CALL NLVC(NDM2,2*NDIM,2,SMAT,V,IR,IC)
       CALL NRMLZ(NDM2,V)
!
       DO I=1,NDM2
         U(NDM+I)=V(I)
       ENDDO
!
       U(NDIM-1)=ROM
       U(NDIM)=PAR(ICP(2))
!
       DEALLOCATE(DFU,F,V,SMAT,IR,IC)
      RETURN
      END SUBROUTINE STPNHW
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!          Periodic Solutions and Fixed Period Orbits
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNPS(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates the equations for the continuation of periodic orbits.
!
      INTEGER IAP(*),ICP(*)
      DOUBLE PRECISION RAP(*),U(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
      DOUBLE PRECISION UOLD(*)
!
! Generate the function.
!
       IF(ICP(2).EQ.11)THEN
!          **Variable period continuation
           CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
           PERIOD=PAR(11)
           DO I=1,NDIM
             DFDP(I,11)=F(I)
             F(I)=PERIOD*DFDP(I,11)
           ENDDO
           IF(IJAC.EQ.0)RETURN
!          **Generate the Jacobian.
           DO I=1,NDIM
             DO J=1,NDIM
               DFDU(I,J)=PERIOD*DFDU(I,J)
             ENDDO
             DFDP(I,ICP(1))=PERIOD*DFDP(I,ICP(1))
           ENDDO
       ELSE
!          **Fixed period continuation
           PERIOD=PAR(11)
           CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
           DO I=1,NDIM
             F(I)=PERIOD*F(I)
           ENDDO
           IF(IJAC.EQ.0)RETURN
!          **Generate the Jacobian.
           DO I=1,NDIM
             DO J=1,NDIM
               DFDU(I,J)=PERIOD*DFDU(I,J)
             ENDDO
             DO J=1,2
               DFDP(I,ICP(J))=PERIOD*DFDP(I,ICP(J))
             ENDDO
           ENDDO
       ENDIF
!
      RETURN
      END SUBROUTINE FNPS
!
!     ---------- ----
      SUBROUTINE BCPS(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      POINTER NRTN(:)
      COMMON /BLRTN/ NRTN,IRTN
!
      DIMENSION PAR(*),ICP(*),U0(*),U1(*),F(*),DBC(NBC,*)
!
       DO I=1,NDIM
         F(I)=U0(I)-U1(I)
       ENDDO
!
! Rotations
       IF(IRTN.NE.0)THEN
         DO I=1,NDIM
           IF(NRTN(I).NE.0)F(I)=F(I) + PAR(19)*NRTN(I)
         ENDDO
       ENDIF
!
       IF(IJAC.EQ.0)RETURN
!
       NN=2*NDIM+NPARX
       DO I=1,NBC
         DO J=1,NN
           DBC(I,J)=0.d0
         ENDDO
       ENDDO
!
       DO I=1,NDIM
         DBC(I,I)=1
         DBC(I,NDIM+I)=-1
       ENDDO
!
      RETURN
      END SUBROUTINE BCPS
!
!     ---------- ----
      SUBROUTINE ICPS(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD, &
           F,IJAC,DINT)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
      DIMENSION ICP(*),PAR(*)
!
       F(1)=0.d0
       DO I=1,NDIM
         F(1)=F(1)+(U(I)-UOLD(I))*UPOLD(I)
       ENDDO
!
       IF(IJAC.EQ.0)RETURN
!
       NN=NDIM+NPARX
       DO I=1,NN
         DINT(1,I)=0.d0
       ENDDO
!
       DO I=1,NDIM
          DINT(1,I)=UPOLD(I)
       ENDDO
!
      RETURN
      END SUBROUTINE ICPS
!
!     ---------- -----
      SUBROUTINE PDBLE(NDIM,NTST,NCOL,NDX,UPS,UDOTPS,TM,PAR)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Preprocesses restart data for switching branches at a period doubling
!
!
      POINTER NRTN(:)
      COMMON /BLRTN/ NRTN,IRTN
      DIMENSION TM(*),UPS(NDX,*),UDOTPS(NDX,*),PAR(*)
!
       PAR(11)=2.d0*PAR(11)
       IF(IRTN.NE.0)PAR(19)=2.d0*PAR(19)
!
       DO I=1,NTST
         TM(I)=.5d0*TM(I)
         TM(NTST+I)=.5d0+TM(I)
       ENDDO
!
       TM(2*NTST+1)=1
!
       DO J=1,NTST+1
         DO I1=1,NDIM
          DO I2=1,NCOL
           I=(I2-1)*NDIM+I1
              UPS(I,NTST+J)=   UPS(I1,NTST+1)+   UPS(I,J)-   UPS(I1,1)
           UDOTPS(I,NTST+J)=UDOTPS(I1,NTST+1)+UDOTPS(I,J)-UDOTPS(I1,1)
          ENDDO
         ENDDO
       ENDDO
!
       NTST=2*NTST
!
      RETURN
      END SUBROUTINE PDBLE
!
!     ---------- ------
      SUBROUTINE STPNPS(IAP,RAP,PAR,ICP,NTSR,NCOLRS, &
           RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates starting data for the continuation of a branch of periodic
! solutions from a Hopf bifurcation point.
!
      DIMENSION PAR(*),ICP(*),IAP(*),RAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*),TM(*),DTM(*)
! Local
      ALLOCATABLE DFU(:),SMAT(:,:),IR(:),IC(:),RNLLV(:),F(:),U(:)
      DOUBLE PRECISION DUMDFP(1),UOLD(1)
!
      LOGICAL FOUND
!
       NDIM=IAP(1)
       IRS=IAP(3)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
       ALLOCATE(DFU(NDIM*NDIM),F(NDIM),U(NDIM),RNLLV(2*NDIM))
       ALLOCATE(SMAT(2*NDIM,2*NDIM))
!
       CALL FINDLB(IAP,IRS,NFPR1,FOUND)
       CALL READLB(IAP,U,PAR)
!
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
!
       PERIOD=PAR(11)
       TPI=PI(2.d0)
       RIMHB=TPI/PERIOD
       NTSR=NTST
       NCOLRS=NCOL
!
       NDIM2=2*NDIM
       DO I=1,NDIM2
         DO J=1,NDIM2
           SMAT(I,J)=0.d0
         ENDDO
       ENDDO
!
       DO I=1,NDIM
         SMAT(I,I)=-RIMHB
         SMAT(NDIM+I,NDIM+I)=RIMHB
       ENDDO
!
       CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,1,F,DFU,DUMDFP)
!
       DO I=1,NDIM
         DO J=1,NDIM
           SMAT(I,NDIM+J)=DFU((J-1)*NDIM+I)
           SMAT(NDIM+I,J)=DFU((J-1)*NDIM+I)
         ENDDO
       ENDDO
!
       ALLOCATE(IR(2*NDIM),IC(2*NDIM))
       CALL NLVC(NDIM2,NDIM2,2,SMAT,RNLLV,IR,IC)
       CALL NRMLZ(NDIM2,RNLLV)
!
! Generate the (initially uniform) mesh.
!
       CALL MSH(NTST,TM)
       DT=1.d0/NTST
!
       DO J=1,NTST+1
         T=TM(J)
         S=DSIN(TPI*T)
         C=DCOS(TPI*T)
         DO K=1,NDIM
           UDOTPS(K,J)=S*RNLLV(K)+C*RNLLV(NDIM+K)
           UPOLDP(K,J)=C*RNLLV(K)-S*RNLLV(NDIM+K)
           UPS(K,J)=U(K)
         ENDDO
       ENDDO
!
       DO I=1,NCOL-1
         DO J=1,NTST
           T=TM(J)+I*( TM(J+1)-TM(J) )/NCOL
           S=DSIN(TPI*T)
           C=DCOS(TPI*T)
           DO  K=1,NDIM
             K1=I*NDIM+K
             UDOTPS(K1,J)=S*RNLLV(K)+C*RNLLV(NDIM+K)
             UPOLDP(K1,J)=C*RNLLV(K)-S*RNLLV(NDIM+K)
             UPS(K1,J)=U(K)
           ENDDO
         ENDDO
       ENDDO
!
       RLDOT(1)=0.d0
       RLDOT(2)=0.d0
!
       DO I=1,NTST
         DTM(I)=DT
       ENDDO
!
       CALL SCALEB(IAP,NDIM,NDX,UDOTPS,RLDOT,DTM,THL,THU)
!
       NODIR=-1
!
       DEALLOCATE(DFU,F,U,RNLLV,SMAT,IR,IC)
      RETURN
      END SUBROUTINE STPNPS
!
!     ---------- ------
      SUBROUTINE STPNPB(IAP,RAP,PAR,ICP,NTSR,NCOLRS, &
           RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates starting data for the continuation of a branch of periodic
! solutions from a Hopf bifurcation point, when the user has supplied
! BCND, ICND, and period-scaled F in FUNC.
! The difference with STPNPS is that the user period-scaling of F must
! be taken into account.
!
      DIMENSION PAR(*),ICP(*),IAP(*),RAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*),TM(*),DTM(*)
! Local
      ALLOCATABLE DFU(:),SMAT(:,:),IR(:),IC(:),RNLLV(:),F(:),U(:)
      DOUBLE PRECISION UOLD(1),DUMDFP(1)
!
      LOGICAL FOUND
!
       NDIM=IAP(1)
       IRS=IAP(3)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
       ALLOCATE(DFU(NDIM*NDIM),F(NDIM),U(NDIM),RNLLV(2*NDIM))
       ALLOCATE(SMAT(2*NDIM,2*NDIM))
!
       CALL FINDLB(IAP,IRS,NFPR1,FOUND)
       CALL READLB(IAP,U,PAR)
!
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
!
       PERIOD=PAR(11)
       TPI=PI(2.d0)
       RIMHB=TPI/PERIOD
       NTSR=NTST
       NCOLRS=NCOL
!
       NDIM2=2*NDIM
       DO I=1,NDIM2
         DO J=1,NDIM2
           SMAT(I,J)=0.d0
         ENDDO
       ENDDO
!
       DO I=1,NDIM
         SMAT(I,I)=-RIMHB
         SMAT(NDIM+I,NDIM+I)=RIMHB
       ENDDO
!
       CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,1,F,DFU,DUMDFP)
!
       DO I=1,NDIM
         DO J=1,NDIM
! Note that the user period-scaling in FUNC is taken into account:
           SMAT(I,NDIM+J)=DFU((J-1)*NDIM+I)/PAR(11)
           SMAT(NDIM+I,J)=DFU((J-1)*NDIM+I)/PAR(11)
         ENDDO
       ENDDO
!
       ALLOCATE(IR(NDIM2),IC(NDIM2))
       CALL NLVC(NDIM2,NDIM2,2,SMAT,RNLLV,IR,IC)
       CALL NRMLZ(NDIM2,RNLLV)
!
! Generate the (initially uniform) mesh.
!
       CALL MSH(NTST,TM)
       DT=1.d0/NTST
!
       DO J=1,NTST+1
         T=TM(J)
         S=DSIN(TPI*T)
         C=DCOS(TPI*T)
         DO K=1,NDIM
           UDOTPS(K,J)=S*RNLLV(K)+C*RNLLV(NDIM+K)
           UPOLDP(K,J)=C*RNLLV(K)-S*RNLLV(NDIM+K)
           UPS(K,J)=U(K)
         ENDDO
       ENDDO
!
       DO I=1,NCOL-1
         DO J=1,NTST
           T=TM(J)+I*( TM(J+1)-TM(J) )/NCOL
           S=DSIN(TPI*T)
           C=DCOS(TPI*T)
           DO  K=1,NDIM
             K1=I*NDIM+K
             UDOTPS(K1,J)=S*RNLLV(K)+C*RNLLV(NDIM+K)
             UPOLDP(K1,J)=C*RNLLV(K)-S*RNLLV(NDIM+K)
             UPS(K1,J)=U(K)
           ENDDO
         ENDDO
       ENDDO
!
       RLDOT(1)=0.d0
       RLDOT(2)=0.d0
!
       DO I=1,NTST
         DTM(I)=DT
       ENDDO
!
       CALL SCALEB(IAP,NDIM,NDX,UDOTPS,RLDOT,DTM,THL,THU)
!
       NODIR=-1
!
       DEALLOCATE(DFU,F,U,RNLLV,SMAT,IR,IC)
      RETURN
      END SUBROUTINE STPNPB
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!          Travelling Wave Solutions to Parabolic PDEs
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNWS(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Sets up equations for the continuation of spatially homogeneous
! solutions to parabolic systems, for the purpose of finding
! bifurcations to travelling wave solutions.
!
      DIMENSION IAP(*),U(*),ICP(*),PAR(*)
      DOUBLE PRECISION RAP(*),UOLD(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(*),DFDU(NDIM,*),DFDP(NDIM,*)
! Local
      ALLOCATABLE DFU(:,:),DFP(:,:)
!
       NDM=IAP(23)
!
! Generate the function.
!
       NDM=NDM/2
!
       IF(IJAC.NE.0)THEN
         ALLOCATE(DFU(NDM,NDM))
         IF(IJAC.NE.1)THEN
           ALLOCATE(DFP(NDM,NPARX))
         ENDIF
       ENDIF
!
       NFPR=IAP(29)
!
       C=PAR(10)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,IJAC,F,DFU,DFP)
!
       DO I=1,NDM
         F(NDM+I)=-( C*U(NDM+I) + F(I) )/PAR(14+I)
         F(I)=U(NDM+I)
       ENDDO
!
       IF(IJAC.EQ.0)RETURN
!
       DO I=1,NDM
         DO J=1,NDM
           DFDU(I,J)        =0.d0
           DFDU(I,J+NDM)    =0.d0
           DFDU(I+NDM,J)    =-DFU(I,J)/PAR(14+I)
           DFDU(I+NDM,J+NDM)=0.d0
         ENDDO
         DFDU(I,I+NDM)     =1
         DFDU(I+NDM,I+NDM)=-C/PAR(14+I)
       ENDDO
!
       DEALLOCATE(DFU)
       IF(IJAC.EQ.1)RETURN
!
       DO I=1,NDM
         IF(ICP(1).LT.10)THEN
           DFDP(I,ICP(1))    =0.d0
           DFDP(I+NDM,ICP(1))=-DFP(I,ICP(1))/PAR(14+I)
         ENDIF
         IF(NFPR.GT.1.AND.ICP(2).LT.10)THEN
           DFDP(I,ICP(2))    =0.d0
           DFDP(I+NDM,ICP(2))=-DFP(I,ICP(2))/PAR(14+I)
         ENDIF
       ENDDO
!
! Derivative with respect to the wave speed.
!
       DO I=1,NDM
         DFDP(I,10)    =0.d0
         DFDP(I+NDM,10)=-U(NDM+I)/PAR(14+I)
       ENDDO
!
! Derivatives with respect to the diffusion coefficients.
!
       DO J=1,NDM
         DO I=1,NDM
           DFDP(I,14+J)    =0.d0
           DFDP(I+NDM,14+J)=0.d0
         ENDDO
         DFDP(J+NDM,14+J)=-F(J+NDM)/PAR(14+J)
       ENDDO
!
      DEALLOCATE(DFP)
      RETURN
      END SUBROUTINE FNWS
!
!     ---------- ----
      SUBROUTINE FNWP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Equations for the continuation of traveling waves.
!
      DIMENSION IAP(*),U(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*),ICP(*)
      DOUBLE PRECISION RAP(*),UOLD(*)
!
! Generate the function and Jacobian.
!
       IF(ICP(2).EQ.11)THEN
!          **Variable wave length
           CALL FNWS(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
           PERIOD=PAR(11)
           DO I=1,NDIM
             DFDP(I,11)=F(I)
             F(I)=PERIOD*F(I)
           ENDDO
           IF(IJAC.EQ.0)RETURN
           DO I=1,NDIM
             DO J=1,NDIM
               DFDU(I,J)=PERIOD*DFDU(I,J)
             ENDDO
           ENDDO
           DO I=1,NDIM
             DFDP(I,ICP(1))=PERIOD*DFDP(I,ICP(1))
           ENDDO
       ELSE
!          **Fixed wave length
           CALL FNWS(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
           PERIOD=PAR(11)
           DO I=1,NDIM
             F(I)=PERIOD*F(I)
           ENDDO
           IF(IJAC.EQ.0)RETURN
           DO I=1,NDIM
             DO J=1,NDIM
               DFDU(I,J)=PERIOD*DFDU(I,J)
             ENDDO
           ENDDO
           DO I=1,NDIM
             DO J=1,2
               DFDP(I,ICP(J))=PERIOD*DFDP(I,ICP(J))
             ENDDO
           ENDDO
       ENDIF
!
      RETURN
      END SUBROUTINE FNWP
!
!     ---------- ------
      SUBROUTINE STPNWP(IAP,RAP,PAR,ICP,NTSR,NCOLRS, &
           RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!
! Generates starting data for the continuation of a branch of periodic
! solutions starting from a Hopf bifurcation point (Waves).
!
      DIMENSION PAR(*),ICP(*),IAP(*),RAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*),TM(*),DTM(*)
! Local
      ALLOCATABLE DFU(:),SMAT(:,:),IR(:),IC(:),RNLLV(:),F(:),U(:)
      DOUBLE PRECISION DUMDFP(1),UOLD(1)
!
      LOGICAL FOUND
!
       NDIM=IAP(1)
       IRS=IAP(3)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
       ALLOCATE(DFU(NDIM*NDIM),F(NDIM),U(NDIM),RNLLV(2*NDIM))
       ALLOCATE(SMAT(2*NDIM,2*NDIM))
!
       CALL FINDLB(IAP,IRS,NFPR1,FOUND)
       CALL READLB(IAP,U,PAR)
!
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
!
       PERIOD=PAR(11)
       TPI=PI(2.d0)
       RIMHB=TPI/PERIOD
       NTSR=NTST
       NCOLRS=NCOL
!
       NDIM2=2*NDIM
       DO I=1,NDIM2
         DO J=1,NDIM2
           SMAT(I,J)=0.d0
         ENDDO
       ENDDO
!
       DO I=1,NDIM
         SMAT(I,I)=-RIMHB
         SMAT(NDIM+I,NDIM+I)=RIMHB
       ENDDO
!
       IJAC=1
       CALL FNWS(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFU,DUMDFP)
!
       DO I=1,NDIM
         DO J=1,NDIM
           SMAT(I,NDIM+J)=DFU((J-1)*NDIM+I)
           SMAT(NDIM+I,J)=DFU((J-1)*NDIM+I)
         ENDDO
       ENDDO
!
       ALLOCATE(IR(NDIM2),IC(NDIM2))
       CALL NLVC(NDIM2,NDIM2,2,SMAT,RNLLV,IR,IC)
       CALL NRMLZ(NDIM2,RNLLV)
!
! Generate the (initially uniform) mesh.
!
       CALL MSH(NTST,TM)
       DT=1.d0/NTST
!
       DO J=1,NTST+1
         T=TM(J)
         S=DSIN(TPI*T)
         C=DCOS(TPI*T)
         DO K=1,NDIM
           UDOTPS(K,J)=S*RNLLV(K)+C*RNLLV(NDIM+K)
           UPOLDP(K,J)=C*RNLLV(K)-S*RNLLV(NDIM+K)
           UPS(K,J)=U(K)
         ENDDO
       ENDDO
!
       DO I=1,NCOL-1
         DO J=1,NTST
           T=TM(J)+I*( TM(J+1)-TM(J) )/NCOL
           S=DSIN(TPI*T)
           C=DCOS(TPI*T)
           DO K=1,NDIM
             K1=I*NDIM+K
             UDOTPS(K1,J)=S*RNLLV(K)+C*RNLLV(NDIM+K)
             UPOLDP(K1,J)=C*RNLLV(K)-S*RNLLV(NDIM+K)
             UPS(K1,J)=U(K)
           ENDDO
         ENDDO
       ENDDO
!
       RLDOT(1)=0.d0
       RLDOT(2)=0.d0
!
       DO I=1,NTST
         DTM(I)=DT
       ENDDO
!
       CALL SCALEB(IAP,NDIM,NDX,UDOTPS,RLDOT,DTM,THL,THU)
!
       NODIR=-1
!
       DEALLOCATE(DFU,F,U,RNLLV,SMAT,IR,IC)
      RETURN
      END SUBROUTINE STPNWP
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!             Parabolic PDEs : Stationary States
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNSP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!
! Generates the equations for taking one time step (Implicit Euler).
!
      DIMENSION IAP(*),RAP(*),U(*),UOLD(*),ICP(*),PAR(*)
      DIMENSION F(*),DFDU(NDIM,*),DFDP(NDIM,*)
! Local
      ALLOCATABLE DFU(:,:),DFP(:,:)
!
       NDM=IAP(23)
!
! Generate the function and Jacobian.
!
       IF(IJAC.NE.0)THEN
         ALLOCATE(DFU(NDM,NDM))
         IF(IJAC.NE.1)THEN
           ALLOCATE(DFP(NDM,NPARX))
         ENDIF
       ENDIF
!
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,IJAC,F(NDM+1),DFU,DFP)
!
       PERIOD=PAR(11)
       DO I=1,NDM
         F(I)    = PERIOD*U(NDM+I)
         F(NDM+I)=-PERIOD*F(NDM+I)/PAR(14+I)
       ENDDO
!
       IF(IJAC.EQ.0)RETURN
!
       DO I=1,NDM
         DO J=1,NDM
           DFDU(I,J)        =0.d0
           DFDU(I,J+NDM)    =0.d0
           DFDU(I+NDM,J)    =-PERIOD*DFU(I,J)/PAR(14+I)
           DFDU(I+NDM,J+NDM)=0.d0
         ENDDO
         DFDU(I,I+NDM)     =PERIOD
       ENDDO
       DEALLOCATE(DFU)
       IF(IJAC.EQ.1)RETURN
       DO I=1,NDM
         IF(ICP(1).EQ.11)THEN
           DFDP(I,ICP(1))    = F(I)/PERIOD
           DFDP(NDM+I,ICP(1))= F(NDM+I)/PERIOD
         ELSEIF(ICP(1).EQ.14+I)THEN
           DFDP(I,ICP(1))    = 0.d0
           DFDP(NDM+I,ICP(1))=-F(NDM+I)/PAR(14+I)
         ELSEIF(ICP(1).NE.11 .AND.  &
              .NOT. (ICP(1).GT.14 .AND. ICP(1).LE.14+NDM) )THEN
           DFDP(I,ICP(1))    =0.d0
           DFDP(I+NDM,ICP(1))=-PERIOD*DFP(I,ICP(1))/PAR(14+I)
         ENDIF
       ENDDO
!
      DEALLOCATE(DFP)
      RETURN
      END SUBROUTINE FNSP
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!            Time Evolution of Parabolic PDEs
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNPE(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!
! Generates the equations for taking one time step (Implicit Euler).
!
      DIMENSION IAP(*),RAP(*),U(*),UOLD(*),ICP(*),PAR(*)
      DIMENSION F(*),DFDU(NDIM,*),DFDP(NDIM,*)
! Local
      ALLOCATABLE DFU(:,:)
      DOUBLE PRECISION DUMDFP(1)
!
       NDM=IAP(23)
!
! Generate the function and Jacobian.
!
       IF(IJAC.NE.0)THEN
         ALLOCATE(DFU(NDM,NDM))
       ENDIF
!
       DS=RAP(1)
       DSMIN=RAP(2)
!
       PERIOD=PAR(11)
       T=PAR(ICP(1))
       RLOLD=RAP(15)
       DT=T-RLOLD
       IF(DABS(DT).LT.DSMIN)DT=DS
!
       IIJAC=IJAC
       IF(IJAC.GT.1)IIJAC=1
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,IIJAC,F(NDM+1),DFU,DUMDFP)
!
       DO I=1,NDM
         F(I)=PERIOD*U(NDM+I)
         F(NDM+I)=PERIOD*( (U(I)-UOLD(I))/DT - F(NDM+I) )/PAR(14+I)
       ENDDO
!
       IF(IJAC.EQ.0)RETURN
!
       DO I=1,NDM
         DO J=1,NDM
           DFDU(I,J)        =0.d0
           DFDU(I,J+NDM)    =0.d0
           DFDU(I+NDM,J)    =-PERIOD*DFU(I,J)/PAR(14+I)
           DFDU(I+NDM,J+NDM)=0.d0
         ENDDO
         DFDU(I,I+NDM)     =PERIOD
         DFDU(I+NDM,I)     =DFDU(I+NDM,I) + PERIOD/(DT*PAR(14+I))
       ENDDO
       DEALLOCATE(DFU)
       IF(IJAC.EQ.1)RETURN
!
       DO I=1,NDM
         DFDP(I,ICP(1))    =0.d0
         DFDP(I+NDM,ICP(1))=-PERIOD*(U(I)-UOLD(I))/(DT**2*PAR(14+I))
       ENDDO
!
      RETURN
      END SUBROUTINE FNPE
!
!     ---------- ----
      SUBROUTINE ICPE(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD, &
           F,IJAC,DINT)
!
! Dummy integral condition subroutine for parabolic systems.
!
      RETURN
      END SUBROUTINE ICPE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!    Subroutines for the Continuation of Folds for Periodic Solution
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNPL(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
      DIMENSION IAP(*)
      DIMENSION U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
      DOUBLE PRECISION RAP(*),UOLD(*)
! Local
      ALLOCATABLE DFU(:),DFP(:),UU1(:),UU2(:),FF1(:),FF2(:)
!
       NDM=IAP(23)
       NFPR=IAP(29)
!
! Generate the function.
!
       ALLOCATE(DFU(NDM*NDM),DFP(NDM*NPARX))
       CALL FFPL(IAP,RAP,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
!
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU,DFP)
         RETURN
       ENDIF
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NDIM),FF2(NDIM))
!
! Generate the Jacobian.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U(J)
           UU2(J)=U(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FFPL(IAP,RAP,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         CALL FFPL(IAP,RAP,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(UU1,UU2,FF2)
       IF (IJAC.EQ.1)THEN
         DEALLOCATE(DFU,DFP,FF1)
         RETURN
       ENDIF
!
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FFPL(IAP,RAP,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
      ENDDO
!
      DEALLOCATE(DFU,DFP,FF1)
      RETURN
      END SUBROUTINE FNPL
!
!     ---------- ----
      SUBROUTINE FFPL(IAP,RAP,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
      DOUBLE PRECISION RAP(*),UOLD(*)
!
       PERIOD=PAR(11)
       BETA=PAR(12)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
!
       DO I=1,NDM
         F(NDM+I)=0.d0
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
         ENDDO
           IF(ICP(3).EQ.11)THEN
!            ** Variable period
           F(NDM+I)=PERIOD*F(NDM+I)+BETA*F(I)
         ELSE
!            ** Fixed period
           F(NDM+I)=PERIOD*F(NDM+I)+BETA*DFDP(I,ICP(2))
         ENDIF
         F(I)=PERIOD*F(I)
       ENDDO
!
      RETURN
      END SUBROUTINE FFPL
!
!     ---------- ----
      SUBROUTINE BCPL(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
!
! Boundary conditions for continuing folds (Periodic solutions)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      POINTER NRTN(:)
      COMMON /BLRTN/ NRTN,IRTN
!
      DIMENSION IAP(*),PAR(*),ICP(*),U0(*),U1(*),F(*),DBC(NBC,*)
!
       DO I=1,NDIM
         F(I)=U0(I)-U1(I)
       ENDDO
!
! Rotations
       IF(IRTN.NE.0)THEN
         NDM=IAP(23)
         DO I=1,NDM
           IF(NRTN(I).NE.0)F(I)=F(I) + PAR(19)*NRTN(I)
         ENDDO
       ENDIF
!
       IF(IJAC.EQ.0)RETURN
!
       NN=2*NDIM+NPARX
       DO I=1,NBC
         DO J=1,NN
           DBC(I,J)=0.d0
         ENDDO
       ENDDO
!
       DO I=1,NDIM
         DBC(I,I)=1
         DBC(I,NDIM+I)=-1
       ENDDO
!
      RETURN
      END SUBROUTINE BCPL
!
!     ---------- ----
      SUBROUTINE ICPL(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD, &
           F,IJAC,DINT)
!
! Integral conditions for continuing folds (Periodic solutions)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
!
       NDM=IAP(23)
!
       F(1)=0.d0
       F(2)=0.d0
       F(3)=PAR(12)**2 - PAR(13)
!
       DO I=1,NDM
         F(1)=F(1)+(U(I)-UOLD(I))*UPOLD(I)
         F(2)=F(2)+U(NDM+I)*UPOLD(I)
         F(3)=F(3)+U(NDM+I)*U(NDM+I)
       ENDDO
!
       IF(IJAC.EQ.0)RETURN
!
       NN=NDIM+NPARX
       DO I=1,NINT
         DO J=1,NN
           DINT(I,J)=0.d0
         ENDDO
       ENDDO
!
       DO I=1,NDM
         DINT(1,I)=UPOLD(I)
         DINT(2,NDM+I)=UPOLD(I)
         DINT(3,NDM+I)=2.d0*U(NDM+I)
       ENDDO
!
       DINT(3,NDIM+12)=2.d0*PAR(12)
       DINT(3,NDIM+13)=-1.d0
!
      RETURN
      END SUBROUTINE ICPL
!
!     ---------- ------
      SUBROUTINE STPNPL(IAP,RAP,PAR,ICP,NTSR,NCOLRS, &
           RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!
! Generates starting data for the 2-parameter continuation of folds
! on a branch of periodic solutions.
!
      DIMENSION PAR(*),ICP(*),IAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*),TM(*),DTM(*)
! Local
      DIMENSION ICPRS(NPARX),RLDOTRS(NPARX)
!
      LOGICAL FOUND
!
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
       NFPR=IAP(29)
!
       CALL FINDLB(IAP,IRS,NFPR1,FOUND)
       CALL READBV(IAP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOTRS,UPS,UDOTPS, &
            TM,ITPRS,NDX)
!
! Complement starting data
         PAR(12)=0.d0
         PAR(13)=0.d0
         IF(ICP(3).EQ.11)THEN
!          Variable period
           RLDOT(1)=RLDOTRS(1)
           RLDOT(2)=0.d0
           RLDOT(3)=RLDOTRS(2)
           RLDOT(4)=0.d0
!          Variable period
         ELSE
!          Fixed period
           RLDOT(1)=RLDOTRS(1)
           RLDOT(2)=RLDOTRS(2)
           RLDOT(3)=0.d0
           RLDOT(4)=0.d0
         ENDIF
         DO J=1,NTSR
           DO I=1,NCOLRS
             K1=(I-1)*NDIM+NDM+1
             K2=I*NDIM
             DO K=K1,K2
               UPS(K,J)=0.d0
               UDOTPS(K,J)=0.d0
             ENDDO
           ENDDO
         ENDDO
         K1=NDM+1
         NRSP1=NTSR+1
         DO K=K1,NDIM
           UPS(K,NRSP1)=0.d0
           UDOTPS(K,NRSP1)=0.d0
         ENDDO
!
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
!
       NODIR=0
!
      RETURN
      END SUBROUTINE STPNPL
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!   Subroutines for BP cont (Periodic Solutions) (by F. Dercole)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- -----
      SUBROUTINE FNPBP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
      DIMENSION IAP(*)
      DIMENSION U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
      DOUBLE PRECISION RAP(*),UOLD(*)
! Local
      ALLOCATABLE DFU(:),DFP(:),UU1(:),UU2(:),FF1(:),FF2(:)
!
       NDM=IAP(23)
       NFPR=IAP(29)
!
! Generate the function.
!
       ALLOCATE(DFU(NDM*NDM),DFP(NDM*NPARX))
       CALL FFPBP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
!
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU,DFP)
         RETURN
       ENDIF
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NDIM),FF2(NDIM))
!
! Generate the Jacobian.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U(J)
           UU2(J)=U(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FFPBP(IAP,RAP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         CALL FFPBP(IAP,RAP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(UU1,UU2,FF2)
       IF (IJAC.EQ.1)THEN
         DEALLOCATE(DFU,DFP,FF1)
         RETURN
       ENDIF
!
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FFPBP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
      ENDDO
!
      DEALLOCATE(DFU,DFP,FF1)
      RETURN
      END SUBROUTINE FNPBP
!
!     ---------- -----
      SUBROUTINE FFPBP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
      DOUBLE PRECISION RAP(*),UOLD(*)
! Local
      DOUBLE PRECISION DUM(1),UPOLD(NDM)
!
       PERIOD=PAR(11)
       ISW=IAP(10)
!
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
       CALL FUNI(IAP,RAP,NDM,UOLD,UOLD,ICP,PAR,0,UPOLD,DUM,DUM)
!
       IF(ISW.GT.0) THEN
!        ** restart 1 or 2
         DO I=1,NDM
           F(NDM+I)=0.d0
           DO J=1,NDM
             F(NDM+I)=F(NDM+I)-DFDU(J,I)*U(NDM+J)
           ENDDO
           F(NDM+I)=PERIOD*(F(NDM+I)+UPOLD(I)*PAR(16))
         ENDDO
       ELSE
!        ** start
         DO I=1,NDM
           F(NDM+I)=0.d0
           F(2*NDM+I)=0.d0
           F(3*NDM+I)=0.d0
           DO J=1,NDM
             F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
             F(2*NDM+I)=F(2*NDM+I)+DFDU(I,J)*U(2*NDM+J)
             F(3*NDM+I)=F(3*NDM+I)-DFDU(J,I)*U(3*NDM+J)
           ENDDO
           F(NDM+I)=PERIOD*(F(NDM+I)+DFDP(I,ICP(1))*PAR(12))
           F(2*NDM+I)=PERIOD*(F(2*NDM+I)+DFDP(I,ICP(1))*PAR(14))
           F(3*NDM+I)=PERIOD*(F(3*NDM+I)+UPOLD(I)*PAR(16))+ &
                PAR(20)*U(NDM+I)+PAR(21)*U(2*NDM+I)
           IF(ICP(4).EQ.11)THEN
!            ** Variable period
             F(NDM+I)=F(NDM+I)+F(I)*PAR(13)
             F(2*NDM+I)=F(2*NDM+I)+F(I)*PAR(15)
           ELSE
!            ** Fixed period
             F(NDM+I)=F(NDM+I)+PERIOD*DFDP(I,ICP(2))*PAR(13)
             F(2*NDM+I)=F(2*NDM+I)+PERIOD*DFDP(I,ICP(2))*PAR(15)
           ENDIF
         ENDDO
       ENDIF
!
       IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
!        ** Non-generic and/or start
         DO I=1,NDM
           F(I)=PERIOD*F(I)-PAR(18)*U(NDIM-NDM+I)
         ENDDO
       ELSE
!        ** generic and restart
         DO I=1,NDM
           F(I)=PERIOD*F(I)
         ENDDO
       ENDIF
!
      RETURN
      END SUBROUTINE FFPBP
!
!     ---------- -----
      SUBROUTINE BCPBP(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
!
! Boundary conditions for continuing BP (Periodic solutions)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      POINTER NRTN(:)
      COMMON /BLRTN/ NRTN,IRTN
!
      DIMENSION IAP(*),PAR(*),ICP(*),U0(*),U1(*),FB(*),DBC(NBC,*)
!
       ISW=IAP(10)
       NDM=IAP(23)
!
       DO I=1,NDIM
         FB(I)=U0(I)-U1(I)
       ENDDO
!
       IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
!        ** Non-generic and/or start
         DO I=1,NDM
           FB(I)=FB(I)+PAR(18)*U0(NDIM-NDM+I)
         ENDDO
       ENDIF
!
! Rotations
       IF(IRTN.NE.0)THEN
         DO I=1,NDM
           IF(NRTN(I).NE.0)FB(I)=FB(I)+PAR(19)*NRTN(I)
         ENDDO
       ENDIF
!
       IF(IJAC.EQ.0)RETURN
!
       NN=2*NDIM+NPARX
       DO I=1,NBC
         DO J=1,NN
           DBC(I,J)=0.d0
         ENDDO
       ENDDO
!
       DO I=1,NDIM
         DBC(I,I)=1
         DBC(I,NDIM+I)=-1
       ENDDO
!
       IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
!        ** Non-generic and/or start
         DO I=1,NDM
           DBC(I,NDIM-NDM+I)=PAR(18)
         ENDDO
       ENDIF
!
       IF(IJAC.EQ.1)RETURN
!
       IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
!        ** Non-generic and/or start
         DO I=1,NDM
           DBC(I,2*NDIM+18)=U0(NDIM-NDM+I)
         ENDDO
       ENDIF
!
      RETURN
      END SUBROUTINE BCPBP
!
!     ---------- -----
      SUBROUTINE ICPBP(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD, &
           F,IJAC,DINT) 
!
! Integral conditions for continuing BP (Periodic solutions)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
      DIMENSION IAP(*),RAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
!
! Local
      ALLOCATABLE UU1(:),UU2(:),FF1(:),FF2(:)
!
       NFPR=IAP(29)
!
! Generate the function.
!
       CALL FIPBP(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F)
!
       IF(IJAC.EQ.0)RETURN
!
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NINT),FF2(NINT))
!
! Generate the Jacobian.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U(J)
           UU2(J)=U(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FIPBP(IAP,RAP,NDIM,PAR,ICP,NINT,UU1,UOLD,UDOT,UPOLD,FF1)
         CALL FIPBP(IAP,RAP,NDIM,PAR,ICP,NINT,UU2,UOLD,UDOT,UPOLD,FF2)
         DO J=1,NINT
           DINT(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(UU1,UU2,FF2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF1)
         RETURN
       ENDIF
!
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FIPBP(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FF1)
         DO J=1,NINT
           DINT(J,NDIM+ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
!
       DEALLOCATE(FF1)
!
      RETURN
      END SUBROUTINE ICPBP
!
!     ---------- -----
      SUBROUTINE FIPBP(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),RAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),FI(*)
!
! Local
      ALLOCATABLE F(:),DFU(:,:),DFP(:,:)
!
       PERIOD=PAR(11)
       ISW=IAP(10)
       NDM=IAP(23)
!
       ALLOCATE(F(NDM),DFU(NDM,NDM),DFP(NDM,NPARX))
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFU,DFP)
!
       FI(1)=0.d0
       FI(NINT)=PAR(16)**2-PAR(17)
       DO I=1,NDM
         FI(1)=FI(1)+(U(I)-UOLD(I))*UPOLD(I)
         FI(NINT)=FI(NINT)+U(NDIM-NDM+I)**2
       ENDDO
!
       IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
!        ** Non-generic and/or start
         FI(1)=FI(1)+PAR(18)*PAR(16)
       ENDIF
!
       IF(ISW.GT.0) THEN
!        ** restart 1 or 2
         FI(2)=0.d0
         FI(3)=0.d0
         DO I=1,NDM
           FI(2)=FI(2)-PERIOD*DFP(I,ICP(1))*U(NDM+I)
           IF(ICP(4).EQ.11)THEN
!            ** Variable period
             FI(3)=FI(3)-F(I)*U(NDM+I)
           ELSE
!            ** Fixed period
             FI(3)=FI(3)-PERIOD*DFP(I,ICP(2))*U(NDM+I)
           ENDIF
         ENDDO
       ELSE
!        ** start
         FI(2)=0.d0
         FI(3)=0.d0
         FI(4)=PAR(12)**2+PAR(13)**2-1.d0
         FI(5)=PAR(14)**2+PAR(15)**2-1.d0
         FI(6)=PAR(12)*PAR(14)+PAR(13)*PAR(15)
         FI(7)=FI(6)
         FI(8)=PAR(20)*PAR(12)+PAR(21)*PAR(14)
         FI(9)=PAR(20)*PAR(13)+PAR(21)*PAR(15)
         DO I=1,NDM
           FI(2)=FI(2)+U(NDM+I)*UPOLD(I)
           FI(3)=FI(3)+U(2*NDM+I)*UPOLD(I)
           FI(4)=FI(4)+U(NDM+I)*UOLD(NDM+I)
           FI(5)=FI(5)+U(2*NDM+I)*UOLD(2*NDM+I)
           FI(6)=FI(6)+U(NDM+I)*UOLD(2*NDM+I)
           FI(7)=FI(7)+U(2*NDM+I)*UOLD(NDM+I)
           FI(8)=FI(8)-PERIOD*DFP(I,ICP(1))*U(3*NDM+I)
           IF(ICP(4).EQ.11)THEN
!            ** Variable period
             FI(9)=FI(9)-F(I)*U(3*NDM+I)
           ELSE
!            ** Fixed period
             FI(9)=FI(9)-PERIOD*DFP(I,ICP(2))*U(3*NDM+I)
           ENDIF
         ENDDO
       ENDIF
!
       DEALLOCATE(F,DFU,DFP)
!
      RETURN
      END SUBROUTINE FIPBP
!
!     ---------- -------
      SUBROUTINE STPNPBP(IAP,RAP,PAR,ICP,NTSR,NCOLRS, &
           RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
!
      USE SOLVEBV
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates starting data for the 2-parameter continuation of BP
! on a branch of periodic solutions.
!
      DIMENSION PAR(*),ICP(*),IAP(*),RAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*)
      DIMENSION TM(*),DTM(*),THL(*),THU(*)
! Local
      ALLOCATABLE DUPS(:,:),VPS(:,:),VDOTPS(:,:),RVDOT(:)
      ALLOCATABLE THU1(:),THL1(:)
      ALLOCATABLE FA(:,:),FC(:),P0(:,:),P1(:,:)
      ALLOCATABLE U(:),UPOLD(:)
      DIMENSION ICPRS(NPARX),RLDOTRS(NPARX)
      DOUBLE PRECISION DUM(1)
!
      LOGICAL FOUND
!
       NDIM=IAP(1)
       IRS=IAP(3)
       ISW=IAP(10)
       NDM=IAP(23)
       NFPR=IAP(29)
!
       CALL FINDLB(IAP,IRS,NFPR1,FOUND)
       READ(3,*)IBR,NTOTRS,ITPRS,IRS,NFPR1,ISWRS,NTPLRS,NARS
       BACKSPACE 3
       NDIM3=NARS-1
!
       IF(NDIM.EQ.NDIM3) THEN
!        ** restart 2
         CALL STPNBV(IAP,RAP,PAR,ICP,NTSR,NCOLRS,RLCUR,RLDOT, &
              NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
         RETURN
       ENDIF
!
       NRSP1=NTSR+1
!
       IF(ISW.LT.0) THEN
!
! Start
!
!        ** allocation
         ALLOCATE(DUPS(NDX,NRSP1),VDOTPS(NDX,NRSP1),RVDOT(2))
         ALLOCATE(THU1(NDM),THL1(NFPR))
         ALLOCATE(FA(NDM*NCOLRS,NRSP1),FC(NDM+2))
         ALLOCATE(P0(NDM,NDM),P1(NDM,NDM))
         ALLOCATE(U(NDM),UPOLD(NDM))
!
!        ** redefine IAP(1)
         IAP(1)=NDM
!
!        ** read the std branch
         CALL READBV(IAP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOTRS,UPS, &
              UDOTPS,TM,ITPRS,NDX)
!
         DO I=1,NTSR
           DTM(I)=TM(I+1)-TM(I)
         ENDDO
!
         RLCUR(1)=PAR(ICPRS(1))
         RLCUR(2)=PAR(ICPRS(2))
!
! Compute the second null vector
!
!        ** redefine IAP, RAP
         NTST=IAP(5)
         NCOL=IAP(6)
         IAP(5)=NTSR
         IAP(6)=NCOLRS
         NBC=IAP(12)
         NINT=IAP(13)
         IAP(12)=NDM
         IAP(13)=1
         IAP(29)=2
         DET=RAP(14)
!
!        ** compute UPOLDP
         DO J=1,NTSR
           DO I=1,NCOLRS
             DO K=1,NDM
               U(K)=UPS((I-1)*NDM+K,J)
             ENDDO
             CALL FUNI(IAP,RAP,NDM,U,U,ICPRS,PAR,0,UPOLD,DUM,DUM)
             DO K=1,NDM
               UPOLDP((I-1)*NDM+K,J)=PAR(11)*UPOLD(K)
             ENDDO
           ENDDO
         ENDDO
         DO I=1,NDM
           U(I)=UPS(I,NRSP1)
         ENDDO
         CALL FUNI(IAP,RAP,NDM,U,U,ICPRS,PAR,0,UPOLD,DUM,DUM)
         DO I=1,NDM
           UPOLDP(I,NRSP1)=PAR(11)*UPOLD(I)
         ENDDO
!
!        ** unit weights
         DO I=1,NFPR
           THL1(I)=1.d0
         ENDDO
         DO I=1,NDM
           THU1(I)=1.d0
         ENDDO
!
!        ** call SOLVBV
         RDSZ=0.d0
         NLLV=1
         IFST=1
         CALL SOLVBV(IFST,IAP,RAP,PAR,ICPRS,FNPS,BCPS,ICPS,RDSZ,NLLV, &
              RLCUR,RLCUR,RLDOTRS,NDX,UPS,DUPS,UPS,UDOTPS,UPOLDP,DTM, &
              FA,FC,P0,P1,THL1,THU1)
!
         DO I=1,NDM
           VDOTPS(I,NRSP1)=FC(I)
         ENDDO
         RVDOT(1)=FC(NDM+1)
         RVDOT(2)=FC(NDM+2)
!
         DO J=1,NTSR
           DO I=1,NCOLRS
             DO K=1,NDM
               VDOTPS((I-1)*NDM+K,J)=FA((I-1)*NDM+K,J)
             ENDDO
           ENDDO
         ENDDO
!
!        ** normalization
         CALL SCALEB(IAP,NDM,NDX,UDOTPS,RLDOTRS,DTM,THL1,THU1)
         CALL SCALEB(IAP,NDM,NDX,VDOTPS,RVDOT,DTM,THL1,THU1)
!
!        ** restore IAP, RAP
         IAP(1)=NDIM
         IAP(5)=NTST
         IAP(6)=NCOL
         IAP(12)=NBC
         IAP(13)=NINT
         IAP(29)=NFPR
         RAP(14)=DET
!
!        ** init UPS,PAR
         DO J=1,NTSR
           DO I=NCOLRS,1,-1
             DO K=1,NDM
               UPS((I-1)*NDIM+K,J)=UPS((I-1)*NDM+K,J)
               UPS((I-1)*NDIM+NDM+K,J)=UDOTPS((I-1)*NDM+K,J)
               UPS((I-1)*NDIM+2*NDM+K,J)=VDOTPS((I-1)*NDM+K,J)
               UPS((I-1)*NDIM+3*NDM+K,J)=0.d0
               UDOTPS((I-1)*NDIM+K,J)=0.d0
               UDOTPS((I-1)*NDIM+NDM+K,J)=0.d0
               UDOTPS((I-1)*NDIM+2*NDM+K,J)=0.d0
               UDOTPS((I-1)*NDIM+3*NDM+K,J)=0.d0
             ENDDO
           ENDDO
         ENDDO
         DO K=1,NDM
           UPS(K+NDM,NRSP1)=UDOTPS(K,NRSP1)
           UPS(K+2*NDM,NRSP1)=VDOTPS(K,NRSP1)
           UPS(K+3*NDM,NRSP1)=0.d0
           UDOTPS(K,NRSP1)=0.d0
           UDOTPS(K+NDM,NRSP1)=0.d0
           UDOTPS(K+2*NDM,NRSP1)=0.d0
           UDOTPS(K+3*NDM,NRSP1)=0.d0
         ENDDO
!
!        ** init q,r,psi^*3,a,b,c1,c1
         PAR(12)=RLDOTRS(1)
         PAR(13)=RLDOTRS(2)
         PAR(14)=RVDOT(1)
         PAR(15)=RVDOT(2)
         PAR(16)=0.d0
         PAR(17)=0.d0
         PAR(18)=0.d0
         PAR(20)=0.d0
         PAR(21)=0.d0
         RLDOT(1)=0.d0
         RLDOT(2)=0.d0
         IF(ICP(4).EQ.11)THEN
!          ** Variable period
           RLDOT(3)=1.d0
           RLDOT(4)=0.d0
         ELSE
!          ** Fixed period
           RLDOT(3)=0.d0
           RLDOT(4)=1.d0
         ENDIF
         RLDOT(5)=0.d0
         RLDOT(6)=0.d0
         RLDOT(7)=0.d0
         RLDOT(8)=0.d0
         RLDOT(9)=0.d0
         RLDOT(10)=0.d0
         RLDOT(11)=0.d0
!
         DEALLOCATE(DUPS,VDOTPS,RVDOT)
         DEALLOCATE(THU1,THL1)
         DEALLOCATE(FA,FC)
         DEALLOCATE(P0,P1)
         DEALLOCATE(U,UPOLD)
!
         NODIR=0
!
       ELSE
!
! Restart 1
!
         ALLOCATE(VPS(2*NDX,NRSP1),VDOTPS(2*NDX,NRSP1))
!
!        ** read the std branch
         IAP(1)=2*NDIM
         CALL READBV(IAP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOTRS,VPS, &
              VDOTPS,TM,ITPRS,2*NDX)
         IAP(1)=NDIM
!
         DO J=1,NTSR
           DO I=1,NCOLRS
             DO K=1,NDM
               UPS((I-1)*NDIM+K,J)=VPS((I-1)*2*NDIM+K,J)
               UPS((I-1)*NDIM+K+NDM,J)=VPS((I-1)*2*NDIM+K+3*NDM,J)
             ENDDO
           ENDDO
         ENDDO
         DO K=1,NDM
           UPS(K,NRSP1)=VPS(K,NRSP1)
           UPS(K+NDM,NRSP1)=VPS(K+3*NDM,NRSP1)
         ENDDO
!
         DEALLOCATE(VPS,VDOTPS)
!
         NODIR=1
!
       ENDIF
!
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
!
      RETURN
      END SUBROUTINE STPNPBP
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!   Subroutines for the Continuation of Period Doubling Bifurcations
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNPD(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
      DIMENSION IAP(*)
      DIMENSION U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
      DOUBLE PRECISION RAP(*),UOLD(*)
! Local
      ALLOCATABLE DFU(:),UU1(:),UU2(:),FF1(:),FF2(:)
!
       NDM=IAP(23)
       NFPR=IAP(29)
!
! Generate the function.
!
       ALLOCATE(DFU(NDM*NDM))
       CALL FFPD(IAP,RAP,U,UOLD,ICP,PAR,F,NDM,DFU)
!
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU)
         RETURN
       ENDIF
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NDIM),FF2(NDIM))
!
! Generate the Jacobian.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U(J)
           UU2(J)=U(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FFPD(IAP,RAP,UU1,UOLD,ICP,PAR,FF1,NDM,DFU)
         CALL FFPD(IAP,RAP,UU2,UOLD,ICP,PAR,FF2,NDM,DFU)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(UU1,UU2,FF2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF1,DFU)
         RETURN
       ENDIF
!
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FFPD(IAP,RAP,U,UOLD,ICP,PAR,FF1,NDM,DFU)
         DO J=1,NDIM
           DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
      ENDDO
!
      DEALLOCATE(FF1,DFU)
      RETURN
      END SUBROUTINE FNPD
!
!     ---------- ----
      SUBROUTINE FFPD(IAP,RAP,U,UOLD,ICP,PAR,F,NDM,DFDU)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDM,*)
      DOUBLE PRECISION RAP(*),UOLD(*)
! Local
      DOUBLE PRECISION DUMDP(1)
!
       PERIOD=PAR(11)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DUMDP)
!
       DO I=1,NDM
         F(NDM+I)=0.d0
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
        ENDDO
         F(I)=PERIOD*F(I)
         F(NDM+I)=PERIOD*F(NDM+I)
       ENDDO
!
      RETURN
      END SUBROUTINE FFPD
!
!     ---------- ----
      SUBROUTINE BCPD(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      POINTER NRTN(:)
      COMMON /BLRTN/ NRTN,IRTN
!
! Generate boundary conditions for the 2-parameter continuation
! of period doubling bifurcations.
!
      DIMENSION IAP(*),PAR(*),ICP(*),U0(*),U1(*),F(*),DBC(NBC,*)
!
       NDM=IAP(23)
!
       DO I=1,NDM
         F(I)=U0(I)-U1(I)
         F(NDM+I)=U0(NDM+I)+U1(NDM+I)
       ENDDO
!
! Rotations
       IF(IRTN.NE.0)THEN
         DO I=1,NDM
           IF(NRTN(I).NE.0)F(I)=F(I) + PAR(19)*NRTN(I)
         ENDDO
       ENDIF
!
       IF(IJAC.EQ.0)RETURN
!
       NN=2*NDIM+NPARX
       DO I=1,NBC
         DO J=1,NN
           DBC(I,J)=0.d0
         ENDDO
       ENDDO
!
       DO I=1,NDIM
         DBC(I,I)=1
         IF(I.LE.NDM) THEN
           DBC(I,NDIM+I)=-1
         ELSE
           DBC(I,NDIM+I)=1
         ENDIF
       ENDDO
!
      RETURN
      END SUBROUTINE BCPD
!
!     ---------- ----
      SUBROUTINE ICPD(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD, &
           F,IJAC,DINT)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!
      DIMENSION IAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
!
       NDM=IAP(23)
!
       F(1)=0.d0
       F(2)=-PAR(13)
!
       DO I=1,NDM
         F(1)=F(1)+(U(I)-UOLD(I))*UPOLD(I)
         F(2)=F(2)+U(NDM+I)*U(NDM+I)
       ENDDO
!
       IF(IJAC.EQ.0)RETURN
!
       NN=NDIM+NPARX
       DO I=1,NINT
         DO J=1,NN
           DINT(I,J)=0.d0
         ENDDO
       ENDDO
!
       DO I=1,NDM
         DINT(1,I)=UPOLD(I)
         DINT(2,NDM+I)=2.d0*U(NDM+I)
       ENDDO
!
       DINT(2,NDIM+13)=-1.d0
!
      RETURN
      END SUBROUTINE ICPD
!
!     ---------- ------
      SUBROUTINE STPNPD(IAP,RAP,PAR,ICP,NTSR,NCOLRS, &
           RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates starting data for the 2-parameter continuation of
! period-doubling bifurcations on a branch of periodic solutions.
!
      DIMENSION PAR(*),ICP(*),IAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*),TM(*),DTM(*)
! Local
      DIMENSION ICPRS(NPARX),RLDOTRS(NPARX)
!
      LOGICAL FOUND
!
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
       NFPR=IAP(29)
!
       CALL FINDLB(IAP,IRS,NFPR1,FOUND)
       CALL READBV(IAP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOTRS,UPS,UDOTPS, &
            TM,ITPRS,NDX)
       RLDOT(1)=RLDOTRS(1)
       RLDOT(2)=RLDOTRS(2)
!
! Complement starting data 
         PAR(13)=0.d0
         RLDOT(3)=0.d0
         DO J=1,NTSR
           DO I=1,NCOLRS
             K1=(I-1)*NDIM+NDM+1
             K2=I*NDIM
             DO K=K1,K2
               UPS(K,J)=0.d0
               UDOTPS(K,J)=0.d0
             ENDDO
           ENDDO
         ENDDO
         K1=NDM+1
         NRSP1=NTSR+1
         DO K=K1,NDIM
           UPS(NRSP1,K)=0.d0
           UDOTPS(NRSP1,K)=0.d0
         ENDDO
!
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
!
       NODIR=0
!
      RETURN
      END SUBROUTINE STPNPD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!       Subroutines for the Continuation of Torus Bifurcations
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNTR(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates the equations for the 2-parameter continuation of
! torus bifurcations.
!
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
      DOUBLE PRECISION RAP(*),UOLD(*)
! Local
      ALLOCATABLE DFU(:),UU1(:),UU2(:),FF1(:),FF2(:)
!
       NDM=IAP(23)
       NFPR=IAP(29)
!
! Generate the function.
!
       ALLOCATE(DFU(NDM*NDM))
       CALL FFTR(IAP,RAP,U,UOLD,ICP,PAR,F,NDM,DFU)
!
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU)
         RETURN
       ENDIF
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NDIM),FF2(NDIM))
!
! Generate the Jacobian.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U(J)
           UU2(J)=U(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FFTR(IAP,RAP,UU1,UOLD,ICP,PAR,FF1,NDM,DFU)
         CALL FFTR(IAP,RAP,UU2,UOLD,ICP,PAR,FF2,NDM,DFU)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(UU1,UU2,FF2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF1,DFU)
         RETURN
       ENDIF
!
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FFTR(IAP,RAP,U,UOLD,ICP,PAR,FF1,NDM,DFU)
         DO J=1,NDIM
           DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
!
      DEALLOCATE(FF1,DFU)
      RETURN
      END SUBROUTINE FNTR
!
!     ---------- ----
      SUBROUTINE FFTR(IAP,RAP,U,UOLD,ICP,PAR,F,NDM,DFDU)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      INTEGER IAP(*),ICP(*)
      DOUBLE PRECISION RAP(*),U(*),UOLD(*),PAR(*),F(*),DFDU(NDM,*)
! Local
      DOUBLE PRECISION DUMDP(1)
!
       PERIOD=PAR(11)
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DUMDP)
!
       NDM2=2*NDM
       DO I=1,NDM
         F(NDM+I)=0.d0
         F(NDM2+I)=0.d0
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
           F(NDM2+I)=F(NDM2+I)+DFDU(I,J)*U(NDM2+J)
         ENDDO
         F(NDM+I)=PERIOD*F(NDM+I)
         F(NDM2+I)=PERIOD*F(NDM2+I)
         F(I)=PERIOD*F(I)
       ENDDO
!
      RETURN
      END SUBROUTINE FFTR
!
!     ---------- ----
      SUBROUTINE BCTR(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      POINTER NRTN(:)
      COMMON /BLRTN/ NRTN,IRTN
!
      DIMENSION IAP(*),PAR(*),ICP(*),U0(*),U1(*),F(*),DBC(NBC,*)
!
       NDM=IAP(23)
!
       NDM2=2*NDM
       THETA=PAR(12)
!
       SS=DSIN(THETA)
       CS=DCOS(THETA)
!
       DO I=1,NDM
         F(I)=U0(I)-U1(I)
         F(NDM+I)= U1(NDM+I) -CS*U0(NDM+I) +SS*U0(NDM2+I)
         F(NDM2+I)=U1(NDM2+I)-CS*U0(NDM2+I)-SS*U0(NDM+I)
       ENDDO
!
! Rotations
       IF(IRTN.NE.0)THEN
         DO I=1,NDM
           IF(NRTN(I).NE.0)F(I)=F(I) + PAR(19)*NRTN(I)
         ENDDO
       ENDIF
!
       IF(IJAC.EQ.0)RETURN
!
       NN=2*NDIM+NPARX
       DO I=1,NBC
         DO J=1,NN
           DBC(I,J)=0.d0
         ENDDO
       ENDDO
!
       DO I=1,NDM
         DBC(I,I)=1
         DBC(I,NDIM+I)=-1
         DBC(NDM+I,NDM+I)=-CS
         DBC(NDM+I,NDM2+I)=SS
         DBC(NDM+I,NDIM+NDM+I)=1
         DBC(NDM+I,2*NDIM+12)=CS*U0(NDM2+I)+SS*U0(NDM+I)
         DBC(NDM2+I,NDM+I)=-SS
         DBC(NDM2+I,NDM2+I)=-CS
         DBC(NDM2+I,NDIM+NDM2+I)=1
         DBC(NDM2+I,2*NDIM+12)=SS*U0(NDM2+I)-CS*U0(NDM+I)
       ENDDO
!
      RETURN
      END SUBROUTINE BCTR
!
!     ---------- ----
      SUBROUTINE ICTR(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD, &
           F,IJAC,DINT)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
      DIMENSION ICP(*),PAR(*)
!
       NDM=IAP(23)
       NDM2=2*NDM
!
       F(1)=0.d0
       F(2)=0.d0
       F(3)=-PAR(13)
!
       DO I=1,NDM
         F(1)=F(1)+(U(I)-UOLD(I))*UPOLD(I)
         F(2)=F(2)+U(NDM+I)*UOLD(NDM2+I)-U(NDM2+I)*UOLD(NDM+I)
         F(3)=F(3)+U(NDM+I)*U(NDM+I) +U(NDM2+I)*U(NDM2+I)
       ENDDO
!
       IF(IJAC.EQ.0)RETURN
!
       NN=NDIM+NPARX
       DO I=1,NINT
         DO J=1,NN
           DINT(I,J)=0.d0
         ENDDO
       ENDDO
!
      DO I=1,NDM
        DINT(1,I)=UPOLD(I)
        DINT(2,NDM+I)=UOLD(NDM2+I)
        DINT(2,NDM2+I)=-UOLD(NDM+I)
        DINT(3,NDM+I)=2*U(NDM+I)
        DINT(3,NDM2+I)=2*U(NDM2+I)
      ENDDO
!
      DINT(3,NDIM+13)=-1
!
      RETURN
      END SUBROUTINE ICTR
!
!     ---------- ------
      SUBROUTINE STPNTR(IAP,RAP,PAR,ICP,NTSR,NCOLRS, & 
           RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates starting data for the 2-parameter continuation of torus
! bifurcations.
!
      DIMENSION PAR(*),ICP(*),IAP(*),RAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),TM(*),DTM(*)
! Local
      DIMENSION ICPRS(NPARX),RLDOTRS(NPARX)
!
      LOGICAL FOUND
!
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
       NFPR=IAP(29)
!
       CALL FINDLB(IAP,IRS,NFPR1,FOUND)
       CALL READBV(IAP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOTRS,UPS,UDOTPS, &
            TM,ITPRS,NDX)
!
       RLDOT(1)=RLDOTRS(1)
       RLDOT(2)=RLDOTRS(2)
       RLDOT(3)=0.d0
       RLDOT(4)=0.d0
!
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=K1+NDM-1
           K2P1=K2+1
           K3=K2+NDM
           T=TM(J)+(I-1)*(TM(J+1)-TM(J))/NCOLRS
           DO K=K2P1,K3
             UPS(K,J)    =0.0001d0*SIN(T)
             UPS(K+NDM,J)=0.0001d0*COS(T)
             UDOTPS(K,J)=0.d0
             UDOTPS(K+NDM,J)=0.d0
           ENDDO
         ENDDO
       ENDDO
       NRSP1=NTSR+1
       DO I=1,NDM
         UPS(NDM+I,NRSP1)=0.d0
         UPS(2*NDM+I,NRSP1)=0.d0
       ENDDO
       DO I=1,NDM
         UDOTPS(NDM+I,NRSP1)=0.d0
         UDOTPS(2*NDM+I,NRSP1)=0.d0
       ENDDO
!
       PAR(13)=0.d0
!
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
!
       NODIR=0
!
      RETURN
      END SUBROUTINE STPNTR
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        Subroutines for Optimization of Periodic Solutions
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNPO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates the equations for periodic optimization problems.
!
      DIMENSION IAP(*),RAP(*),U(*),UOLD(*),ICP(*),PAR(*),F(*)
      DIMENSION DFDU(NDIM,*),DFDP(NDIM,*)
! Local
      ALLOCATABLE DFU(:),FF1(:),FF2(:),UPOLD(:)
!
       NDM=IAP(23)
       NFPR=IAP(29)
!
! Generate F(UOLD)
!
       ALLOCATE(UPOLD(NDIM))
       CALL FUNC(NDM,UOLD,ICP,PAR,0,UPOLD,DUMDU,DUMDP)
       PERIOD=PAR(11)
       DO I=1,NDM
         UPOLD(I)=PERIOD*UPOLD(I)
       ENDDO
!
! Generate the function.
!
      CALL FFPO(IAP,RAP,U,UOLD,UPOLD,ICP,PAR,F,NDM,DFDU)
!
      IF(IJAC.EQ.0)THEN
        DEALLOCATE(UPOLD)
        RETURN
      ENDIF
!
      ALLOCATE(DFU(NDIM*NDIM),FF1(NDIM),FF2(NDIM))
!
! Generate the Jacobian.
!
      UMX=0.d0
      DO I=1,NDIM
        IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
      ENDDO
!
      EP=HMACH*(1+UMX)
!
      DO I=1,NDIM
        UU=U(I)
        U(I)=UU-EP
        CALL FFPO(IAP,RAP,U,UOLD,UPOLD,ICP,PAR,FF1,NDM,DFU)
        U(I)=UU+EP
        CALL FFPO(IAP,RAP,U,UOLD,UPOLD,ICP,PAR,FF2,NDM,DFU)
        U(I)=UU
        DO J=1,NDIM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
        ENDDO
      ENDDO
!
      DEALLOCATE(FF2)
      IF(IJAC.EQ.1)THEN
        DEALLOCATE(UPOLD,DFU,FF1)
        RETURN
      ENDIF
!
      DO I=1,NFPR
        PAR(ICP(I))=PAR(ICP(I))+EP
        CALL FFPO(IAP,RAP,U,UOLD,UPOLD,ICP,PAR,FF1,NDM,DFU)
        DO J=1,NDIM
          DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
        ENDDO
        PAR(ICP(I))=PAR(ICP(I))-EP
      ENDDO
!
      DEALLOCATE(UPOLD,DFU,FF1)
      RETURN
      END SUBROUTINE FNPO
!
!     ---------- ----
      SUBROUTINE FFPO(IAP,RAP,U,UOLD,UPOLD,ICP,PAR,F,NDM,DFDU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),RAP(*),ICP(*),PAR(*),F(*),DFDU(NDM,*)
      DIMENSION U(*),UOLD(*),UPOLD(*)
! Local
      DOUBLE PRECISION DUMDP(1)

       PERIOD=PAR(11)
       RKAPPA=PAR(13)
       GAMMA =PAR(14)
!
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F(NDM+1),DFDU,DUMDP)
       CALL FOPI(IAP,RAP,NDM,U,ICP,PAR,1,FOP,F,DUMDP)
!
       DO I=1,NDM
         DFU=F(I)
         F(I)=F(NDM+I)
         F(NDM+I)=0.d0
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)-DFDU(J,I)*U(NDM+J)
         ENDDO
         F(I)=PERIOD*F(I)
         F(NDM+I)=PERIOD*F(NDM+I)+ RKAPPA*UPOLD(I) + GAMMA*DFU
       ENDDO
!
      RETURN
      END SUBROUTINE FFPO
!
!     ---------- ----
      SUBROUTINE BCPO(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
      POINTER NRTN(:)
      COMMON /BLRTN/ NRTN,IRTN
!
! Generates the boundary conditions for periodic optimization problems.
!
      DIMENSION IAP(*),U0(*),U1(*),F(NBC),ICP(*),PAR(*),DBC(NBC,*)
!
      NFPR=IAP(29)
!
      DO I=1,NBC
        F(I)=U0(I)-U1(I)
      ENDDO
!
! Rotations
       IF(IRTN.NE.0)THEN
         NBC0=IAP(24)
         DO I=1,NBC0
           IF(NRTN(I).NE.0)F(I)=F(I) + PAR(19)*NRTN(I)
         ENDDO
       ENDIF
!
       IF(IJAC.EQ.0)RETURN
!
      DO I=1,NBC
        DO J=1,2*NDIM
         DBC(I,J)=0.d0
        ENDDO
        DBC(I,I)=1.D0
        DBC(I,NDIM+I)=-1.d0
        DO J=1,NFPR
          DBC(I,2*NDIM+ICP(J))=0.d0
        ENDDO
      ENDDO
 
      RETURN
      END SUBROUTINE BCPO
!
!     ---------- ----
      SUBROUTINE ICPO(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD, &
           F,IJAC,DINT)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates integral conditions for periodic optimization problems.
!
      DIMENSION IAP(*),RAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
! Local
      ALLOCATABLE DFU(:),DFP(:),F1(:),F2(:),DNT(:,:)
      ALLOCATE(DNT(NINT,NDIM+NPARX),DFU(NDIM*NDIM),DFP(NDIM*NPARX))
!
       NDM=IAP(23)
       NNT0=IAP(25)
       NFPR=IAP(29)
!
! Generate the function.
!
       CALL FIPO(IAP,RAP,PAR,ICP,NINT,NNT0,U,UOLD,UDOT, &
            UPOLD,F,DNT,NDM,DFU,DFP)
!
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DNT,DFU,DFP)
         RETURN
       ENDIF
!
! Generate the Jacobian.
!
       ALLOCATE(F1(NINT),F2(NINT))
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         UU=U(I)
         U(I)=UU-EP
         CALL FIPO(IAP,RAP,PAR,ICP,NINT,NNT0,U,UOLD,UDOT, &
              UPOLD,F1,DNT,NDM,DFU,DFP)
         U(I)=UU+EP
         CALL FIPO(IAP,RAP,PAR,ICP,NINT,NNT0,U,UOLD,UDOT, &
              UPOLD,F2,DNT,NDM,DFU,DFP)
         U(I)=UU
         DO J=1,NINT
           DINT(J,I)=(F2(J)-F1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FIPO(IAP,RAP,PAR,ICP,NINT,NNT0,U,UOLD,UDOT, &
              UPOLD,F1,DNT,NDM,DFU,DFP)
         DO J=1,NINT
           DINT(J,NDIM+ICP(I))=(F1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
!
       DEALLOCATE(DNT,F1,F2,DFU,DFP)
      RETURN
      END SUBROUTINE ICPO
!
!     ---------- ----
      SUBROUTINE FIPO(IAP,RAP,PAR,ICP,NINT,NNT0, &
           U,UOLD,UDOT,UPOLD,FI,DINT,NDMT,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!
      DIMENSION IAP(*),RAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),FI(*),DINT(NNT0,*)
      DIMENSION DFDU(NDMT,NDMT),DFDP(NDMT,*)
!
! Local
      DIMENSION DFP(NPARX)
      ALLOCATABLE DFU(:),F(:)
!
       NDM=IAP(23)
       NFPR=IAP(29)
!
       FI(1)=0.d0
       DO I=1,NDM
         FI(1)=FI(1)+(U(I)-UOLD(I))*UPOLD(I)
       ENDDO
!
       DO I=1,NPARX
        DFP(I)=0.d0
       ENDDO
       ALLOCATE(DFU(NDM),F(NDM))
       CALL FOPI(IAP,RAP,NDM,U,ICP,PAR,2,FOP,DFU,DFP)
       FI(2)=PAR(10)-FOP
!
       FI(3)=PAR(13)**2+PAR(14)**2-PAR(12)
       DO I=1,NDM
         FI(3)=FI(3)+U(NDM+I)**2
       ENDDO
!
       DO I=1,NDM
         DO J=1,NPARX
           DFDP(I,J)=0.d0
         ENDDO
       ENDDO
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
!
       DO L=4,NINT
         INDX=ICP(NFPR+L-3)
         IF(INDX.EQ.11)THEN
           FI(L)=-PAR(14)*DFP(INDX) - PAR(20+INDX)
           DO I=1,NDM
             FI(L)=FI(L)+F(I)*U(NDM+I)
           ENDDO
         ELSE
           FI(L)=-PAR(14)*DFP(INDX) - PAR(20+INDX)
           DO I=1,NDM
             FI(L)=FI(L)+PAR(11)*DFDP(I,INDX)*U(NDM+I)
           ENDDO
         ENDIF
       ENDDO
!
      DEALLOCATE(DFU,F)
      RETURN
      END SUBROUTINE FIPO
!
!     ---------- ------
      SUBROUTINE STPNPO(IAP,RAP,PAR,ICP,NTSR,NCOLRS, &
           RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates starting data for optimization of periodic solutions.
!
      DIMENSION PAR(*),ICP(*),IAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*),TM(*),DTM(*)
      DOUBLE PRECISION RAP(*)
! Local
      DIMENSION ICPRS(NPARX),RLDOTRS(NPARX)
      ALLOCATABLE U(:)
!
      LOGICAL FOUND
!
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
       NFPR=IAP(29)
!
       CALL FINDLB(IAP,IRS,NFPR1,FOUND)
       CALL READBV(IAP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOTRS,UPS,UDOTPS, &
            TM,ITPRS,NDX)
       DO J=1,NTSR
         DTM(J)=TM(J+1)-TM(J)
       ENDDO
!
! Compute the starting value of the objective functional
! (using UPOLDP for temporary storage)
       ALLOCATE(U(NDM))
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=K1+NDM-1
           DO K=K1,K2
             U(K-K1+1)=UPS(K,J)
           ENDDO
           CALL FOPT(NDM,U,ICP,PAR,0,FS,DUMU,DUMP)
           UPOLDP(K1,J)=FS
         ENDDO
       ENDDO
       NRSP1=NTSR+1
       DO K=1,NDM
          U(K)=UPS(K,NRSP1)
       ENDDO
       CALL FOPT(NDM,U,ICP,PAR,0,FS,DUMU,DUMP)
       DEALLOCATE(U)
       UPOLDP(1,NRSP1)=FS
       PAR(10)=RINTG(IAP,NDX,1,UPOLDP,DTM)
!
! Complement starting data
!
       DO I=12,NPARX
         PAR(I)=0.d0
       ENDDO
!
       DO  J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+NDM+1
           K2=I*NDIM
           DO K=K1,K2
             UPS(K,J)=0.d0
           ENDDO
         ENDDO
       ENDDO
       K1=NDM+1
       DO K=K1,NDIM
         UPS(K,NRSP1)=0.d0
       ENDDO
!
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
!
       NODIR=1
!
      RETURN
      END SUBROUTINE STPNPO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        Subroutines for the Continuation of Folds for BVP.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FNBL(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates the equations for the 2-parameter continuation
! of folds (BVP).
!
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
      DOUBLE PRECISION RAP(*),UOLD(*)
! Local
      ALLOCATABLE DFU(:),DFP(:),UU1(:),UU2(:),FF1(:),FF2(:)
!
       NDM=IAP(23)
       NFPR=IAP(29)
!
! Generate the function.
!
      ALLOCATE(DFU(NDM*NDM),DFP(NDM*NPARX))
      CALL FFBL(IAP,RAP,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
!
      IF(IJAC.EQ.0)THEN
        DEALLOCATE(DFU,DFP)
        RETURN
      ENDIF
      ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NDIM),FF2(NDIM))
!
! Generate the Jacobian.
!
      UMX=0.d0
      DO I=1,NDIM
        IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
      ENDDO
!
      EP=HMACH*(1+UMX)
!
      DO I=1,NDIM
        DO J=1,NDIM
          UU1(J)=U(J)
          UU2(J)=U(J)
        ENDDO
        UU1(I)=UU1(I)-EP
        UU2(I)=UU2(I)+EP
        CALL FFBL(IAP,RAP,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
        CALL FFBL(IAP,RAP,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
        DO J=1,NDIM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
        ENDDO
      ENDDO
!
      DEALLOCATE(UU1,UU2,FF2)
      IF (IJAC.EQ.1)THEN
        DEALLOCATE(DFU,DFP,FF1)
        RETURN
      ENDIF
!
      DO I=1,NFPR
        PAR(ICP(I))=PAR(ICP(I))+EP
        CALL FFBL(IAP,RAP,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
        DO J=1,NDIM
          DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
        ENDDO
        PAR(ICP(I))=PAR(ICP(I))-EP
      ENDDO
!
      DEALLOCATE(DFU,DFP,FF1)
      RETURN
      END SUBROUTINE FNBL
!
!     ---------- ----
      SUBROUTINE FFBL(IAP,RAP,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
      DOUBLE PRECISION RAP(*),UOLD(*)
!
       NFPR=IAP(29)
!
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
!
       NFPX=NFPR/2-1
       DO I=1,NDM
         F(NDM+I)=0.d0
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
         ENDDO
         IF(NFPX.GT.0)THEN
           DO J=1,NFPX
             F(NDM+I)=F(NDM+I) &
             + DFDP(I,ICP(1+J))*PAR(ICP(NFPR-NFPX+J))
           ENDDO
         ENDIF
       ENDDO
!
      RETURN
      END SUBROUTINE FFBL
!
!     ---------- ----
      SUBROUTINE BCBL(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates the boundary conditions for the 2-parameter continuation
! of folds (BVP).
!
      DIMENSION IAP(*),U0(*),U1(*),F(NBC),ICP(*),PAR(*),DBC(NBC,*)
! Local
      ALLOCATABLE UU1(:),UU2(:),FF1(:),FF2(:),DFU(:,:)
!
       NDM=IAP(23)
       NBC0=IAP(24)
       NFPR=IAP(29)
       ALLOCATE(DFU(NBC0,2*NDM+NPARX))
!
! Generate the function.
!
       CALL FBBL(IAP,RAP,NDIM,PAR,ICP,NBC0,U0,U1,F,DFU)
!
       IF(IJAC.EQ.0)THEN
          DEALLOCATE(DFU)
          RETURN
       ENDIF
!
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NBC),FF2(NBC))
!
! Derivatives with respect to U0.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U0(I)).GT.UMX)UMX=DABS(U0(I))
       ENDDO
       EP=HMACH*(1+UMX)
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U0(J)
           UU2(J)=U0(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FBBL(IAP,RAP,NDIM,PAR,ICP,NBC0,UU1,U1,FF1,DFU)
         CALL FBBL(IAP,RAP,NDIM,PAR,ICP,NBC0,UU2,U1,FF2,DFU)
         DO J=1,NBC
           DBC(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
! Derivatives with respect to U1.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U1(I)).GT.UMX)UMX=DABS(U1(I))
       ENDDO
       EP=HMACH*(1+UMX)
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U1(J)
           UU2(J)=U1(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FBBL(IAP,RAP,NDIM,PAR,ICP,NBC0,U0,UU1,FF1,DFU)
         CALL FBBL(IAP,RAP,NDIM,PAR,ICP,NBC0,U0,UU2,FF2,DFU)
         DO J=1,NBC
           DBC(J,NDIM+I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(FF1,UU1,UU2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF2,DFU)
         RETURN
       ENDIF
!
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FBBL(IAP,RAP,NDIM,PAR,ICP,NBC0,U0,U1,FF2,DFU)
         DO J=1,NBC
           DBC(J,2*NDIM+ICP(I))=(FF2(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
!
      DEALLOCATE(DFU,FF2)
      RETURN
      END SUBROUTINE BCBL
!
!     ---------- ----
      SUBROUTINE FBBL(IAP,RAP,NDIM,PAR,ICP,NBC0,U0,U1,F,DBC)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),PAR(*),ICP(*),U0(*),U1(*),F(*),DBC(NBC0,*)
!
       NDM=IAP(23)
       NFPR=IAP(29)
!
       NFPX=NFPR/2-1
       CALL BCNI(IAP,RAP,NDM,PAR,ICP,NBC0,U0,U1,F,2,DBC)
       DO I=1,NBC0
         F(NBC0+I)=0.d0
         DO J=1,NDM
           F(NBC0+I)=F(NBC0+I)+DBC(I,J)*U0(NDM+J)
           F(NBC0+I)=F(NBC0+I)+DBC(I,NDM+J)*U1(NDM+J)
         ENDDO
         IF(NFPX.NE.0) THEN
           DO J=1,NFPX
             F(NBC0+I)=F(NBC0+I) &
                  + DBC(I,NDIM+ICP(1+J))*PAR(ICP(NFPR-NFPX+J))
           ENDDO
         ENDIF
       ENDDO
!
      RETURN
      END SUBROUTINE FBBL
!
!     ---------- ----
      SUBROUTINE ICBL(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD, &
           F,IJAC,DINT)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates integral conditions for the 2-parameter continuation of
! folds (BVP).
!
      DIMENSION IAP(*),RAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
! Local
      ALLOCATABLE UU1(:),UU2(:),FF1(:),FF2(:),DFU(:,:)
!
       NDM=IAP(23)
       NNT0=IAP(25)
       NFPR=IAP(29)
       ALLOCATE(DFU(NNT0,NDM+NPARX))
!
! Generate the function.
!
       CALL FIBL(IAP,RAP,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,F,DFU)
!
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU)
         RETURN
       ENDIF
!
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NINT),FF2(NINT))
!
! Generate the Jacobian.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U(J)
           UU2(J)=U(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FIBL(IAP,RAP,PAR,ICP,NINT,NNT0,UU1,UOLD,UDOT, &
              UPOLD,FF1,DFU)
         CALL FIBL(IAP,RAP,PAR,ICP,NINT,NNT0,UU2,UOLD,UDOT, &
              UPOLD,FF2,DFU)
         DO J=1,NINT
           DINT(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(UU1,UU2,FF2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF1,DFU)
         RETURN
       ENDIF
!
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FIBL(IAP,RAP,PAR,ICP,NINT,NNT0,U,UOLD,UDOT, &
              UPOLD,FF1,DFU)
         DO J=1,NINT
           DINT(J,NDIM+ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
!
      DEALLOCATE(FF1,DFU)
      RETURN
      END SUBROUTINE ICBL
!
!     ---------- ----
      SUBROUTINE FIBL(IAP,RAP,PAR,ICP,NINT,NNT0, &
           U,UOLD,UDOT,UPOLD,F,DINT)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),RAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NNT0,*)
!
       NDM=IAP(23)
       NFPR=IAP(29)
!
       NFPX=0
       IF(NINT.GT.1) THEN
         NFPX=NFPR/2-1
         CALL ICNI(IAP,RAP,NDM,PAR,ICP,NNT0,U,UOLD,UDOT,UPOLD,F,2,DINT)
         DO I=1,NNT0
           F(NNT0+I)=0.d0
           DO J=1,NDM
             F(NNT0+I)=F(NNT0+I)+DINT(I,J)*U(NDM+J)
           ENDDO
           IF(NFPX.NE.0) THEN
             DO J=1,NFPX
               F(NNT0+I)=F(NNT0+I) &
                    + DINT(I,NDM+ICP(1+J))*PAR(ICP(NFPR-NFPX+J))
             ENDDO
           ENDIF
         ENDDO
       ENDIF
!
! Note that PAR(11+NFPR/2) is used to keep the norm of the null vector
       F(NINT)=-PAR(11+NFPR/2)
       DO I=1,NDM
         F(NINT)=F(NINT)+U(NDM+I)*U(NDM+I)
       ENDDO
       IF(NFPX.NE.0) THEN
         DO I=1,NFPX
           F(NINT)=F(NINT)+PAR(ICP(NFPR-NFPX+I))**2
         ENDDO
       ENDIF
!
      RETURN
      END SUBROUTINE FIBL
!
!     ---------- ------
      SUBROUTINE STPNBL(IAP,RAP,PAR,ICP,NTSR,NCOLRS, &
           RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates starting data for the 2-parameter continuation of folds.
! (BVP).
!
      DIMENSION PAR(*),ICP(*),IAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),TM(*),DTM(*)
! Local
      DIMENSION ICPRS(NPARX),RLDOTRS(NPARX)
!
      LOGICAL FOUND
!
       NDIM=IAP(1)
       IRS=IAP(3)
       NDM=IAP(23)
       NFPR=IAP(29)
!
       CALL FINDLB(IAP,IRS,NFPR1,FOUND)
       CALL READBV(IAP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOTRS,UPS,UDOTPS, &
            TM,ITPRS,NDX)
!
       NFPR0=NFPR/2
       DO I=1,NFPR0
         RLDOT(I)=RLDOTRS(I)
       ENDDO
!
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+NDM+1
           K2=I*NDIM
           DO K=K1,K2
             UPS(K,J)=0.d0
             UDOTPS(K,J)=0.d0
           ENDDO
         ENDDO
       ENDDO
       K1=NDM+1
       NRSP1=NTSR+1
       DO K=K1,NDIM
         UPS(K,NRSP1)=0.d0
         UDOTPS(K,NRSP1)=0.d0
       ENDDO
!
       NFPX=NFPR/2-1
       IF(NFPX.GT.0) THEN
         DO I=1,NFPX
           PAR(ICP(NFPR0+1+I))=0.d0
           RLDOT(NFPR0+I+1)=0.d0
         ENDDO
       ENDIF
! Initialize the norm of the null vector
       PAR(11+NFPR/2)=0.
       RLDOT(NFPR0+1)=0.d0
!
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
!
       NODIR=0
!
      RETURN
      END SUBROUTINE STPNBL
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!   Subroutines for BP cont (BVPs) (by F. Dercole)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- -----
      SUBROUTINE FNBBP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates the equations for the 2-parameter continuation
! of BP (BVP).
!
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
      DOUBLE PRECISION RAP(*),UOLD(*)
! Local
      ALLOCATABLE DFU(:),DFP(:),UU1(:),UU2(:),FF1(:),FF2(:)
!
       NDM=IAP(23)
       NFPR=IAP(29)
!
! Generate the function.
!
       ALLOCATE(DFU(NDM*NDM),DFP(NDM*NPARX))
       CALL FFBBP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
!
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU,DFP)
         RETURN
       ENDIF
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NDIM),FF2(NDIM))
!
! Generate the Jacobian.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U(J)
           UU2(J)=U(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FFBBP(IAP,RAP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         CALL FFBBP(IAP,RAP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(UU1,UU2,FF2)
       IF (IJAC.EQ.1)THEN
         DEALLOCATE(DFU,DFP,FF1)
         RETURN
       ENDIF
!
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FFBBP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
!
       DEALLOCATE(DFU,DFP,FF1)
!
      RETURN
      END SUBROUTINE FNBBP
!
!     ---------- -----
      SUBROUTINE FFBBP(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),U(*),ICP(*),PAR(*),F(*),DFDU(NDM,*),DFDP(NDM,*)
      DOUBLE PRECISION RAP(*),UOLD(*)
! Local
      ALLOCATABLE FI(:),DINT(:,:)
      DOUBLE PRECISION DUM(1),UPOLD(NDM)
!
       ISW=IAP(10)
       NBC=IAP(12)
       NINT=IAP(13)
!
       IF(ISW.LT.0) THEN
!        ** start
         NBC0=(4*NBC-NINT-5*NDM+2)/15
         NNT0=(-NBC+4*NINT+5*NDM-23)/15
       ELSE IF(ISW.EQ.2) THEN
!        ** Non-generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ELSE
!        ** generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ENDIF
       NFPX=NBC0+NNT0-NDM+1
!
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
       IF(NNT0.GT.0) THEN
         ALLOCATE(FI(NNT0),DINT(NNT0,NDM))
         CALL FUNI(IAP,RAP,NDM,UOLD,UOLD,ICP,PAR,0,UPOLD,DUM,DUM)
         CALL ICNI(IAP,RAP,NDM,PAR,ICP,NNT0,U,UOLD,DUM,UPOLD,FI,1,DINT)
       ENDIF
!
       IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
!        ** Non-generic and/or start
         DO I=1,NDM
           F(I)=F(I)-PAR(11+3*NFPX+NDM+1)*U(NDIM-NDM+I)
         ENDDO
       ENDIF
!
       IF(ISW.GT.0) THEN
!        ** restart 1 or 2
         DO I=1,NDM
           F(NDM+I)=0.d0
           DO J=1,NDM
             F(NDM+I)=F(NDM+I)-DFDU(J,I)*U(NDM+J)
           ENDDO
           DO J=1,NNT0
             F(NDM+I)=F(NDM+I)+DINT(J,I)*PAR(11+2*NFPX+NBC0+J)
           ENDDO
         ENDDO
       ELSE
!        ** start
         DO I=1,NDM
           F(NDM+I)=0.d0
           F(2*NDM+I)=0.d0
           F(3*NDM+I)=PAR(11+3*NFPX+NDM+2)*U(NDM+I)+ &
                PAR(11+3*NFPX+NDM+3)*U(2*NDM+I)
           DO J=1,NDM
             F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
             F(2*NDM+I)=F(2*NDM+I)+DFDU(I,J)*U(2*NDM+J)
             F(3*NDM+I)=F(3*NDM+I)-DFDU(J,I)*U(3*NDM+J)
           ENDDO
           DO J=1,NFPX
             F(NDM+I)=F(NDM+I)+DFDP(I,ICP(J))*PAR(11+J)
             F(2*NDM+I)=F(2*NDM+I)+DFDP(I,ICP(J))*PAR(11+NFPX+J)
           ENDDO
           DO J=1,NNT0
             F(3*NDM+I)=F(3*NDM+I)+DINT(J,I)*PAR(11+2*NFPX+NBC0+J)
           ENDDO
         ENDDO
       ENDIF
       IF(NNT0.GT.0) THEN
         DEALLOCATE(FI,DINT)
       ENDIF
!
      RETURN
      END SUBROUTINE FFBBP
!
!     ---------- -----
      SUBROUTINE BCBBP(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates the boundary conditions for the 2-parameter continuation
! of BP (BVP).
!
      DIMENSION IAP(*),U0(*),U1(*),F(NBC),ICP(*),PAR(*),DBC(NBC,*)
! Local
      ALLOCATABLE UU1(:),UU2(:),FF1(:),FF2(:),DFU(:,:)
!
       ISW=IAP(10)
       NINT=IAP(13)
       NDM=IAP(23)
!      NBC0=IAP(24)
!      NNT0=IAP(25)
       NFPR=IAP(29)
!
       IF(ISW.LT.0) THEN
!        ** start
         NBC0=(4*NBC-NINT-5*NDM+2)/15
       ELSE IF(ISW.EQ.2) THEN
!        ** Non-generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
       ELSE
!        ** generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
       ENDIF
!
! Generate the function.
!
       ALLOCATE(DFU(NBC0,2*NDM+NPARX))
       CALL FBBBP(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,F,DFU)
!
       IF(IJAC.EQ.0)THEN
          DEALLOCATE(DFU)
          RETURN
       ENDIF
!
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NBC),FF2(NBC))
!
! Derivatives with respect to U0.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U0(I)).GT.UMX)UMX=DABS(U0(I))
       ENDDO
       EP=HMACH*(1+UMX)
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U0(J)
           UU2(J)=U0(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FBBBP(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,UU1,U1,FF1,DFU)
         CALL FBBBP(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,UU2,U1,FF2,DFU)
         DO J=1,NBC
           DBC(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
! Derivatives with respect to U1.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U1(I)).GT.UMX)UMX=DABS(U1(I))
       ENDDO
       EP=HMACH*(1+UMX)
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U1(J)
           UU2(J)=U1(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FBBBP(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,U0,UU1,FF1,DFU)
         CALL FBBBP(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,U0,UU2,FF2,DFU)
         DO J=1,NBC
           DBC(J,NDIM+I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(UU1,UU2,FF2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF1,DFU)
         RETURN
       ENDIF
!
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FBBBP(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,FF1,DFU)
         DO J=1,NBC
           DBC(J,2*NDIM+ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
!
       DEALLOCATE(FF1,DFU)
!
      RETURN
      END SUBROUTINE BCBBP
!
!     ---------- -----
      SUBROUTINE FBBBP(IAP,RAP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,FB,DBC)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),PAR(*),ICP(*),U0(*),U1(*),FB(*),DBC(NBC0,*)
!
       ISW=IAP(10)
       NINT=IAP(13)
       NDM=IAP(23)
!
       IF(ISW.LT.0) THEN
!        ** start
         NNT0=(-NBC+4*NINT+5*NDM-23)/15
       ELSE IF(ISW.EQ.2) THEN
!        ** Non-generic case
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ELSE
!        ** generic case
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ENDIF
       NFPX=NBC0+NNT0-NDM+1
!
       CALL BCNI(IAP,RAP,NDM,PAR,ICP,NBC0,U0,U1,FB,2,DBC)
!
       IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
!        ** Non-generic and/or start
         DO I=1,NBC0
           FB(I)=FB(I)+PAR(11+3*NFPX+NDM+1)*PAR(11+2*NFPX+I)
         ENDDO
       ENDIF
!
       IF(ISW.GT.0) THEN
!        ** restart 1 or 2
         DO I=1,NDM
           FB(NBC0+I)=-U0(NDM+I)
           FB(NBC0+NDM+I)=U1(NDM+I)
           DO J=1,NBC0
             FB(NBC0+I)=FB(NBC0+I)+DBC(J,I)*PAR(11+2*NFPX+J)
             FB(NBC0+NDM+I)=FB(NBC0+NDM+I)+DBC(J,NDM+I)*PAR(11+2*NFPX+J)
           ENDDO
         ENDDO
         DO I=1,NFPX
           FB(NBC0+2*NDM+I)=PAR(11+3*NFPX+NDM+3+I)
           DO J=1,NBC0
             FB(NBC0+2*NDM+I)=FB(NBC0+2*NDM+I)+ &
                  DBC(J,2*NDM+ICP(I))*PAR(11+2*NFPX+J)
           ENDDO
         ENDDO
       ELSE
!        ** start
         DO I=1,NBC0
           FB(NBC0+I)=0.d0
           FB(2*NBC0+I)=0.d0
           DO J=1,NDM
             FB(NBC0+I)=FB(NBC0+I)+DBC(I,J)*U0(NDM+J)
             FB(NBC0+I)=FB(NBC0+I)+DBC(I,NDM+J)*U1(NDM+J)
             FB(2*NBC0+I)=FB(2*NBC0+I)+DBC(I,J)*U0(2*NDM+J)
             FB(2*NBC0+I)=FB(2*NBC0+I)+DBC(I,NDM+J)*U1(2*NDM+J)
           ENDDO
           DO J=1,NFPX
             FB(NBC0+I)=FB(NBC0+I)+DBC(I,2*NDM+ICP(J))*PAR(11+J)
             FB(2*NBC0+I)=FB(2*NBC0+I)+ &
                  DBC(I,2*NDM+ICP(J))*PAR(11+NFPX+J)
           ENDDO
         ENDDO
         DO I=1,NDM
           FB(3*NBC0+I)=-U0(3*NDM+I)
           FB(3*NBC0+NDM+I)=U1(3*NDM+I)
           DO J=1,NBC0
             FB(3*NBC0+I)=FB(3*NBC0+I)+DBC(J,I)*PAR(11+2*NFPX+J)
             FB(3*NBC0+NDM+I)=FB(3*NBC0+NDM+I)+ &
                  DBC(J,NDM+I)*PAR(11+2*NFPX+J)
           ENDDO
         ENDDO
         DO I=1,NFPX
           FB(3*NBC0+2*NDM+I)=PAR(11+3*NFPX+NDM+3+I)
           DO J=1,NBC0
             FB(3*NBC0+2*NDM+I)=FB(3*NBC0+2*NDM+I)+ &
                  DBC(J,2*NDM+ICP(I))*PAR(11+2*NFPX+J)
           ENDDO
         ENDDO
       ENDIF
!
      RETURN
      END SUBROUTINE FBBBP
!
!     ---------- -----
      SUBROUTINE ICBBP(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD, &
           F,IJAC,DINT)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates integral conditions for the 2-parameter continuation
! of BP (BVP).
!
      DIMENSION IAP(*),RAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
! Local
      ALLOCATABLE UU1(:),UU2(:),FF1(:),FF2(:),DFU(:)
!
       ISW=IAP(10)
       NBC=IAP(12)
       NDM=IAP(23)
       NFPR=IAP(29)
!
       IF(ISW.LT.0) THEN
!        ** start
         NNT0=(-NBC+4*NINT+5*NDM-23)/15
       ELSE IF(ISW.EQ.2) THEN
!        ** Non-generic case
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ELSE
!        ** generic case
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ENDIF
!
! Generate the function.
!
       IF(NNT0.GT.0) THEN
         ALLOCATE(DFU(NNT0*(NDM+NPARX)))
       ELSE
         ALLOCATE(DFU(1))
       ENDIF
       CALL FIBBP(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT, &
            UPOLD,F,DFU)
!
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU)
         RETURN
       ENDIF
!
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NINT),FF2(NINT))
!
! Generate the Jacobian.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U(J)
           UU2(J)=U(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FIBBP(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,UU1,UOLD,UDOT, &
              UPOLD,FF1,DFU)
         CALL FIBBP(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,UU2,UOLD,UDOT, &
              UPOLD,FF2,DFU)
         DO J=1,NINT
           DINT(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(UU1,UU2,FF2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF1,DFU)
         RETURN
       ENDIF
!
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FIBBP(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT, &
              UPOLD,FF1,DFU)
         DO J=1,NINT
           DINT(J,NDIM+ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
!
       DEALLOCATE(FF1,DFU)
!
      RETURN
      END SUBROUTINE ICBBP
!
!     ---------- -----
      SUBROUTINE FIBBP(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0, &
           U,UOLD,UDOT,UPOLD,FI,DINT)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),RAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),FI(*),DINT(NNT0,*)
!
! Local
      ALLOCATABLE F(:),DFU(:,:),DFP(:,:)
!
       ISW=IAP(10)
       NBC=IAP(12)
       NDM=IAP(23)
!
       IF(ISW.LT.0) THEN
!        ** start
         NBC0=(4*NBC-NINT-5*NDM+2)/15
       ELSE IF(ISW.EQ.2) THEN
!        ** Non-generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
       ELSE
!        ** generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
       ENDIF
       NFPX=NBC0+NNT0-NDM+1
!
       ALLOCATE(F(NDM),DFU(NDM,NDM),DFP(NDM,NPARX))
       CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,2,F,DFU,DFP)
       IF(NNT0.GT.0) THEN
         CALL ICNI(IAP,RAP,NDM,PAR,ICP,NNT0,U,UOLD,UDOT,UPOLD,FI,2,DINT)
!
         IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
!          ** Non-generic and/or start
           DO I=1,NNT0
             FI(I)=FI(I)+PAR(11+3*NFPX+NDM+1)*PAR(11+2*NFPX+NBC0+I)
           ENDDO
         ENDIF
       ENDIF
!
       IF(ISW.GT.0) THEN
!        ** restart 1 or 2
         DO I=1,NFPX
           FI(NNT0+I)=-PAR(11+3*NFPX+NDM+3+I)
           DO J=1,NDM
             FI(NNT0+I)=FI(NNT0+I)-DFP(J,ICP(I))*U(NDM+J)
           ENDDO
           DO J=1,NNT0
             FI(NNT0+I)=FI(NNT0+I)+ &
                  DINT(J,NDM+ICP(I))*PAR(11+2*NFPX+NBC0+J)
           ENDDO
         ENDDO
       ELSE
!        ** start
         DO I=1,NNT0
           FI(NNT0+I)=0.d0
           FI(2*NNT0+I)=0.d0
           DO J=1,NDM
             FI(NNT0+I)=FI(NNT0+I)+DINT(I,J)*U(NDM+J)
             FI(2*NNT0+I)=FI(2*NNT0+I)+DINT(I,J)*U(2*NDM+J)
           ENDDO
           DO J=1,NFPX
             FI(NNT0+I)=FI(NNT0+I)+DINT(I,NDM+ICP(J))*PAR(11+J)
             FI(2*NNT0+I)=FI(2*NNT0+I)+DINT(I,NDM+ICP(J))*PAR(11+NFPX+J)
           ENDDO
         ENDDO
         FI(3*NNT0+1)=-1.d0
         FI(3*NNT0+2)=-1.d0
         FI(3*NNT0+3)=0.d0
         FI(3*NNT0+4)=0.d0
         DO I=1,NDM
           FI(3*NNT0+1)=FI(3*NNT0+1)+U(NDM+I)*UOLD(NDM+I)
           FI(3*NNT0+2)=FI(3*NNT0+2)+U(2*NDM+I)*UOLD(2*NDM+I)
           FI(3*NNT0+3)=FI(3*NNT0+3)+U(NDM+I)*UOLD(2*NDM+I)
           FI(3*NNT0+4)=FI(3*NNT0+4)+U(2*NDM+I)*UOLD(NDM+I)
         ENDDO
         DO I=1,NFPX
           FI(3*NNT0+1)=FI(3*NNT0+1)+PAR(11+I)**2
           FI(3*NNT0+2)=FI(3*NNT0+2)+PAR(11+NFPX+I)**2
           FI(3*NNT0+3)=FI(3*NNT0+3)+PAR(11+I)*PAR(11+NFPX+I)
           FI(3*NNT0+4)=FI(3*NNT0+4)+PAR(11+I)*PAR(11+NFPX+I)
           FI(3*NNT0+4+I)=-PAR(11+3*NFPX+NDM+3+I)+ &
                PAR(11+3*NFPX+NDM+2)*PAR(11+I)+ &
                PAR(11+3*NFPX+NDM+3)*PAR(11+NFPX+I)
           DO J=1,NDM
             FI(3*NNT0+4+I)=FI(3*NNT0+4+I)-DFP(J,ICP(I))*U(3*NDM+J)
           ENDDO
           DO J=1,NNT0
              FI(3*NNT0+4+I)=FI(3*NNT0+4+I)+DINT(J,NDM+ICP(I))* &
                   PAR(11+2*NFPX+NBC0+J)
           ENDDO
         ENDDO
       ENDIF
       DEALLOCATE(F,DFU,DFP)
!
       FI(NINT)=-PAR(11+3*NFPX+NDM)
       DO I=1,NDM
         FI(NINT)=FI(NINT)+U(NDIM-NDM+I)**2
       ENDDO
       DO I=1,NBC0+NNT0
         FI(NINT)=FI(NINT)+PAR(11+2*NFPX+I)**2
       ENDDO
!
      RETURN
      END SUBROUTINE FIBBP
!
!     ---------- -------
      SUBROUTINE STPNBBP(IAP,RAP,PAR,ICP,NTSR,NCOLRS, &
           RLCUR,RLDOT,NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
!
      USE SOLVEBV
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates starting data for the 2-parameter continuation
! of BP (BVP).
!
      DIMENSION PAR(*),ICP(*),IAP(*),RAP(*),RLCUR(*),RLDOT(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*)
      DIMENSION TM(*),DTM(*),THL(*),THU(*)
! Local
      ALLOCATABLE DUPS(:,:),VPS(:,:),VDOTPS(:,:),RVDOT(:)
      ALLOCATABLE THU1(:),THL1(:)
      ALLOCATABLE FA(:,:),FC(:),P0(:,:),P1(:,:)
      ALLOCATABLE U(:),UPOLD(:)
      DIMENSION ICPRS(NPARX),RLDOTRS(NPARX)
      DOUBLE PRECISION DUM(1)
!
      LOGICAL FOUND
!
       NDIM=IAP(1)
       IRS=IAP(3)
       ISW=IAP(10)
       NBC=IAP(12)
       NINT=IAP(13)
       NDM=IAP(23)
       NFPR=IAP(29)
!
       CALL FINDLB(IAP,IRS,NFPR1,FOUND)
       READ(3,*)IBR,NTOTRS,ITPRS,IRS,NFPR1,ISWRS,NTPLRS,NARS
       BACKSPACE 3
       NDIM3=NARS-1
!
       IF(NDIM.EQ.NDIM3) THEN
!        ** restart 2
         CALL STPNBV(IAP,RAP,PAR,ICP,NTSR,NCOLRS,RLCUR,RLDOT, &
              NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
         RETURN
       ENDIF
!
       IF(ISW.LT.0) THEN
!        ** start
         NBC0=(4*NBC-NINT-5*NDM+2)/15
         NNT0=(-NBC+4*NINT+5*NDM-23)/15
       ELSE IF(ISW.EQ.2) THEN
!        ** Non-generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ELSE
!        ** generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ENDIF
       NFPX=NBC0+NNT0-NDM+1
       NRSP1=NTSR+1
!
       IF(ISW.LT.0) THEN
!
! Start
!
!        ** allocation
         ALLOCATE(DUPS(NDX,NRSP1),VDOTPS(NDX,NRSP1),RVDOT(NFPX))
         ALLOCATE(THU1(NDM),THL1(NFPR))
         ALLOCATE(FA(NDM*NCOLRS,NRSP1),FC(NBC0+NNT0+1))
         ALLOCATE(P0(NDM,NDM),P1(NDM,NDM))
         ALLOCATE(U(NDM),UPOLD(NDM))
!
!        ** redefine IAP(1)
         IAP(1)=NDM
!
!        ** read the std branch
         CALL READBV(IAP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOTRS,UPS, &
              UDOTPS,TM,ITPRS,NDX)
!
         DO I=1,NTSR
           DTM(I)=TM(I+1)-TM(I)
         ENDDO
!
         DO I=1,NFPX
           RLCUR(I)=PAR(ICPRS(I))
         ENDDO
!
! Compute the second null vector
!
!        ** redefine IAP, RAP
         NTST=IAP(5)
         NCOL=IAP(6)
         IAP(5)=NTSR
         IAP(6)=NCOLRS
         IAP(12)=NBC0
         IAP(13)=NNT0
         IAP(29)=NFPX
         DET=RAP(14)
!
!        ** compute UPOLDP
         IF(NNT0.GT.0) THEN
           DO J=1,NTSR
             DO I=1,NCOLRS
               DO K=1,NDM
                 U(K)=UPS((I-1)*NDIM+K,J)
               ENDDO
               CALL FUNI(IAP,RAP,NDM,U,U,ICPRS,PAR,0,UPOLD,DUM,DUM)
               DO K=1,NDM
                 UPOLDP((I-1)*NDIM+K,J)=UPOLD(K)
               ENDDO
             ENDDO
           ENDDO
           DO I=1,NDM
             U(I)=UPS(I,NRSP1)
           ENDDO
           CALL FUNI(IAP,RAP,NDM,U,U,ICPRS,PAR,0,UPOLD,DUM,DUM)
           DO I=1,NDM
             UPOLDP(I,NRSP1)=UPOLD(I)
           ENDDO
         ENDIF
!
!        ** unit weights
         DO I=1,NFPR
           THL1(I)=1.d0
         ENDDO
         DO I=1,NDM
           THU1(I)=1.d0
         ENDDO
!
!        ** call SOLVBV
         RDSZ=0.d0
         NLLV=1
         IFST=1
         CALL SOLVBV(IFST,IAP,RAP,PAR,ICPRS,FUNI,BCNI,ICNI,RDSZ,NLLV, &
              RLCUR,RLCUR,RLDOTRS,NDX,UPS,DUPS,UPS,UDOTPS,UPOLDP,DTM,FA,FC, &
              P0,P1,THL1,THU1)
!
         DO I=1,NDM
           VDOTPS(I,NRSP1)=FC(I)
         ENDDO
         DO I=1,NFPX
           RVDOT(I)=FC(NDM+I)
         ENDDO
!
         DO J=1,NTSR
           DO I=1,NCOLRS
             DO K=1,NDM
               VDOTPS((I-1)*NDM+K,J)=FA((I-1)*NDM+K,J)
             ENDDO
           ENDDO
         ENDDO
!
!        ** normalization
         CALL SCALEB(IAP,NDM,NDX,UDOTPS,RLDOTRS,DTM,THL1,THU1)
         CALL SCALEB(IAP,NDM,NDX,VDOTPS,RVDOT,DTM,THL1,THU1)
!
!        ** restore IAP, RAP
         IAP(1)=NDIM
         IAP(5)=NTST
         IAP(6)=NCOL
         IAP(12)=NBC
         IAP(13)=NINT
         IAP(29)=NFPR
         RAP(14)=DET

!        ** init UPS,PAR
         DO J=1,NTSR
           DO I=NCOLRS,1,-1
             DO K=1,NDM
               UPS((I-1)*NDIM+K,J)=UPS((I-1)*NDM+K,J)
               UPS((I-1)*NDIM+NDM+K,J)=UDOTPS((I-1)*NDM+K,J)
               UPS((I-1)*NDIM+2*NDM+K,J)=VDOTPS((I-1)*NDM+K,J)
               UPS((I-1)*NDIM+3*NDM+K,J)=0.d0
               UDOTPS((I-1)*NDIM+K,J)=0.d0
               UDOTPS((I-1)*NDIM+NDM+K,J)=0.d0
               UDOTPS((I-1)*NDIM+2*NDM+K,J)=0.d0
               UDOTPS((I-1)*NDIM+3*NDM+K,J)=0.d0
             ENDDO
           ENDDO
         ENDDO
         DO K=1,NDM
           UPS(K+NDM,NRSP1)=UDOTPS(K,NRSP1)
           UPS(K+2*NDM,NRSP1)=VDOTPS(K,NRSP1)
           UPS(K+3*NDM,NRSP1)=0.d0
           UDOTPS(K,NRSP1)=0.d0
           UDOTPS(K+NDM,NRSP1)=0.d0
           UDOTPS(K+2*NDM,NRSP1)=0.d0
           UDOTPS(K+3*NDM,NRSP1)=0.d0
         ENDDO
!
         DO I=1,NFPX
           PAR(11+I)=RLDOTRS(I)
           PAR(11+NFPX+I)=RVDOT(I)
           RLDOT(I)=0.d0
           RLDOT(NFPX+I+2)=0.d0
           RLDOT(2*NFPX+I+2)=0.d0
         ENDDO
!
!        ** init psi^*2,psi^*3
         DO I=1,NBC0+NNT0
           PAR(11+2*NFPX+I)=0.d0
           RLDOT(3*NFPX+I+2)=0.d0
         ENDDO
!
!        ** init a,b,c1,c1,d
         PAR(11+3*NFPX+NDM)=0.d0
         PAR(11+3*NFPX+NDM+1)=0.d0
         PAR(11+3*NFPX+NDM+2)=0.d0
         PAR(11+3*NFPX+NDM+3)=0.d0
         RLDOT(NFPX+1)=0.d0
         RLDOT(NFPX+2)=1.d0
         RLDOT(4*NFPX+NDM+2)=0.d0
         RLDOT(4*NFPX+NDM+3)=0.d0
         DO I=1,NFPX
           PAR(11+3*NFPX+NDM+3+I)=0.d0
           RLDOT(4*NFPX+NDM+I+3)=0.d0
         ENDDO
!
         DEALLOCATE(DUPS,VDOTPS,RVDOT)
         DEALLOCATE(THU1,THL1)
         DEALLOCATE(FA,FC)
         DEALLOCATE(P0,P1)
         DEALLOCATE(U,UPOLD)
!
         NODIR=0
!
       ELSE
!
! Restart 1
!
         ALLOCATE(VPS(2*NDX,NRSP1),VDOTPS(2*NDX,NRSP1))
!
!        ** read the std branch
         IAP(1)=2*NDIM
         CALL READBV(IAP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOTRS,VPS, &
              VDOTPS,TM,ITPRS,2*NDX)
         IAP(1)=NDIM
!
         DO J=1,NTSR
           DO I=1,NCOLRS
             DO K=1,NDM
               UPS((I-1)*NDIM+K,J)=VPS((I-1)*2*NDIM+K,J)
               UPS((I-1)*NDIM+K+NDM,J)=VPS((I-1)*2*NDIM+K+3*NDM,J)
             ENDDO
           ENDDO
         ENDDO
         DO K=1,NDM
           UPS(K,NRSP1)=VPS(K,NRSP1)
           UPS(K+NDM,NRSP1)=VPS(K+3*NDM,NRSP1)
         ENDDO
!
         DEALLOCATE(VPS,VDOTPS)
!
         NODIR=1
!
       ENDIF
!
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
!
      RETURN
      END SUBROUTINE STPNBBP
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!          Routines for Interface with User Supplied Routines
!  (To generate Jacobian by differencing, if not supplied analytically)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ---------- ----
      SUBROUTINE FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      IMPLICIT NONE
!
      DOUBLE PRECISION HMACH,RSMALL,RLARGE
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Interface subroutine to user supplied FUNC.
!
      INTEGER IAP(*),ICP(*),NDIM,IJAC
      DOUBLE PRECISION RAP(*),U(*),UOLD(*),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(*),DFDU(NDIM,*),DFDP(NDIM,*)
!
      INTEGER JAC,I,J,NFPR,IJC
      DOUBLE PRECISION UMX,EP,UU
!
       JAC=IAP(22)
!
! Generate the function.
!
!
! if the user specified the Jacobian but not the
! parameter derivatives we do not generate the Jacobian here
!
       IF(JAC.EQ.0.AND.IJAC.NE.0)THEN
!
! Generate the Jacobian by differencing.
!
         UMX=0.d0
         DO I=1,NDIM
           IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
         ENDDO
!
         EP=HMACH*(1+UMX)
!
         DO I=1,NDIM
           UU=U(I)
           U(I)=UU-EP
           CALL FUNC(NDIM,U,ICP,PAR,0,F,DFDU,DFDP)
           U(I)=UU+EP
           CALL FUNC(NDIM,U,ICP,PAR,0,DFDU(1,I),DFDU,DFDP)
           U(I)=UU
           DO J=1,NDIM
             DFDU(J,I)=(DFDU(J,I)-F(J))/(2*EP)
           ENDDO
         ENDDO
!
       ENDIF
!
       IF((IJAC.EQ.1.AND.JAC.NE.0).OR.(IJAC.EQ.2.AND.JAC.EQ.1))THEN
         IJC=IJAC
       ELSE
         IJC=0
       ENDIF
       CALL FUNC(NDIM,U,ICP,PAR,IJC,F,DFDU,DFDP)
       IF(JAC.EQ.1.OR.IJAC.NE.2)RETURN
       NFPR=IAP(29)
       DO I=1,NFPR
         EP=HMACH*( 1 +DABS(PAR(ICP(I))) )
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FUNC(NDIM,U,ICP,PAR,0,DFDP(1,ICP(I)),DFDU,DFDP)
         DO J=1,NDIM
           DFDP(J,ICP(I))=(DFDP(J,ICP(I))-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
!
      RETURN
      END SUBROUTINE FUNI
!
!     ---------- ----
      SUBROUTINE BCNI(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Interface subroutine to the user supplied BCND.
!
      DIMENSION IAP(*),PAR(*),ICP(*),U0(*),U1(*),F(*),DBC(NBC,*)
! Local
      ALLOCATABLE U1ZZ(:),U2ZZ(:),F1ZZ(:),F2ZZ(:)
!
       JAC=IAP(22)
       NFPR=IAP(29)
!
! Generate the function.
!
       IF(JAC.EQ.0)THEN
         IJC=0
       ELSE
         IJC=IJAC
       ENDIF
       CALL BCND(NDIM,PAR,ICP,NBC,U0,U1,F,IJC,DBC)
!
       IF(JAC.EQ.1 .OR. IJAC.EQ.0)RETURN
!
       ALLOCATE(U1ZZ(NDIM),U2ZZ(NDIM),F1ZZ(NBC),F2ZZ(NBC))
!
! Generate the Jacobian by differencing.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U0(I)).GT.UMX)UMX=DABS(U0(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         DO J=1,NDIM
           U1ZZ(J)=U0(J)
           U2ZZ(J)=U0(J)
         ENDDO
         U1ZZ(I)=U1ZZ(I)-EP
         U2ZZ(I)=U2ZZ(I)+EP
         CALL BCND(NDIM,PAR,ICP,NBC,U1ZZ,U1,F1ZZ,0,DBC)
         CALL BCND(NDIM,PAR,ICP,NBC,U2ZZ,U1,F2ZZ,0,DBC)
         DO J=1,NBC
           DBC(J,I)=(F2ZZ(J)-F1ZZ(J))/(2*EP)
         ENDDO
       ENDDO
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U1(I)).GT.UMX)UMX=DABS(U1(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         DO J=1,NDIM
           U1ZZ(J)=U1(J)
           U2ZZ(J)=U1(J)
         ENDDO
         U1ZZ(I)=U1ZZ(I)-EP
         U2ZZ(I)=U2ZZ(I)+EP
         CALL BCND(NDIM,PAR,ICP,NBC,U0,U1ZZ,F1ZZ,0,DBC)
         CALL BCND(NDIM,PAR,ICP,NBC,U0,U2ZZ,F2ZZ,0,DBC)
         DO J=1,NBC
           DBC(J,NDIM+I)=(F2ZZ(J)-F1ZZ(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(U1ZZ,U2ZZ,F2ZZ)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(F1ZZ)
         RETURN
       ENDIF
!
       DO I=1,NFPR
         EP=HMACH*( 1 +DABS(PAR(ICP(I))) )
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL BCND(NDIM,PAR,ICP,NBC,U0,U1,F1ZZ,0,DBC)
         DO J=1,NBC
           DBC(J,2*NDIM+ICP(I))=(F1ZZ(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
!
      DEALLOCATE(F1ZZ)
      RETURN
      END SUBROUTINE BCNI
!
!     ---------- ----
      SUBROUTINE ICNI(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD, &
           F,IJAC,DINT)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Interface subroutine to user supplied ICND.
!
      DIMENSION IAP(*),RAP(*),U(*),UOLD(*),UDOT(*),UPOLD(*)
      DIMENSION F(*),DINT(NINT,*),ICP(*),PAR(*)
! Local
      ALLOCATABLE U1ZZ(:),U2ZZ(:),F1ZZ(:),F2ZZ(:)
!
       JAC=IAP(22)
       NFPR=IAP(29)
!
! Generate the integrand.
!
       IF(JAC.EQ.0)THEN
         IJC=0
       ELSE
         IJC=IJAC
       ENDIF
       CALL ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJC,DINT)
!
       IF(JAC.EQ.1 .OR. IJAC.EQ.0)RETURN
!
       ALLOCATE(U1ZZ(NDIM),U2ZZ(NDIM),F1ZZ(NINT),F2ZZ(NINT))
!
! Generate the Jacobian by differencing.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       DO I=1,NDIM
         DO J=1,NDIM
           U1ZZ(J)=U(J)
           U2ZZ(J)=U(J)
         ENDDO
         U1ZZ(I)=U1ZZ(I)-EP
         U2ZZ(I)=U2ZZ(I)+EP
         CALL ICND(NDIM,PAR,ICP,NINT,U1ZZ,UOLD,UDOT,UPOLD,F1ZZ,0,DINT)
         CALL ICND(NDIM,PAR,ICP,NINT,U2ZZ,UOLD,UDOT,UPOLD,F2ZZ,0,DINT)
         DO J=1,NINT
           DINT(J,I)=(F2ZZ(J)-F1ZZ(J))/(2*EP)
         ENDDO
       ENDDO
!
       DEALLOCATE(U1ZZ,U2ZZ,F2ZZ)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(F1ZZ)
         RETURN
       ENDIF
!
       DO I=1,NFPR
         EP=HMACH*( 1 +DABS(PAR(ICP(I))) )
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F1ZZ,0,DINT)
         DO J=1,NINT
           DINT(J,NDIM+ICP(I))=(F1ZZ(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
!
      DEALLOCATE(F1ZZ)
      RETURN
      END SUBROUTINE ICNI
!
!     ---------- ----
      SUBROUTINE FOPI(IAP,RAP,NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Interface subroutine to user supplied FOPT.
!
      DIMENSION IAP(*),RAP(*),U(*),ICP(*),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F,DFDU(*),DFDP(*)
! Local
      ALLOCATABLE U1ZZ(:),U2ZZ(:)
!
       JAC=IAP(22)
       NFPR=IAP(29)
!
! Generate the objective function.
!
       IF(JAC.EQ.0)THEN
         IJC=0
       ELSE
         IJC=IJAC
       ENDIF
       CALL FOPT(NDIM,U,ICP,PAR,IJC,F,DFDU,DFDP)
!
       IF(JAC.EQ.1 .OR. IJAC.EQ.0)RETURN
!
! Generate the Jacobian by differencing.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
!
       EP=HMACH*(1+UMX)
!
       ALLOCATE(U1ZZ(NDIM),U2ZZ(NDIM))
       DO I=1,NDIM
         DO J=1,NDIM
           U1ZZ(J)=U(J)
           U2ZZ(J)=U(J)
         ENDDO
         U1ZZ(I)=U1ZZ(I)-EP
         U2ZZ(I)=U2ZZ(I)+EP
         CALL FOPT(NDIM,U1ZZ,ICP,PAR,0,F1,DFDU,DFDP)
         CALL FOPT(NDIM,U2ZZ,ICP,PAR,0,F2,DFDU,DFDP)
         DFDU(I)=(F2-F1)/(2*EP)
       ENDDO
       DEALLOCATE(U1ZZ,U2ZZ)
!
       IF(IJAC.EQ.1)RETURN
!
       DO I=1,NFPR
         EP=HMACH*( 1 +DABS(PAR(ICP(I))) )
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FOPT(NDIM,U,ICP,PAR,0,F1,DFDU,DFDP)
         DFDP(ICP(I))=(F1-F)/EP
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
!
       RETURN
       END SUBROUTINE FOPI
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      END MODULE INTERFACES
