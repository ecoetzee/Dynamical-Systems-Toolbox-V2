!========================================================================
! AUTLIB5.F - Subroutine for AUTO
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
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        Subroutines for Homoclinic Bifurcation Analysis
!       (A. R. Champneys, Yu. A. Kuznetsov, B. Sandstede,
!        B. E. Oldeman, E. J. Doedel)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      MODULE HOMCONT

      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST

      PRIVATE

      PUBLIC :: FNHO,BCHO,ICHO,PVLSHO,STPNHO,INHO,PREHO

!     This common block is also used by demos: don't remove it!!
!
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV

      PARAMETER(NPSIX=NPARX)
      POINTER IREV(:)
      INTEGER, SAVE :: IPSI(NPSIX),IFIXED(NPSIX),IREV
      DOUBLE PRECISION, SAVE :: COMPZERO

      CONTAINS

!     ---------- ----
      SUBROUTINE FNHO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates the equations for homoclinic bifurcation analysis
!
      DIMENSION IAP(*),RAP(*),ICP(*)
      DIMENSION U(*),UOLD(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
! Local
      ALLOCATABLE DFU(:)
!
       NDM=IAP(23)
       NFPR=IAP(29)
!
! Generate the function.
!
      IF(ISTART.GE.0.AND.ITWIST.EQ.1)THEN
        ALLOCATE(DFU(NDIM*NDIM))
      ENDIF
      IF(IJAC.EQ.0)THEN
        CALL FFHO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU)
        IF(ALLOCATED(DFU))DEALLOCATE(DFU)
        RETURN
      ENDIF
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
        CALL FFHO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,DFDU(1,I),NDM,DFU)
        U(I)=UU+EP
        CALL FFHO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU)
        U(I)=UU
        DO J=1,NDIM
          DFDU(J,I)=(F(J)-DFDU(J,I))/(2*EP)
        ENDDO
      ENDDO
!
      CALL FFHO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU)
      IF(IJAC==1)THEN
        IF(ALLOCATED(DFU))DEALLOCATE(DFU)
        RETURN
      ENDIF
!
      DO I=1,NFPR
        PAR(ICP(I))=PAR(ICP(I))+EP
        CALL FFHO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,DFDP(1,ICP(I)),NDM,DFU)
        DO J=1,NDIM
          DFDP(J,ICP(I))=(DFDP(J,ICP(I))-F(J))/EP
        ENDDO
        PAR(ICP(I))=PAR(ICP(I))-EP
      ENDDO
!
      IF(ALLOCATED(DFU))DEALLOCATE(DFU)
      RETURN
      END SUBROUTINE FNHO
!
!     ---------- ----
      SUBROUTINE FFHO(IAP,RAP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU)
!
      USE INTERFACES, ONLY:FUNI
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),ICP(*)
      DIMENSION RAP(*),U(NDIM),UOLD(*),PAR(*),F(*)
      DIMENSION DFDU(NDM,*)
!
!       Local
      DOUBLE PRECISION DDUM1(1)
!
      NDM=IAP(23)
!
      IF(ISTART.GE.0)THEN
         IF(ITWIST.EQ.0)THEN
!           *Evaluate the R.-H. sides
            CALL FUNC(NDM,U,ICP,PAR,0,F,DFDU,DUM1)
         ELSEIF(ITWIST.EQ.1)THEN
!           *Adjoint variational equations for normal vector
            CALL FUNI(IAP,RAP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DDUM1)
!           *Set F = - (Df)^T u
            DO J=1,NDM
               DUM1=0.0D0
               DO I=1,NDM
                  DUM1=DUM1+DFDU(I,J)*U(NDM+I)
               ENDDO
               F(NDM+J) = -DUM1
            ENDDO
!           *Set F =  F + PAR(10) * f
            DO J=1,NDM
               F(NDM+J) = F(NDM+J) + PAR(10) * F(J)
            ENDDO
         ENDIF
      ELSE
!        Homoclinic branch switching
         DO J=0,NDIM-NDM,NDM
            CALL FUNC(NDM,U(J+1),ICP,PAR,0,F(J+1),DFDU,DUM1)
         ENDDO
      ENDIF
!
! Scale by truncation interval T=PAR(11)
!
      IF (ISTART.GE.0) THEN
         DO I=1,NDIM
            F(I)=PAR(11)*F(I)
         ENDDO
      ELSE
         DO I=1,NDM
            F(I)=PAR(10)*F(I)
            DO J=1,NDIM/NDM-2
               F(I+NDM*J)=PAR(19+J*2)*F(I+NDM*J)
            ENDDO
            F(I+NDIM-NDM)=PAR(11)*F(I+NDIM-NDM)
         ENDDO   
      ENDIF
!	
      RETURN
      END SUBROUTINE FFHO
!
!     ---------- ----
      SUBROUTINE BCHO(IAP,RAP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates the boundary conditions for homoclinic bifurcation analysis
!
      DIMENSION IAP(*),ICP(*)
      DIMENSION RAP(*),U0(*),U1(*),F(NBC),PAR(*),DBC(NBC,*)
! Local
      ALLOCATABLE UU(:),FF1(:),FF2(:)
!
       NFPR=IAP(29)
!
! Generate the function.
!
       CALL FBHO(IAP,RAP,NDIM,PAR,ICP,NBC,.TRUE.,U0,U1,F)
!
       IF(IJAC.EQ.0)RETURN
       ALLOCATE(UU(NDIM),FF1(NBC),FF2(NBC))
!
! Derivatives with respect to U0.
!
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U0(I)).GT.UMX)UMX=DABS(U0(I))
       ENDDO
       EP=HMACH*(1+UMX)
       DO I=1,NDIM
         UU(I)=U0(I)
       ENDDO
       DO I=1,NDIM
         UU(I)=U0(I)-EP
         CALL FBHO(IAP,RAP,NDIM,PAR,ICP,NBC,.FALSE.,UU,U1,FF1)
         UU(I)=U0(I)+EP
         CALL FBHO(IAP,RAP,NDIM,PAR,ICP,NBC,.FALSE.,UU,U1,FF2)
         UU(I)=U0(I)
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
         UU(I)=U1(I)
       ENDDO
       DO I=1,NDIM
         UU(I)=U1(I)-EP
         CALL FBHO(IAP,RAP,NDIM,PAR,ICP,NBC,.FALSE.,U0,UU,FF1)
         UU(I)=U1(I)+EP
         CALL FBHO(IAP,RAP,NDIM,PAR,ICP,NBC,.FALSE.,U0,UU,FF2)
         UU(I)=U1(I)
         DO J=1,NBC
           DBC(J,NDIM+I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
!
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FBHO(IAP,RAP,NDIM,PAR,ICP,NBC,.FALSE.,U0,U1,FF2)
         DO J=1,NBC
           DBC(J,2*NDIM+ICP(I))=(FF2(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
!
      DEALLOCATE(FF1,FF2,UU)
      RETURN
      END SUBROUTINE BCHO
!
!     ---------- ----
      SUBROUTINE FBHO(IAP,RAP,NDIM,PAR,ICP,NBC,CSAVE,U0,U1,FB)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates the boundary conditions for homoclinic orbits.
!
      DIMENSION ICP(*),IAP(*)
      DIMENSION RAP(*),PAR(*),U0(*),U1(*),FB(*)
      LOGICAL CSAVE
! Local
      ALLOCATABLE VR(:,:,:),VT(:,:,:),UMAX(:)
      ALLOCATABLE BOUND(:,:),RR(:,:),RI(:,:),XEQUIB1(:),XEQUIB2(:)
      SAVE UMAX
!
      POINTER NRTN(:)
      COMMON /BLRTN/ NRTN,IRTN
!
      NDM=IAP(23)
      ALLOCATE(VR(NDM,NDM,2),VT(NDM,NDM,2),BOUND(NDM,NDM))
      ALLOCATE(RR(NDM,2),RI(NDM,2),XEQUIB1(NDM),XEQUIB2(NDM))
!
!     *Initialization
      DO I=1,NBC
         FB(I) = 0.0D0
      ENDDO
      JB = 1
!     
      IF ((IEQUIB.EQ.0).OR.(IEQUIB.EQ.-1)) THEN
         CALL PVLS(NDM,U0,PAR)
      ENDIF
!              write(9,*) 'Xequib:'
      DO I=1,NDM
         XEQUIB1(I)=PAR(11+I)
!              write(9,*) I,XEQUIB1(I)
      ENDDO
!     ** Rotations */
      IF(IRTN.NE.0)THEN
         DO I=1,NDM
            XEQUIB2(I)=XEQUIB1(I)
            IF(NRTN(I).NE.0)THEN
               IF(ISTART.LT.0)THEN
                  PAR(19)=-ISTART*PI(2.d0)
               ENDIF
               XEQUIB2(I)=XEQUIB2(I)+PAR(19)*NRTN(I)
            ENDIF
         ENDDO
      ELSEIF(IEQUIB.GE.0) THEN
         DO I=1,NDM
            XEQUIB2(I)=PAR(11+I)
         ENDDO
      ELSE
         DO I=1,NDM
            XEQUIB2(I)=PAR(NDM+11+I)
         ENDDO
      ENDIF
!
!     **Regular Continuation**
      IF(ISTART.NE.3) THEN
!        *Projection boundary conditions for the homoclinic orbit
!        *NSTAB boundary conditions at t=0
	     CALL PRJCTI(IAP,RAP,BOUND,CSAVE,XEQUIB1,ICP,PAR,-1,1,1,NDM)
             DO I=1,NSTAB
                DO K=1,NDM
                   FB(JB)=FB(JB)+(U0(K)-XEQUIB1(K))*BOUND(I,K)
                ENDDO
                JB = JB+1
             ENDDO
!
!        *NUNSTAB boundary conditions at t=1
         IF(NREV.EQ.0) THEN
            CALL PRJCTI(IAP,RAP,BOUND,CSAVE,XEQUIB2,ICP,PAR,1,2,1,NDM)
            DO I=1,NUNSTAB
               DO K=1,NDM
                  IF (ISTART.GE.0) THEN
                    FB(JB)=FB(JB)+(U1(K)-XEQUIB2(K))*BOUND(I,K)
                  ELSE
                    FB(JB)=FB(JB)+(U1(NDIM-NDM+K)-XEQUIB2(K))*BOUND(I,K)
                    IF (ITWIST.EQ.0) THEN
!                     allow jump at end.
                       FB(JB)=FB(JB)+PAR(22)
                    ENDIF
                 ENDIF
               ENDDO
               JB = JB+1
            ENDDO
         ELSE
!         *NUNSTAB symmetric boundary conditions at t=1 if NREV=1
!
            DO I=1,NDIM
               IF(IREV(I).GT.0) THEN
                  FB(JB)=U1(I)  
                  JB=JB+1
               ENDIF
            ENDDO
         ENDIF
         INEIG=0
!        *NFIXED extra boundary conditions for the fixed conditions
         IF (NFIXED.GT.0) THEN
            CALL EIGHI(IAP,RAP,2,RR(1,1),RI(1,1),VR(1,1,1), &
                 XEQUIB1,ICP,PAR,NDM)
            IF(IEQUIB.LT.0) THEN
               CALL EIGHI(IAP,RAP,2,RR(1,2),RI(1,2),VR(1,1,2), &
                    XEQUIB2,ICP,PAR,NDM)
            ENDIF
            DO I=1,NFIXED
               IF((IFIXED(I).GT.10).AND.(INEIG.EQ.0)) THEN
                  CALL EIGHI(IAP,RAP,1,RR(1,1),RI(1,1),VT(1,1,1), &
                       XEQUIB1,ICP,PAR,NDM)
                  INEIG=1
                  IF(IEQUIB.LT.0) THEN
                     CALL EIGHI(IAP,RAP,1,RR(1,2),RI(1,2),VT(1,1,2), &
                          XEQUIB2,ICP,PAR,NDM)
                  ENDIF
               ENDIF
               FB(JB)=PSIHO(IAP,IFIXED(I),RR,RI,VR,VT,ICP,PAR,U0,U1)
               JB = JB+1 
            ENDDO
         ENDIF
!        *extra boundary condition in the case of a saddle-node homoclinic
         IF (IEQUIB.EQ.2) THEN
            IF(INEIG.EQ.0) THEN
               CALL EIGHI(IAP,RAP,1,RR(1,1),RI(1,1),VT(1,1,1), &
                    XEQUIB1,ICP,PAR,NDM)
               INEIG=1
	    ENDIF
	    FB(JB)=RR(NSTAB+1,1)
	    JB=JB+1
         ENDIF
!        *NDM initial conditions for the equilibrium if IEQUIB=1,2,-2
         IF ((IEQUIB.NE.0).AND.(IEQUIB.NE.-1)) THEN
            CALL FUNC(NDM,XEQUIB1,ICP,PAR,0,FB(JB),DUM1,DUM2)
            JB=JB+NDM
!        *NDM extra initial conditions for the equilibrium if IEQUIB=-2
            IF (IEQUIB.EQ.-2) THEN
               CALL FUNC(NDM,XEQUIB2,ICP,PAR,0,FB(JB),DUM1,DUM2)
               JB=JB+NDM
            ENDIF
         ENDIF
!        *boundary conditions for normal vector
         IF ((ISTART.GE.0).AND.(ITWIST.EQ.1)) THEN
!           *-orthogonal to the unstable directions of A  at t=0
            CALL PRJCTI(IAP,RAP,BOUND,CSAVE,XEQUIB1,ICP,PAR,1,1,2,NDM)
            DO I=1,NUNSTAB
               DUM=0.0
               DO K=1,NDM
                  DUM=DUM+U0(NDM+K)*BOUND(I,K)
               ENDDO
               FB(JB)=DUM 
               JB = JB+1
            ENDDO
!           *-orthogonal to the stable directions of A  at t=1
            CALL PRJCTI(IAP,RAP,BOUND,CSAVE,XEQUIB2,ICP,PAR,-1,2,2,NDM)
            DO I=1,NSTAB
               DUM=0.0
               DO K=1,NDM
                  DUM=DUM+U1(NDM+K)*BOUND(I,K)
               ENDDO
               FB(JB)=DUM 
               JB = JB+1
            ENDDO
!      Branch switching to n-homoclinic orbits.
         ELSEIF(ISTART.LT.0) THEN
!         More boundary conditions: continuity+gaps
            DO K=0,NDIM/NDM-2
               DO I=1,NDM
                  FB(JB)=U0(NDM*(K+1)+I)-U1(NDM*K+I)
                  IF (ITWIST.EQ.1) THEN 
!     Lin(-Sandstede): PAR(20,22,...) contain the gap sizes,
!     PAR(NPARX-2*NDM+1...NPARX-NDM) contains the adjoint unit
!     vector at the gaps.
                     FB(JB)=FB(JB)-PAR(20+2*K)*PAR(NPARX-2*NDM+I)
                  ENDIF
                  JB = JB+1
               ENDDO
            ENDDO
!     Poincare sections: <x-x_0,\dot x_0>=0
!     PAR(NPARX-NDM+1...NPARX) contains the derivatives of the
!     point x_0 in the original
!     homoclinic orbit that is furthest from the equilibrium.
!     x_0=umax is initialized at each run to an end point, and so
!     is always in the Poincare section
            IF (.NOT.ALLOCATED(UMAX)) THEN
               ALLOCATE(UMAX(NDIM))
               DO I=1,NDIM
                  UMAX(I) = U1(I)
               ENDDO
            ENDIF
            DO K=0,NDIM/NDM-2
               DO I=1,NDM
                  FB(JB)=FB(JB)+ &
                       (U1(K*NDM+I)-UMAX(K*NDM+I))*PAR(NPARX-NDM+I)
               ENDDO
               JB = JB + 1
            ENDDO
         ENDIF
      ELSE
!     **Starting Solutions using Homotopy**
         IP=12
         IF(IEQUIB.GE.0) THEN 
            IP=IP+NDM
         ELSE
            IP=IP+2*NDM
         ENDIF
         KP=IP
!        *Explicit boundary conditions for homoclinic orbit at t=0
         CALL EIGHI(IAP,RAP,2,RR,RI,VR,XEQUIB1,ICP,PAR,NDM)
         JB=NDM+1
         IF(NUNSTAB.GT.1) THEN
            FB(JB)=0.0
            KP=IP+NUNSTAB
            DO J=1,NUNSTAB
               DO I=1,NDM
                  FB(I)=FB(I)+U0(I)-XEQUIB1(I)-PAR(IP+J)* &
                       VR(NDM-NUNSTAB+J,I,1)
               ENDDO
               FB(JB)=FB(JB)+PAR(IP+J)**2
            ENDDO
            FB(JB)=FB(JB)-PAR(IP)
	    JB=JB+1
         ELSE
            KP=IP+1
            DO I=1,NDM
               FB(I)=U0(I)-XEQUIB1(I)-PAR(IP)*PAR(IP+1)* &
                    VR(NDM-NUNSTAB+1,I,1)
            ENDDO
         ENDIF
!        *Projection boundary conditions for the homoclinic orbit at t=1
         CALL EIGHI(IAP,RAP,1,RR,RI,VT,XEQUIB2,ICP,PAR,NDM)
         DO I=NDM-NUNSTAB+1,NDM
            DUM=0.0D0
            DO J=1,NDM
               DUM=DUM+(U1(J)-XEQUIB2(J))*VT(I,J,1)
            ENDDO 
            KP=KP+1
            FB(JB)=DUM-PAR(KP)
	    JB=JB+1
         ENDDO
!        *NDM initial conditions for the equilibrium if IEQUIB=1,2,-2
         IF ((IEQUIB.NE.0).AND.(IEQUIB.NE.-1)) THEN
            CALL FUNC(NDM,XEQUIB1,ICP,PAR,0,FB(JB),DUM1,DUM2)
            JB=JB+NDM
!        *NDM extra initial conditions for the equilibrium if IEQUIB=-2
            IF (IEQUIB.EQ.-2) THEN
               CALL FUNC(NDM,XEQUIB2,ICP,PAR,0,FB(JB),DUM1,DUM2)
               JB=JB+NDM
            ENDIF
         ENDIF
      ENDIF
!
      NBCN=NBC-JB+1
!      write(9,*) NBCN,NBC
! *user defined extra boundary conditions
      IF (NBCN.GT.0) THEN
         CALL BCND(NDIM,PAR,ICP,NBCN,U0,U1,FB(JB),0,0)
      ELSEIF (NBCN.LT.0) THEN
         PRINT*,'Evil BUG!: Negative number of boundary conditions left'
         CALL AUTOSTOPWITHERROR('Evil BUG!: Negative number of boundary      &
              conditions left')
      END IF
!
      DEALLOCATE(VR,VT,BOUND,RR,RI,XEQUIB1,XEQUIB2)
      RETURN
      END SUBROUTINE FBHO
!
!     ---------- ----
      SUBROUTINE ICHO(IAP,RAP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD, &
           F,IJAC,DINT)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
!
! Generates integral conditions for homoclinic bifurcation analysis
!
      DIMENSION IAP(*),RAP(*),ICP(*),PAR(*)
      DIMENSION U(*),UOLD(*),UDOT(*),UPOLD(*),F(*),DINT(NINT,*)
!
       NNT0=IAP(25)
       NFPR=IAP(29)
!
! Generate the function.
!
       IF(IJAC.EQ.0)THEN
         CALL FIHO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,F)
         RETURN
       ENDIF
!
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
         CALL FIHO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT, &
              UPOLD,F)
         U(I)=UU+EP
         CALL FIHO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT, &
              UPOLD,DINT(1,I))
         U(I)=UU
!
         DO J=1,NINT
           DINT(J,I)=(DINT(J,I)-F(J))/(2*EP)
         ENDDO
       ENDDO
!
! Generate the function.
!
       CALL FIHO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,F)
!
       IF(IJAC.EQ.1)RETURN
       DO I=1,NFPR
         PAR(ICP(I))=PAR(ICP(I))+EP
         CALL FIHO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT, &
              UPOLD,DINT(1,NDIM+ICP(I)))
         DO J=1,NINT
           DINT(J,NDIM+ICP(I))=(DINT(J,NDIM+ICP(I))-F(J))/EP
         ENDDO
         PAR(ICP(I))=PAR(ICP(I))-EP
       ENDDO
!
      RETURN
      END SUBROUTINE ICHO
!
!     ---------- ----
      SUBROUTINE FIHO(IAP,RAP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT, &
           UPOLD,FI)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates the integral conditions for homoclinic orbits.
!
      DIMENSION ICP(*),IAP(*)
      DIMENSION RAP(*),PAR(*),U(*),UOLD(*),UDOT(*),UPOLD(*),FI(*)
!
      NDM=IAP(23)
      JB=0
!
! Integral phase condition for homoclinic orbit
!    
      IF((NREV.EQ.0).AND.(ISTART.GE.0)) THEN
         DUM=0.d0
         DO I=1,NDM
            DUM=DUM+UPOLD(I)*(U(I)-UOLD(I))
         ENDDO
         JB=JB+1
         FI(JB)=DUM
!     
! Integral phase condition for adjoint equation     
!
         IF ((ITWIST.EQ.1)) THEN
            DUM=0.d0
            DO I=1,NDM
               DUM=DUM+UOLD(NDM+I)*(U(NDM+I)-UOLD(NDM+I))
            ENDDO
            JB=JB+1
            FI(JB)=DUM
         ENDIF
      ENDIF
!
! User-defined integral constraints
!
      IF (JB.LT.NINT) THEN
         CALL ICND(NDM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI(JB),0,DUM1)
      END IF
!
      RETURN
      END SUBROUTINE FIHO
!
!     ---------- ----
      SUBROUTINE INHO(IAP,ICP,PAR)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(HMACHHO=1.0d-13)
      DIMENSION PAR(*),IAP(*),ICP(*)
!
! Reads from fort.11 specific constants for homoclinic continuation.
! Sets up re-defined constants in IAP. 
! Sets other constants in the module common blocks.
!
! set various constants 
!
      NDIM=IAP(1)
      ISW=IAP(10)
      NBC=IAP(12)
      NINT=IAP(13)
      NUZR=IAP(15)
      NDM=NDIM
      COMPZERO=HMACHHO
!
      OPEN(UNIT=12,FILE='fort.12',STATUS='OLD',ACCESS='sequential')
      READ(12,*)NUNSTAB,NSTAB,IEQUIB,ITWIST,ISTART
!
! updated reading in of constants for reversible equations
! replaces location in datafile of compzero
!
      ALLOCATE(IREV(NDM))
      READ(12,*)NREV
      IF(NREV.GT.0)READ(12,*)(IREV(I),I=1,NDM)
!
      READ(12,*)NFIXED
      IF (NFIXED.GT.0)READ(12,*)(IFIXED(I),I=1,NFIXED)
      READ(12,*)NPSI
      IF (NPSI.GT.0)READ(12,*)(IPSI(I),I=1,NPSI)
      CLOSE(UNIT=12,STATUS='KEEP')
      NFREE=2+NFIXED-NREV+NINT+NBC
      IF (ISTART.LT.0) THEN
!        n-homoclinic branch switching
         NFREE=NFREE-ISTART-1
         NDIM=NDM*(-ISTART+1)
!      
! Free parameter (artificial parameter for psi)
! nondegeneracy parameter of the adjoint
!
      ELSEIF (ITWIST.EQ.1) THEN
         NFREE = NFREE + 1
         ICP(NFREE) = 10
         PAR(10)= 0.0D0
         NDIM=NDM*2
      ENDIF
!
! Extra free parameters for equilibrium if iequib=1,2,-2
!
      IF ((IEQUIB.NE.0).AND.(IEQUIB.NE.-1)) THEN
         DO I=1,NDM
            ICP(NFREE+I)=11+I
         ENDDO
      ENDIF
!
      IF (IEQUIB.EQ.-2) THEN
         DO I=1,NDM
            ICP(NFREE+NDM+I)=11+NDM+I
         ENDDO
      ENDIF 
!
      IF (ISTART.NE.3) THEN
!     *regular continuation
        IF (ISTART.GE.0) THEN
           NINT=NINT+ITWIST+1-NREV
        ENDIF 
        IF (ISW.EQ.2) THEN
          ICORR = 2
        ELSE
          ICORR = 1
        ENDIF
        NBC=NBC+NSTAB+NUNSTAB+NDIM-NDM+IEQUIB*NDM+NFREE-NINT-ICORR
        IF (IEQUIB.EQ.2) THEN
	  NBC=NBC-NDM+1
        ENDIF
        IF (IEQUIB.LT.0) THEN
           NBC=NBC-(3*IEQUIB+2)*NDM
        ENDIF
      ELSE
!     *starting solutions using homotopy
        IF (NUNSTAB.EQ.1) THEN
          NBC=NDM*(1+IEQUIB)+1
        ELSE
          NBC=NDM*(1+IEQUIB)+NUNSTAB+1
        ENDIF
        IF (IEQUIB.EQ.2) THEN 
        IF(FORT9DST==1)WRITE(9,*)'WARNING: IEQUIB=2 NOT ALLOWED WITH IST&
             ART=3'
        ENDIF
        IF (IEQUIB.LT.0) THEN
          NBC=NBC-NDM*(3*IEQUIB+2)
        ENDIF
        NINT=0
      ENDIF
!
! write new constants into IAP
!
      IAP(1)=NDIM
      IAP(12)=NBC
      IAP(13)=NINT
      IAP(15)=NUZR
      IAP(23)=NDM
!
      RETURN
      END SUBROUTINE INHO
!
!     ---------- ------
      SUBROUTINE INTPHO(NDM,NCOLRS,TM,DTM,NDX,UPS,UDOTPS,T,DT,N, &
           NDIM,J,J1)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION UPS(NDX,*), UDOTPS(NDX,*)
!
! Local
!
      ALLOCATABLE X(:),W(:)
!
!     Finds interpolant (TM(.) , UPS(.), UDOTPS(.) ) on the new mesh
!     at times TM,TM+DTM using the old mesh at times T,T+DT.
!
!     Used by TRANHO to initiate branch switching to n-homoclinic orbits.
!
      NCP1=NCOLRS+1
      ALLOCATE(X(NCP1),W(NCP1))
!
      D=DTM/NCOLRS
      DO L=1,NCP1
         X(L)=TM+(L-1)*D
      ENDDO
      DO I=0,NCOLRS-1
         Z=T+DT*I/NCOLRS
         CALL INTWTS(NCP1,Z,X,W)
         K1=I*NDIM+N
         DO K=1,NDM
            UPS(K1+K,J1)=W(NCP1)*UPS(N+K,J+1)
            UDOTPS(K1+K,J1)=W(NCP1)*UDOTPS(N+K,J+1)
            DO L=1,NCOLRS
               L1=K+(L-1)*NDIM+N
               UPS(K1+K,J1)=UPS(K1+K,J1)+W(L)*UPS(L1,J)
               UDOTPS(K1+K,J1)=UDOTPS(K1+K,J1)+W(L)*UDOTPS(L1,J)
            ENDDO
         ENDDO
      ENDDO
!
      DEALLOCATE(X,W)
      RETURN
      END SUBROUTINE INTPHO
!
!     ---------- ------
      SUBROUTINE TRANHO(NTSR,NCOLRS,NDM,NDIM,TM,DTM,NDX,UPS, &
           UDOTPS,PAR)
!
!     Transform the data representation of the homoclinic orbit into
!     an object suitable for homoclinic branch switching:
!
!     dim|1...............NDM|NDM+1......NDIM-NDM|NDIM-NDM+1......NDIM|
!        |                   |                   |                    |
!     t=0|start of hom. orbit|maximum from equil.| maximum from equil.|
!        |       :           |       :           |       :            |
!        |       :           |end of hom. orbit  |       :            |
!        |       :           |start of hom. orbit|       :            |
!        |       :           |        :          |       :            |
!     t=1|maximum from equil.|maximum from equil.| end of hom. orbit  |
!
!     Called by PREHO
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TM(*), DTM(*), UPS(NDX,*), UDOTPS(NDX,*), PAR(*)
! Local
      DIMENSION J2(3),A(3),B(3),T(3),TT(3)
      ALLOCATABLE TTM(:),UMAX(:)
!
      POINTER NRTN(:)
      COMMON /BLRTN/ NRTN,IRTN
      ALLOCATE(TTM(NTSR*2),UMAX(NDM))
!
! First find maximum from the equilibrium
!     
      UPSMAX=0
      JMAX=1
      DO J=1,NTSR+1
         UPSI=0
         DO I=1,NDM
            UPSI=UPSI+(UPS(I,J)-PAR(11+I))*(UPS(I,J)-PAR(11+I))
         ENDDO
         IF (UPSI.GT.UPSMAX) THEN
            UPSMAX=UPSI
            JMAX=J
         ENDIF
      ENDDO
      IF(IRTN.NE.0)THEN
! Just use the point in the middle
         UPSMAX = 0
         DO I=1,NDM
            IF(NRTN(I).NE.0)GOTO 1
         ENDDO
 1       CONTINUE
         DO J=1,NTSR+1
            D1=DABS(UPS(I,J)-PAR(I+11))
            UPSI=DABS(UPS(I,J)-(PAR(I+11)+PAR(19)*NRTN(I)))
            IF(D1.LT.UPSI)UPSI=D1
            IF(UPSI.GT.UPSMAX)THEN
               UPSMAX=UPSI
               JMAX=J
            ENDIF
         ENDDO
      ENDIF
      TMMAX=TM(JMAX)
      DO I=1,NDM
         UMAX(I) = UPS(I,JMAX)
      ENDDO
      CALL FUNC(NDM,UMAX,ICP,PAR,0,PAR(NPARX-NDM+1),DUM1,DUM2)
!     
!     PAR(NPARX-NDM+1...NPARX) contains the point furthest from
!     the equilibrium.
!     PAR(10)=the time for the unstable manifold tail.
!     PAR(11)=the time for the stable manifold tail.
!     PAR(20,22,...) contain the gap sizes.
!     PAR(21,23,...) contain the times between Poincare sections
!     
      PAR(10)=PAR(11)*TMMAX
      PAR(20)=0D0
      DO K=1,NDIM/NDM-2
         PAR(19+2*K)=PAR(11)
         PAR(20+2*K)=0D0
      ENDDO
      PAR(11)=PAR(11)*(1D0-TMMAX)
! 
!     Remember adjoint at maximum for applying Lin's method
!     PAR(NPARX-2*NDM+1...NPARX-NDM) will contain the adjoint unit
!     vector at the gaps.
!
      IF (ITWIST.EQ.1) THEN
         DNORM=0.D0
         DO I=1,NDM
            PAR(NPARX-2*NDM+I)=UPS(NDM+I,JMAX)
            DNORM=DNORM+UPS(NDM+I,JMAX)*UPS(NDM+I,JMAX)
         ENDDO
         DNORM=DSQRT(DNORM)
         DO I=1,NDM
            PAR(NPARX-2*NDM+I)=PAR(NPARX-2*NDM+I)/DNORM
         ENDDO
      ENDIF
!     
!     Prepare the new NDIM*NCOLRS dimensional UPS matrix
!     Move everything to the end in "middle part format"
!     so that we can subsequently overwrite the beginning.
! 
      PHDIFF=0
      IADDPH=1
      DO L=2*NTSR,NTSR,-1
         J=L-2*NTSR+JMAX
         IF (J.LE.0) THEN
            J=J+NTSR
            IADDPH=0
         ENDIF
         TTM(L)=TM(J)-TMMAX
         IF (TTM(L).LT.0) TTM(L)=TTM(L)+1D0
         DO K=0,(NCOLRS-1)*NDIM,NDIM
            DO I=K+1,K+NDM
               IF(IRTN.NE.0)THEN
                  PHDIFF=0
                  IF(IADDPH.NE.0)PHDIFF=PAR(19)*NRTN(MOD(I-1,NDM)+1)
               ENDIF
               UPS(I+NDM,L)=UPS(I,J)+PHDIFF
               UDOTPS(I+NDM,L)=UDOTPS(I,J)+PHDIFF
               UPS(I,L)=UPS(I,J)
               UDOTPS(I,L)=UDOTPS(I,J)
               IF (L.LE.2*NTSR-JMAX) THEN
                  IF(IRTN.NE.0)THEN
                     PHDIFF=PAR(19)*NRTN(MOD(I-1,NDM)+1)*(-ISTART-1)
                  ENDIF
                  UPS(I+NDIM-NDM,L+JMAX)=UPS(I,J)+PHDIFF
                  UDOTPS(I+NDIM-NDM,L+JMAX)=UDOTPS(I,J)+PHDIFF
               ENDIF
            ENDDO
         ENDDO
      ENDDO
      TTM(2*NTSR)=1D0
!     
!     create matching mesh
!     merge TM(1..JMAX)/TMMAX, TM(JMAX..NTSR)-TMMAX,
!           TM(1..JMAX)+1D0-TMMAX, 
!           (TM(JMAX..NTSR)-TMMAX)/(1D0-TMMAX)
!
      J2(1)=2*NTSR-JMAX+2
      J2(2)=NTSR+1
      J2(3)=NTSR+1
      A(1)=TMMAX-1D0
      A(2)=0D0
      A(3)=0D0
      B(1)=TMMAX
      B(2)=1D0
      B(3)=1D0-TMMAX
      NTSR=NTSR*2-2
      DO I=1,3
         T(I) = (TTM(J2(I))+A(I))/B(I)
         TT(I) = (TTM(J2(I)-1)+A(I))/B(I)
      ENDDO
      DO J=2,NTSR+1
         TM(J)=T(1)
         I2=1
         DO I=2,3
            IF (T(I).LT.TM(J)) THEN
               TM(J)=T(I)
               I2=I
            ENDIF
         ENDDO
         DTM(J-1)=TM(J)-TM(J-1)
!     
!     copy first part to temp arrays upst
!     Replace UPS and UDOTPS by its interpolant on the new mesh :
!     
         CALL INTPHO(NDM,NCOLRS,TT(1),T(1)-TT(1),NDX,UPS,UDOTPS, &
              TM(J-1),DTM(J-1),0,NDIM,J2(1)-1,J-1)
!
!     Remesh middle part :
!     
         CALL INTPHO(NDM,NCOLRS,TT(2),T(2)-TT(2),NDX,UPS,UDOTPS, &
              TM(J-1),DTM(J-1),NDM,NDIM,J2(2)-1,J-1)
!     
!     Remesh last part :
!     
         CALL INTPHO(NDM,NCOLRS,TT(3),T(3)-TT(3),NDX,UPS,UDOTPS, &
              TM(J-1),DTM(J-1),NDIM-NDM,NDIM,J2(3)+JMAX-1,J-1)
!     
!     Copy middle parts, this applies only for 1->n switching
!     where n>=3 and NDIM=(n+1)*NDM: (NDIM/NDM)-3 times.
!     
         DO K2=NDM,NDIM-3*NDM,NDM
            DO K=NDM,(NCOLRS-1)*NDIM+NDM,NDIM
               DO I=K+1,K+NDM
                  IF(IRTN.NE.0)THEN
                     PHDIFF=PAR(19)*NRTN(MOD(I-1,NDM)+1)*(K2/NDM)
                  ENDIF
                  UPS(I+K2,J-1)=UPS(I,J-1)+PHDIFF
                  UDOTPS(I+K2,J-1)=UDOTPS(I,J-1)+PHDIFF
               ENDDO
            ENDDO
         ENDDO
         J2(I2)=J2(I2)+1
         TT(I2)=T(I2)
         IF(J.LE.NTSR)THEN
            T(I2)=(TTM(J2(I2))+A(I2))/B(I2)
         ENDIF
      ENDDO
!
!     Adjust end points
!
      DO I=1,NDM
         IF(IRTN.NE.0)PHDIFF=PAR(19)*NRTN(I)
         DO K2=I,NDIM-NDM,NDM
            UPS(K2,NTSR+1)=UPS(I+NDM,NTSR+2)+PHDIFF*((K2-I)/NDM-1)
            UDOTPS(K2,NTSR+1)=UDOTPS(I+NDM,NTSR+2)+PHDIFF*((K2-I)/NDM-1)
         ENDDO
         UPS(I+NDIM-NDM,NTSR+1)=UPS(I,1)+PHDIFF*(-ISTART)
         UDOTPS(I+NDIM-NDM,NTSR+1)=UDOTPS(I,1)+PHDIFF*(-ISTART)
      ENDDO
!
!     Rotations: PAR(19) needs adjustment
!
      IF(IRTN.NE.0)PAR(19)=PAR(19)*(-ISTART)
      DEALLOCATE(TTM,UMAX)
      RETURN
      END SUBROUTINE TRANHO
!
!     ---------- ------
      SUBROUTINE CPBKHO(NTSR,NCOLRS,NAR,NDM,TM,DTM,NDX,UPS,UDOTPS,PAR)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION UPS(NDX,*), UDOTPS(NDX,*), TM(*), PAR(*), DTM(*)
!
!     Copy the homoclinic orbit back from the special representation 
!     gotten from TRANHO to the usual representation.
!     This is called from PREHO in order to perform normal continuation
!     again once the branch switching is complete.
!
      NDIM=NDM*(ITWIST+1)
      NCOPY=NAR/NDM
      J=1
      TIME=PAR(10)+PAR(11)
      DO K=1,NCOPY-1
         TIME=TIME+PAR(19+2*K)
      ENDDO
      TBASE=TIME-PAR(11)
      TM(NTSR*NCOPY+1)=1.0D0
!
!     first init last point; otherwise it's overwritten
!
      DO K=1,NDM
         UPS(K,NTSR*NCOPY+1)=UPS(K+(NCOPY-1)*NDM,NTSR+1)
         UDOTPS(K,NTSR*NCOPY+1)=UDOTPS(K+(NCOPY-1)*NDM,NTSR+1)
      ENDDO
      DO K=NCOPY-1,0,-1
         DO J=NTSR,1,-1
            I=J+NTSR*K
            DO L=0,NCOLRS-1
               DO M=1,NDM
                  UPS(L*NDIM+M,I)=UPS(L*NAR+K*NDM+M,J)
                  UDOTPS(L*NDIM+M,I)=UDOTPS(L*NAR+K*NDM+M,J)
               ENDDO
            ENDDO
            IF (K.EQ.0) THEN
               TM(I)=TM(J)*PAR(10)/TIME
            ELSEIF (K.EQ.NCOPY-1) THEN
               TM(I)=(TBASE+TM(J)*PAR(11))/TIME
            ELSE
               TM(I)=(TBASE+TM(J)*PAR(19+K*2))/TIME
            ENDIF
            DTM(I)=TM(I+1)-TM(I)
         ENDDO
         IF (K.EQ.1) THEN
            TBASE=TBASE-PAR(10)
         ELSE
            TBASE=TBASE-PAR(17+K*2)
         ENDIF
      ENDDO
      NTSR=NTSR*NCOPY
!
      PAR(10)=0.D0
      PAR(11)=TIME
      NAR=NDM
      RETURN
      END SUBROUTINE CPBKHO
!
!     ---------- -----
      SUBROUTINE PREHO(IAP,PAR,ICP,NDX,NTSR,NAR,NCOLRS,UPS,UDOTPS, &
           TM,DTM)
!
!     Special homoclinic orbit preprocessing.
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION UPS(NDX,*), TM(*), DTM(*), UDOTPS(NDX,*), PAR(*), IAP(*)
      DIMENSION ICP(*)
      POINTER NRTN(:)
      COMMON /BLRTN/ NRTN,IRTN
!
! Local
!
      ALLOCATABLE F(:),UI(:)
!
      NDIM=IAP(1)
      NDM=IAP(23)
!
      IF (ISTART.GE.0.AND.NAR.GT.2*NDM) THEN
!        Use the usual representation again for normal continuation.
         CALL CPBKHO(NTSR,NCOLRS,NAR,NDM,TM,DTM,NDX,UPS,UDOTPS,PAR)
      ENDIF
!     Look for rotations
      CALL SETRTN(NDM,NTSR,NDX,UPS,PAR)
      IF (ISTART.LT.0 .AND. .NOT.(NAR.LT.NDIM .AND. NAR.LT.3*NDM)) THEN
!        Adjust rotations
        IF(IRTN.EQ.0)ALLOCATE(NRTN(NDM))
        IRTN=0
        DO I=1,NDM
          NRTN(I)=NINT( (UPS(NAR-NDM+I,NTSR+1)-UPS(I,1)) /  &
               (PI(2.d0) * (-ISTART)) )
          IF(NRTN(I).NE.0)THEN
             PAR(19)=PI(2.d0)
             IRTN=1
          ENDIF
        ENDDO
        IF(IRTN.EQ.0)DEALLOCATE(NRTN)
      ENDIF
!
! Shift phase if necessary if continuing from
! a periodic orbit into a homoclinic one
!
      IF (ISTART.EQ.4) THEN

! Try to find an approximate value for the equilibrium if it's not
! explicitely given. This is just the point where the speed is minimal.
! We hope that Newton's method will do the rest.

         IF (IEQUIB.GT.0) THEN
            ALLOCATE(UI(NDM),F(NDM))
            UPSMIN=1D20
            JMIN=1
            DO J=1,NTSR+1
               DO I=1,NDM
                  UI(I) = UPS(I,J)
               ENDDO
               CALL FUNC(NDM,UI,ICP,PAR,0,F,DUM1,DUM2)
               UPSI=0
               DO I=1,NDM
                  UPSI=UPSI+F(I)*F(I)
               ENDDO
               IF (UPSI.LT.UPSMIN) THEN
                  JMIN = J
                  UPSMIN = UPSI
               ENDIF
            ENDDO
            DO I=1,NDM
               PAR(11+I)=UPS(I,JMIN)
            ENDDO
            DEALLOCATE(UI,F)
         ENDIF
!
! Find smallest value in norm
!
       UPSMIN=1D20
       JMIN=1
       DO J=1,NTSR+1
         UPSI=0
         DO I=1,NDM
           UPSI=UPSI+(UPS(I,J)-PAR(11+I))*(UPS(I,J)-PAR(11+I))
         ENDDO
         IF (UPSI.LT.UPSMIN) THEN
           UPSMIN=UPSI
           JMIN=J
         ENDIF
       ENDDO
       IF(UPSMIN.LT.COMPZERO)THEN
!
!      try to get time central value if all points within a range
!      are within an epsilon neighbourhood of the equilibrium
!
          T=0
          J2=JMIN
          J1=J2
          J=JMIN-1
          DO WHILE(J.NE.JMIN+1)
             IF(J.EQ.0)J=NTSR+1
             UPSI=0
             DO I=1,NDM
                UPSI=UPSI+(UPS(I,J)-PAR(I+11))*(UPS(I,J)-PAR(I+11))
             ENDDO
             IF(UPSI.GT.COMPZERO)THEN
                J1=J+1
                GOTO 1
             ENDIF
             J=J-1
          ENDDO
 1        CONTINUE
          J=JMIN+1
          DO WHILE(J.NE.J1)
             IF(J.EQ.NTSR+2)J=1
             UPSI=0
             DO I=1,NDM
                UPSI=UPSI+(UPS(I,J)-PAR(I+11))*(UPS(I,J)-PAR(I+11))
             ENDDO
             IF(UPSI.GT.COMPZERO)THEN
                J2=J-1
                GOTO 2
             ENDIF
             J=J+1
          ENDDO
 2        CONTINUE
          T=(TM(J2)+TM(J1))/2
          IF(J1.GT.J2)THEN
             T=(TM(J2)+TM(J1)+1)/2
             IF(T.GE.1)T=T-1
             IF(TM(J1).LE.T)THEN
                J2=NTSR+2
             ELSE
                J1=0
             ENDIF
          ENDIF
          DO WHILE((TM(J1).LE.T).AND.(J1.LT.J2))
             J1=J1+1
          ENDDO
          JMIN=j1
          IF(T-TM(JMIN-1).LT.TM(JMIN)-T) JMIN=JMIN-1
       ENDIF
       TMMIN=TM(JMIN)
!
! And then do the actual shift
! 
       IF (JMIN.NE.1) THEN
        IST=0
        J=NTSR+1
        DO II=1,NTSR
           IF (J.EQ.NTSR+1) THEN
              IST=IST+1
              TM(J)=TM(IST)
              DO K=1,NCOLRS*NDIM
                  UPS(K,J)=UPS(K,IST)
               UDOTPS(K,J)=UDOTPS(K,IST)
              ENDDO
              J=IST
           ENDIF
           I=J
           J=J+JMIN-1
           IF (J.GT.NTSR) J=J-NTSR
           IF (J.EQ.IST) J=NTSR+1
           TM(I)=TM(J)-TMMIN
           IF (TM(I).LT.0) TM(I)=TM(I)+1.0D0
           DO K=1,NCOLRS*NDIM
                  UPS(K,I)=UPS(K,J)
               UDOTPS(K,I)=UDOTPS(K,J)
           ENDDO
        ENDDO
!
! Last equal to first
!
        TM(NTSR+1)=1.0D0
        DO K=1,NCOLRS*NDIM
             UPS(K,NTSR+1)=UPS(K,1)
          UDOTPS(K,NTSR+1)=UDOTPS(K,1)
        ENDDO
!
! Rotations
!
        IF(IRTN.NE.0)THEN
           JR=0
           DO J=1,NTSR+1
              DO I=1,NDM
                 IF(NRTN(I).NE.0) THEN
                    IF(DABS((UPS(I,J+1)-UPS(I,J))/NRTN(I)).GT. &
                         DABS(PAR(19)/2)) THEN
                       JR=J+1
                       GOTO 3
                    ENDIF
                 ENDIF
              ENDDO
           ENDDO
 3         IF(JR.NE.0)THEN
              DO J=JR,NTSR+1
                 DO I=1,NCOLRS*NDIM
                    IF (NRTN(MOD(I-1,NDIM)+1).NE.0) THEN
                       UPS(I,J)=UPS(I,J)+PAR(19)*NRTN(MOD(I-1,NDIM)+1)
                    ENDIF
                 ENDDO
              ENDDO
           ENDIF
        ENDIF
!
       ENDIF
      ENDIF
!
! If ISTART<0 we perform homoclinic branch switching and need
! to change the representation of the homoclinic orbit in UPS and
! UDOTPS.
!
      IF (ISTART.LT.0 .AND. NAR.LT.NDIM .AND. NAR.LT.3*NDM) THEN
         CALL TRANHO(NTSR,NCOLRS,NDM,NDIM,TM,DTM,NDX,UPS,UDOTPS,PAR)
      ELSEIF  &
           (ISTART.LT.0 .AND. NAR.LT.NDIM .AND. NAR.GE.3*NDM) THEN
! Copy forelast part
         DO J=1,NTSR+1
            DO K=0,NDIM*(NCOLRS-1),NDIM
               DO I=NDIM,NAR-NDM+1,-1
                  UPS(K+I,J)=UPS(K+I-NDIM+NAR,J)
                  UDOTPS(K+I,J)=UDOTPS(K+I-NDIM+NAR,J)
               ENDDO
            ENDDO
         ENDDO
         DO I=1,(NDIM-NAR)/NDM
            PAR(16+2*(NAR/NDM+I))=PAR(16+2*NAR/NDM)
            PAR(15+2*(NAR/NDM+I))=PAR(15+2*NAR/NDM)
         ENDDO
         PAR(16+2*NAR/NDM)=(UPS(NAR-NDM+1,1)- &
              UPS(NAR-2*NDM+1,NTSR+1))/ PAR(NPARX-2*NDM+1)
      ENDIF
!       
! Preprocesses (perturbs) restart data to enable 
! initial computation of the adjoint variable
!
      IF (NAR.NE.NDIM .AND. ISTART.GE.0 .AND. ITWIST.EQ.1) THEN  
       DO J=1,NTSR
         DO I=1,NCOLRS
           K1=(I-1)*NDIM+1
           K2=I*NDIM
           DO K=K1+NAR,K2
             UPS(K,J)=0.1d0
           ENDDO
         ENDDO
       ENDDO
       DO K=1+NAR,NDIM
         UPS(K,NTSR+1)=0.1d0
       ENDDO
      ENDIF
!
      RETURN
      END SUBROUTINE PREHO
!
!     ---------- ------
      SUBROUTINE STPNHO(IAP,RAP,PAR,ICP,NTSR,NCOLRS,RLCUR,RLDOT, &
           NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates a starting point for the continuation of a branch of
! of solutions to general boundary value problems by calling the user
! supplied subroutine STPNT where an analytical solution is given.
!
!  
! Generates a starting point for homoclinic continuation
! If ISTART=2 it calls STPNUB.
! If ISTART=3 it sets up the homotopy method.
!
      DIMENSION IAP(*),RAP(*),UPS(NDX,*),UDOTPS(NDX,*),TM(*),DTM(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLDOT(*)
! Local
      ALLOCATABLE RR(:),RI(:),VR(:,:),VT(:,:)
!
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       NDM=IAP(23)
!
! Generate the (initially uniform) mesh.
!
       CALL STPNUB(IAP,RAP,PAR,ICP,NTSR,NCOLRS,RLCUR,RLDOT,NDX, &
            UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
!
! Initialize solution and additional parameters
!
       CALL SETRTN(NDM,NTSR,NDX,UPS,PAR)
       IF (ISTART.NE.3) THEN
          RETURN
       ENDIF
!
       ALLOCATE(RR(NDM),RI(NDM),VR(NDM,NDM),VT(NDM,NDM))
       DT=1.d0/(NTST*NCOL)
       CALL PVLS(NDM,UPS,PAR)
       CALL EIGHI(IAP,RAP,1,RR,RI,VT,PAR(12),ICP,PAR,NDM)
       CALL EIGHI(IAP,RAP,2,RR,RI,VR,PAR(12),ICP,PAR,NDM)
!
! Set up artificial parameters at the left-hand end point of orbit
!
       IP=12
       IF(IEQUIB.GE.0) THEN 
          IP=IP+NDM
       ELSE
          IP=IP+2*NDM
       ENDIF
       KP=IP
!
! Parameters xi_1=1, xi_i=0, i=2,NSTAB
!
       PAR(IP+1)=1.0d0
       IF(NUNSTAB.GT.1) THEN
          DO I=2,NUNSTAB
             PAR(IP+I)=0.0
          ENDDO
       ENDIF
       IP=IP+NUNSTAB
!     
! Starting guess for homoclinic orbit in real principal unstable direction
!
       DO J=1,NTST+1
          IF(J.EQ.(NTST+1)) THEN
             NCOL1=1
          ELSE
             NCOL1=NCOL
          ENDIF
          DO I=1,NCOL1
             T=TM(J)+(I-1)*DT
             K2=(I-1)*NDIM
             DO K=1,NDIM
                UPS(K2+K,J)=PAR(11+K)+VR(NSTAB+1,K)*PAR(KP)*PAR(KP+1)* &
                     EXP(RR(NSTAB+1)*T*PAR(11))
             ENDDO
             IF(FORT9DST==1)WRITE(9,111)(UPS(J,K2+K),K=1,NDIM)
 111         format('stpho : ',e20.10)
          ENDDO
       ENDDO
!
! Artificial parameters at the right-hand end point of the orbit
! omega_i=<x(1)-x_o,w_i^*>
!
       DO I=1,NUNSTAB
          PAR(IP+I)=0.0
          DO J=1,NDM
             PAR(IP+I)=PAR(IP+I)+VR(NSTAB+1,J)*PAR(KP)*PAR(KP+1)* &
                  EXP(RR(NSTAB+1)*PAR(11))*VT(NSTAB+I,J)
          ENDDO
       ENDDO
       IP=IP+NUNSTAB
!
      DEALLOCATE(RR,RI,VR,VT)
      RETURN
      END SUBROUTINE STPNHO
!
!     ---------- ------
      SUBROUTINE PVLSHO(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),RAP(*),ICP(*),DTM(*),UPS(NDX,*),PAR(*)
      DIMENSION P0(NDIM,*),P1(NDIM,*)
! Local
      ALLOCATABLE PU0(:),PU1(:)
      ALLOCATABLE RR(:,:),RI(:,:),V(:,:,:),VT(:,:,:)
!
      ALLOCATE(PU0(NDIM),PU1(NDIM))
      ALLOCATE(RR(NDIM,2),RI(NDIM,2),V(NDIM,NDIM,2),VT(NDIM,NDIM,2))
!
       IID=IAP(18)
       NDM=IAP(23)
       NTST=IAP(5)
!
        CALL PVLSBV(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
!
!      *Compute eigenvalues
       INEIG=0
       CALL EIGHI(IAP,RAP,2,RR(1,1),RI(1,1),V(1,1,1),PAR(12),ICP, &
            PAR,NDM)
       IF(IEQUIB.LT.0)THEN
          CALL EIGHI(IAP,RAP,2,RR(1,2),RI(1,2),V(1,1,2),PAR(12+NDM),ICP, &
               PAR,NDM)
       ENDIF
       IF(IID.GE.3)THEN
         IF(FORT9DST==1)WRITE(9,*) 'EIGENVALUES'
         DO J=1,NDM
          IF(FORT9DST==1)WRITE(9,101) RR(J,1),RI(J,1)
         ENDDO
         IF(IEQUIB.LT.0)THEN
            IF(FORT9DST==1)WRITE(9,*) 'EIGENVALUES OF RHS EQUILIBRIUM'
            DO J=1,NDM
               IF(FORT9DST==1)WRITE(9,101) RR(J,2),RI(J,2)
            ENDDO
         ENDIF
       ENDIF
       IF (((ITWIST.EQ.1).AND.(ISTART.GE.0)).OR.NPSI.GT.0) THEN
          DO I=1,NDIM
             PU0(I)=UPS(I,1)
             PU1(I)=UPS(I,NTST+1)
          ENDDO
       ENDIF
       IF ((ITWIST.EQ.1).AND.(ISTART.GE.0)) THEN
          CALL EIGHI(IAP,RAP,1,RR(1,1),RI(1,1),VT(1,1,1),PAR(12),ICP, &
               PAR,NDM)
          IF(IEQUIB.LT.0)THEN
             CALL EIGHI(IAP,RAP,1,RR(1,2),RI(1,2),VT(1,1,2),PAR(12+NDM), &
                  ICP,PAR,NDM)
          ENDIF
          INEIG=1
          ORIENT = PSIHO(IAP,0,RR,RI,V,VT,ICP,PAR,PU0,PU1)
          IF(IID.GE.3)THEN
            IF (ORIENT.LT.0.0D0) THEN
               IF(FORT9DST==1)WRITE(9,102) ORIENT             
            ELSE
               IF(FORT9DST==1)WRITE(9,103) ORIENT   
            ENDIF
          ENDIF
       ENDIF             
!
      DO I=1,NPSI
        IF((IPSI(I).GT.10).AND.(INEIG.EQ.0)) THEN
          CALL EIGHI(IAP,RAP,1,RR(1,1),RI(1,1),VT(1,1,1),PAR(12),ICP, &
               PAR,NDM)
          IF(IEQUIB.LT.0)THEN
             CALL EIGHI(IAP,RAP,1,RR(1,2),RI(1,2),VT(1,1,2),PAR(12+NDM), &
                  ICP,PAR,NDM)
          ENDIF
          INEIG=1
        ENDIF
        PAR(20+IPSI(I))=PSIHO(IAP,IPSI(I),RR,RI,V,VT,ICP,PAR,PU0,PU1)
        IF(IID.GE.3)THEN
           IF(FORT9DST==1)THEN
             WRITE(9,104)IPSI(I),PAR(20+IPSI(I))
           ENDIF
        ENDIF
      ENDDO
!  
      DEALLOCATE(PU0,PU1,RR,RI,V,VT)
      RETURN
!
 101  FORMAT(1X,'(',F12.7,',',1X,F12.7,')')
 102  FORMAT(1X,'Non-orientable',' (',D20.10,')')
 103  FORMAT(1X,'orientable',' (',D20.10,')')      
 104  FORMAT(1X,'PSI(',I2,')=',D20.10)
!
      END SUBROUTINE PVLSHO
!
!     -------- ------- -------- -----
      DOUBLE PRECISION FUNCTION PSIHO(IAP,IS,RR,RI,V,VT,ICP,PAR,PU0,PU1)
!
! The conditions for degenerate homoclinic orbits are given by PSI(IS)=0.
! 
! RR and RI contain the real and imaginary parts of eigenvalues which are
! ordered with respect to their real parts (smallest first).    
! The (generalised) real eigenvectors are stored as the ROWS of V. 
! The (generalised) real left eigenvectors are in the ROWS of VT.
! In the block ENDPTS are stored the co-ordinates of the left (PU0)
! and right (PU1) endpoints of the solution (+  vector if that is computed)
!
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IAP(*),ICP(*),PAR(*),RR(IAP(23),*),RI(IAP(23),*)
      DIMENSION V(IAP(23),IAP(23),*),VT(IAP(23),IAP(23),*),PU0(*),PU1(*)
! Local
      ALLOCATABLE F0(:),F1(:)
!
      NDM=IAP(23)
!
      ALLOCATE(F0(NDM),F1(NDM))
      CALL FUNC(NDM,PU0,ICP,PAR,0,F0,DUM1,DUM2)
      CALL FUNC(NDM,PU1,ICP,PAR,0,F1,DUM1,DUM2)
!
      PSIHO=0.0D0
!
!  Compute orientation
!
      IF (IS.EQ.0) THEN
         S1 = 0.0D0
         S2 = 0.0D0
         F0NORM = 0.0D0
         F1NORM = 0.0D0
         U0NORM = 0.0D0
         U1NORM = 0.0D0
         DO J=1,NDM
            S1 = S1 + F1(J)*PU0(NDM+J)
            S2 = S2 + F0(J)*PU1(NDM+J) 
            F0NORM=F0NORM+F0(J)**2
            F1NORM=F1NORM+F1(J)**2
            U0NORM=U0NORM+PU0(J+NDM)**2
            U1NORM=U1NORM+PU1(J+NDM)**2
         ENDDO
         DROOT=DSQRT(F0NORM*F1NORM*U0NORM*U1NORM)
         IF(DROOT.NE.0.d0)THEN
           PSIHO= - S1*S2/DROOT
         ELSE
           PSIHO=0.d0
         ENDIF
         RETURN
      ENDIF
!
      IF(IS.NE.11)DEALLOCATE(F1)
      IF(IS.NE.12)DEALLOCATE(F0)
      GOTO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)IS
!
! Resonant eigenvalues (neutral saddle)
!
 1    CONTINUE
      PSIHO=RR(NSTAB,1)+RR(NSTAB+1,1)+RI(NSTAB,1)+RI(NSTAB+1,1)
      RETURN     
!
! Double real leading eigenvalues (stable)
!   (saddle, saddle-focus transition)
!
 2    CONTINUE
      IF (ABS(RI(NSTAB,1)).GT.COMPZERO) THEN
	 PSIHO=-(RI(NSTAB,1)-RI(NSTAB-1,1))**2
      ELSE
	 PSIHO=(RR(NSTAB,1)-RR(NSTAB-1,1))**2
      ENDIF
      RETURN
!     
! Double real positive eigenvalues (unstable)
!   (saddle, saddle-focus transition)
!
 3    CONTINUE
      IF (ABS(RI(NSTAB+1,1)).GT.COMPZERO) THEN
         PSIHO=-(RI(NSTAB+1,1)-RI(NSTAB+2,1))**2
      ELSE
         PSIHO=(RR(NSTAB+1,1)-RR(NSTAB+2,1))**2
      ENDIF
      RETURN
!
! Neutral saddle, saddle-focus or bi-focus (includes 1, above, also) 
!
 4    CONTINUE
      PSIHO=RR(NSTAB,1)+RR(NSTAB+1,1)
      RETURN     
!
! Neutrally-divergent saddle-focus (stable eigenvalues complex)
!
 5    CONTINUE
      PSIHO=RR(NSTAB,1)+RR(NSTAB+1,1)+RR(NSTAB-1,1)
      RETURN
!
! Neutrally-divergent saddle-focus (unstable eigenvalues complex)
!
 6    CONTINUE
      PSIHO=RR(NSTAB,1)+RR(NSTAB+1,1)+RR(NSTAB+2,1)
      RETURN
!
! Three leading eigenvalues (stable)
!
 7    CONTINUE
      VNORM1 = 0D0
      VNORM2 = 0D0      
      DO I=1,NDM
          VNORM1 = VNORM1 + ABS(V(NSTAB,I,1))
          VNORM2 = VNORM2 + ABS(V(NSTAB-2,I,1))
      ENDDO
      IF (VNORM1.GT.VNORM2) THEN
        PSIHO=RR(NSTAB,1)-RR(NSTAB-2,1)
      ELSE
        PSIHO=RR(NSTAB-2,1)-RR(NSTAB,1)
      ENDIF
      RETURN
!
! Three leading eigenvalues (unstable)
!
 8    CONTINUE
      VNORM1 = 0D0
      VNORM2 = 0D0      
      DO I=1,NDM
          VNORM1 = VNORM1 + ABS(V(NSTAB+1,I,1))
          VNORM2 = VNORM2 + ABS(V(NSTAB+3,I,1))
      ENDDO
      IF (VNORM1.GT.VNORM2) THEN
        PSIHO=RR(NSTAB+1,1)-RR(NSTAB+3,1)
      ELSE
        PSIHO=RR(NSTAB+3,1)-RR(NSTAB+1,1)
      ENDIF
      RETURN
!
! Local bifurcation (zero eigenvalue or Hopf): NSTAB decreases
!  (nb. the problem becomes ill-posed after a zero of 9 or 10)
!
 9    CONTINUE
      PSIHO=RR(NSTAB,1)
      RETURN
!
! Local bifurcation (zero eigenvalue or Hopf): NSTAB increases 
!
 10   CONTINUE
      PSIHO=RR(NSTAB+1,1) 
      RETURN     
!     
! Orbit flip (with respect to leading stable direction)
!     e.g. 1D unstable manifold
!
 11   CONTINUE
      DO J=1,NDM
         PSIHO= PSIHO + F1(J)*VT(NSTAB,J,1)
      ENDDO
      PSIHO= PSIHO * DEXP(-PAR(11)*RR(NSTAB,1)/2.0D0)
      DEALLOCATE(F1)
      RETURN
!
! Orbit flip (with respect to leading unstable direction)
!     e.g. 1D stable manifold
!
 12   CONTINUE
      DO J=1,NDM
         PSIHO= PSIHO + F0(J)*VT(NSTAB+1,J,1)
      ENDDO
      PSIHO= PSIHO * DEXP(PAR(11)*RR(NSTAB+1,1)/2.0D0)
      DEALLOCATE(F0)
      RETURN
!
! Inclination flip (critically twisted) with respect to stable manifold
!   e.g. 1D unstable manifold   
!
 13   CONTINUE
      DO I=1,NDM
          PSIHO= PSIHO + PU0(NDM+I)*V(NSTAB,I,1)
      ENDDO
      PSIHO= PSIHO * DEXP(-PAR(11)*RR(NSTAB,1)/2.0D0)
      RETURN
!
! Inclination flip (critically twisted) with respect to unstable manifold
!   e.g. 1D stable manifold
!
 14   CONTINUE
      DO I=1,NDM
         PSIHO= PSIHO + PU1(NDM+I)*V(NSTAB+1,I,1)
      ENDDO
      PSIHO= PSIHO * DEXP(PAR(11)*RR(NSTAB+1,1)/2.0D0)
      RETURN
!
! Non-central homoclinic to saddle-node (in stable manifold)
!
 15   CONTINUE
      DO I=1,NDM 
        PSIHO=PSIHO+(PAR(11+I)-PU1(I))*V(NSTAB+1,I,1)
      ENDDO
      RETURN
!
! Non-central homoclinic to saddle-node (in unstable manifold)
!
 16   CONTINUE
      DO I=1,NDM 
        PSIHO=PSIHO+(PAR(11+I)-PU0(I))*V(NSTAB+1,I,1)
      ENDDO 
      RETURN
!
      END FUNCTION PSIHO
!     
!     ---------- -----
      SUBROUTINE EIGHI(IAP,RAP,ITRANS,RR,RI,VRET,XEQUIB,ICP,PAR,NDM)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER IAP(*),ICP(*)
      DOUBLE PRECISION RAP(*),RR(*),RI(*),VRET(NDM,*),XEQUIB(*),PAR(*)
! Local
      ALLOCATABLE DFDU(:,:),DFDP(:,:),ZZ(:,:)
!
        ALLOCATE(DFDU(NDM,NDM),DFDP(NDM,NPARX),ZZ(NDM,NDM))
        CALL EIGHO(IAP,RAP,ITRANS,RR,RI,VRET,XEQUIB,ICP,PAR,NDM, &
             DFDU,DFDP,ZZ)
        DEALLOCATE(DFDU,DFDP,ZZ)
!
      RETURN
      END SUBROUTINE EIGHI
!
!     ---------- -----
      SUBROUTINE EIGHO(IAP,RAP,ITRANS,RR,RI,VRET,XEQUIB,ICP,PAR,NDM, &
           DFDU,DFDP,ZZ)
!
! Uses EISPACK routine RG to calculate the eigenvalues/eigenvectors
! of the linearization matrix a (obtained from DFHO) and orders them
! according to their real parts. Simple continuity with respect
! previous call with same value of ITRANS.
!
!	input variables
!               ITRANS = 1 use transpose of A
!                      = 2 otherwise
!
!       output variables
!		RR,RI real and imaginary parts of eigenvalues, ordered w.r.t
!	           real parts (largest first)
!	        VRET the rows of which are real parts of corresponding 
!                  eigenvectors 
!
      USE AUTO_CONSTANTS, ONLY:NPARX,NBIFX,NIAP,NRAP,FORT9DST
      USE INTERFACES, ONLY:FUNI
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),RAP(*),ICP(*),PAR(*),RR(*),RI(*),VRET(NDM,*)
      DIMENSION XEQUIB(*),DFDU(NDM,*),DFDP(NDM,*),ZZ(NDM,*)
! Local
      DIMENSION IEIGC(2)
      DOUBLE PRECISION DUM1(1)
      ALLOCATABLE VI(:,:),VR(:,:),F(:),FV1(:),IV1(:)
      ALLOCATABLE VRPREV(:,:,:)
      SAVE IEIGC,VRPREV
!
      ALLOCATE(VI(NDM,NDM),VR(NDM,NDM),F(NDM),FV1(NDM),IV1(NDM))
      IFAIL=0
!     
      CALL FUNI(IAP,RAP,NDM,XEQUIB,DUM1,ICP,PAR,1,F,DFDU,DFDP)
!     
      IF (ITRANS.EQ.1) THEN
         DO I=1,NDM
            DO J=1,I-1
               TMP=DFDU(I,J)
               DFDU(I,J)=DFDU(J,I)
               DFDU(J,I)=TMP
            ENDDO
         ENDDO
      ENDIF
!
! EISPACK call for eigenvalues and eigenvectors
      CALL RG(NDM,NDM,DFDU,RR,RI,1,ZZ,IV1,FV1,IFAIL)
!
      IF (IFAIL.NE.0) THEN   
         IF(FORT9DST==1)WRITE(9,*) 'EISPACK EIGENVALUE ROUTINE FAILED !'
      ENDIF
! 
      DO J=1,NDM 
        IF((RI(J).GT.COMPZERO).AND.(J.LT.NDM))THEN
          DO I=1,NDM
            VR(I,J)=ZZ(I,J)
            VI(I,J)=ZZ(I,J+1)
          ENDDO
        ELSEIF((RI(J).LT.-COMPZERO).AND.(J.GT.1))THEN
          DO I=1,NDM
            VR(I,J)= ZZ(I,J-1)
            VI(I,J)=-ZZ(I,J)
          ENDDO
        ELSE
          DO I=1,NDM
            VR(I,J)=ZZ(I,J)
            VI(I,J)=0.d0
          ENDDO
        ENDIF
      ENDDO   
! Order the eigenvectors/values according size of real part of eigenvalue.
!     (smallest first)
!
      DO I=1,NDM-1
         DO J=I+1,NDM
            IF (RR(I).GT.RR(J)) THEN
               TMP=RR(I)
               RR(I)=RR(J)
               RR(J)=TMP
               TMP=RI(I)
               RI(I)=RI(J)
               RI(J)=TMP
               DO K=1,NDM
                  TMP=VR(K,I)
                  VR(K,I)=VR(K,J)
                  VR(K,J)=TMP
                  TMP=VI(K,I)
                  VI(K,I)=VI(K,J)
                  VI(K,J)=TMP
               ENDDO
            ENDIF
         ENDDO
      ENDDO
!
! Choose sign of real part of eigenvectors to be 
! commensurate with that of the corresponding eigenvector 
! from the previous call with the same value of ITRANS
!
      IF (IEIGC(ITRANS).EQ.0) THEN
         IF(.NOT.ALLOCATED(VRPREV))ALLOCATE(VRPREV(2,NDM,NDM))
         DO J=1,NDM
            DO I=1,NDM
               VRPREV(ITRANS,I,J)=VR(I,J)
            ENDDO
         ENDDO
         IEIGC(ITRANS)=1
      ENDIF
!
      DO I=1,NDM
         VDOT=0.0D0
         DO J=1,NDM
            VDOT=VDOT+VR(J,I)*VRPREV(ITRANS,J,I)
         ENDDO
         IF (VDOT.LT.0.0D0) THEN
            DO J=1,NDM
               VR(J,I)=-VR(J,I)
!               VI(J,I)=-VI(J,I)
            ENDDO
         ENDIF
         DO J=1,NDM
            VRPREV(ITRANS,J,I)=VR(J,I)
         ENDDO
      ENDDO
!
! Send back the transpose of the matrix of real parts of eigenvectors
!
      DO I=1,NDM
         DO J=1,NDM
            VRET(I,J)=VR(J,I)
         ENDDO
      ENDDO
!     
      DEALLOCATE(VI,VR,F,FV1,IV1)
      RETURN
      END SUBROUTINE EIGHO
!
!     ---------- ------
      SUBROUTINE PRJCTI(IAP,RAP,BOUND,CSAVE,XEQUIB,ICP,PAR, &
           IMFD,IS,ITRANS,NDM)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL CSAVE
      INTEGER IAP(*),ICP(*)
      DOUBLE PRECISION RAP(*),BOUND(NDM,*),XEQUIB(*),PAR(*)
! Local
      ALLOCATABLE A(:,:),V(:,:)
!
      ALLOCATE(A(NDM,NDM),V(NDM,NDM))
      CALL PRJCTN(IAP,RAP,BOUND,CSAVE,XEQUIB,ICP,PAR, &
           IMFD,IS,ITRANS,NDM,A,V)
      DEALLOCATE(A,V)
!
      RETURN
      END SUBROUTINE PRJCTI
!
!     ---------- ------
      SUBROUTINE PRJCTN(IAP,RAP,BOUND,CSAVE,XEQUIB,ICP,PAR, &
           IMFD,IS,ITRANS,NDM,A,V)
!
! Compute NUNSTAB (or NSTAB) projection boundary condition functions
! onto to the UNSTABLE (or STABLE) manifold of the appropriate equilibrium
!
!    IMFD   = -1 stable eigenspace
!           =  1 unstable eigenspace
!    ITRANS =  1 use transpose of A
!           =  2 otherwise
!    IS     =  I (1 or 2) implies use the ith equilibrium in XEQUIB
!
! Use the normalization in Beyn 1990 (4.4) to ensure continuity 
! w.r.t parameters.
! For the purposes of this routine the "previous point on the
! branch" is at the values of PAR at which the routine was last
! called with the same values of IS and ITRANS.
!
      USE INTERFACES, ONLY:FUNI
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION IAP(*),RAP(*),ICP(*),PAR(*),A(NDM,*),V(NDM,*)
      DIMENSION BOUND(NDM,*),XEQUIB(*)
      LOGICAL CSAVE
! Local
      INTEGER TYPE,IFLAG(2,2)
      DOUBLE PRECISION UDUM(1),DDUM(1)
      ALLOCATABLE ER(:),EI(:),D(:,:),CPREV(:,:,:,:)
      ALLOCATABLE DUM1(:,:),DUM2(:,:),FDUM(:),ORT(:)
      ALLOCATABLE IR(:),IC(:),TYPE(:)
!
      SAVE CPREV,IFLAG
!
      ALLOCATE(FDUM(NDM))
      CALL FUNI(IAP,RAP,NDM,XEQUIB,UDUM,ICP,PAR,1,FDUM,A,DDUM)
      DEALLOCATE(FDUM)
!
! Compute transpose of A if ITRANS=1
      IF (ITRANS.EQ.1) THEN
        DO I=1,NDM
          DO J=1,I-1
            TMP=A(I,J)
            A(I,J)=A(J,I)
            A(J,I)=TMP
          ENDDO
        ENDDO
      ENDIF
!
! Compute basis V to put A in upper Hessenberg form
!    
        ALLOCATE(ORT(NDM))
        CALL ORTHES(NDM,NDM,1,NDM,A,ORT)
        CALL ORTRAN(NDM,NDM,1,NDM,A,ORT,V)
        DEALLOCATE(ORT)
!
! Force A to be upper Hessenberg
        IF (NDM.GT.2) THEN
          DO I=3,NDM   
            DO J=1,I-2
              A(I,J) = 0.0D0
            ENDDO 
          ENDDO
        ENDIF
!
! Computes basis to put A in "Quasi Upper-Triangular form"
! with the positive (negative) eigenvalues first if IMFD =-1 (=1)
        EPS = COMPZERO
        ALLOCATE(TYPE(NDM),ER(NDM),EI(NDM))
        CALL HQR3LC(A,V,NDM,1,NDM,EPS,ER,EI,TYPE,NDM,NDM,IMFD)
        DEALLOCATE(TYPE,ER,EI)
!
! Determine basis of the appropriate part of the matrix V
        IF (IMFD.EQ.1) THEN
           MCOND = NUNSTAB
        ELSE
           MCOND = NSTAB
        ENDIF
!
! Set previous matrix to be the present one if this is the first call
      IF (IFLAG(IS,ITRANS).NE.1234) THEN
         IF (.NOT.ALLOCATED(CPREV))ALLOCATE(CPREV(NDM,NDM,2,2))
         DO I=1,MCOND
            DO J=1,NDM
               CPREV(I,J,IS,ITRANS)=V(J,I)
	       BOUND(I,J)=V(J,I)
            ENDDO
         ENDDO
         IFLAG(IS,ITRANS)=1234
	 RETURN
      ENDIF
!     
! Calculate the (transpose of the) BEYN matrix D and hence BOUND 
      ALLOCATE(D(NDM,NDM),DUM1(NDM,NDM),DUM2(NDM,NDM))
      DO I=1,MCOND
        DO J=1,MCOND
          DUM1(I,J)=0.0D0
          DUM2(I,J)=0.0D0
          DO K=1,NDM
             DUM1(I,J) = DUM1(I,J)+CPREV(I,K,IS,ITRANS)* &
                  V(K,J)
             DUM2(I,J) = DUM2(I,J)+CPREV(I,K,IS,ITRANS)* &
                  CPREV(J,K,IS,ITRANS)
          ENDDO
        ENDDO
      ENDDO
!     
      IF(MCOND.GT.0)THEN
        ALLOCATE(IR(NDM),IC(NDM))
        CALL GE(0,MCOND,NDM,DUM1,MCOND,NDM,D,NDM,DUM2,IR,IC,DET)
        DEALLOCATE(IR,IC)
      ENDIF
!     
      DO I=1,MCOND
         DO J=1,NDM
            BOUND(I,J)=0.0
            DO K=1,MCOND
               BOUND(I,J)=BOUND(I,J)+D(K,I)*V(J,K)
            ENDDO
         ENDDO
      ENDDO
!     
      IF(CSAVE)THEN
         DO I=1,MCOND
            DO J=1,NDM
               CPREV(I,J,IS,ITRANS)=BOUND(I,J)
            ENDDO
         ENDDO
      ENDIF
!     
      DEALLOCATE(D,DUM1,DUM2)
      RETURN
      END SUBROUTINE PRJCTN
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      END MODULE HOMCONT

