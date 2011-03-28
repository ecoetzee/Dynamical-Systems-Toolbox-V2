!========================================================================
! EISPACK.F - Subroutine for AUTO
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
!          Eigenvalue solver from EISPACK
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE RG(NM,N,A,WR,WI,MATZ,Z,IV1,FV1,IERR)
!
      INTEGER N,NM,IS1,IS2,IERR,MATZ
      DOUBLE PRECISION A(NM,N),WR(N),WI(N),Z(NM,N),FV1(N)
      INTEGER IV1(N)
!
!     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF
!     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK)
!     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED)
!     OF A REAL GENERAL MATRIX.
!
!     ON INPUT
!
!        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL
!        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!        DIMENSION STATEMENT.
!
!        N  IS THE ORDER OF THE MATRIX  A.
!
!        A  CONTAINS THE REAL GENERAL MATRIX.
!
!        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF
!        ONLY EIGENVALUES ARE DESIRED.  OTHERWISE IT IS SET TO
!        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS.
!
!     ON OUTPUT
!
!        WR  AND  WI  CONTAIN THE REAL AND IMAGINARY PARTS,
!        RESPECTIVELY, OF THE EIGENVALUES.  COMPLEX CONJUGATE
!        PAIRS OF EIGENVALUES APPEAR CONSECUTIVELY WITH THE
!        EIGENVALUE HAVING THE POSITIVE IMAGINARY PART FIRST.
!
!        Z  CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS
!        IF MATZ IS NOT ZERO.  IF THE J-TH EIGENVALUE IS REAL, THE
!        J-TH COLUMN OF  Z  CONTAINS ITS EIGENVECTOR.  IF THE J-TH
!        EIGENVALUE IS COMPLEX WITH POSITIVE IMAGINARY PART, THE
!        J-TH AND (J+1)-TH COLUMNS OF  Z  CONTAIN THE REAL AND
!        IMAGINARY PARTS OF ITS EIGENVECTOR.  THE CONJUGATE OF THIS
!        VECTOR IS THE EIGENVECTOR FOR THE CONJUGATE EIGENVALUE.
!
!        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN ERROR
!           COMPLETION CODE DESCRIBED IN THE DOCUMENTATION FOR HQR
!           AND HQR2.  THE NORMAL COMPLETION CODE IS ZERO.
!
!        IV1  AND  FV1  ARE TEMPORARY STORAGE ARRAYS.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
!     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
!
!     THIS VERSION DATED AUGUST 1983.
!
!     ------------------------------------------------------------------
!
      IF (N .LE. NM) GO TO 10
      IERR = 10 * N
      GO TO 50
!
   10 CALL  BALANC(NM,N,A,IS1,IS2,FV1)
      CALL  ELMHES(NM,N,IS1,IS2,A,IV1)
      IF (MATZ .NE. 0) GO TO 20
!     .......... FIND EIGENVALUES ONLY ..........
      CALL  HQR(NM,N,IS1,IS2,A,WR,WI,IERR)
      GO TO 50
!     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS ..........
   20 CALL  ELTRAN(NM,N,IS1,IS2,A,IV1,Z)
      CALL  HQR2(NM,N,IS1,IS2,A,WR,WI,Z,IERR)
      IF (IERR .NE. 0) GO TO 50
      CALL  BALBAK(NM,N,IS1,IS2,FV1,N,Z)
   50 RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE HQR(NM,N,LOW,IGH,H,WR,WI,IERR)
!
      INTEGER I,J,K,L,M,N,EN,LL,MM,NA,NM,IGH,ITN,ITS,LOW,MP2,ENM2,IERR
      DOUBLE PRECISION H(NM,N),WR(N),WI(N)
      DOUBLE PRECISION P,Q,R,S,T,W,X,Y,ZZ,NORM,TST1,TST2
      LOGICAL NOTLAS
!
!     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE HQR,
!     NUM. MATH. 14, 219-231(1970) BY MARTIN, PETERS, AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 359-371(1971).
!
!     THIS SUBROUTINE FINDS THE EIGENVALUES OF A REAL
!     UPPER HESSENBERG MATRIX BY THE QR METHOD.
!
!     ON INPUT
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
!          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,
!          SET LOW=1, IGH=N.
!
!        H CONTAINS THE UPPER HESSENBERG MATRIX.  INFORMATION ABOUT
!          THE TRANSFORMATIONS USED IN THE REDUCTION TO HESSENBERG
!          FORM BY  ELMHES  OR  ORTHES, IF PERFORMED, IS STORED
!          IN THE REMAINING TRIANGLE UNDER THE HESSENBERG MATRIX.
!
!     ON OUTPUT
!
!        H HAS BEEN DESTROYED.  THEREFORE, IT MUST BE SAVED
!          BEFORE CALLING  HQR  IF SUBSEQUENT CALCULATION AND
!          BACK TRANSFORMATION OF EIGENVECTORS IS TO BE PERFORMED.
!
!        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE EIGENVALUES.  THE EIGENVALUES
!          ARE UNORDERED EXCEPT THAT COMPLEX CONJUGATE PAIRS
!          OF VALUES APPEAR CONSECUTIVELY WITH THE EIGENVALUE
!          HAVING THE POSITIVE IMAGINARY PART FIRST.  IF AN
!          ERROR EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT
!          FOR INDICES IERR+1,...,N.
!
!        IERR IS SET TO
!          ZERO       FOR NORMAL RETURN,
!          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED
!                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
!     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
!
!     THIS VERSION DATED AUGUST 1983.
!
!     ------------------------------------------------------------------
!
      IERR = 0
      NORM = 0.0D0
      K = 1
!     .......... STORE ROOTS ISOLATED BY BALANC
!                AND COMPUTE MATRIX NORM ..........
      DO 50 I = 1, N
!
         DO 40 J = K, N
   40    NORM = NORM + DABS(H(I,J))
!
         K = I
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 50
         WR(I) = H(I,I)
         WI(I) = 0.0D0
   50 CONTINUE
!
      EN = IGH
      T = 0.0D0
      ITN = 30*N
!     .......... SEARCH FOR NEXT EIGENVALUES ..........
   60 IF (EN .LT. LOW) GO TO 1001
      ITS = 0
      NA = EN - 1
      ENM2 = NA - 1
!     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
!                FOR L=EN STEP -1 UNTIL LOW DO -- ..........
   70 DO 80 LL = LOW, EN
         L = EN + LOW - LL
         IF (L .EQ. LOW) GO TO 100
         S = DABS(H(L-1,L-1)) + DABS(H(L,L))
         IF (S .EQ. 0.0D0) S = NORM
         TST1 = S
         TST2 = TST1 + DABS(H(L,L-1))
         IF (TST2 .EQ. TST1) GO TO 100
   80 CONTINUE
!     .......... FORM SHIFT ..........
  100 X = H(EN,EN)
      IF (L .EQ. EN) GO TO 270
      Y = H(NA,NA)
      W = H(EN,NA) * H(NA,EN)
      IF (L .EQ. NA) GO TO 280
      IF (ITN .EQ. 0) GO TO 1000
      IF (ITS .NE. 10 .AND. ITS .NE. 20) GO TO 130
!     .......... FORM EXCEPTIONAL SHIFT ..........
      T = T + X
!
      DO 120 I = LOW, EN
  120 H(I,I) = H(I,I) - X
!
      S = DABS(H(EN,NA)) + DABS(H(NA,ENM2))
      X = 0.75D0 * S
      Y = X
      W = -0.4375D0 * S * S
  130 ITS = ITS + 1
      ITN = ITN - 1
!     .......... LOOK FOR TWO CONSECUTIVE SMALL
!                SUB-DIAGONAL ELEMENTS.
!                FOR M=EN-2 STEP -1 UNTIL L DO -- ..........
      DO 140 MM = L, ENM2
         M = ENM2 + L - MM
         ZZ = H(M,M)
         R = X - ZZ
         S = Y - ZZ
         P = (R * S - W) / H(M+1,M) + H(M,M+1)
         Q = H(M+1,M+1) - ZZ - R - S
         R = H(M+2,M+1)
         S = DABS(P) + DABS(Q) + DABS(R)
         P = P / S
         Q = Q / S
         R = R / S
         IF (M .EQ. L) GO TO 150
         TST1 = DABS(P)*(DABS(H(M-1,M-1)) + DABS(ZZ) + DABS(H(M+1,M+1)))
         TST2 = TST1 + DABS(H(M,M-1))*(DABS(Q) + DABS(R))
         IF (TST2 .EQ. TST1) GO TO 150
  140 CONTINUE
!
  150 MP2 = M + 2
!
      DO 160 I = MP2, EN
         H(I,I-2) = 0.0D0
         IF (I .EQ. MP2) GO TO 160
         H(I,I-3) = 0.0D0
  160 CONTINUE
!     .......... DOUBLE QR STEP INVOLVING ROWS L TO EN AND
!                COLUMNS M TO EN ..........
      DO 260 K = M, NA
         NOTLAS = K .NE. NA
         IF (K .EQ. M) GO TO 170
         P = H(K,K-1)
         Q = H(K+1,K-1)
         R = 0.0D0
         IF (NOTLAS) R = H(K+2,K-1)
         X = DABS(P) + DABS(Q) + DABS(R)
         IF (X .EQ. 0.0D0) GO TO 260
         P = P / X
         Q = Q / X
         R = R / X
  170    S = DSIGN(DSQRT(P*P+Q*Q+R*R),P)
         IF (K .EQ. M) GO TO 180
         H(K,K-1) = -S * X
         GO TO 190
  180    IF (L .NE. M) H(K,K-1) = -H(K,K-1)
  190    P = P + S
         X = P / S
         Y = Q / S
         ZZ = R / S
         Q = Q / P
         R = R / P
         IF (NOTLAS) GO TO 225
!     .......... ROW MODIFICATION ..........
         DO 200 J = K, N
            P = H(K,J) + Q * H(K+1,J)
            H(K,J) = H(K,J) - P * X
            H(K+1,J) = H(K+1,J) - P * Y
  200    CONTINUE
!
         J = MIN0(EN,K+3)
!     .......... COLUMN MODIFICATION ..........
         DO 210 I = 1, J
            P = X * H(I,K) + Y * H(I,K+1)
            H(I,K) = H(I,K) - P
            H(I,K+1) = H(I,K+1) - P * Q
  210    CONTINUE
         GO TO 255
  225    CONTINUE
!     .......... ROW MODIFICATION ..........
         DO 230 J = K, N
            P = H(K,J) + Q * H(K+1,J) + R * H(K+2,J)
            H(K,J) = H(K,J) - P * X
            H(K+1,J) = H(K+1,J) - P * Y
            H(K+2,J) = H(K+2,J) - P * ZZ
  230    CONTINUE
!
         J = MIN0(EN,K+3)
!     .......... COLUMN MODIFICATION ..........
         DO 240 I = 1, J
            P = X * H(I,K) + Y * H(I,K+1) + ZZ * H(I,K+2)
            H(I,K) = H(I,K) - P
            H(I,K+1) = H(I,K+1) - P * Q
            H(I,K+2) = H(I,K+2) - P * R
  240    CONTINUE
  255    CONTINUE
!
  260 CONTINUE
!
      GO TO 70
!     .......... ONE ROOT FOUND ..........
  270 WR(EN) = X + T
      WI(EN) = 0.0D0
      EN = NA
      GO TO 60
!     .......... TWO ROOTS FOUND ..........
  280 P = (Y - X) / 2.0D0
      Q = P * P + W
      ZZ = DSQRT(DABS(Q))
      X = X + T
      IF (Q .LT. 0.0D0) GO TO 320
!     .......... REAL PAIR ..........
      ZZ = P + DSIGN(ZZ,P)
      WR(NA) = X + ZZ
      WR(EN) = WR(NA)
      IF (ZZ .NE. 0.0D0) WR(EN) = X - W / ZZ
      WI(NA) = 0.0D0
      WI(EN) = 0.0D0
      GO TO 330
!     .......... COMPLEX PAIR ..........
  320 WR(NA) = X + P
      WR(EN) = X + P
      WI(NA) = ZZ
      WI(EN) = -ZZ
  330 EN = ENM2
      GO TO 60
!     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT
!                CONVERGED AFTER 30*N ITERATIONS ..........
 1000 IERR = EN
 1001 RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE HQR2(NM,N,LOW,IGH,H,WR,WI,Z,IERR)
!
      INTEGER I,J,K,L,M,N,EN,II,JJ,LL,MM,NA,NM,NN, &
           IGH,ITN,ITS,LOW,MP2,ENM2,IERR
      DOUBLE PRECISION H(NM,N),WR(N),WI(N),Z(NM,N)
      DOUBLE PRECISION P,Q,R,S,T,W,X,Y,RA,SA,VI,VR,ZZ,NORM,TST1,TST2
      LOGICAL NOTLAS
!
!     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE HQR2,
!     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).
!
!     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
!     OF A REAL UPPER HESSENBERG MATRIX BY THE QR METHOD.  THE
!     EIGENVECTORS OF A REAL GENERAL MATRIX CAN ALSO BE FOUND
!     IF  ELMHES  AND  ELTRAN  OR  ORTHES  AND  ORTRAN  HAVE
!     BEEN USED TO REDUCE THIS GENERAL MATRIX TO HESSENBERG FORM
!     AND TO ACCUMULATE THE SIMILARITY TRANSFORMATIONS.
!
!     ON INPUT
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
!          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,
!          SET LOW=1, IGH=N.
!
!        H CONTAINS THE UPPER HESSENBERG MATRIX.
!
!        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED BY  ELTRAN
!          AFTER THE REDUCTION BY  ELMHES, OR BY  ORTRAN  AFTER THE
!          REDUCTION BY  ORTHES, IF PERFORMED.  IF THE EIGENVECTORS
!          OF THE HESSENBERG MATRIX ARE DESIRED, Z MUST CONTAIN THE
!          IDENTITY MATRIX.
!
!     ON OUTPUT
!
!        H HAS BEEN DESTROYED.
!
!        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE EIGENVALUES.  THE EIGENVALUES
!          ARE UNORDERED EXCEPT THAT COMPLEX CONJUGATE PAIRS
!          OF VALUES APPEAR CONSECUTIVELY WITH THE EIGENVALUE
!          HAVING THE POSITIVE IMAGINARY PART FIRST.  IF AN
!          ERROR EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT
!          FOR INDICES IERR+1,...,N.
!
!        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.
!          IF THE I-TH EIGENVALUE IS REAL, THE I-TH COLUMN OF Z
!          CONTAINS ITS EIGENVECTOR.  IF THE I-TH EIGENVALUE IS COMPLEX
!          WITH POSITIVE IMAGINARY PART, THE I-TH AND (I+1)-TH
!          COLUMNS OF Z CONTAIN THE REAL AND IMAGINARY PARTS OF ITS
!          EIGENVECTOR.  THE EIGENVECTORS ARE UNNORMALIZED.  IF AN
!          ERROR EXIT IS MADE, NONE OF THE EIGENVECTORS HAS BEEN FOUND.
!
!        IERR IS SET TO
!          ZERO       FOR NORMAL RETURN,
!          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED
!                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.
!
!     CALLS CDIV FOR COMPLEX DIVISION.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
!     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
!
!     THIS VERSION DATED AUGUST 1983.
!
!     ------------------------------------------------------------------
!
      IERR = 0
      NORM = 0.0D0
      K = 1
!     .......... STORE ROOTS ISOLATED BY BALANC
!                AND COMPUTE MATRIX NORM ..........
      DO 50 I = 1, N
!
         DO 40 J = K, N
   40    NORM = NORM + DABS(H(I,J))
!
         K = I
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 50
         WR(I) = H(I,I)
         WI(I) = 0.0D0
   50 CONTINUE
!
      EN = IGH
      T = 0.0D0
      ITN = 30*N
!     .......... SEARCH FOR NEXT EIGENVALUES ..........
   60 IF (EN .LT. LOW) GO TO 340
      ITS = 0
      NA = EN - 1
      ENM2 = NA - 1
!     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
!                FOR L=EN STEP -1 UNTIL LOW DO -- ..........
   70 DO 80 LL = LOW, EN
         L = EN + LOW - LL
         IF (L .EQ. LOW) GO TO 100
         S = DABS(H(L-1,L-1)) + DABS(H(L,L))
         IF (S .EQ. 0.0D0) S = NORM
         TST1 = S
         TST2 = TST1 + DABS(H(L,L-1))
         IF (TST2 .EQ. TST1) GO TO 100
   80 CONTINUE
!     .......... FORM SHIFT ..........
  100 X = H(EN,EN)
      IF (L .EQ. EN) GO TO 270
      Y = H(NA,NA)
      W = H(EN,NA) * H(NA,EN)
      IF (L .EQ. NA) GO TO 280
      IF (ITN .EQ. 0) GO TO 1000
      IF (ITS .NE. 10 .AND. ITS .NE. 20) GO TO 130
!     .......... FORM EXCEPTIONAL SHIFT ..........
      T = T + X
!
      DO 120 I = LOW, EN
  120 H(I,I) = H(I,I) - X
!
      S = DABS(H(EN,NA)) + DABS(H(NA,ENM2))
      X = 0.75D0 * S
      Y = X
      W = -0.4375D0 * S * S
  130 ITS = ITS + 1
      ITN = ITN - 1
!     .......... LOOK FOR TWO CONSECUTIVE SMALL
!                SUB-DIAGONAL ELEMENTS.
!                FOR M=EN-2 STEP -1 UNTIL L DO -- ..........
      DO 140 MM = L, ENM2
         M = ENM2 + L - MM
         ZZ = H(M,M)
         R = X - ZZ
         S = Y - ZZ
         P = (R * S - W) / H(M+1,M) + H(M,M+1)
         Q = H(M+1,M+1) - ZZ - R - S
         R = H(M+2,M+1)
         S = DABS(P) + DABS(Q) + DABS(R)
         P = P / S
         Q = Q / S
         R = R / S
         IF (M .EQ. L) GO TO 150
         TST1 = DABS(P)*(DABS(H(M-1,M-1)) + DABS(ZZ) + DABS(H(M+1,M+1)))
         TST2 = TST1 + DABS(H(M,M-1))*(DABS(Q) + DABS(R))
         IF (TST2 .EQ. TST1) GO TO 150
  140 CONTINUE
!
  150 MP2 = M + 2
!
      DO 160 I = MP2, EN
         H(I,I-2) = 0.0D0
         IF (I .EQ. MP2) GO TO 160
         H(I,I-3) = 0.0D0
  160 CONTINUE
!     .......... DOUBLE QR STEP INVOLVING ROWS L TO EN AND
!                COLUMNS M TO EN ..........
      DO 260 K = M, NA
         NOTLAS = K .NE. NA
         IF (K .EQ. M) GO TO 170
         P = H(K,K-1)
         Q = H(K+1,K-1)
         R = 0.0D0
         IF (NOTLAS) R = H(K+2,K-1)
         X = DABS(P) + DABS(Q) + DABS(R)
         IF (X .EQ. 0.0D0) GO TO 260
         P = P / X
         Q = Q / X
         R = R / X
  170    S = DSIGN(DSQRT(P*P+Q*Q+R*R),P)
         IF (K .EQ. M) GO TO 180
         H(K,K-1) = -S * X
         GO TO 190
  180    IF (L .NE. M) H(K,K-1) = -H(K,K-1)
  190    P = P + S
         X = P / S
         Y = Q / S
         ZZ = R / S
         Q = Q / P
         R = R / P
         IF (NOTLAS) GO TO 225
!     .......... ROW MODIFICATION ..........
         DO 200 J = K, N
            P = H(K,J) + Q * H(K+1,J)
            H(K,J) = H(K,J) - P * X
            H(K+1,J) = H(K+1,J) - P * Y
  200    CONTINUE
!
         J = MIN0(EN,K+3)
!     .......... COLUMN MODIFICATION ..........
         DO 210 I = 1, J
            P = X * H(I,K) + Y * H(I,K+1)
            H(I,K) = H(I,K) - P
            H(I,K+1) = H(I,K+1) - P * Q
  210    CONTINUE
!     .......... ACCUMULATE TRANSFORMATIONS ..........
         DO 220 I = LOW, IGH
            P = X * Z(I,K) + Y * Z(I,K+1)
            Z(I,K) = Z(I,K) - P
            Z(I,K+1) = Z(I,K+1) - P * Q
  220    CONTINUE
         GO TO 255
  225    CONTINUE
!     .......... ROW MODIFICATION ..........
         DO 230 J = K, N
            P = H(K,J) + Q * H(K+1,J) + R * H(K+2,J)
            H(K,J) = H(K,J) - P * X
            H(K+1,J) = H(K+1,J) - P * Y
            H(K+2,J) = H(K+2,J) - P * ZZ
  230    CONTINUE
!
         J = MIN0(EN,K+3)
!     .......... COLUMN MODIFICATION ..........
         DO 240 I = 1, J
            P = X * H(I,K) + Y * H(I,K+1) + ZZ * H(I,K+2)
            H(I,K) = H(I,K) - P
            H(I,K+1) = H(I,K+1) - P * Q
            H(I,K+2) = H(I,K+2) - P * R
  240    CONTINUE
!     .......... ACCUMULATE TRANSFORMATIONS ..........
         DO 250 I = LOW, IGH
            P = X * Z(I,K) + Y * Z(I,K+1) + ZZ * Z(I,K+2)
            Z(I,K) = Z(I,K) - P
            Z(I,K+1) = Z(I,K+1) - P * Q
            Z(I,K+2) = Z(I,K+2) - P * R
  250    CONTINUE
  255    CONTINUE
!
  260 CONTINUE
!
      GO TO 70
!     .......... ONE ROOT FOUND ..........
  270 H(EN,EN) = X + T
      WR(EN) = H(EN,EN)
      WI(EN) = 0.0D0
      EN = NA
      GO TO 60
!     .......... TWO ROOTS FOUND ..........
  280 P = (Y - X) / 2.0D0
      Q = P * P + W
      ZZ = DSQRT(DABS(Q))
      H(EN,EN) = X + T
      X = H(EN,EN)
      H(NA,NA) = Y + T
      IF (Q .LT. 0.0D0) GO TO 320
!     .......... REAL PAIR ..........
      ZZ = P + DSIGN(ZZ,P)
      WR(NA) = X + ZZ
      WR(EN) = WR(NA)
      IF (ZZ .NE. 0.0D0) WR(EN) = X - W / ZZ
      WI(NA) = 0.0D0
      WI(EN) = 0.0D0
      X = H(EN,NA)
      S = DABS(X) + DABS(ZZ)
      P = X / S
      Q = ZZ / S
      R = DSQRT(P*P+Q*Q)
      P = P / R
      Q = Q / R
!     .......... ROW MODIFICATION ..........
      DO 290 J = NA, N
         ZZ = H(NA,J)
         H(NA,J) = Q * ZZ + P * H(EN,J)
         H(EN,J) = Q * H(EN,J) - P * ZZ
  290 CONTINUE
!     .......... COLUMN MODIFICATION ..........
      DO 300 I = 1, EN
         ZZ = H(I,NA)
         H(I,NA) = Q * ZZ + P * H(I,EN)
         H(I,EN) = Q * H(I,EN) - P * ZZ
  300 CONTINUE
!     .......... ACCUMULATE TRANSFORMATIONS ..........
      DO 310 I = LOW, IGH
         ZZ = Z(I,NA)
         Z(I,NA) = Q * ZZ + P * Z(I,EN)
         Z(I,EN) = Q * Z(I,EN) - P * ZZ
  310 CONTINUE
!
      GO TO 330
!     .......... COMPLEX PAIR ..........
  320 WR(NA) = X + P
      WR(EN) = X + P
      WI(NA) = ZZ
      WI(EN) = -ZZ
  330 EN = ENM2
      GO TO 60
!     .......... ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND
!                VECTORS OF UPPER TRIANGULAR FORM ..........
  340 IF (NORM .EQ. 0.0D0) GO TO 1001
!     .......... FOR EN=N STEP -1 UNTIL 1 DO -- ..........
      DO 800 NN = 1, N
         EN = N + 1 - NN
         P = WR(EN)
         Q = WI(EN)
         NA = EN - 1
!         IF (Q) 710, 600, 800 -- obsolete: translate by B.O.
         IF (Q.LT.0) GO TO 710
         IF (Q.GT.0) GO TO 800
!     .......... REAL VECTOR ..........
  600    M = EN
         H(EN,EN) = 1.0D0
         IF (NA .EQ. 0) GO TO 800
!     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- ..........
         DO 700 II = 1, NA
            I = EN - II
            W = H(I,I) - P
            R = 0.0D0
!
            DO 610 J = M, EN
  610       R = R + H(I,J) * H(J,EN)
!
            IF (WI(I) .GE. 0.0D0) GO TO 630
            ZZ = W
            S = R
            GO TO 700
  630       M = I
            IF (WI(I) .NE. 0.0D0) GO TO 640
            T = W
            IF (T .NE. 0.0D0) GO TO 635
               TST1 = NORM
               T = TST1
  632          T = 0.01D0 * T
               TST2 = NORM + T
               IF (TST2 .GT. TST1) GO TO 632
  635       H(I,EN) = -R / T
            GO TO 680
!     .......... SOLVE REAL EQUATIONS ..........
  640       X = H(I,I+1)
            Y = H(I+1,I)
            Q = (WR(I) - P) * (WR(I) - P) + WI(I) * WI(I)
            T = (X * S - ZZ * R) / Q
            H(I,EN) = T
            IF (DABS(X) .LE. DABS(ZZ)) GO TO 650
            H(I+1,EN) = (-R - W * T) / X
            GO TO 680
  650       H(I+1,EN) = (-S - Y * T) / ZZ
!
!     .......... OVERFLOW CONTROL ..........
  680       T = DABS(H(I,EN))
            IF (T .EQ. 0.0D0) GO TO 700
            TST1 = T
            TST2 = TST1 + 1.0D0/TST1
            IF (TST2 .GT. TST1) GO TO 700
            DO 690 J = I, EN
               H(J,EN) = H(J,EN)/T
  690       CONTINUE
!
  700    CONTINUE
!     .......... END REAL VECTOR ..........
         GO TO 800
!     .......... COMPLEX VECTOR ..........
  710    M = NA
!     .......... LAST VECTOR COMPONENT CHOSEN IMAGINARY SO THAT
!                EIGENVECTOR MATRIX IS TRIANGULAR ..........
         IF (DABS(H(EN,NA)) .LE. DABS(H(NA,EN))) GO TO 720
         H(NA,NA) = Q / H(EN,NA)
         H(NA,EN) = -(H(EN,EN) - P) / H(EN,NA)
         GO TO 730
  720    CALL CDIV(0.0D0,-H(NA,EN),H(NA,NA)-P,Q,H(NA,NA),H(NA,EN))
  730    H(EN,NA) = 0.0D0
         H(EN,EN) = 1.0D0
         ENM2 = NA - 1
         IF (ENM2 .EQ. 0) GO TO 800
!     .......... FOR I=EN-2 STEP -1 UNTIL 1 DO -- ..........
         DO 795 II = 1, ENM2
            I = NA - II
            W = H(I,I) - P
            RA = 0.0D0
            SA = 0.0D0
!
            DO 760 J = M, EN
               RA = RA + H(I,J) * H(J,NA)
               SA = SA + H(I,J) * H(J,EN)
  760       CONTINUE
!
            IF (WI(I) .GE. 0.0D0) GO TO 770
            ZZ = W
            R = RA
            S = SA
            GO TO 795
  770       M = I
            IF (WI(I) .NE. 0.0D0) GO TO 780
            CALL CDIV(-RA,-SA,W,Q,H(I,NA),H(I,EN))
            GO TO 790
!     .......... SOLVE COMPLEX EQUATIONS ..........
  780       X = H(I,I+1)
            Y = H(I+1,I)
            VR = (WR(I) - P) * (WR(I) - P) + WI(I) * WI(I) - Q * Q
            VI = (WR(I) - P) * 2.0D0 * Q
            IF (VR .NE. 0.0D0 .OR. VI .NE. 0.0D0) GO TO 784
               TST1 = NORM * (DABS(W) + DABS(Q) + DABS(X) &
                    + DABS(Y) + DABS(ZZ))
               VR = TST1
  783          VR = 0.01D0 * VR
               TST2 = TST1 + VR
               IF (TST2 .GT. TST1) GO TO 783
  784       CALL CDIV(X*R-ZZ*RA+Q*SA,X*S-ZZ*SA-Q*RA,VR,VI, &
                 H(I,NA),H(I,EN))
            IF (DABS(X) .LE. DABS(ZZ) + DABS(Q)) GO TO 785
            H(I+1,NA) = (-RA - W * H(I,NA) + Q * H(I,EN)) / X
            H(I+1,EN) = (-SA - W * H(I,EN) - Q * H(I,NA)) / X
            GO TO 790
  785       CALL CDIV(-R-Y*H(I,NA),-S-Y*H(I,EN),ZZ,Q, &
                 H(I+1,NA),H(I+1,EN))
!
!     .......... OVERFLOW CONTROL ..........
  790       T = DMAX1(DABS(H(I,NA)), DABS(H(I,EN)))
            IF (T .EQ. 0.0D0) GO TO 795
            TST1 = T
            TST2 = TST1 + 1.0D0/TST1
            IF (TST2 .GT. TST1) GO TO 795
            DO 792 J = I, EN
               H(J,NA) = H(J,NA)/T
               H(J,EN) = H(J,EN)/T
  792       CONTINUE
!
  795    CONTINUE
!     .......... END COMPLEX VECTOR ..........
  800 CONTINUE
!     .......... END BACK SUBSTITUTION.
!                VECTORS OF ISOLATED ROOTS ..........
      DO 840 I = 1, N
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 840
!
         DO 820 J = I, N
  820    Z(I,J) = H(I,J)
!
  840 CONTINUE
!     .......... MULTIPLY BY TRANSFORMATION MATRIX TO GIVE
!                VECTORS OF ORIGINAL FULL MATRIX.
!                FOR J=N STEP -1 UNTIL LOW DO -- ..........
      DO 880 JJ = LOW, N
         J = N + LOW - JJ
         M = MIN0(J,IGH)
!
         DO 880 I = LOW, IGH
            ZZ = 0.0D0
!
            DO 860 K = LOW, M
  860       ZZ = ZZ + Z(I,K) * H(K,J)
!
            Z(I,J) = ZZ
  880 CONTINUE
!
      GO TO 1001
!     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT
!                CONVERGED AFTER 30*N ITERATIONS ..........
 1000 IERR = EN
 1001 RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE CDIV(AR,AI,BR,BI,CR,CI)
      DOUBLE PRECISION AR,AI,BR,BI,CR,CI
!
!     COMPLEX DIVISION, (CR,CI) = (AR,AI)/(BR,BI)
!
      DOUBLE PRECISION S,ARS,AIS,BRS,BIS
      S = DABS(BR) + DABS(BI)
      ARS = AR/S
      AIS = AI/S
      BRS = BR/S
      BIS = BI/S
      S = BRS**2 + BIS**2
      CR = (ARS*BRS + AIS*BIS)/S
      CI = (AIS*BRS - ARS*BIS)/S
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE BALANC(NM,N,A,LOW,IGH,SCALE)
!
      INTEGER I,J,K,L,M,N,JJ,NM,IGH,LOW,IEXC
      DOUBLE PRECISION A(NM,N),SCALE(N)
      DOUBLE PRECISION C,F,G,R,S,B2,RADIX
      LOGICAL NOCONV
!
!     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE BALANCE,
!     NUM. MATH. 13, 293-304(1969) BY PARLETT AND REINSCH.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971).
!
!     THIS SUBROUTINE BALANCES A REAL MATRIX AND ISOLATES
!     EIGENVALUES WHENEVER POSSIBLE.
!
!     ON INPUT
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        A CONTAINS THE INPUT MATRIX TO BE BALANCED.
!
!     ON OUTPUT
!
!        A CONTAINS THE BALANCED MATRIX.
!
!        LOW AND IGH ARE TWO INTEGERS SUCH THAT A(I,J)
!          IS EQUAL TO ZERO IF
!           (1) I IS GREATER THAN J AND
!           (2) J=1,...,LOW-1 OR I=IGH+1,...,N.
!
!        SCALE CONTAINS INFORMATION DETERMINING THE
!           PERMUTATIONS AND SCALING FACTORS USED.
!
!     SUPPOSE THAT THE PRINCIPAL SUBMATRIX IN ROWS LOW THROUGH IGH
!     HAS BEEN BALANCED, THAT P(J) DENOTES THE INDEX INTERCHANGED
!     WITH J DURING THE PERMUTATION STEP, AND THAT THE ELEMENTS
!     OF THE DIAGONAL MATRIX USED ARE DENOTED BY D(I,J).  THEN
!        SCALE(J) = P(J),    FOR J = 1,...,LOW-1
!                 = D(J,J),      J = LOW,...,IGH
!                 = P(J)         J = IGH+1,...,N.
!     THE ORDER IN WHICH THE INTERCHANGES ARE MADE IS N TO IGH+1,
!     THEN 1 TO LOW-1.
!
!     NOTE THAT 1 IS RETURNED FOR IGH IF IGH IS ZERO FORMALLY.
!
!     THE ALGOL PROCEDURE EXC CONTAINED IN BALANCE APPEARS IN
!     BALANC  IN LINE.  (NOTE THAT THE ALGOL ROLES OF IDENTIFIERS
!     K,L HAVE BEEN REVERSED.)
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
!     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
!
!     THIS VERSION DATED AUGUST 1983.
!
!     ------------------------------------------------------------------
!
      RADIX = 16.0D0
!
      B2 = RADIX * RADIX
      K = 1
      L = N
      GO TO 100
!     .......... IN-LINE PROCEDURE FOR ROW AND
!                COLUMN EXCHANGE ..........
   20 SCALE(M) = J
      IF (J .EQ. M) GO TO 50
!
      DO 30 I = 1, L
         F = A(I,J)
         A(I,J) = A(I,M)
         A(I,M) = F
   30 CONTINUE
!
      DO 40 I = K, N
         F = A(J,I)
         A(J,I) = A(M,I)
         A(M,I) = F
   40 CONTINUE
!
   50 GO TO (80,130), IEXC
!     .......... SEARCH FOR ROWS ISOLATING AN EIGENVALUE
!                AND PUSH THEM DOWN ..........
   80 IF (L .EQ. 1) GO TO 280
      L = L - 1
!     .......... FOR J=L STEP -1 UNTIL 1 DO -- ..........
  100 DO 120 JJ = 1, L
         J = L + 1 - JJ
!
         DO 110 I = 1, L
            IF (I .EQ. J) GO TO 110
            IF (A(J,I) .NE. 0.0D0) GO TO 120
  110    CONTINUE
!
         M = L
         IEXC = 1
         GO TO 20
  120 CONTINUE
!
      GO TO 140
!     .......... SEARCH FOR COLUMNS ISOLATING AN EIGENVALUE
!                AND PUSH THEM LEFT ..........
  130 K = K + 1
!
  140 DO 170 J = K, L
!
         DO 150 I = K, L
            IF (I .EQ. J) GO TO 150
            IF (A(I,J) .NE. 0.0D0) GO TO 170
  150    CONTINUE
!
         M = K
         IEXC = 2
         GO TO 20
  170 CONTINUE
!     .......... NOW BALANCE THE SUBMATRIX IN ROWS K TO L ..........
      DO 180 I = K, L
  180 SCALE(I) = 1.0D0
!     .......... ITERATIVE LOOP FOR NORM REDUCTION ..........
  190 NOCONV = .FALSE.
!
      DO 270 I = K, L
         C = 0.0D0
         R = 0.0D0
!
         DO 200 J = K, L
            IF (J .EQ. I) GO TO 200
            C = C + DABS(A(J,I))
            R = R + DABS(A(I,J))
  200    CONTINUE
!     .......... GUARD AGAINST ZERO C OR R DUE TO UNDERFLOW ..........
         IF (C .EQ. 0.0D0 .OR. R .EQ. 0.0D0) GO TO 270
         G = R / RADIX
         F = 1.0D0
         S = C + R
  210    IF (C .GE. G) GO TO 220
         F = F * RADIX
         C = C * B2
         GO TO 210
  220    G = R * RADIX
  230    IF (C .LT. G) GO TO 240
         F = F / RADIX
         C = C / B2
         GO TO 230
!     .......... NOW BALANCE ..........
  240    IF ((C + R) / F .GE. 0.95D0 * S) GO TO 270
         G = 1.0D0 / F
         SCALE(I) = SCALE(I) * F
         NOCONV = .TRUE.
!
         DO 250 J = K, N
  250    A(I,J) = A(I,J) * G
!
         DO 260 J = 1, L
  260    A(J,I) = A(J,I) * F
!
  270 CONTINUE
!
      IF (NOCONV) GO TO 190
!
  280 LOW = K
      IGH = L
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE BALBAK(NM,N,LOW,IGH,SCALE,M,Z)
!
      INTEGER I,J,K,M,N,II,NM,IGH,LOW
      DOUBLE PRECISION SCALE(N),Z(NM,M)
      DOUBLE PRECISION S
!
!     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE BALBAK,
!     NUM. MATH. 13, 293-304(1969) BY PARLETT AND REINSCH.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971).
!
!     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A REAL GENERAL
!     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING
!     BALANCED MATRIX DETERMINED BY  BALANC.
!
!     ON INPUT
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        LOW AND IGH ARE INTEGERS DETERMINED BY  BALANC.
!
!        SCALE CONTAINS INFORMATION DETERMINING THE PERMUTATIONS
!          AND SCALING FACTORS USED BY  BALANC.
!
!        M IS THE NUMBER OF COLUMNS OF Z TO BE BACK TRANSFORMED.
!
!        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGEN-
!          VECTORS TO BE BACK TRANSFORMED IN ITS FIRST M COLUMNS.
!
!     ON OUTPUT
!
!        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE
!          TRANSFORMED EIGENVECTORS IN ITS FIRST M COLUMNS.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
!     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
!
!     THIS VERSION DATED AUGUST 1983.
!
!     ------------------------------------------------------------------
!
      IF (M .EQ. 0) GO TO 200
      IF (IGH .EQ. LOW) GO TO 120
!
      DO 110 I = LOW, IGH
         S = SCALE(I)
!     .......... LEFT HAND EIGENVECTORS ARE BACK TRANSFORMED
!                IF THE FOREGOING STATEMENT IS REPLACED BY
!                S=1.0D0/SCALE(I). ..........
         DO 100 J = 1, M
  100    Z(I,J) = Z(I,J) * S
!
  110 CONTINUE
!     ......... FOR I=LOW-1 STEP -1 UNTIL 1,
!               IGH+1 STEP 1 UNTIL N DO -- ..........
  120 DO 140 II = 1, N
         I = II
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 140
         IF (I .LT. LOW) I = LOW - II
         K = SCALE(I)
         IF (K .EQ. I) GO TO 140
!
         DO 130 J = 1, M
            S = Z(I,J)
            Z(I,J) = Z(K,J)
            Z(K,J) = S
  130    CONTINUE
!
  140 CONTINUE
!
  200 RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE ELMHES(NM,N,LOW,IGH,A,INT)
!
      INTEGER I,J,M,N,LA,NM,IGH,KP1,LOW,MM1,MP1
      DOUBLE PRECISION A(NM,N)
      DOUBLE PRECISION X,Y
      INTEGER INT(IGH)
!
!     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ELMHES,
!     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).
!
!     GIVEN A REAL GENERAL MATRIX, THIS SUBROUTINE
!     REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS
!     LOW THROUGH IGH TO UPPER HESSENBERG FORM BY
!     STABILIZED ELEMENTARY SIMILARITY TRANSFORMATIONS.
!
!     ON INPUT
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
!          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,
!          SET LOW=1, IGH=N.
!
!        A CONTAINS THE INPUT MATRIX.
!
!     ON OUTPUT
!
!        A CONTAINS THE HESSENBERG MATRIX.  THE MULTIPLIERS
!          WHICH WERE USED IN THE REDUCTION ARE STORED IN THE
!          REMAINING TRIANGLE UNDER THE HESSENBERG MATRIX.
!
!        INT CONTAINS INFORMATION ON THE ROWS AND COLUMNS
!          INTERCHANGED IN THE REDUCTION.
!          ONLY ELEMENTS LOW THROUGH IGH ARE USED.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
!     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
!
!     THIS VERSION DATED AUGUST 1983.
!
!     ------------------------------------------------------------------
!
      LA = IGH - 1
      KP1 = LOW + 1
      IF (LA .LT. KP1) GO TO 200
!
      DO 180 M = KP1, LA
         MM1 = M - 1
         X = 0.0D0
         I = M
!
         DO 100 J = M, IGH
            IF (DABS(A(J,MM1)) .LE. DABS(X)) GO TO 100
            X = A(J,MM1)
            I = J
  100    CONTINUE
!
         INT(M) = I
         IF (I .EQ. M) GO TO 130
!     .......... INTERCHANGE ROWS AND COLUMNS OF A ..........
         DO 110 J = MM1, N
            Y = A(I,J)
            A(I,J) = A(M,J)
            A(M,J) = Y
  110    CONTINUE
!
         DO 120 J = 1, IGH
            Y = A(J,I)
            A(J,I) = A(J,M)
            A(J,M) = Y
  120    CONTINUE
!     .......... END INTERCHANGE ..........
  130    IF (X .EQ. 0.0D0) GO TO 180
         MP1 = M + 1
!
         DO 160 I = MP1, IGH
            Y = A(I,MM1)
            IF (Y .EQ. 0.0D0) GO TO 160
            Y = Y / X
            A(I,MM1) = Y
!
            DO 140 J = M, N
  140       A(I,J) = A(I,J) - Y * A(M,J)
!
            DO 150 J = 1, IGH
  150       A(J,M) = A(J,M) + Y * A(J,I)
!
  160    CONTINUE
!
  180 CONTINUE
!
  200 RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE ELTRAN(NM,N,LOW,IGH,A,INT,Z)
!
      INTEGER I,J,N,KL,MM,MP,NM,IGH,LOW,MP1
      DOUBLE PRECISION A(NM,IGH),Z(NM,N)
      INTEGER INT(IGH)
!
!     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ELMTRANS,
!     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).
!
!     THIS SUBROUTINE ACCUMULATES THE STABILIZED ELEMENTARY
!     SIMILARITY TRANSFORMATIONS USED IN THE REDUCTION OF A
!     REAL GENERAL MATRIX TO UPPER HESSENBERG FORM BY  ELMHES.
!
!     ON INPUT
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
!          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,
!          SET LOW=1, IGH=N.
!
!        A CONTAINS THE MULTIPLIERS WHICH WERE USED IN THE
!          REDUCTION BY  ELMHES  IN ITS LOWER TRIANGLE
!          BELOW THE SUBDIAGONAL.
!
!        INT CONTAINS INFORMATION ON THE ROWS AND COLUMNS
!          INTERCHANGED IN THE REDUCTION BY  ELMHES.
!          ONLY ELEMENTS LOW THROUGH IGH ARE USED.
!
!     ON OUTPUT
!
!        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE
!          REDUCTION BY  ELMHES.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
!     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
!
!     THIS VERSION DATED AUGUST 1983.
!
!     ------------------------------------------------------------------
!
!     .......... INITIALIZE Z TO IDENTITY MATRIX ..........
      DO 80 J = 1, N
!
         DO 60 I = 1, N
   60    Z(I,J) = 0.0D0
!
         Z(J,J) = 1.0D0
   80 CONTINUE
!
      KL = IGH - LOW - 1
      IF (KL .LT. 1) GO TO 200
!     .......... FOR MP=IGH-1 STEP -1 UNTIL LOW+1 DO -- ..........
      DO 140 MM = 1, KL
         MP = IGH - MM
         MP1 = MP + 1
!
         DO 100 I = MP1, IGH
  100    Z(I,MP) = A(I,MP-1)
!
         I = INT(MP)
         IF (I .EQ. MP) GO TO 140
!
         DO 130 J = MP, IGH
            Z(MP,J) = Z(I,J)
            Z(I,J) = 0.0D0
  130    CONTINUE
!
         Z(I,MP) = 1.0D0
  140 CONTINUE
!
  200 RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  EISPACK routines needed in the computation of Floquet multipliers
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE QZHES(NM,N,A,B,MATZ,Z)
!
      INTEGER I,J,K,L,N,LB,L1,NM,NK1,NM1,NM2
      DOUBLE PRECISION A(NM,N),B(NM,N),Z(NM,N)
      DOUBLE PRECISION R,S,T,U1,U2,V1,V2,RHO
      LOGICAL MATZ
!
!     THIS SUBROUTINE IS THE FIRST STEP OF THE QZ ALGORITHM
!     FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,
!     SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART.
!
!     THIS SUBROUTINE ACCEPTS A PAIR OF REAL GENERAL MATRICES AND
!     REDUCES ONE OF THEM TO UPPER HESSENBERG FORM AND THE OTHER
!     TO UPPER TRIANGULAR FORM USING ORTHOGONAL TRANSFORMATIONS.
!     IT IS USUALLY FOLLOWED BY  QZIT,  QZVAL  AND, POSSIBLY,  QZVEC.
!
!     ON INPUT
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRICES.
!
!        A CONTAINS A REAL GENERAL MATRIX.
!
!        B CONTAINS A REAL GENERAL MATRIX.
!
!        MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS
!          ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING
!          EIGENVECTORS, AND TO .FALSE. OTHERWISE.
!
!     ON OUTPUT
!
!        A HAS BEEN REDUCED TO UPPER HESSENBERG FORM.  THE ELEMENTS
!          BELOW THE FIRST SUBDIAGONAL HAVE BEEN SET TO ZERO.
!
!        B HAS BEEN REDUCED TO UPPER TRIANGULAR FORM.  THE ELEMENTS
!          BELOW THE MAIN DIAGONAL HAVE BEEN SET TO ZERO.
!
!        Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS IF
!          MATZ HAS BEEN SET TO .TRUE.  OTHERWISE, Z IS NOT REFERENCED.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
!     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
!
!     THIS VERSION DATED AUGUST 1983.
!
!     ------------------------------------------------------------------
!
!     .......... INITIALIZE Z ..........
      IF (.NOT. MATZ) GO TO 10
!
      DO 3 J = 1, N
!
         DO 2 I = 1, N
            Z(I,J) = 0.0D0
    2    CONTINUE
!
         Z(J,J) = 1.0D0
    3 CONTINUE
!     .......... REDUCE B TO UPPER TRIANGULAR FORM ..........
   10 IF (N .LE. 1) GO TO 170
      NM1 = N - 1
!
      DO 100 L = 1, NM1
         L1 = L + 1
         S = 0.0D0
!
         DO 20 I = L1, N
            S = S + DABS(B(I,L))
   20    CONTINUE
!
         IF (S .EQ. 0.0D0) GO TO 100
         S = S + DABS(B(L,L))
         R = 0.0D0
!
         DO 25 I = L, N
            B(I,L) = B(I,L) / S
            R = R + B(I,L)**2
   25    CONTINUE
!
         R = DSIGN(DSQRT(R),B(L,L))
         B(L,L) = B(L,L) + R
         RHO = R * B(L,L)
!
         DO 50 J = L1, N
            T = 0.0D0
!
            DO 30 I = L, N
               T = T + B(I,L) * B(I,J)
   30       CONTINUE
!
            T = -T / RHO
!
            DO 40 I = L, N
               B(I,J) = B(I,J) + T * B(I,L)
   40       CONTINUE
!
   50    CONTINUE
!
         DO 80 J = 1, N
            T = 0.0D0
!
            DO 60 I = L, N
               T = T + B(I,L) * A(I,J)
   60       CONTINUE
!
            T = -T / RHO
!
            DO 70 I = L, N
               A(I,J) = A(I,J) + T * B(I,L)
   70       CONTINUE
!
   80    CONTINUE
!
         B(L,L) = -S * R
!
         DO 90 I = L1, N
            B(I,L) = 0.0D0
   90    CONTINUE
!
  100 CONTINUE
!     .......... REDUCE A TO UPPER HESSENBERG FORM, WHILE
!                KEEPING B TRIANGULAR ..........
      IF (N .EQ. 2) GO TO 170
      NM2 = N - 2
!
      DO 160 K = 1, NM2
         NK1 = NM1 - K
!     .......... FOR L=N-1 STEP -1 UNTIL K+1 DO -- ..........
         DO 150 LB = 1, NK1
            L = N - LB
            L1 = L + 1
!     .......... ZERO A(L+1,K) ..........
            S = DABS(A(L,K)) + DABS(A(L1,K))
            IF (S .EQ. 0.0D0) GO TO 150
            U1 = A(L,K) / S
            U2 = A(L1,K) / S
            R = DSIGN(DSQRT(U1*U1+U2*U2),U1)
            V1 =  -(U1 + R) / R
            V2 = -U2 / R
            U2 = V2 / V1
!
            DO 110 J = K, N
               T = A(L,J) + U2 * A(L1,J)
               A(L,J) = A(L,J) + T * V1
               A(L1,J) = A(L1,J) + T * V2
  110       CONTINUE
!
            A(L1,K) = 0.0D0
!
            DO 120 J = L, N
               T = B(L,J) + U2 * B(L1,J)
               B(L,J) = B(L,J) + T * V1
               B(L1,J) = B(L1,J) + T * V2
  120       CONTINUE
!     .......... ZERO B(L+1,L) ..........
            S = DABS(B(L1,L1)) + DABS(B(L1,L))
            IF (S .EQ. 0.0D0) GO TO 150
            U1 = B(L1,L1) / S
            U2 = B(L1,L) / S
            R = DSIGN(DSQRT(U1*U1+U2*U2),U1)
            V1 =  -(U1 + R) / R
            V2 = -U2 / R
            U2 = V2 / V1
!
            DO 130 I = 1, L1
               T = B(I,L1) + U2 * B(I,L)
               B(I,L1) = B(I,L1) + T * V1
               B(I,L) = B(I,L) + T * V2
  130       CONTINUE
!
            B(L1,L) = 0.0D0
!
            DO 140 I = 1, N
               T = A(I,L1) + U2 * A(I,L)
               A(I,L1) = A(I,L1) + T * V1
               A(I,L) = A(I,L) + T * V2
  140       CONTINUE
!
            IF (.NOT. MATZ) GO TO 150
!
            DO 145 I = 1, N
               T = Z(I,L1) + U2 * Z(I,L)
               Z(I,L1) = Z(I,L1) + T * V1
               Z(I,L) = Z(I,L) + T * V2
  145       CONTINUE
!
  150    CONTINUE
!
  160 CONTINUE
!
  170 RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE QZIT(NM,N,A,B,EPS1,MATZ,Z,IERR)
!
      INTEGER I,J,K,L,N,EN,K1,K2,LD,LL,L1,NA,NM,ISH,ITN,ITS,KM1,LM1, &
           ENM2,IERR,LOR1,ENORN
      DOUBLE PRECISION A(NM,N),B(NM,N),Z(NM,N)
      DOUBLE PRECISION R,S,T,A1,A2,A3,EP,SH,U1,U2,U3,V1,V2,V3,ANI,A11, &
           A12,A21,A22,A33,A34,A43,A44,BNI,B11,B12,B22,B33,B34, &
           B44,EPSA,EPSB,EPS1,ANORM,BNORM,EPSLON
      LOGICAL MATZ,NOTLAS
!
!     THIS SUBROUTINE IS THE SECOND STEP OF THE QZ ALGORITHM
!     FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,
!     SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART,
!     AS MODIFIED IN TECHNICAL NOTE NASA TN D-7305(1973) BY WARD.
!
!     THIS SUBROUTINE ACCEPTS A PAIR OF REAL MATRICES, ONE OF THEM
!     IN UPPER HESSENBERG FORM AND THE OTHER IN UPPER TRIANGULAR FORM.
!     IT REDUCES THE HESSENBERG MATRIX TO QUASI-TRIANGULAR FORM USING
!     ORTHOGONAL TRANSFORMATIONS WHILE MAINTAINING THE TRIANGULAR FORM
!     OF THE OTHER MATRIX.  IT IS USUALLY PRECEDED BY  QZHES  AND
!     FOLLOWED BY  QZVAL  AND, POSSIBLY,  QZVEC.
!
!     ON INPUT
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRICES.
!
!        A CONTAINS A REAL UPPER HESSENBERG MATRIX.
!
!        B CONTAINS A REAL UPPER TRIANGULAR MATRIX.
!
!        EPS1 IS A TOLERANCE USED TO DETERMINE NEGLIGIBLE ELEMENTS.
!          EPS1 = 0.0 (OR NEGATIVE) MAY BE INPUT, IN WHICH CASE AN
!          ELEMENT WILL BE NEGLECTED ONLY IF IT IS LESS THAN ROUNDOFF
!          ERROR TIMES THE NORM OF ITS MATRIX.  IF THE INPUT EPS1 IS
!          POSITIVE, THEN AN ELEMENT WILL BE CONSIDERED NEGLIGIBLE
!          IF IT IS LESS THAN EPS1 TIMES THE NORM OF ITS MATRIX.  A
!          POSITIVE VALUE OF EPS1 MAY RESULT IN FASTER EXECUTION,
!          BUT LESS ACCURATE RESULTS.
!
!        MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS
!          ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING
!          EIGENVECTORS, AND TO .FALSE. OTHERWISE.
!
!        Z CONTAINS, IF MATZ HAS BEEN SET TO .TRUE., THE
!          TRANSFORMATION MATRIX PRODUCED IN THE REDUCTION
!          BY  QZHES, IF PERFORMED, OR ELSE THE IDENTITY MATRIX.
!          IF MATZ HAS BEEN SET TO .FALSE., Z IS NOT REFERENCED.
!
!     ON OUTPUT
!
!        A HAS BEEN REDUCED TO QUASI-TRIANGULAR FORM.  THE ELEMENTS
!          BELOW THE FIRST SUBDIAGONAL ARE STILL ZERO AND NO TWO
!          CONSECUTIVE SUBDIAGONAL ELEMENTS ARE NONZERO.
!
!        B IS STILL IN UPPER TRIANGULAR FORM, ALTHOUGH ITS ELEMENTS
!          HAVE BEEN ALTERED.  THE LOCATION B(N,1) IS USED TO STORE
!          EPS1 TIMES THE NORM OF B FOR LATER USE BY  QZVAL  AND  QZVEC.
!
!        Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS
!          (FOR BOTH STEPS) IF MATZ HAS BEEN SET TO .TRUE..
!
!        IERR IS SET TO
!          ZERO       FOR NORMAL RETURN,
!          J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED
!                     WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
!     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
!
!     THIS VERSION DATED AUGUST 1983.
!
!     ------------------------------------------------------------------
!
      IERR = 0
!     .......... COMPUTE EPSA,EPSB ..........
      ANORM = 0.0D0
      BNORM = 0.0D0
!
      DO 30 I = 1, N
         ANI = 0.0D0
         IF (I .NE. 1) ANI = DABS(A(I,I-1))
         BNI = 0.0D0
!
         DO 20 J = I, N
            ANI = ANI + DABS(A(I,J))
            BNI = BNI + DABS(B(I,J))
   20    CONTINUE
!
         IF (ANI .GT. ANORM) ANORM = ANI
         IF (BNI .GT. BNORM) BNORM = BNI
   30 CONTINUE
!
      IF (ANORM .EQ. 0.0D0) ANORM = 1.0D0
      IF (BNORM .EQ. 0.0D0) BNORM = 1.0D0
      EP = EPS1
      IF (EP .GT. 0.0D0) GO TO 50
!     .......... USE ROUNDOFF LEVEL IF EPS1 IS ZERO ..........
      EP = EPSLON(1.0D0)
   50 EPSA = EP * ANORM
      EPSB = EP * BNORM
!     .......... REDUCE A TO QUASI-TRIANGULAR FORM, WHILE
!                KEEPING B TRIANGULAR ..........
      LOR1 = 1
      ENORN = N
      EN = N
      ITN = 30*N
!     .......... BEGIN QZ STEP ..........
   60 IF (EN .LE. 2) GO TO 1001
      IF (.NOT. MATZ) ENORN = EN
      ITS = 0
      NA = EN - 1
      ENM2 = NA - 1
   70 ISH = 2
!     .......... CHECK FOR CONVERGENCE OR REDUCIBILITY.
!                FOR L=EN STEP -1 UNTIL 1 DO -- ..........
      DO 80 LL = 1, EN
         LM1 = EN - LL
         L = LM1 + 1
         IF (L .EQ. 1) GO TO 95
         IF (DABS(A(L,LM1)) .LE. EPSA) GO TO 90
   80 CONTINUE
!
   90 A(L,LM1) = 0.0D0
      IF (L .LT. NA) GO TO 95
!     .......... 1-BY-1 OR 2-BY-2 BLOCK ISOLATED ..........
      EN = LM1
      GO TO 60
!     .......... CHECK FOR SMALL TOP OF B ..........
   95 LD = L
  100 L1 = L + 1
      B11 = B(L,L)
      IF (DABS(B11) .GT. EPSB) GO TO 120
      B(L,L) = 0.0D0
      S = DABS(A(L,L)) + DABS(A(L1,L))
      U1 = A(L,L) / S
      U2 = A(L1,L) / S
      R = DSIGN(DSQRT(U1*U1+U2*U2),U1)
      V1 = -(U1 + R) / R
      V2 = -U2 / R
      U2 = V2 / V1
!
      DO 110 J = L, ENORN
         T = A(L,J) + U2 * A(L1,J)
         A(L,J) = A(L,J) + T * V1
         A(L1,J) = A(L1,J) + T * V2
         T = B(L,J) + U2 * B(L1,J)
         B(L,J) = B(L,J) + T * V1
         B(L1,J) = B(L1,J) + T * V2
  110 CONTINUE
!
      IF (L .NE. 1) A(L,LM1) = -A(L,LM1)
      LM1 = L
      L = L1
      GO TO 90
  120 A11 = A(L,L) / B11
      A21 = A(L1,L) / B11
      IF (ISH .EQ. 1) GO TO 140
!     .......... ITERATION STRATEGY ..........
      IF (ITN .EQ. 0) GO TO 1000
      IF (ITS .EQ. 10) GO TO 155
!     .......... DETERMINE TYPE OF SHIFT ..........
      B22 = B(L1,L1)
      IF (DABS(B22) .LT. EPSB) B22 = EPSB
      B33 = B(NA,NA)
      IF (DABS(B33) .LT. EPSB) B33 = EPSB
      B44 = B(EN,EN)
      IF (DABS(B44) .LT. EPSB) B44 = EPSB
      A33 = A(NA,NA) / B33
      A34 = A(NA,EN) / B44
      A43 = A(EN,NA) / B33
      A44 = A(EN,EN) / B44
      B34 = B(NA,EN) / B44
      T = 0.5D0 * (A43 * B34 - A33 - A44)
      R = T * T + A34 * A43 - A33 * A44
      IF (R .LT. 0.0D0) GO TO 150
!     .......... DETERMINE SINGLE SHIFT ZEROTH COLUMN OF A ..........
      ISH = 1
      R = DSQRT(R)
      SH = -T + R
      S = -T - R
      IF (DABS(S-A44) .LT. DABS(SH-A44)) SH = S
!     .......... LOOK FOR TWO CONSECUTIVE SMALL
!                SUB-DIAGONAL ELEMENTS OF A.
!                FOR L=EN-2 STEP -1 UNTIL LD DO -- ..........
      DO 130 LL = LD, ENM2
         L = ENM2 + LD - LL
         IF (L .EQ. LD) GO TO 140
         LM1 = L - 1
         L1 = L + 1
         T = A(L,L)
         IF (DABS(B(L,L)) .GT. EPSB) T = T - SH * B(L,L)
         IF (DABS(A(L,LM1)) .LE. DABS(T/A(L1,L)) * EPSA) GO TO 100
  130 CONTINUE
!
  140 A1 = A11 - SH
      A2 = A21
      IF (L .NE. LD) A(L,LM1) = -A(L,LM1)
      GO TO 160
!     .......... DETERMINE DOUBLE SHIFT ZEROTH COLUMN OF A ..........
  150 A12 = A(L,L1) / B22
      A22 = A(L1,L1) / B22
      B12 = B(L,L1) / B22
      A1 = ((A33 - A11) * (A44 - A11) - A34 * A43 + A43 * B34 * A11) &
          / A21 + A12 - A11 * B12
      A2 = (A22 - A11) - A21 * B12 - (A33 - A11) - (A44 - A11) &
           + A43 * B34
      A3 = A(L1+1,L1) / B22
      GO TO 160
!     .......... AD HOC SHIFT ..........
  155 A1 = 0.0D0
      A2 = 1.0D0
      A3 = 1.1605D0
  160 ITS = ITS + 1
      ITN = ITN - 1
      IF (.NOT. MATZ) LOR1 = LD
!     .......... MAIN LOOP ..........
      DO 260 K = L, NA
         NOTLAS = K .NE. NA .AND. ISH .EQ. 2
         K1 = K + 1
         K2 = K + 2
         KM1 = MAX0(K-1,L)
         LL = MIN0(EN,K1+ISH)
         IF (NOTLAS) GO TO 190
!     .......... ZERO A(K+1,K-1) ..........
         IF (K .EQ. L) GO TO 170
         A1 = A(K,KM1)
         A2 = A(K1,KM1)
  170    S = DABS(A1) + DABS(A2)
         IF (S .EQ. 0.0D0) GO TO 70
         U1 = A1 / S
         U2 = A2 / S
         R = DSIGN(DSQRT(U1*U1+U2*U2),U1)
         V1 = -(U1 + R) / R
         V2 = -U2 / R
         U2 = V2 / V1
!
         DO 180 J = KM1, ENORN
            T = A(K,J) + U2 * A(K1,J)
            A(K,J) = A(K,J) + T * V1
            A(K1,J) = A(K1,J) + T * V2
            T = B(K,J) + U2 * B(K1,J)
            B(K,J) = B(K,J) + T * V1
            B(K1,J) = B(K1,J) + T * V2
  180    CONTINUE
!
         IF (K .NE. L) A(K1,KM1) = 0.0D0
         GO TO 240
!     .......... ZERO A(K+1,K-1) AND A(K+2,K-1) ..........
  190    IF (K .EQ. L) GO TO 200
         A1 = A(K,KM1)
         A2 = A(K1,KM1)
         A3 = A(K2,KM1)
  200    S = DABS(A1) + DABS(A2) + DABS(A3)
         IF (S .EQ. 0.0D0) GO TO 260
         U1 = A1 / S
         U2 = A2 / S
         U3 = A3 / S
         R = DSIGN(DSQRT(U1*U1+U2*U2+U3*U3),U1)
         V1 = -(U1 + R) / R
         V2 = -U2 / R
         V3 = -U3 / R
         U2 = V2 / V1
         U3 = V3 / V1
!
         DO 210 J = KM1, ENORN
            T = A(K,J) + U2 * A(K1,J) + U3 * A(K2,J)
            A(K,J) = A(K,J) + T * V1
            A(K1,J) = A(K1,J) + T * V2
            A(K2,J) = A(K2,J) + T * V3
            T = B(K,J) + U2 * B(K1,J) + U3 * B(K2,J)
            B(K,J) = B(K,J) + T * V1
            B(K1,J) = B(K1,J) + T * V2
            B(K2,J) = B(K2,J) + T * V3
  210    CONTINUE
!
         IF (K .EQ. L) GO TO 220
         A(K1,KM1) = 0.0D0
         A(K2,KM1) = 0.0D0
!     .......... ZERO B(K+2,K+1) AND B(K+2,K) ..........
  220    S = DABS(B(K2,K2)) + DABS(B(K2,K1)) + DABS(B(K2,K))
         IF (S .EQ. 0.0D0) GO TO 240
         U1 = B(K2,K2) / S
         U2 = B(K2,K1) / S
         U3 = B(K2,K) / S
         R = DSIGN(DSQRT(U1*U1+U2*U2+U3*U3),U1)
         V1 = -(U1 + R) / R
         V2 = -U2 / R
         V3 = -U3 / R
         U2 = V2 / V1
         U3 = V3 / V1
!
         DO 230 I = LOR1, LL
            T = A(I,K2) + U2 * A(I,K1) + U3 * A(I,K)
            A(I,K2) = A(I,K2) + T * V1
            A(I,K1) = A(I,K1) + T * V2
            A(I,K) = A(I,K) + T * V3
            T = B(I,K2) + U2 * B(I,K1) + U3 * B(I,K)
            B(I,K2) = B(I,K2) + T * V1
            B(I,K1) = B(I,K1) + T * V2
            B(I,K) = B(I,K) + T * V3
  230    CONTINUE
!
         B(K2,K) = 0.0D0
         B(K2,K1) = 0.0D0
         IF (.NOT. MATZ) GO TO 240
!
         DO 235 I = 1, N
            T = Z(I,K2) + U2 * Z(I,K1) + U3 * Z(I,K)
            Z(I,K2) = Z(I,K2) + T * V1
            Z(I,K1) = Z(I,K1) + T * V2
            Z(I,K) = Z(I,K) + T * V3
  235    CONTINUE
!     .......... ZERO B(K+1,K) ..........
  240    S = DABS(B(K1,K1)) + DABS(B(K1,K))
         IF (S .EQ. 0.0D0) GO TO 260
         U1 = B(K1,K1) / S
         U2 = B(K1,K) / S
         R = DSIGN(DSQRT(U1*U1+U2*U2),U1)
         V1 = -(U1 + R) / R
         V2 = -U2 / R
         U2 = V2 / V1
!
         DO 250 I = LOR1, LL
            T = A(I,K1) + U2 * A(I,K)
            A(I,K1) = A(I,K1) + T * V1
            A(I,K) = A(I,K) + T * V2
            T = B(I,K1) + U2 * B(I,K)
            B(I,K1) = B(I,K1) + T * V1
            B(I,K) = B(I,K) + T * V2
  250    CONTINUE
!
         B(K1,K) = 0.0D0
         IF (.NOT. MATZ) GO TO 260
!
         DO 255 I = 1, N
            T = Z(I,K1) + U2 * Z(I,K)
            Z(I,K1) = Z(I,K1) + T * V1
            Z(I,K) = Z(I,K) + T * V2
  255    CONTINUE
!
  260 CONTINUE
!     .......... END QZ STEP ..........
      GO TO 70
!     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT
!                CONVERGED AFTER 30*N ITERATIONS ..........
 1000 IERR = EN
!     .......... SAVE EPSB FOR USE BY QZVAL AND QZVEC ..........
 1001 IF (N .GT. 1) B(N,1) = EPSB
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE QZVAL(NM,N,A,B,ALFR,ALFI,BETA,MATZ,Z)
!
      INTEGER I,J,N,EN,NA,NM,NN,ISW
      DOUBLE PRECISION A(NM,N),B(NM,N),ALFR(N),ALFI(N),BETA(N),Z(NM,N)
      DOUBLE PRECISION C,D,E,R,S,T,AN,A1,A2,BN,CQ,CZ,DI,DR,EI,TI,TR,U1, &
           U2,V1,V2,A1I,A11,A12,A2I,A21,A22,B11,B12,B22,SQI,SQR, &
           SSI,SSR,SZI,SZR,A11I,A11R,A12I,A12R,A22I,A22R,EPSB
      LOGICAL MATZ
!
!     THIS SUBROUTINE IS THE THIRD STEP OF THE QZ ALGORITHM
!     FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,
!     SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART.
!
!     THIS SUBROUTINE ACCEPTS A PAIR OF REAL MATRICES, ONE OF THEM
!     IN QUASI-TRIANGULAR FORM AND THE OTHER IN UPPER TRIANGULAR FORM.
!     IT REDUCES THE QUASI-TRIANGULAR MATRIX FURTHER, SO THAT ANY
!     REMAINING 2-BY-2 BLOCKS CORRESPOND TO PAIRS OF COMPLEX
!     EIGENVALUES, AND RETURNS QUANTITIES WHOSE RATIOS GIVE THE
!     GENERALIZED EIGENVALUES.  IT IS USUALLY PRECEDED BY  QZHES
!     AND  QZIT  AND MAY BE FOLLOWED BY  QZVEC.
!
!     ON INPUT
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRICES.
!
!        A CONTAINS A REAL UPPER QUASI-TRIANGULAR MATRIX.
!
!        B CONTAINS A REAL UPPER TRIANGULAR MATRIX.  IN ADDITION,
!          LOCATION B(N,1) CONTAINS THE TOLERANCE QUANTITY (EPSB)
!          COMPUTED AND SAVED IN  QZIT.
!
!        MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS
!          ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING
!          EIGENVECTORS, AND TO .FALSE. OTHERWISE.
!
!        Z CONTAINS, IF MATZ HAS BEEN SET TO .TRUE., THE
!          TRANSFORMATION MATRIX PRODUCED IN THE REDUCTIONS BY QZHES
!          AND QZIT, IF PERFORMED, OR ELSE THE IDENTITY MATRIX.
!          IF MATZ HAS BEEN SET TO .FALSE., Z IS NOT REFERENCED.
!
!     ON OUTPUT
!
!        A HAS BEEN REDUCED FURTHER TO A QUASI-TRIANGULAR MATRIX
!          IN WHICH ALL NONZERO SUBDIAGONAL ELEMENTS CORRESPOND TO
!          PAIRS OF COMPLEX EIGENVALUES.
!
!        B IS STILL IN UPPER TRIANGULAR FORM, ALTHOUGH ITS ELEMENTS
!          HAVE BEEN ALTERED.  B(N,1) IS UNALTERED.
!
!        ALFR AND ALFI CONTAIN THE REAL AND IMAGINARY PARTS OF THE
!          DIAGONAL ELEMENTS OF THE TRIANGULAR MATRIX THAT WOULD BE
!          OBTAINED IF A WERE REDUCED COMPLETELY TO TRIANGULAR FORM
!          BY UNITARY TRANSFORMATIONS.  NON-ZERO VALUES OF ALFI OCCUR
!          IN PAIRS, THE FIRST MEMBER POSITIVE AND THE SECOND NEGATIVE.
!
!        BETA CONTAINS THE DIAGONAL ELEMENTS OF THE CORRESPONDING B,
!          NORMALIZED TO BE REAL AND NON-NEGATIVE.  THE GENERALIZED
!          EIGENVALUES ARE THEN THE RATIOS ((ALFR+I*ALFI)/BETA).
!
!        Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS
!          (FOR ALL THREE STEPS) IF MATZ HAS BEEN SET TO .TRUE.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
!     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
!
!     THIS VERSION DATED AUGUST 1983.
!
!     ------------------------------------------------------------------
!
      EPSB = B(N,1)
      ISW = 1
!     .......... FIND EIGENVALUES OF QUASI-TRIANGULAR MATRICES.
!                FOR EN=N STEP -1 UNTIL 1 DO -- ..........
      DO 510 NN = 1, N
         EN = N + 1 - NN
         NA = EN - 1
         IF (ISW .EQ. 2) GO TO 505
         IF (EN .EQ. 1) GO TO 410
         IF (A(EN,NA) .NE. 0.0D0) GO TO 420
!     .......... 1-BY-1 BLOCK, ONE REAL ROOT ..........
  410    ALFR(EN) = A(EN,EN)
         IF (B(EN,EN) .LT. 0.0D0) ALFR(EN) = -ALFR(EN)
         BETA(EN) = DABS(B(EN,EN))
         ALFI(EN) = 0.0D0
         GO TO 510
!     .......... 2-BY-2 BLOCK ..........
  420    IF (DABS(B(NA,NA)) .LE. EPSB) GO TO 455
         IF (DABS(B(EN,EN)) .GT. EPSB) GO TO 430
         A1 = A(EN,EN)
         A2 = A(EN,NA)
         BN = 0.0D0
         GO TO 435
  430    AN = DABS(A(NA,NA)) + DABS(A(NA,EN)) + DABS(A(EN,NA)) &
              + DABS(A(EN,EN))
         BN = DABS(B(NA,NA)) + DABS(B(NA,EN)) + DABS(B(EN,EN))
         A11 = A(NA,NA) / AN
         A12 = A(NA,EN) / AN
         A21 = A(EN,NA) / AN
         A22 = A(EN,EN) / AN
         B11 = B(NA,NA) / BN
         B12 = B(NA,EN) / BN
         B22 = B(EN,EN) / BN
         E = A11 / B11
         EI = A22 / B22
         S = A21 / (B11 * B22)
         T = (A22 - E * B22) / B22
         IF (DABS(E) .LE. DABS(EI)) GO TO 431
         E = EI
         T = (A11 - E * B11) / B11
  431    C = 0.5D0 * (T - S * B12)
         D = C * C + S * (A12 - E * B12)
         IF (D .LT. 0.0D0) GO TO 480
!     .......... TWO REAL ROOTS.
!                ZERO BOTH A(EN,NA) AND B(EN,NA) ..........
         E = E + (C + DSIGN(DSQRT(D),C))
         A11 = A11 - E * B11
         A12 = A12 - E * B12
         A22 = A22 - E * B22
         IF (DABS(A11) + DABS(A12) .LT. &
              DABS(A21) + DABS(A22)) GO TO 432
         A1 = A12
         A2 = A11
         GO TO 435
  432    A1 = A22
         A2 = A21
!     .......... CHOOSE AND APPLY REAL Z ..........
  435    S = DABS(A1) + DABS(A2)
         U1 = A1 / S
         U2 = A2 / S
         R = DSIGN(DSQRT(U1*U1+U2*U2),U1)
         V1 = -(U1 + R) / R
         V2 = -U2 / R
         U2 = V2 / V1
!
         DO 440 I = 1, EN
            T = A(I,EN) + U2 * A(I,NA)
            A(I,EN) = A(I,EN) + T * V1
            A(I,NA) = A(I,NA) + T * V2
            T = B(I,EN) + U2 * B(I,NA)
            B(I,EN) = B(I,EN) + T * V1
            B(I,NA) = B(I,NA) + T * V2
  440    CONTINUE
!
         IF (.NOT. MATZ) GO TO 450
!
         DO 445 I = 1, N
            T = Z(I,EN) + U2 * Z(I,NA)
            Z(I,EN) = Z(I,EN) + T * V1
            Z(I,NA) = Z(I,NA) + T * V2
  445    CONTINUE
!
  450    IF (BN .EQ. 0.0D0) GO TO 475
         IF (AN .LT. DABS(E) * BN) GO TO 455
         A1 = B(NA,NA)
         A2 = B(EN,NA)
         GO TO 460
  455    A1 = A(NA,NA)
         A2 = A(EN,NA)
!     .......... CHOOSE AND APPLY REAL Q ..........
  460    S = DABS(A1) + DABS(A2)
         IF (S .EQ. 0.0D0) GO TO 475
         U1 = A1 / S
         U2 = A2 / S
         R = DSIGN(DSQRT(U1*U1+U2*U2),U1)
         V1 = -(U1 + R) / R
         V2 = -U2 / R
         U2 = V2 / V1
!
         DO 470 J = NA, N
            T = A(NA,J) + U2 * A(EN,J)
            A(NA,J) = A(NA,J) + T * V1
            A(EN,J) = A(EN,J) + T * V2
            T = B(NA,J) + U2 * B(EN,J)
            B(NA,J) = B(NA,J) + T * V1
            B(EN,J) = B(EN,J) + T * V2
  470    CONTINUE
!
  475    A(EN,NA) = 0.0D0
         B(EN,NA) = 0.0D0
         ALFR(NA) = A(NA,NA)
         ALFR(EN) = A(EN,EN)
         IF (B(NA,NA) .LT. 0.0D0) ALFR(NA) = -ALFR(NA)
         IF (B(EN,EN) .LT. 0.0D0) ALFR(EN) = -ALFR(EN)
         BETA(NA) = DABS(B(NA,NA))
         BETA(EN) = DABS(B(EN,EN))
         ALFI(EN) = 0.0D0
         ALFI(NA) = 0.0D0
         GO TO 505
!     .......... TWO COMPLEX ROOTS ..........
  480    E = E + C
         EI = DSQRT(-D)
         A11R = A11 - E * B11
         A11I = EI * B11
         A12R = A12 - E * B12
         A12I = EI * B12
         A22R = A22 - E * B22
         A22I = EI * B22
         IF (DABS(A11R) + DABS(A11I) + DABS(A12R) + DABS(A12I) .LT. &
              DABS(A21) + DABS(A22R) + DABS(A22I)) GO TO 482
         A1 = A12R
         A1I = A12I
         A2 = -A11R
         A2I = -A11I
         GO TO 485
  482    A1 = A22R
         A1I = A22I
         A2 = -A21
         A2I = 0.0D0
!     .......... CHOOSE COMPLEX Z ..........
  485    CZ = DSQRT(A1*A1+A1I*A1I)
         IF (CZ .EQ. 0.0D0) GO TO 487
         SZR = (A1 * A2 + A1I * A2I) / CZ
         SZI = (A1 * A2I - A1I * A2) / CZ
         R = DSQRT(CZ*CZ+SZR*SZR+SZI*SZI)
         CZ = CZ / R
         SZR = SZR / R
         SZI = SZI / R
         GO TO 490
  487    SZR = 1.0D0
         SZI = 0.0D0
  490    IF (AN .LT. (DABS(E) + EI) * BN) GO TO 492
         A1 = CZ * B11 + SZR * B12
         A1I = SZI * B12
         A2 = SZR * B22
         A2I = SZI * B22
         GO TO 495
  492    A1 = CZ * A11 + SZR * A12
         A1I = SZI * A12
         A2 = CZ * A21 + SZR * A22
         A2I = SZI * A22
!     .......... CHOOSE COMPLEX Q ..........
  495    CQ = DSQRT(A1*A1+A1I*A1I)
         IF (CQ .EQ. 0.0D0) GO TO 497
         SQR = (A1 * A2 + A1I * A2I) / CQ
         SQI = (A1 * A2I - A1I * A2) / CQ
         R = DSQRT(CQ*CQ+SQR*SQR+SQI*SQI)
         CQ = CQ / R
         SQR = SQR / R
         SQI = SQI / R
         GO TO 500
  497    SQR = 1.0D0
         SQI = 0.0D0
!     .......... COMPUTE DIAGONAL ELEMENTS THAT WOULD RESULT
!                IF TRANSFORMATIONS WERE APPLIED ..........
  500    SSR = SQR * SZR + SQI * SZI
         SSI = SQR * SZI - SQI * SZR
         I = 1
         TR = CQ * CZ * A11 + CQ * SZR * A12 + SQR * CZ * A21 &
              + SSR * A22
         TI = CQ * SZI * A12 - SQI * CZ * A21 + SSI * A22
         DR = CQ * CZ * B11 + CQ * SZR * B12 + SSR * B22
         DI = CQ * SZI * B12 + SSI * B22
         GO TO 503
  502    I = 2
         TR = SSR * A11 - SQR * CZ * A12 - CQ * SZR * A21 &
              + CQ * CZ * A22
         TI = -SSI * A11 - SQI * CZ * A12 + CQ * SZI * A21
         DR = SSR * B11 - SQR * CZ * B12 + CQ * CZ * B22
         DI = -SSI * B11 - SQI * CZ * B12
  503    T = TI * DR - TR * DI
         J = NA
         IF (T .LT. 0.0D0) J = EN
         R = DSQRT(DR*DR+DI*DI)
         BETA(J) = BN * R
         ALFR(J) = AN * (TR * DR + TI * DI) / R
         ALFI(J) = AN * T / R
         IF (I .EQ. 1) GO TO 502
  505    ISW = 3 - ISW
  510 CONTINUE
      B(N,1) = EPSB
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION EPSLON (X)
      DOUBLE PRECISION X
!
!     ESTIMATE UNIT ROUNDOFF IN QUANTITIES OF SIZE X.
!
      DOUBLE PRECISION A,B,C,EPS
!
!     THIS PROGRAM SHOULD FUNCTION PROPERLY ON ALL SYSTEMS
!     SATISFYING THE FOLLOWING TWO ASSUMPTIONS,
!        1.  THE BASE USED IN REPRESENTING FLOATING POINT
!            NUMBERS IS NOT A POWER OF THREE.
!        2.  THE QUANTITY  A  IN STATEMENT 10 IS REPRESENTED TO
!            THE ACCURACY USED IN FLOATING POINT VARIABLES
!            THAT ARE STORED IN MEMORY.
!     THE STATEMENT NUMBER 10 AND THE GO TO 10 ARE INTENDED TO
!     FORCE OPTIMIZING COMPILERS TO GENERATE CODE SATISFYING
!     ASSUMPTION 2.
!     UNDER THESE ASSUMPTIONS, IT SHOULD BE TRUE THAT,
!            A  IS NOT EXACTLY EQUAL TO FOUR-THIRDS,
!            B  HAS A ZERO FOR ITS LAST BIT OR DIGIT,
!            C  IS NOT EXACTLY EQUAL TO ONE,
!            EPS  MEASURES THE SEPARATION OF 1.0 FROM
!                 THE NEXT LARGER FLOATING POINT NUMBER.
!     THE DEVELOPERS OF EISPACK WOULD APPRECIATE BEING INFORMED
!     ABOUT ANY SYSTEMS WHERE THESE ASSUMPTIONS DO NOT HOLD.
!
!     THIS VERSION DATED 4/6/83.
!
      A = 4.0D0/3.0D0
   10 B = A - 1.0D0
      C = B + B + B
      EPS = DABS(C-1.0D0)
      IF (EPS .EQ. 0.0D0) GO TO 10
      EPSLON = EPS*DABS(X)
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! Demmel-Kahan SVD routines needed for computing the Floquet multipliers
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE ezsvd(x,ldx,n,p,s,e,u,ldu,v,ldv,work,job,info,tol)
!
      integer ldx,n,p,ldu,ldv,job,info,skip
      double precision x(ldx,1),s(1),e(1),u(ldu,1),v(ldv,1),work(1)
      double precision maxsin,tol
      integer idbg, ifull, iidir, maxitr, limshf, kount, &
           kount1, kount2
!
!     new svd by J. Demmel, W. Kahan
!     finds singular values of bidiagonal matrices with guaranteed high
!     relative precision
!
!     easy to use version of ndsvd ("hard to use" version, below)
!     with defaults for some ndsvd parameters
!
!     all parameters same as linpack dsvdc except for tol:
!
!     tol  = if positive, desired relative precision in singular values
!            if negative, desired absolute precision in singular values
!               (expressed as abs(tol) * sigma-max)
!            (in both cases, abs(tol) should be less than 1 and 
!             greater than macheps)
!
!        I have tested this software on a SUN 3 in double precision
!        IEEE arithmetic with macheps about 2.2e-16 and tol=1e-14; 
!        In general I recommend tol 10-100 times larger than macheps.
!
!        On the average it appears to be as fast or faster than dsvdc.
!        I have seen it go 3.5 times faster and 2 times slower at the 
!        extremes.
!
!     defaults for ndsvd parameters (see ndsvd for more description of
!     these parameters) are:
!
!     set to no debug output
      idbg = 0
!     use zero-shift normally
      ifull = 0     
!     use normal bidiagonalization code
      skip = 0     
!     choose chase direction normally
      iidir = 0
!     maximum 30 QR sweeps per singular value
      maxitr = 30
!
      call ndsvd(x,ldx,n,p,s,e,u,ldu,v,ldv,work,job,info,maxitr, &
           tol,idbg,ifull,kount,kount1,kount2,skip,limshf, &
           maxsin,iidir)
      return
      end
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE ndrotg(f,g,cs,sn)
!     faster version of drotg, except g unchanged on return
!     cs, sn returned so that -sn*f+cs*g = 0
!     and returned f = cs*f + sn*g
!    
!     if g=0, then cs=1 and sn=0 (in case svd adds extra zero row
!         to bidiagonal, this makes sure last row rotation is trivial)
!
!     if f=0 and g.ne.0, then cs=0 and sn=1 without floating point work
!         (in case s(i)=0 in svd so that bidiagonal deflates, this
!          computes rotation without any floating point operations)
!
      double precision f, g, cs, sn, t, tt
      if (f.eq.0.0d0) then
        if (g.eq.0.0d0) then
!         this case needed in case extra zero row added in svd, so
!         bottom rotation always trivial
          cs = 1.0d0
          sn = 0.0d0
        else
!         this case needed for s(i)=0 in svd to compute rotation
!         cheaply
          cs = 0.0d0
          sn = 1.0d0
          f = g
        endif
      else
        if (abs(f) .gt. abs(g)) then
          t = g/f
          tt = dsqrt(1.0d0 + t*t )
          cs = 1.0d0/tt
          sn = t*cs
          f = f*tt
        else
          t = f/g
          tt = dsqrt(1.0d0 + t*t )
          sn = 1.0d0/tt
          cs = t*sn
          f = g*tt
        endif
      endif
      return
      end
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE ndsvd(x,ldx,n,p,s,e,u,ldu,v,ldv,work,job,info,maxitr, &
           tol,idbg,ifull,kount,kount1,kount2,skip,limshf, &
           maxsin,iidir)
!
!     LINPACK SVD modified by:
!     James Demmel                      W. Kahan
!     Courant Institute                 Computer Science Dept.
!     demmel@acf8.nyu.edu               U.C. Berkeley
!
!     modified version designed to guarantee relative accuracy of
!     all singular values of intermediate bidiagonal form
!
!     extra input/output parameters in addition to those from LINPACK SVD:
!
!     extra input paramters:
!
!     tol  = if positive, desired relative precision in singular values
!            if negative, desired absolute precision in singular values
!               (expressed as abs(tol) * sigma-max)
!            (abs(tol) should be less than 1 and greater than macheps)
!
!     idbg = 0 for no debug output (normal setting)
!          = 1 convergence, shift decisions (written to standard output)
!          = 2 for above plus before, after qr
!
!     ifull= 0 if decision to use zero-shift set normally (normal setting)
!          = 1 if always set to nonzero-shift
!          = 2 if always set to zero-shift
!
!     skip =-1 means standard code but do all work of bidiagonalization
!              (even if input bidiagonal)
!            0 means standard code (normal setting)
!            1 means assume x is bidiagonal, and skip bidiagonalization
!              entirely
!          (skip used for timing tests)
!
!     iidir = 0 if idir (chase direction) chosen normally
!             1 if idir=1 (chase top to bottom) always
!             2 if idir=2 (chase bottom to top) always
!
!     extra output parameters:
!
!     kount =number of qr sweeps taken
!
!     kount1=number of passes through inner loop of full qr
!
!     kount2=number of passes through inner loop of zero-shift qr
!
!     limshf = number of times the shift was greater than its threshold
!              (nct*smin) and had to be decreased
!
!     maxsin = maximum sin in inner loop of zero-shift
!
      integer ldx,n,p,ldu,ldv,job,info,skip
      integer idbg, ifull, iidir, maxitr, limshf, kount, &
           kount1, kount2
      double precision x(ldx,1),s(1),e(1),u(ldu,1),v(ldv,1),work(1)
      double precision maxsin
!
!     new version designed to be robust with respect to over/underflow
!     have fast inner loop when shift is zero,
!     guarantee relative accuracy of all singular values
!
!     dsvdc is a subroutine to reduce a double precision nxp matrix x
!     by orthogonal transformations u and v to diagonal form.  the
!     diagonal elements s(i) are the singular values of x.  the
!     columns of u are the corresponding left singular vectors,
!     and the columns of v the right singular vectors.
!
!     on entry
!
!         x         double precision(ldx,p), where ldx.ge.n.
!                   x contains the matrix whose singular value
!                   decomposition is to be computed.  x is
!                   destroyed by dsvdc.
!
!         ldx       integer.
!                   ldx is the leading dimension of the array x.
!
!         n         integer.
!                   n is the number of rows of the matrix x.
!
!         p         integer.
!                   p is the number of columns of the matrix x.
!
!         ldu       integer.
!                   ldu is the leading dimension of the array u.
!                   (see below).
!
!         ldv       integer.
!                   ldv is the leading dimension of the array v.
!                   (see below).
!
!         work      double precision(n).
!                   work is a scratch array.
!
!         job       integer.
!                   job controls the computation of the singular
!                   vectors.  it has the decimal expansion ab
!                   with the following meaning
!
!                        a.eq.0    do not compute the left singular
!                                  vectors.
!                        a.eq.1    return the n left singular vectors
!                                  in u.
!                        a.ge.2    return the first min(n,p) singular
!                                  vectors in u.
!                        b.eq.0    do not compute the right singular
!                                  vectors.
!                        b.eq.1    return the right singular vectors
!                                  in v.
!
!     on return
!
!         s         double precision(mm), where mm=min(n+1,p).
!                   the first min(n,p) entries of s contain the
!                   singular values of x arranged in descending
!                   order of magnitude.
!
!         e         double precision(p), 
!                   e ordinarily contains zeros.  however see the
!                   discussion of info for exceptions.
!
!         u         double precision(ldu,k), where ldu.ge.n.  if
!                                   joba.eq.1 then k.eq.n, if joba.ge.2
!                                   then k.eq.min(n,p).
!                   u contains the matrix of left singular vectors.
!                   u is not referenced if joba.eq.0.  if n.le.p
!                   or if joba.eq.2, then u may be identified with x
!                   in the subroutine call.
!
!         v         double precision(ldv,p), where ldv.ge.p.
!                   v contains the matrix of right singular vectors.
!                   v is not referenced if job.eq.0.  if p.le.n,
!                   then v may be identified with x in the
!                   subroutine call.
!
!         info      integer.
!                   the singular values (and their corresponding
!                   singular vectors) s(info+1),s(info+2),...,s(m)
!                   are correct (here m=min(n,p)).  thus if
!                   info.eq.0, all the singular values and their
!                   vectors are correct.  in any event, the matrix
!                   b = trans(u)*x*v is the bidiagonal matrix
!                   with the elements of s on its diagonal and the
!                   elements of e on its super-diagonal (trans(u)
!                   is the transpose of u).  thus the singular
!                   values of x and b are the same.
!
!     linpack. this version dated 08/14/78 .
!              correction made to shift 2/84.
!     g.w. stewart, university of maryland, argonne national lab.
!
!     dsvdc uses the following functions and subprograms.
!
!     external drot
!     blas daxpy,ddot,dscal,dswap,dnrm2,drotg
!     fortran dabs,dmax1,max0,min0,mod,dsqrt,dsign
!     prse,ndrotg,sig22,sndrtg,sigmin
!
!     internal variables
!
      integer i,iter,j,jobu,l,ll,lm1,lp1,lu,m,maxit, &
           nct,nctp1,ncu,nrt,nrtp1
      double precision ddot,t
      double precision cs,f,g,dnrm2,shift,sn, &
           test
      logical wantu,wantv
!
!     new variables
      double precision tol,smin,smax,abss,abse,temp,sigmin,sminl,gg
      double precision smm1,sm,emm1,sll,oldcs,oldsn,thresh
!     double precision sg1,sg2
      double precision sigmn,sigmx,sinr,cosr,sinl,cosl
      double precision mu,lambda
      integer oldll,oldm,oldacc, minnp, mm, lll, idir, k, iisub
!
!
!     set the maximum number of iterations.
!
!     maxit = 30
      kount=0
      kount1=0
      kount2=0
      limshf=0
      maxsin=0.
!
!     determine what is to be computed.
!
      wantu = .false.
      wantv = .false.
      jobu = mod(job,100)/10
      ncu = n
      if (jobu .gt. 1) ncu = min0(n,p)
      if (jobu .ne. 0) wantu = .true.
      if (mod(job,10) .ne. 0) wantv = .true.
!
!     reduce x to bidiagonal form, storing the diagonal elements
!     in s and the super-diagonal elements in e.
!
      info = 0
      nct = min0(n-1,p)
      nrt = max0(0,min0(p-2,n))
      lu = max0(nct,nrt)
      if (skip.le.0) then
      if (lu .lt. 1) go to 170
      do 160 l = 1, lu
         lp1 = l + 1
         if (l .gt. nct) go to 20
!
!           compute the transformation for the l-th column and
!           place the l-th diagonal in s(l).
!
            s(l) = dnrm2(n-l+1,x(l,l),1)
            if (s(l) .eq. 0.0d0 .and. skip.eq.0) go to 10
               if (x(l,l) .ne. 0.0d0) s(l) = dsign(s(l),x(l,l))
               call dscal(n-l+1,1.0d0/s(l),x(l,l),1)
               x(l,l) = 1.0d0 + x(l,l)
   10       continue
            s(l) = -s(l)
   20    continue
         if (p .lt. lp1) go to 50
         do 40 j = lp1, p
            if (l .gt. nct) go to 30
            if (s(l) .eq. 0.0d0 .and. skip.eq.0) go to 30
!
!              apply the transformation.
!
               t = -ddot(n-l+1,x(l,l),1,x(l,j),1)/x(l,l)
               call daxpy(n-l+1,t,x(l,l),1,x(l,j),1)
   30       continue
!
!           place the l-th row of x into  e for the
!           subsequent calculation of the row transformation.
!
            e(j) = x(l,j)
   40    continue
   50    continue
         if (.not.wantu .or. l .gt. nct) go to 70
!
!           place the transformation in u for subsequent back
!           multiplication.
!
            do 60 i = l, n
               u(i,l) = x(i,l)
   60       continue
   70    continue
         if (l .gt. nrt) go to 150
!
!           compute the l-th row transformation and place the
!           l-th super-diagonal in e(l).
!
            e(l) = dnrm2(p-l,e(lp1),1)
            if (e(l) .eq. 0.0d0 .and. skip.eq.0) go to 80
               if (e(lp1) .ne. 0.0d0) e(l) = dsign(e(l),e(lp1))
               call dscal(p-l,1.0d0/e(l),e(lp1),1)
               e(lp1) = 1.0d0 + e(lp1)
   80       continue
            e(l) = -e(l)
            if (lp1 .gt. n .or. (e(l) .eq. 0.0d0 .and. skip.eq.0)) &
                 go to 120
!
!              apply the transformation.
!
               do 90 i = lp1, n
                  work(i) = 0.0d0
   90          continue
               do 100 j = lp1, p
                  call daxpy(n-l,e(j),x(lp1,j),1,work(lp1),1)
  100          continue
               do 110 j = lp1, p
                  call daxpy(n-l,-e(j)/e(lp1),work(lp1),1,x(lp1,j),1)
  110          continue
  120       continue
            if (.not.wantv) go to 140
!
!              place the transformation in v for subsequent
!              back multiplication.
!
               do 130 i = lp1, p
                  v(i,l) = e(i)
  130          continue
  140       continue
  150    continue
  160 continue
  170 continue
      endif
!
!     set up the final bidiagonal matrix or order m.
!
      m = min0(p,n+1)
      nctp1 = nct + 1
      nrtp1 = nrt + 1
      if (skip.le.0) then
      if (nct .lt. p) s(nctp1) = x(nctp1,nctp1)
      if (n .lt. m) s(m) = 0.0d0
      if (nrtp1 .lt. m) e(nrtp1) = x(nrtp1,m)
      e(m) = 0.0d0
!
!     if required, generate u.
!
      if (.not.wantu) go to 300
         if (ncu .lt. nctp1) go to 200
         do 190 j = nctp1, ncu
            do 180 i = 1, n
               u(i,j) = 0.0d0
  180       continue
            u(j,j) = 1.0d0
  190    continue
  200    continue
         if (nct .lt. 1) go to 290
         do 280 ll = 1, nct
            l = nct - ll + 1
            if (s(l) .eq. 0.0d0) go to 250
               lp1 = l + 1
               if (ncu .lt. lp1) go to 220
               do 210 j = lp1, ncu
                  t = -ddot(n-l+1,u(l,l),1,u(l,j),1)/u(l,l)
                  call daxpy(n-l+1,t,u(l,l),1,u(l,j),1)
  210          continue
  220          continue
               call dscal(n-l+1,-1.0d0,u(l,l),1)
               u(l,l) = 1.0d0 + u(l,l)
               lm1 = l - 1
               if (lm1 .lt. 1) go to 240
               do 230 i = 1, lm1
                  u(i,l) = 0.0d0
  230          continue
  240          continue
            go to 270
  250       continue
               do 260 i = 1, n
                  u(i,l) = 0.0d0
  260          continue
               u(l,l) = 1.0d0
  270       continue
  280    continue
  290    continue
  300 continue
!
!     if it is required, generate v.
!
      if (.not.wantv) go to 350
         do 340 ll = 1, p
            l = p - ll + 1
            lp1 = l + 1
            if (l .gt. nrt) go to 320
            if (e(l) .eq. 0.0d0) go to 320
               do 310 j = lp1, p
                  t = -ddot(p-l,v(lp1,l),1,v(lp1,j),1)/v(lp1,l)
                  call daxpy(p-l,t,v(lp1,l),1,v(lp1,j),1)
  310          continue
  320       continue
            do 330 i = 1, p
               v(i,l) = 0.0d0
  330       continue
            v(l,l) = 1.0d0
  340    continue
  350 continue
      endif
!
!
      if (skip.eq.1) then
!       set up s,e,u,v assuming x bidiagonal on input
        minnp=min(n,p)
        do 351 i=1,minnp
          s(i)=x(i,i)
          if (i.lt.p) e(i)=x(i,i+1)
351     continue
        if (n.lt.p) s(n+1)=0.
        e(m)=0
        if (wantu) then
          do 352 j=1,ncu
            do 353 i=1,n
              u(i,j)=0.
353         continue
            u(j,j)=1.d0
352       continue
        endif
        if (wantv) then
          do 354 j=1,p
            do 355 i=1,p
              v(i,j)=0.
355         continue
            v(j,j)=1.d0
354       continue
        endif
      endif
!
!     main iteration loop for the singular values.
!
!     convert maxit to bound on total number of passes through
!     inner loops of qr iteration (half number of rotations)
      maxit = maxitr*m*m/2
      iter=0
      oldll = -1
      oldm = -1
      oldacc = -1
      if (tol.gt.0.0) then
!       relative accuracy desired
        thresh = 0.0d0
      else
!       absolute accuracy desired
        smax = abs(s(m))
        do 1111 i=1,m-1
          smax = max(smax,abs(s(i)),abs(e(i)))
1111    continue
        thresh = abs(tol)*smax
      endif
      mm=m
!
!     begin loop
999   continue
      if (idbg.gt.0) then
        print *,'top of loop'
        print *,'oldll,oldm,oldacc,m,iter,maxit,ifull,thresh=', &
             oldll,oldm,oldacc,m,iter,maxit,ifull,thresh
        call prse(1,mm,n,p,s,e)
      endif
!
!     check for being done
      if (m.eq.1) goto 998
!
!     check number of iterations
      if (iter.ge.maxit) goto 997
!
!     compute minimum s(i) and max of all s(i),e(i)
      if (tol.le.0. .and. abs(s(m)).le.thresh) s(m) = 0.0
      smax = abs(s(m))
      smin = smax
!
!     reset convergence threshold if starting new part of matrix
      if (m.le.oldll .and. tol.gt.0.0) thresh = 0.0d0
      if (idbg.gt.0) print *,'thresh=',thresh
      
      do 1001 lll=1,m
        ll=m-lll
        if (ll.eq.0) goto 1003
        if (tol.le.0. .and. abs(s(ll)).le.thresh) s(ll) = 0.0
        if (abs(e(ll)).le.thresh) goto 1002
        abss = abs(s(ll))
        abse = abs(e(ll))
        smin = min(smin,abss)
        smax = max(smax,abss,abse)
1001  continue
1002  continue
      e(ll) = 0.0d0
!
!     matrix splits since e(ll)=0
      if (ll.eq.m-1) then
!       convergence of bottom singular values
        m=m-1
        if (idbg.gt.0) print *, 'convergence'
        goto 999
      endif
1003  continue
      ll=ll+1
!     e(ll) ... e(m-1) are nonzero
      if (idbg.gt.0) then
        print *,'work on block ll,m=',ll,m
        print *,'smin=',smin
        print *,'smax=',smax
      endif
!
!     2 by 2 block - handle specially to guarantee convergence
      if (ll.eq.m-1) then
!       after one step
        kount1 = kount1 +1
!       shift = sigmin(s(m-1),e(m-1),s(m))
!       rotate, setting e(m-1)=0 and s(m)=+-shift
!       if (s(ll).eq.0.0d0) then
!         f = 0.0d0
!       else
!         f = (abs(s(ll)) - shift)*(dsign(1.0d0,s(ll))+shift/s(ll))
!       endif
!       g=e(ll)
!       call ndrotg(f,g,cs,sn)
!       sg1=dsign(1.0d0,s(m))
!       sg2=dsign(1.0d0,cs)
!       f = cs*s(ll) + sn*e(ll)
!       g = sn*s(m)
!       if (idbg.gt.0) then
!         abss = cs*s(m)
!         abse = -sn*s(ll) + cs*e(ll)
!       endif
!       if (wantv) call drot(p,v(1,ll),1,v(1,m),1,cs,sn)
!       call ndrotg(f,g,cs,sn)
!       s(ll)=f
!       if (wantu.and.ll.lt.n) call drot(n,u(1,ll),1,u(1,m),1,cs,sn)
!       e(ll) = 0.0d0
!       s(m) = shift * dsign(1.0d0,cs) * sg1 * sg2
!       if (idbg.gt.0) then
!         print *,'2 by 2 block'
!         print *,'shift=',shift
!         print *,'check shift=',-sn*abse+cs*abss
!         print *,'check zero=',cs*abse+sn*abss
!       endif
        call sig22(s(m-1),e(m-1),s(m),sigmn,sigmx,sinr,cosr,sinl,cosl)
        s(m-1)=sigmx
        e(m-1)=0.
        s(m)=sigmn
        if (wantv) call drot(p,v(1,ll),1,v(1,m),1,cosr,sinr)
!       if wantu and ll.eq.n, then rotation trivial
        if (wantu.and.ll.lt.n) call drot(n,u(1,ll),1,u(1,m),1,cosl,sinl)
        goto 999
      endif
!
!     choose shift direction if new submatrix
!     if (ll.ne.oldll .or. m.ne.oldm) then
!     choose shift direction if working on entirely new submatrix
      if (ll.gt.oldm .or. m.lt.oldll) then
        if ((abs(s(ll)).ge.abs(s(m)) .and. iidir.eq.0) .or. iidir.eq.1) &
             then
!         chase bulge from top (big end) to bottom (small end)
!         if m=n+1, chase from top to bottom even if s(ll)=0
          idir=1
        else
!         chase bulge from bottom (big end) to top (small end)
          idir=2
        endif
      endif
      if (idbg.gt.0) print *,'idir=',idir
!
!     compute lower bound on smallest singular value
!     if old lower bound still good, do not recompute it
!     if (ll.ne.oldll .or. m.ne.oldm .or. oldacc.ne.1) then
!       compute lower bound
!       sminl = smin
!       oldacc = 1
!       if (sminl.gt.0.0d0) then
!         if (idir.eq.1) then
!           do 1004 lll=ll,m-1
!             abse = abs(e(lll))
!             abss = abs(s(lll))
!             if (abss.lt.abse) then
!               sminl = sminl * (abss/abse)
!               oldacc = -1
!             endif
1004        continue
!         else
!           do 1005 lll=ll,m-1
!             abse = abs(e(lll))
!             abss = abs(s(lll+1))
!             if (abss.lt.abse) then
!               sminl = sminl * (abss/abse)
!               oldacc = -1
!             endif
1005        continue
!         endif
!       endif
!       oldll = ll
!       oldm = m
!       sminl is lower bound on smallest singular value
!       within a factor of sqrt(m*(m+1)/2)
!       if oldacc = 1 as well, sminl is also upper bound
!       note that smin is always an upper bound
!
!       compute convergence threshold
!       thresh = tol*sminl
!     endif
!     if (idbg.gt.0) then
!       print *,'oldll,oldm,oldacc=',oldll,oldm,oldacc
!       print *,'sminl=',sminl
!       print *,'thresh=',thresh
!     endif
!
!     test again for convergence using new thresh
!     iconv = 0
!     do 1014 lll=ll,m-1
!       if (dabs(e(lll)).le.thresh) then
!         e(lll) = 0.0d0
!         iconv = 1
!       endif
1014  continue
!     if (iconv.eq.1) goto 999
!   
!     Kahan's convergence test
      sminl = 0.
      if (tol.gt.0.0) then
        if (idir.eq.1) then
!         forward direction
!         apply test on bottom 2 by 2 only
          if (dabs(e(m-1)).le.tol*dabs(s(m))) then
!           convergence of bottom element
            e(m-1) = 0
            goto 999
          endif
!         apply test in forward direction
          mu = dabs(s(ll))
          sminl = mu
          do 3330 lll=ll,m-1
            if (dabs(e(lll)).le.tol*mu) then
!             test for negligibility satisfied
              if (idbg.ge.1) print *,'knew: e(lll),mu=',e(lll),mu
              e(lll) = 0
              goto 999
            else
              mu = dabs(s(lll+1))*(mu/(mu+dabs(e(lll))))
            endif
            sminl = min(sminl,mu)
3330      continue
        else
!         idir=2,  backwards direction
!         apply test on top 2 by 2 only
          if (dabs(e(ll)).le.tol*dabs(s(ll))) then
!           convergence of top element
            e(ll) = 0
            goto 999
          endif
!         apply test in backward direction
          lambda = dabs(s(m))
          sminl = lambda
          do 3331 lll=m-1,ll,-1
            if (dabs(e(lll)).le.tol*lambda) then
!             test for negligibility satisfied
              if (idbg.ge.1) print *,'knew: e(lll),lambda=',e(lll), &
                   lambda
              e(lll) = 0
              goto 999
            else
              lambda = dabs(s(lll))*(lambda/(lambda+dabs(e(lll))))
            endif
            sminl = min(sminl,lambda)
3331      continue
        endif
        thresh = tol*sminl
!       thresh = 0
      endif
      oldll = ll
      oldm = m
!
!     test for zero shift
      test = 1.0d0 + nct*tol*(sminl/smax)
      if ((test.eq.1.0d0 .and. ifull.ne.1 .and. tol.gt.0.0) &
           .or. ifull.eq.2) then
!       do a zero shift so that roundoff does not contaminate
!       smallest singular value
        shift = 0.0d0
        if (idbg.gt.0) print *,'sminl test for shift is zero'
      else
!     
!       compute shift from 2 by 2 block at end of matrix
        if (idir.eq.1) then
          smm1 = s(m-1)
          emm1 = e(m-1)
          sm = s(m)
          sll = s(ll)
        else
          smm1 = s(ll+1)
          emm1 = e(ll)
          sm = s(ll)
          sll = s(m)
        endif
        if (idbg.gt.0) print *,'smm1,emm1,sm=',smm1,emm1,sm
        shift = sigmin(smm1,emm1,sm)
        if (idbg.gt.0) print *,'sigma-min of 2 by 2 corner=',shift
        if (tol.gt.0.0) then
          if (shift.gt.nct*smin) then
            limshf = limshf+1
            shift = nct*smin
            if (idbg.gt.0) print *,'shift limited'
          endif
          if (idbg.gt.0) print *,'shift=',shift
          temp = shift/sll
          if (idbg.gt.0) print *,'temp=',temp
          test = 1.0d0 - temp**2
!         test to see if shift negligible
          if (ifull.ne.1 .and. test.eq.1.0d0) shift = 0.0d0
        else
!         if shift much larger than s(ll), first rotation could be 0,
!         leading to infinite loop; avoid by doing 0 shift in this case
          if (shift.gt.abs(s(ll))) then
            test = s(ll)/shift
            if (1.0d0 + test .eq. 1.0d0) then
              limshf = limshf+1
              if (ifull.ne.1) shift = 0.
              if (idbg.gt.0 .and. ifull.ne.1) print *,'shift limited'
            endif
          endif
          test = smax+smin
          if (test.eq.smax .and. ifull.ne.1) shift = 0.
        endif
        if (idbg.gt.0) print *,'test,shift=',test,shift
      endif
!
!     increment iteration counter
      iter = iter + m-ll
      kount = kount + 1
      if (idbg.gt.1) then
        print *,'s,e before qr'
        call prse(ll,m,n,p,s,e)
      endif
!
!     if shift = 0, do simplified qr iteration
      if (shift.eq.0.0d0) then
        kount2 = kount2 + m-ll
!
!       if idir=1, chase bulge from top to bottom
        if (idir.eq.1) then
          if (idbg.gt.2) print *,'qr with zero shift, top to bottom'
          oldcs=1.0d0
          f=s(ll)
          g=e(ll)
          do 1006 k=ll,m-1
!           if (idbg.gt.2) print *,'qr inner loop, k=',k
!           if (idbg.gt.3) print *,'f,g=',f,g
            call ndrotg(f,g,cs,sn)
            maxsin=max(maxsin,abs(sn))
!           if (idbg.gt.3) print *,'f,cs,sn=',f,cs,sn
            if (wantv) call drot(p,v(1,k),1,v(1,k+1),1,cs,sn)
            if (k.ne.ll) e(k-1) = oldsn*f
!           if (k.ne.ll .and. idbg.gt.3) print *,'e(k-1)=',e(k-1)
            f = oldcs*f
!           if (idbg.gt.3) print *,'f=',f
            temp = s(k+1)
!           if (idbg.gt.3) print *,'temp=',temp
            g = temp*sn
!           if (idbg.gt.3) print *,'g=',g
            gg = temp*cs
!           if (idbg.gt.3) print *,'gg=',gg
            call ndrotg(f,g,cs,sn)
            maxsin=max(maxsin,abs(sn))
!           if (idbg.gt.3) print *,'f,cs,sn=',f,cs,sn
!           if wantu and k.eq.n, then s(k+1)=0 so g=0 so cs=1 and sn=0
            if (wantu .and. k .lt. n) &
                 call drot(n,u(1,k),1,u(1,k+1),1,cs,sn)
            s(k) = f
!           if (idbg.gt.3) print *,'s(k)=',s(k)
            f = gg
!           if (idbg.gt.3) print *,'f=',f
            g = e(k+1)
!           if (idbg.gt.3) print *,'g=',g
            oldcs = cs
!           if (idbg.gt.3) print *,'oldcs=',oldcs
            oldsn = sn
!           if (idbg.gt.3) print *,'oldsn=',oldsn
!           if (idbg.gt.2) call prse(ll,m,n,p,s,e)
1006      continue
          e(m-1) = gg*sn
!         if (idbg.gt.3) print *,'e(m-1)=',e(m-1)
          s(m) = gg*cs
!         if (idbg.gt.3) print *,'s(m)=',s(m)
!
!         test convergence
          if (idbg.gt.0) then
            print *,'convergence decision for zero shift top to bottom'
            print *,'e(m-1), threshold=',e(m-1),thresh
            if (abs(e(m-1)).le.thresh) print *,'***converged***'
          endif
          if (abs(e(m-1)).le.thresh) e(m-1) = 0.0d0
        else
!       (idir=2, so chase bulge from bottom to top)
          if (idbg.gt.2) print *,'qr with zero shift, bottom to top'
          oldcs=1.0d0
          f = s(m)
          g = e(m-1)
          do 1007 k=m,ll+1,-1
!           if (idbg.gt.2) print *,'qr inner loop, k=',k
!           if (idbg.gt.3) print *,'f,g=',f,g
            call ndrotg(f,g,cs,sn)
            maxsin=max(maxsin,abs(sn))
!           if (idbg.gt.3) print *,'f,cs,sn=',f,cs,sn
!           if m=n+1, always chase from top to bottom so no test for
!           k.lt.n necessary
            if (wantu) call drot(n,u(1,k-1),1,u(1,k),1,cs,-sn)
            if (k.ne.m) e(k) = oldsn*f
!           if (k.ne.m .and. idbg.gt.3) print *,'e(k)=',e(k)
            f = oldcs*f
!           if (idbg.gt.3) print *,'f=',f
            temp = s(k-1)
!           if (idbg.gt.3) print *,'temp=',temp
            g = sn*temp
!           if (idbg.gt.3) print *,'g=',g
            gg = cs*temp
!           if (idbg.gt.3) print *,'gg=',gg
            call ndrotg(f,g,cs,sn)
            maxsin=max(maxsin,abs(sn))
!           if (idbg.gt.3) print *,'f,cs,sn=',f,cs,sn
            if (wantv) call drot(p,v(1,k-1),1,v(1,k),1,cs,-sn)
            s(k) = f
!           if (idbg.gt.3) print *,'s(k)=',s(k)
            f = gg
!           if (idbg.gt.3) print *,'f=',f
            if (k.ne.ll+1) g = e(k-2)
!           if (k.ne.ll+1 .and. idbg.gt.3) print *,'g=',g
            oldcs = cs
!           if (idbg.gt.3) print *,'oldcs=',oldcs
            oldsn = sn
!           if (idbg.gt.3) print *,'oldsn=',oldsn
!           if (idbg.gt.2) call prse(ll,m,n,p,s,e)
1007      continue
          e(ll) = gg*sn
!         if (idbg.gt.3) print *,'e(ll)=',e(ll)
          s(ll) = gg*cs
!         if (idbg.gt.3) print *,'s(ll)=',s(ll)
!
!         test convergence
          if (idbg.gt.0) then
            print *,'convergence decision for zero shift bottom to top'
            print *,'e(ll), threshold=',e(ll),thresh
            if (abs(e(ll)).le.thresh) print *,'***converged***'
          endif
          if (abs(e(ll)).le.thresh) e(ll) = 0.0d0
        endif
      else
!     (shift.ne.0, so do standard qr iteration)
        kount1 = kount1 + m-ll
!
!       if idir=1, chase bulge from top to bottom
        if (idir.eq.1) then
          if (idbg.gt.2) print *,'qr with nonzero shift, top to bottom'
          f = (abs(s(ll)) - shift)*(dsign(1.0d0,s(ll))+shift/s(ll))
          g = e(ll)
          do 1008 k = ll, m-1
!           if (idbg.gt.2) print *,'qr inner loop, k=',k
!           if (idbg.gt.3) print *,'f,g=',f,g
            call ndrotg(f,g,cs,sn)
!           if (idbg.gt.3) print *,'f,cs,sn=',f,cs,sn
            if (k .ne. ll) e(k-1) = f
!           if (k.ne.ll .and. idbg.gt.3) print *,'e(k-1)=',e(k-1)
            f = cs*s(k) + sn*e(k)
!           if (idbg.gt.3) print *,'f=',f
            e(k) = cs*e(k) - sn*s(k)
!           if (idbg.gt.3) print *,'e(k)=',e(k)
            g = sn*s(k+1)
!           if (idbg.gt.3) print *,'g=',g
            s(k+1) = cs*s(k+1)
!           if (idbg.gt.3) print *,'s(k+1)=',s(k+1)
            if (wantv) call drot(p,v(1,k),1,v(1,k+1),1,cs,sn)
            call ndrotg(f,g,cs,sn)
!           if (idbg.gt.3) print *,'f,cs,sn=',f,cs,sn
            s(k) = f
!           if (idbg.gt.3) print *,'s(k)=',s(k)
            f = cs*e(k) + sn*s(k+1)
!           if (idbg.gt.3) print *,'f=',f
            s(k+1) = -sn*e(k) + cs*s(k+1)
!           if (idbg.gt.3) print *,'s(k+1)=',s(k+1)
            g = sn*e(k+1)
!           if (idbg.gt.3) print *,'g=',g
            e(k+1) = cs*e(k+1)
!           if (idbg.gt.3) print *,'e(k+1)=',e(k+1)
!           test for k.lt.n seems unnecessary since k=n causes zero 
!           shift, so test removed from original code
            if (wantu) call drot(n,u(1,k),1,u(1,k+1),1,cs,sn)
!           if (idbg.gt.2) call prse(ll,m,n,p,s,e)
1008      continue
          e(m-1) = f
!         if (idbg.gt.3) print *,'e(m-1)=',e(m-1)
!
!         check convergence
          if (idbg.gt.0) then
            print *,'convergence decision for shift top to bottom'
            print *,'e(m-1), threshold=',e(m-1),thresh
            if (abs(e(m-1)).le.thresh) print *,'***converged***'
          endif
          if (abs(e(m-1)).le.thresh) e(m-1) = 0.0d0
        else
!       (idir=2, so chase bulge from bottom to top)
          if (idbg.gt.2) print *,'qr with nonzero shift, bottom to top'
          f = (abs(s(m))-shift)*(dsign(1.0d0,s(m))+shift/s(m))
          g = e(m-1)
          do 1009 k=m,ll+1,-1
!           if (idbg.gt.2) print *,'qr inner loop, k=',k
!           if (idbg.gt.3) print *,'f,g=',f,g
            call ndrotg(f,g,cs,sn)
!           if (idbg.gt.3) print *,'f,cs,sn=',f,cs,sn
            if (k.ne.m) e(k)=f
!           if (k.ne.m .and. idbg.gt.3) print *,'e(k)=',e(k)
            f = cs*s(k) + sn*e(k-1)
!           if (idbg.gt.3) print *,'f=',f
            e(k-1) = -sn*s(k) + cs*e(k-1)
!           if (idbg.gt.3) print *,'e(k-1)=',e(k-1)
            g = sn*s(k-1)
!           if (idbg.gt.3) print *,'g=',g
            s(k-1) = cs*s(k-1)
!           if (idbg.gt.3) print *,'s(k-1)=',s(k-1)
            if (wantu .and. k .le. n) &
                 call drot(n,u(1,k-1),1,u(1,k),1,cs,-sn)
            call ndrotg(f,g,cs,sn)
!           if (idbg.gt.3) print *,'f,cs,sn=',f,cs,sn
            if (wantv) call drot(p,v(1,k-1),1,v(1,k),1,cs,-sn)
            s(k) = f
!           if (idbg.gt.3) print *,'s(k)=',s(k)
            f = sn*s(k-1) + cs*e(k-1)
!           if (idbg.gt.3) print *,'f=',f
            s(k-1) = cs*s(k-1) - sn*e(k-1)
!           if (idbg.gt.3) print *,'s(k-1)=',s(k-1)
            if (k.ne.ll+1) then
              g = sn*e(k-2)
!             if (idbg.gt.3) print *,'g=',g
              e(k-2) = cs*e(k-2)
!             if (idbg.gt.3) print *,'e(k-2)=',e(k-2)
            endif
!           if (idbg.gt.2) call prse(ll,m,n,p,s,e)
1009      continue
          e(ll) = f
!         if (idbg.gt.3) print *,'e(ll)=',e(ll)
!
!         test convergence
          if (idbg.gt.0) then
            print *,'convergence decision for shift bottom to top'
            print *,'e(ll), threshold=',e(ll),thresh
            if (abs(e(ll)).le.thresh) print *,'***converged***'
          endif
          if (abs(e(ll)).le.thresh) e(ll) = 0.0d0
        endif
      endif
!
      if (idbg.gt.1) then
        print *,'s,e after qr'
        call prse(ll,m,n,p,s,e)
      endif
!
!     qr iteration finished, go back to check convergence
      goto 999
!
998   continue
!
!     make singular values positive
      m=min(n,p)
      do 1010 i=1,m
        if (s(i).lt.0.0d0) then
          s(i)=-s(i)
          if (wantv) call dscal(p,-1.0d0,v(1,i),1)
        endif
1010  continue
!
!     sort singular values from largest at top to smallest
!     at bottom (use insertion sort)
      do 1011 i=1,m-1
!       scan for smallest s(j)
        iisub = 1
        smin = s(1)
        do 1012 j=2,m+1-i
          if (s(j).lt.smin) then
            iisub = j
            smin = s(j)
          endif
1012    continue
        if (iisub.ne.m+1-i) then
!         swap singular values, vectors
          temp = s(m+1-i)
          s(m+1-i) = s(iisub)
          s(iisub) = temp
          if (wantv) call dswap(p,v(1,m+1-i),1,v(1,iisub),1)
          if (wantu) call dswap(n,u(1,m+1-i),1,u(1,iisub),1)
        endif
1011  continue
!
!     finished, return
      return
!
997   continue
!     maximum number of iterations exceeded
      do 1013 i=m-1,1,-1
        info=i+1
        if (e(i).ne.0.0d0) goto 996
1013  continue
996   continue
      return
      end
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE prse(ll,m,nrow,ncol,s,e)
!     debug routine to print s,e
      double precision s(1),e(1)
      print 100,ll,m
100   format(22x,'s(.)',22x,'e(.) for ll,m=',2i3)
      do 1 i=ll,m-1
        print 101,s(i),e(i)
101     format(2d26.17)
1     continue
      if (m.ge.ncol) print 101,s(m)
      if (m.lt.ncol) print 101,s(m),e(m)
      return
      end
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE sig22(a,b,c,sigmin,sigmax,snr,csr,snl,csl)
!     compute the singular value decomposition of the 2 by 2
!     upper triangular matrix [[a,b];[0,c]]
!     inputs -
!       a,b,c - real*8 - matrix entries
!     outputs -
!       sigmin - real*8 - +-smaller singular value
!       sigmax - real*8 - +-larger singular value
!       snr, csr - real*8 - sin and cos of right rotation (see below)
!       snl, csl - real*8 - sin and cos of left rotation (see below)
!
!       [  csl  snl ]  * [ a b ] * [ csr  -snr ] = [ sigmax    0   ]
!       [ -snl  csl ]    [ 0 c ]   [ snr   csr ]   [    0   sigmin ]
!
!     barring over/underflow all output quantities are correct to 
!     within a few units in their last places
!    
!     let UF denote the underflow and OF the overflow threshold
!     let eps denote the machine precision
!
!     overflow is impossible unless the true value of sigmax exceeds 
!     OF (or does so within a few units in its last place)
!
!     underflow cannot adversely effect sigmin,sigmax unless they are 
!     less than UF/eps for conventional underflow
!     underflow cannot adversely effect sigmin,sigmax if underflow is 
!     gradual and results normalized
!
!     overflow is impossible in computing sinr, cosr, sinl, cosl
!     underflow can adversely effect results only if sigmax<UF/eps
!     or true angle of rotation < UF/eps
!
!     note: if c=0, then csl=1. and snl=0. (needed in general svd)
!
      real*8 a,b,c,sigmin,sigmax,snr,csr,snl,csl
      real*8 sinr,cosr,sinl,cosl
!   
!     local variables:
      real*8 absa,absb,absc,acmn,acmx,as,at,au,temp,temp1,temp2,temp3
      real*8 ac,ca,bac,absbac,sgnmn,sgnmx,sgna,sgnb,sgnc
!
      absa = dabs(a)
      absb = dabs(b)
      absc = dabs(c)
      sgna = dsign(1.0d0,a)
      sgnb = dsign(1.0d0,b)
      sgnc = dsign(1.0d0,c)
      acmn = min(absa,absc)
      acmx = max(absa,absc)
!     bad underflow possible if acmx<UF/eps and standard underflow
!     underflow impossible if underflow gradual
!     either at=0 or eps/2 <= at <= 1
!     if no or gradual underflow, at nearly correctly rounded
      at = (acmx-acmn)
      if (at .ne. 0.0) at = at/acmx
!
!     compute sigmin, sigmax
!
      if ( absb .lt. acmx ) then
!         abs(bac) <= 1, underflow possible
          if (absa .lt. absc) then
              bac = b/c
          else
              bac = b/a
          endif
!         1 <= as <= 2, underflow and roundoff harmless
          as = 1.0d0 + acmn/acmx
!         0 <= au <= 1, underflow possible
          au = bac*bac
!         1 <= temp1 <= sqrt(5), underflow, roundoff harmless
          temp1 = dsqrt(as*as+au)
!         0 <= temp2 <= 1, possible harmful underflow from at
          temp2 = dsqrt(at*at+au)
!         1 <= temp <= sqrt(5) + sqrt(2)
          temp = temp1 + temp2
          sigmin = acmn/temp
          sigmin = sigmin+sigmin
          sigmax = acmx*(temp/2.)
      else
          if (absb.eq.0.0) then
!             matrix identically zero
              sigmin = 0.
              sigmax = 0.
          else
!             0 <= au <= 1, underflow possible
              au = acmx/absb
              if (au.eq.0) then
!                 either au=0 exactly or underflows
!                 sigmin only underflows if true value should
!                 overflow on product acmn*acmx impossible
                  sigmin = (acmx*acmn)/absb
                  sigmax = absb
              else
!                 1 <= as <= 2, underflow and roundoff harmless
                  as = 1.0d0 + acmn/acmx
!                 2 <= temp <= sqrt(5)+sqrt(2), possible harmful 
!                 underflow from at
                  temp = dsqrt(1.0d0+(as*au)**2)+dsqrt(1.0d0+(at*au)**2)
!                 0 < sigmin <= 2
                  sigmin = au+au
!                 bad underflow possible only if true sigmin near UF
                  sigmin = sigmin*(acmn/temp)
                  sigmax = absb*(temp/2.0d0)
              endif
          endif
      endif
!
!     compute rotations
!
      if (absb .le. acmx) then
          if (at .eq. 0.0d0) then
!             assume as = 2, since otherwise underflow will have
!             contaminated at so much that we get a bad answer
!             anyway; this can only happen if sigmax < UF/eps
!             with conventional underflow; this !annot happen
!             with gradual underflow
              if (absb .gt. 0.0d0) then
!                 0 <= absbac <= 1
                  absbac = absb/acmx
!                 1 <= temp3 <= 1+sqrt(5), underflow harmless
                  temp3 = absbac + dsqrt(4.0d0+au)
!                 1/3 <= temp3 <= (1+sqrt(10))/2
                  temp3 = temp3/(2.0d0 + absbac*temp3)
                  sinr = dsign(1.0d0,b)
                  cosr = dsign(temp3,a)
                  sinl = dsign(temp3,c)
                  cosl = sinr
                  sgnmn = sgna*sgnb*sgnc
                  sgnmx = sgnb
              else
!                 matrix diagonal
                  sinr = 0.0d0
                  cosr = 1.0d0
                  sinl = 0.0d0
                  cosl = 1.0d0
                  sgnmn = sgnc
                  sgnmx = sgna
              endif
          else
!             at .ne. 0, so eps/2 <= at <= 1
!             eps/2 <= temp3 <= 1 + sqrt(10)
              temp3 = au + temp1*temp2
              if (absa .lt. absc) then
!                 abs(ac) <= 1
                  ac = a/c
!                 eps <= sinr <= sqrt(13)+3
                  sinr = dsqrt((as*at+au)**2+4.0d0*ac*ac*au)+as*at+au
!                 abs(cosr) <= 2; if underflow, true cosr<UF/eps
                  cosr = ac*bac
                  cosr = cosr + cosr
!                 eps/(3+sqrt(10)) <= sinl <= 1
                  sinl = (as*at + temp3)/(1.0d0 + ac*ac + temp3)
!                 bad underflow possible only if sigmax < UF/eps
                  sinl = c*sinl
                  cosl = b
                  sgnmn = sgna*sgnc
                  sgnmx = 1.0
              else
!                 abs(ca) <= 1
                  ca = c/a
                  sinr = b
                  cosr = (as*at + temp3)/(1.0d0 + ca*ca + temp3)
                  cosr = a*cosr
!                 abs(sinl) <= 2; if underflow, true sinl<UF/eps
                  sinl = ca*bac
                  sinl = sinl + sinl
!                 eps <= cosl <= sqrt(13)+3
                  cosl = dsqrt((as*at+au)**2+4.0d0*ca*ca*au)+as*at+au
                  sgnmn = sgna*sgnc
                  sgnmx = 1.0
              endif
          endif
      else
          if (absa .eq. 0.0d0) then
              cosr = 0.0d0
              sinr = 1.0d0
              ia = 0
          else
              sinr = b
!             sigmin <= abs(a)/sqrt(2), so no bad cancellation in
!             absa-sigmin; overflow extremely unlikely, and in any
!             event only if sigmax overflows as well
              cosr = (absa-sigmin)*(dsign(1.0d0,a)+sigmin/a)
              ia = 1
          endif
          if (absc .eq. 0.0d0) then
              sinl = 0.0d0
              cosl = 1.0d0
              ib = 0
          else
              cosl = b
!             sigmin <= abs(c)/sqrt(2), so no bad cancellation in
!             absc-sigmin; overflow extremely unlikely, and in any
!             event only if sigmax overflows as well
              sinl = (absc-sigmin)*(dsign(1.0d0,c)+sigmin/c)
              ib = 1
          endif
          if (ia.eq.0 .and. ib.eq.0) then
              sgnmn = 1.0
              sgnmx = sgnb
          elseif (ia.eq.0 .and. ib.eq.1) then
              sgnmn = 1.0
              sgnmx = 1.0
          elseif (ia.eq.1 .and. ib.eq.0) then
              sgnmn = sgna*sgnc
              sgnmx = 1.0
          else
              sgnmn = sgna*sgnb*sgnc
              sgnmx = sgnb
          endif
      endif
      sigmin = sgnmn*sigmin
      sigmax = sgnmx*sigmax
      call sndrtg(cosr,sinr,csr,snr)
      call sndrtg(cosl,sinl,csl,snl)
      return
      end
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      double precision function sigmin(a,b,c)
!     compute smallest singular value of 2 by 2 matrix ((a,b);(0,c))
!     answer is accurate to a few ulps if final answer
!     exceeds (underflow_threshold/macheps)
!     overflow is impossible
      double precision a,b,c, aa,ab,ac,acmx,acmn,as,at,au
      aa = abs(a)
      ab = abs(b)
      ac = abs(c)
      acmn = min(aa,ac)
      if (acmn.eq.0.0d0) then
        sigmin = 0.0d0
      else
        acmx = max(aa,ac)
        ab = abs(b)
        if (ab.lt.acmx) then
          as = 1.0d0 + acmn/acmx
          at = (acmx-acmn)/acmx
          au = (ab/acmx)**2
          sigmin = acmn/(dsqrt(as*as+au) + dsqrt(at*at+au))
          sigmin = sigmin + sigmin
        else
          au = acmx/ab
          if (au.eq.0.0d0) then
!           possible harmful underflow
!           if exponent range asymmetric, true sigmin may not 
!           underflow
            sigmin = (acmn*acmx)/ab
          else
            as = 1.0d0 + acmn/acmx
            at = (acmx-acmn)/acmx
            sigmin = acmn/(dsqrt(1.0d0+(as*au)**2) + &
                 dsqrt(1.0d0+(at*au)**2))
            sigmin = au*sigmin
            sigmin = sigmin + sigmin
          endif
        endif
      endif
      return
      end
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE sndrtg(f,g,cs,sn)
!     version of ndrotg, in which sign(f)=sign(cs),sign(g)=sign(sn)
!     cs, sn returned so that -sn*f+cs*g = 0
!     and cs*f + sn*g = sqrt(f**2+g**2) 
      double precision f, g, cs, sn, t, tt
      if (f.eq.0.0d0 .and. g.eq.0.0d0) then
        cs = 1.0d0
        sn = 0.0d0
      else
        if (abs(f) .gt. abs(g)) then
          t = g/f
          tt = dsqrt(1.0d0 + t*t )
          cs = dsign(1.0d0/tt,f)
          sn = dsign(t*cs,g)
        else
          t = f/g
          tt = dsqrt(1.0d0 + t*t )
          sn = dsign(1.0d0/tt,g)
          cs = dsign(t*sn,f)
        endif
      endif
      return
      end
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!              LINPACK and LAPACK routines
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!  hqr3_loc.f orthes.f ortran.f   for computing triangular matrix
!	
      SUBROUTINE HQR3LC(A,V,N,NLOW,NUP,EPS,ER,EI,TYPE,NA,NV,imfd)
        IMPLICIT DOUBLE PRECISION (a-h,o-z)
      INTEGER N, NA, NLOW, NUP, NV, TYPE(N),imfd
      DOUBLE PRECISION A(NA,N), EI(N), ER(N), EPS, V(NV,N)
! HQR3 REDUCES THE UPPER HESSENBERG MATRIX A TO QUASI-
! TRIANGULAR FORM BY UNITARY SIMILARITY TRANSFORMATIONS.
! THE EIGENVALUES OF A, WHICH ARE CONTAINED IN THE 1X1
! AND 2X2 DIAGONAL BLOCKS OF THE REDUCED MATRIX, ARE
! ORDERED IN assending or DESCENDING ORDER OF ALONG THE
! DIAGONAL.  THE TRANSFORMATIONS ARE ACCUMULATED IN THE
! ARRAY V.  HQR3 REQUIRES THE SUBROUTINES EXCHNG,
! QRSTEP, AND SPLIT.  THE PARAMETERS IN THE CALLING
! SEQUENCE ARE (STARRED PARAMETERS ARE ALTERED BY THE
! SUBROUTINE)
!    *A       AN ARRAY THAT INITIALLY CONTAINS THE N X N
!             UPPER HESSENBERG MATRIX TO BE REDUCED.  ON
!             RETURN A CONTAINS THE REDUCED, QUASI-
!             TRIANGULAR MATRIX.
!    *V       AN ARRAY THAT CONTAINS A MATRIX INTO WHICH
!             THE REDUCING TRANSFORMATIONS ARE TO BE
!             MULTIPLIED.
!     N       THE ORDER OF THE MATRICES A AND V.
!     NLOW    A(NLOW,NLOW-1) AND A(NUP,+1,NUP) ARE
!     NUP     ASSUMED TO BE ZERO, AND ONLY ROWS NLOW
!             THROUGH NUP AND COLUMNS NLOW THROUGH
!             NUP ARE TRANSFORMED, RESULTING IN THE
!             CALCULATION OF EIGENVALUES NLOW
!             THROUGH NUP.
!     EPS     A CONVERGENCE CRITERION.
!    *ER      AN ARRAY THAT ON RETURN CONTAINS THE REAL
!             PARTS OF THE EIGENVALUES.
!    *EI      AN ARRAY THAT ON RETURN CONTAINS THE
!             IMAGINARY PARTS OF THE EIGENVALUES.
!    *TYPE    AN INTEGER ARRAY WHOSE I-TH ENTRY IS
!               0   IF THE I-TH EIGENVALUE IS REAL,
!               1   IF THE I-TH EIGENVALUE IS COMPLEX
!                   WITH POSITIVE IMAGINARY PART.
!               2   IF THE I-TH EIGENVALUE IS COMPLEX
!                   WITH NEGATIVE IMAGINARY PART,
!              -1   IF THE I-TH EIGENVALUE WAS NOT
!                   CALCULATED SUCCESSFULLY.
!     NA      THE FIRST DIMENSION OF THE ARRAY A.
!     NV      THE FIRST DIMENSION OF THE ARRAY V.
!
!     imfd    ascending or descending order of real part of eigenvalues
!              -1  ascending (i.e. negative eigenvalues first)
!              +1  descending (positive eigenvalues)
!
!
! THE CONVERGENCE CRITERION EPS IS USED TO DETERMINE
! WHEN A SUBDIAGONAL ELEMENT OF A IS NEGLIGIBLE.
! SPECIFICALLY A(I+1,I) IS REGARDED AS NEGLIGIBLE
! IF
!        ABS(A(I+1),I)) .LE. EPS*(ABS(A(I,I))+ABS(A(I+1,I+1))).
! THIS MEANS THAT THE FINAL MATRIX RETURNED BY THE
! PROGRAM WILL BE EXACTLY SIMILAR TO A + E WHERE E IS
! OF ORDER EPS*NORM(A), FOR ANY REASONABLY BALANCED NORM
! SUCH AS THE ROW-SUM NORM.
! INTERNAL VARIABLES
      INTEGER I, IT, L, MU, NL, NU
      DOUBLE PRECISION E1, E2, P, Q, R, S, T, W, X, Y, Z
      LOGICAL FAIL
! INITIALIZE.
      DO 10 I=NLOW,NUP
        TYPE(I) = -1
   10 CONTINUE
      T = 0.d0
! MAIN LOOP. FIND AND ORDER EIGENVALUES.
      NU = NUP
   20 IF (NU.LT.NLOW) GO TO 240
      IT = 0
! QR LOOP.  FIND NEGLIGIBLE ELEMENTS AND PERFORM
! QR STEPS.
   30 CONTINUE
! SEARCH BACK FOR NEGLIGIBLE ELEMENTS.
      L = NU
   40 CONTINUE
      IF (L.EQ.NLOW) GO TO 50
      IF (DABS(A(L,L-1)).LE.EPS*(DABS(A(L-1,L-1))+DABS(A(L,L)))) GO TO &
           50
      L = L - 1
      GO TO 40
   50 CONTINUE
! TEST TO SEE IF AN EIGENVALUE OR A 2X2 BLOCK
! HAS BEEN FOUND.
      X = A(NU,NU)
      IF (L.EQ.NU) GO TO 160
      Y = A(NU-1,NU-1)
      W = A(NU,NU-1)*A(NU-1,NU)
      IF (L.EQ.NU-1) GO TO 100
! TEST ITERATION COUNT. IF IT IS 30 QUIT.  IF
! IT IS 10 OR 20 SET UP AN AD-HOC SHIFT.
      IF (IT.EQ.30) GO TO 240
      IF (IT.NE.10 .AND. IT.NE.20) GO TO 70
! AD-HOC SHIFT.
      T = T + X
      DO 60 I=NLOW,NU
        A(I,I) = A(I,I) - X
   60 CONTINUE
      S = DABS(A(NU,NU-1)) + DABS(A(NU-1,NU-2))
      X = 0.75d0*S
      Y = X
      W = -0.4375d0*S**2
   70 CONTINUE
      IT = IT + 1
! LOOK FOR TWO CONSECUTIVE SMALL SUB-DIAGONAL
! ELEMENTS.
      NL = NU - 2
   80 CONTINUE
      Z = A(NL,NL)
      R = X - Z
      S = Y - Z
      P = (R*S-W)/A(NL+1,NL) + A(NL,NL+1)
      Q = A(NL+1,NL+1) - Z - R - S
      R = A(NL+2,NL+1)
      S = DABS(P) + DABS(Q) + DABS(R)
      P = P/S
      Q = Q/S
      R = R/S
      IF (NL.EQ.L) GO TO 90
      IF (DABS(A(NL,NL-1))*(DABS(Q)+DABS(R)).LE.EPS*DABS(P) &
           *(DABS(A(NL-1, &
           NL-1))+DABS(Z)+DABS(A(NL+1,NL+1)))) GO TO 90
      NL = NL - 1
      GO TO 80
   90 CONTINUE
! PERFORM A QR STEP BETWEEN NL AND NU.
      CALL QRSTEP(A, V, P, Q, R, NL, NU, N, NA, NV)
      GO TO 30
! 2X2 BLOCK FOUND.
  100 IF (NU.NE.NLOW+1) A(NU-1,NU-2) = 0.d0
      A(NU,NU) = A(NU,NU) + T
      A(NU-1,NU-1) = A(NU-1,NU-1) + T
      TYPE(NU) = 0
      TYPE(NU-1) = 0
      MU = NU
! LOOP TO POSITION  2X2 BLOCK.
  110 CONTINUE
      NL = MU - 1
! ATTEMPT  TO SPLIT THE BLOCK INTO TWO REAL
! EIGENVALUES.
      CALL SPLIT(A, V, N, NL, E1, E2, NA, NV)
! IF THE SPLIT WAS SUCCESSFUL, GO AND ORDER THE
! REAL EIGENVALUES.
      IF (A(MU,MU-1).EQ.0.d0) GO TO 170
! TEST TO SEE IF THE BLOCK IS PROPERLY POSITIONED,
! AND IF NOT EXCHANGE IT
      IF (MU.EQ.NUP) GO TO 230
      IF (MU.EQ.NUP-1) GO TO 130
      IF (A(MU+2,MU+1).EQ.0.d0) GO TO 130
! THE NEXT BLOCK IS 2X2.
!     IF (A(MU-1,MU-1)*A(MU,MU)-A(MU-1,MU)*A(MU,MU-1).GE.A(MU+1,
!    * MU+1)*A(MU+2,MU+2)-A(MU+1,MU+2)*A(MU+2,MU+1)) GO TO 230
!
      if (imfd.eq.1) then
        IF ((A(MU-1,MU-1)+A(MU,MU)).GE.(A(MU+1,MU+1)+A(MU+2,MU+2)))  &
             GO TO 230
      else 
        IF ((A(MU-1,MU-1)+A(MU,MU)).LE.(A(MU+1,MU+1)+A(MU+2,MU+2)))  &
             GO TO 230
      endif 
!
      CALL EXCHNG(A, V, N, NL, 2, 2, EPS, FAIL, NA, NV)
      IF (.NOT.FAIL) GO TO 120
      TYPE(NL) = -1
      TYPE(NL+1) = -1
      TYPE(NL+2) = -1
      TYPE(NL+3) = -1
      GO TO 240
  120 CONTINUE
      MU = MU + 2
      GO TO 150
  130 CONTINUE
! THE NEXT BLOCK IS 1X1.
!     IF (A(MU-1,MU-1)*A(MU,MU)-A(MU-1,MU)*A(MU,MU-1).GE.A(MU+1,
!    * MU+1)**2) GO TO 230
!
      if (imfd.eq.1) then
        IF ((A(MU-1,MU-1)+A(MU,MU)).GE.(2.0D0*A(MU+1,MU+1))) &
             GO TO 230
      else 
        IF ((A(MU-1,MU-1)+A(MU,MU)).LE.(2.0D0*A(MU+1,MU+1))) &
             GO TO 230
      endif 
!
      CALL EXCHNG(A, V, N, NL, 2, 1, EPS, FAIL, NA, NV)
      IF (.NOT.FAIL) GO TO 140
      TYPE(NL) = -1
      TYPE(NL+1) = -1
      TYPE(NL+2) = -1
      GO TO 240
  140 CONTINUE
      MU = MU + 1
  150 CONTINUE
      GO TO 110
! SINGLE EIGENVALUE FOUND.
  160 NL = 0
      A(NU,NU) = A(NU,NU) + T
      IF (NU.NE.NLOW) A(NU,NU-1) = 0.d0
      TYPE(NU) = 0
      MU = NU
! LOOP TO POSITION ONE OR TWO REAL EIGENVALUES.
  170 CONTINUE
! POSITION THE EIGENVALUE LOCATED AT A(NL,NL).
  180 CONTINUE
      IF (MU.EQ.NUP) GO TO 220
      IF (MU.EQ.NUP-1) GO TO 200
      IF (A(MU+2,MU+1).EQ.0.d0) GO TO 200
! THE NEXT BLOCK IS 2X2.
!      IF (A(MU,MU)**2.GE.A(MU+1,MU+1)*A(MU+2,MU+2)-A(MU+1,MU+2)*
!    * A(MU+2,MU+1)) GO TO 220
!
      if (imfd.eq.1) then
        IF ((2.0D0*A(MU,MU)).GE.(A(MU+1,MU+1)+A(MU+2,MU+2))) &
             GO TO 220
      else
        IF ((2.0D0*A(MU,MU)).LE.(A(MU+1,MU+1)+A(MU+2,MU+2))) &
             GO TO 220
      endif
!
      CALL EXCHNG(A, V, N, MU, 1, 2, EPS, FAIL, NA, NV)
      IF (.NOT.FAIL) GO TO 190
      TYPE(MU) = -1
      TYPE(MU+1) = -1
      TYPE(MU+2) = -1
      GO TO 240
  190 CONTINUE
      MU = MU + 2
      GO TO 210
  200 CONTINUE
! THE NEXT BLOCK IS 1X1.
!      IF (ABS(A(MU,MU)).GE.ABS(A(MU+1,MU+1))) GO TO 220
!
      if (imfd.eq.1) then
        IF (A(MU,MU).GE.A(MU+1,MU+1)) GO TO 220
      else
        IF (A(MU,MU).LE.A(MU+1,MU+1)) GO TO 220
      endif
!
      CALL EXCHNG(A, V, N, MU, 1, 1, EPS, FAIL, NA, NV)
      MU = MU + 1
  210 CONTINUE
      GO TO 180
  220 CONTINUE
      MU = NL
      NL = 0
      IF (MU.NE.0) GO TO 170
! GO BACK AND GET THE NEXT EIGENVALUE.
  230 CONTINUE
      NU = L - 1
      GO TO 20
! ALL THE EIGENVALUES HAVE BEEN FOUND AND ORDERED.
! COMPUTE THEIR VALUES AND TYPE.
  240 IF (NU.LT.NLOW) GO TO 260
      DO 250 I=NLOW,NU
        A(I,I) = A(I,I) + T
  250 CONTINUE
  260 CONTINUE
      NU = NUP
  270 CONTINUE
      IF (TYPE(NU).NE.-1) GO TO 280
      NU = NU - 1
      GO TO 310
  280 CONTINUE
      IF (NU.EQ.NLOW) GO TO 290
      IF (A(NU,NU-1).EQ.0.d0) GO TO 290
! 2X2 BLOCK.
      CALL SPLIT(A, V, N, NU-1, E1, E2, NA, NV)
      IF (A(NU,NU-1).EQ.0.d0) GO TO 290
      ER(NU) = E1
      EI(NU-1) = E2
      ER(NU-1) = ER(NU)
      EI(NU) = -EI(NU-1)
      TYPE(NU-1) = 1
      TYPE(NU) = 2
      NU = NU - 2
      GO TO 300
  290 CONTINUE
! SINGLE ROOT.
      ER(NU) = A(NU,NU)
      EI(NU) = 0.d0
      NU = NU - 1
  300 CONTINUE
  310 CONTINUE
      IF (NU.GE.NLOW) GO TO 270
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE SPLIT(A, V, N, L, E1, E2, NA, NV)                     
	IMPLICIT DOUBLE PRECISION (a-h,o-z)
      INTEGER L, N, NA, NV
      DOUBLE PRECISION A(NA,N), V(NV,N)
! GIVEN THE UPPER HESSENBERG MATRIX A WITH A 2X2 BLOCK
! STARTING AT A(L,L), SPLIT DETERMINES IF THE
! CORRESPONDING EIGENVALUES ARE REAL OR COMPLEX. IF THEY
! ARE REAL, A ROTATION IS DETERMINED THAT REDUCES THE
! BLOCK TO UPPER TRIANGULAR FORM WITH THE EIGENVALUE
! OF LARGEST ABSOLUTE VALUE APPEARING FIRST.  THE
! ROTATION IS ACCUMULATED IN V.  THE EIGENVALUES (REAL
! OR COMPLEX) ARE RETURNED IN E1 AND E2.  THE PARAMETERS
! IN THE CALLING SEQUENCE ARE (STARRED PARAMETERS ARE
! ALTERED BY THE SUBROUTINE)
!    *A       THE UPPER HESSENVERG MATRIX WHOSE 2X2
!             BLOCK IS TO BE SPLIT.
!    *V       THE ARRAY IN WHICH THE SPLITTING TRANS-
!             FORMATION IS TO BE ACCUMULATED.
!     N       THE ORDER OF THE MATRIX A.
!     L       THE POSITION OF THE 2X2 BLOCK.
!    *E1      ON RETURN IF THE EIGENVALUES ARE COMPLEX
!    *E2      E1 CONTAINS THEIR COMMON REAL PART AND
!             E2 CONTAINS THE POSITIVE IMAGINARY PART.
!             IF THE EIGENVALUES ARE REAL, E1 CONTAINS
!             THE ONE LARGEST IN ABSOLUTE VALUE AND E2
!             CONTAINS THE OTHER ONE.
!     NA      THE FIRST DIMENSION OF THE ARRAY A.
!     NV      THE FIRST DIMENSION OF THE ARRAY V.
! INTERNAL VARIABLES
      INTEGER I, J, L1
      DOUBLE PRECISION P, Q, R, T, U, W, X, Y, Z
      X = A(L+1,L+1)
      Y = A(L,L)
      W = A(L,L+1)*A(L+1,L)
      P = (Y-X)/2.d0
      Q = P**2 + W
      IF (Q.GE.0.d0) GO TO 10
! COMPLEX EIGENVALUE.
      E1 = P + X
      E2 = DSQRT(-Q)
      RETURN
   10 CONTINUE
! TWO REAL EIGENVALUES.  SET UP TRANSFORMATION.
      Z = DSQRT(Q)
      IF (P.LT.0.d0) GO TO 20
      Z = P + Z
      GO TO 30
   20 CONTINUE
      Z = P - Z
   30 CONTINUE
      IF (Z.EQ.0.d0) GO TO 40
      R = -W/Z
      GO TO 50
   40 CONTINUE
      R = 0.d0
   50 CONTINUE
      IF (DABS(X+Z).GE.DABS(X+R)) Z = R
      Y = Y - X - Z
      X = -Z
      T = A(L,L+1)
      U = A(L+1,L)
      IF (DABS(Y)+DABS(U).LE.DABS(T)+DABS(X)) GO TO 60
      Q = U
      P = Y
      GO TO 70
   60 CONTINUE
      Q = X
      P = T
   70 CONTINUE
      R = DSQRT(P**2+Q**2)
      IF (R.GT.0.d0) GO TO 80
      E1 = A(L,L)
      E2 = A(L+1,L+1)
      A(L+1,L) = 0.d0
      RETURN
   80 CONTINUE
      P = P/R
      Q = Q/R
! PREMULTIPLY.
      DO 90 J=L,N
        Z = A(L,J)
        A(L,J) = P*Z + Q*A(L+1,J)
        A(L+1,J) = P*A(L+1,J) - Q*Z
   90 CONTINUE
! POSTMULTIPLY.
      L1 = L + 1
      DO 100 I=1,L1
        Z = A(I,L)
        A(I,L) = P*Z + Q*A(I,L+1)
        A(I,L+1) = P*A(I,L+1) - Q*Z
  100 CONTINUE
! ACCUMULATE THE TRANSFORMATION IN V.
      DO 110 I=1,N
        Z = V(I,L)
        V(I,L) = P*Z + Q*V(I,L+1)
        V(I,L+1) = P*V(I,L+1) - Q*Z
  110 CONTINUE
      A(L+1,L) = 0.d0
      E1 = A(L,L)
      E2 = A(L+1,L+1)
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE EXCHNG(A, V, N, L, B1, B2, EPS, FAIL, NA, NV)          
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      INTEGER B1, B2, L, NA, NV
      DOUBLE PRECISION A(NA,N), EPS, V(NV,N)
      LOGICAL FAIL
! GIVEN THE UPPER HESSENBERG MATRIX A WITH CONSECUTIVE
! B1XB1 AND B2XB2 DIAGONAL BLOCKS (B1,B2 .LE. 2)
! STARTING AT A(L,L), EXCHNG PRODUCES A UNITARY
! SIMILARITY TRANSFORMATION THAT EXCHANGES THE BLOCKS
! ALONG WITH THEIR EIGENVALUES.  THE TRANSFORMATION
! IS ACCUMULATED IN V.  EXCHNG REQUIRES THE SUBROUTINE
! QRSTEP.  THE PARAMETERS IN THE CALLING SEQUENCE ARE
! %STARRED PARAMETERS ARE ALTERED BY THE SUBROUTINE)
!    *A       THE MATRIX WHOSE BLOCKS ARE TO BE
!             INTERCHANGED.
!    *V       THE ARRAY INTO WHICH THE TRANSFORMATIONS
!             ARE TO BE ACCUMULATED.
!     N       THE ORDER OF THE MATRIX A.
!     L       THE POSITION OF THE BLOCKS.
!     B1      AN INTEGER CONTAINING THE SIZE OF THE
!             FIRST BLOCK.
!     B2      AN INTEGER CONTAINING THE SIZE OF THE
!             SECOND BLOCK.
!     EPS     A CONVERGENCE CRITERION (CF. HQR3).
!    *FAIL    A LOGICAL VARIABLE WHICH IS FALSE ON A
!             NORMAL RETURN.  IF THIRTY ITERATIONS WERE
!             PERFORMED WITHOUT CONVERGENCE, FAIL IS SET
!             TO TRUE AND THE ELEMENT
!             A(L+B2,L+B2-1) CANNOT BE ASSUMED ZERO.
!     NA      THE FIRST DIMENSION OF THE ARRAY A.
!     NV      THE FIRST DIMENSION OF THE ARRAY V.
! INTERNAL VARIABLES.
      INTEGER I, IT, J, L1, M
      DOUBLE PRECISION P, Q, R, S, W, X, Y, Z
      FAIL = .FALSE.
      IF (B1.EQ.2) GO TO 70
      IF (B2.EQ.2) GO TO 40
! INTERCHANGE 1X1 AND 1X1 BLOCKS.
      L1 = L + 1
      Q = A(L+1,L+1) - A(L,L)
      P = A(L,L+1)
      R = DMAX1(DABS(P),DABS(Q))
      IF (R.EQ.0.d0) RETURN
      P = P/R
      Q = Q/R
      R = DSQRT(P**2+Q**2)
      P = P/R
      Q = Q/R
      DO 10 J=L,N
        S = P*A(L,J) + Q*A(L+1,J)
        A(L+1,J) = P*A(L+1,J) - Q*A(L,J)
        A(L,J) = S
   10 CONTINUE
      DO 20 I=1,L1
        S = P*A(I,L) + Q*A(I,L+1)
        A(I,L+1) = P*A(I,L+1) - Q*A(I,L)
        A(I,L) = S
   20 CONTINUE
      DO 30 I=1,N
        S = P*V(I,L) + Q*V(I,L+1)
        V(I,L+1) = P*V(I,L+1) - Q*V(I,L)
        V(I,L) = S
   30 CONTINUE
      A(L+1,L) = 0.d0
      RETURN
   40 CONTINUE
! INTERCHANGE 1X1 AND 2X2 BLOCKS.
      X = A(L,L)
      P = 1.d0
      Q = 1.d0
      R = 1.d0
      CALL QRSTEP(A, V, P, Q, R, L, L+2, N, NA, NV)
      IT = 0
   50 IT = IT + 1
      IF (IT.LE.30) GO TO 60
      FAIL = .TRUE.
      RETURN
   60 CONTINUE
      P = A(L,L) - X
      Q = A(L+1,L)
      R = 0.d0
      CALL QRSTEP(A, V, P, Q, R, L, L+2, N, NA, NV)
      IF (DABS(A(L+2,L+1)).GT.EPS*(DABS(A(L+1,L+1))+DABS(A(L+2,L+2)))) &
           GO TO 50
      A(L+2,L+1) = 0.d0
      RETURN
   70 CONTINUE
! INTERCHANGE 2X2 AND B2XB2 BLOCKS.
      M = L + 2
      IF (B2.EQ.2) M = M + 1
      X = A(L+1,L+1)
      Y = A(L,L)
      W = A(L+1,L)*A(L,L+1)
      P = 1.d0
      Q = 1.d0
      R = 1.d0
      CALL QRSTEP(A, V, P, Q, R, L, M, N, NA, NV)
      IT = 0
   80 IT = IT + 1
      IF (IT.LE.30) GO TO 90
      FAIL = .TRUE.
      RETURN
   90 CONTINUE
      Z = A(L,L)
      R = X - Z
      S = Y - Z
      P = (R*S-W)/A(L+1,L) + A(L,L+1)
      Q = A(L+1,L+1) - Z - R - S
      R = A(L+2,L+1)
      S = DABS(P) + DABS(Q) + DABS(R)
      P = P/S
      Q = Q/S
      R = R/S
      CALL QRSTEP(A, V, P, Q, R, L, M, N, NA, NV)
      IF (DABS(A(M-1,M-2)).GT.EPS*(DABS(A(M-1,M-1))+DABS(A(M-2,M-2)))) &
           GO TO 80
      A(M-1,M-2) = 0.d0
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE QRSTEP(A, V, P, Q, R, NL, NU, N, NA, NV)       
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      INTEGER N, NA, NL, NU, NV
      DOUBLE PRECISION A(NA,N), P, Q, R, V(NV,N)
! QRSTEP PERFORMS ONE IMPLICIT QR STEP ON THE
! UPPER HESSENBERG MATRIX A.  THE SHIFT IS DETERMINED
! BY THE NUMBERS P,Q, AND R, AND THE STEP IS APPLIED TO
! ROWS AND COLUMNS NL THROUGH NU.  THE TRANSFORMATIONS
! ARE ACCUMULATED IN V.  THE PARAMETERS IN THE CALLING
! SEQUENCE ARE (STARRED APRAMETERS ARE ALTERED BY THE
! SUBROUTINE)
!    *A       THE UPPER HESSENBERG MATRIX ON WHICH THE
!             QR STEP IS TO BE PERFORMED.
!    *V       THE ARRAY IN WHICH THE TRANSFORMATIONS
!             ARE TO BE ACCUMULATED
!    *P       PARAMETERS THAT DETERMINE THE SHIFT.
!    *Q
!    *R
!     NL      THE LOWER LIMIT OF THE STEP.
!     NU      THE UPPER LIMIT OF THE STEP.
!     N       THE ORDER OF THE MATRIX A.
!     NA      THE FIRST DIMENSION OF THE ARRAY A.
!     NV      THE FIRST DIMENSION OF THE ARRAY V.
! INTERNAL VARIABLES.
      INTEGER I, J, K, NL2, NL3, NUM1
      DOUBLE PRECISION S, X, Y, Z
      LOGICAL LAST
      NL2 = NL + 2
      DO 10 I=NL2,NU
        A(I,I-2) = 0.d0
   10 CONTINUE
      IF (NL2.EQ.NU) GO TO 30
      NL3 = NL + 3
      DO 20 I=NL3,NU
        A(I,I-3) = 0.d0
   20 CONTINUE
   30 CONTINUE
      NUM1 = NU - 1
      DO 130 K=NL,NUM1
! DETERMINE THE TRANSFORMATION.
        LAST = K.EQ.NUM1
        IF (K.EQ.NL) GO TO 40
        P = A(K,K-1)
        Q = A(K+1,K-1)
        R = 0.d0
        IF (.NOT.LAST) R = A(K+2,K-1)
        X = DABS(P) + DABS(Q) + DABS(R)
        IF (X.EQ.0.d0) GO TO 130
        P = P/X
        Q = Q/X
        R = R/X
   40   CONTINUE
        S = DSQRT(P**2+Q**2+R**2)
        IF (P.LT.0.d0) S = -S
        IF (K.EQ.NL) GO TO 50
        A(K,K-1) = -S*X
        GO TO 60
   50   CONTINUE
        IF (NL.NE.1) A(K,K-1) = -A(K,K-1)
   60   CONTINUE
        P = P + S
        X = P/S
        Y = Q/S
        Z = R/S
        Q = Q/P
        R = R/P
! PREMULTIPLY.
        DO 80 J=K,N
          P = A(K,J) + Q*A(K+1,J)
          IF (LAST) GO TO 70
          P = P + R*A(K+2,J)
          A(K+2,J) = A(K+2,J) - P*Z
   70     CONTINUE
          A(K+1,J) = A(K+1,J) - P*Y
          A(K,J) = A(K,J) - P*X
   80   CONTINUE
! POSTMULTIPLY.
        J = MIN0(K+3,NU)
        DO 100 I=1,J
          P = X*A(I,K) + Y*A(I,K+1)
          IF (LAST) GO TO 90
          P = P + Z*A(I,K+2)
          A(I,K+2) = A(I,K+2) - P*R
   90     CONTINUE
          A(I,K+1) = A(I,K+1) - P*Q
          A(I,K) = A(I,K) - P
  100   CONTINUE
! ACCUMULATE THE TRANSFORMATION IN V.
        DO 120 I=1,N
          P = X*V(I,K) + Y*V(I,K+1)
          IF (LAST) GO TO 110
          P = P + Z*V(I,K+2)
          V(I,K+2) = V(I,K+2) - P*R
  110     CONTINUE
          V(I,K+1) = V(I,K+1) - P*Q
          V(I,K) = V(I,K) - P
  120   CONTINUE
  130 CONTINUE
      RETURN
      END
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE orthes(nm,n,low,igh,a,ort)
!
      integer i,j,m,n,ii,jj,la,mp,nm,igh,kp1,low
      double precision a(nm,n),ort(igh)
      double precision f,g,h,scale
!
!     this subroutine is a translation of the algol procedure orthes,
!     num. math. 12, 349-368(1968) by martin and wilkinson.
!     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
!
!     given a real general matrix, this subroutine
!     reduces a submatrix situated in rows and columns
!     low through igh to upper hessenberg form by
!     orthogonal similarity transformations.
!
!     on input
!
!        nm must be set to the row dimension of two-dimensional
!          array parameters as declared in the calling program
!          dimension statement.
!
!        n is the order of the matrix.
!
!        low and igh are integers determined by the balancing
!          subroutine  balanc.  if  balanc  has not been used,
!          set low=1, igh=n.
!
!        a contains the input matrix.
!
!     on output
!
!        a contains the hessenberg matrix.  information about
!          the orthogonal transformations used in the reduction
!          is stored in the remaining triangle under the
!          hessenberg matrix.
!
!        ort contains further information about the transformations.
!          only elements low through igh are used.
!
!     questions and comments should be directed to burton s. garbow,
!     mathematics and computer science div, argonne national laboratory
!
!     this version dated august 1983.
!
!     ------------------------------------------------------------------
!
      la = igh - 1
      kp1 = low + 1
      if (la .lt. kp1) go to 200
!
      do 180 m = kp1, la
         h = 0.0d0
         ort(m) = 0.0d0
         scale = 0.0d0
!     .......... scale column (algol tol then not needed) ..........
         do 90 i = m, igh
   90    scale = scale + dabs(a(i,m-1))
!
         if (scale .eq. 0.0d0) go to 180
         mp = m + igh
!     .......... for i=igh step -1 until m do -- ..........
         do 100 ii = m, igh
            i = mp - ii
            ort(i) = a(i,m-1) / scale
            h = h + ort(i) * ort(i)
  100    continue
!
         g = -dsign(dsqrt(h),ort(m))
         h = h - ort(m) * g
         ort(m) = ort(m) - g
!     .......... form (i-(u*ut)/h) * a ..........
         do 130 j = m, n
            f = 0.0d0
!     .......... for i=igh step -1 until m do -- ..........
            do 110 ii = m, igh
               i = mp - ii
               f = f + ort(i) * a(i,j)
  110       continue
!
            f = f / h
!
            do 120 i = m, igh
  120       a(i,j) = a(i,j) - f * ort(i)
!
  130    continue
!     .......... form (i-(u*ut)/h)*a*(i-(u*ut)/h) ..........
         do 160 i = 1, igh
            f = 0.0d0
!     .......... for j=igh step -1 until m do -- ..........
            do 140 jj = m, igh
               j = mp - jj
               f = f + ort(j) * a(i,j)
  140       continue
!
            f = f / h
!
            do 150 j = m, igh
  150       a(i,j) = a(i,j) - f * ort(j)
!
  160    continue
!
         ort(m) = scale * ort(m)
         a(m,m-1) = scale * g
  180 continue
!
  200 return
      end
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE ortran(nm,n,low,igh,a,ort,z)
!
      integer i,j,n,kl,mm,mp,nm,igh,low,mp1
      double precision a(nm,igh),ort(igh),z(nm,n)
      double precision g
!
!     this subroutine is a translation of the algol procedure ortrans,
!     num. math. 16, 181-204(1970) by peters and wilkinson.
!     handbook for auto. comp., vol.ii-linear algebra, 372-395(1971).
!
!     this subroutine accumulates the orthogonal similarity
!     transformations used in the reduction of a real general
!     matrix to upper hessenberg form by  orthes.
!
!     on input
!
!        nm must be set to the row dimension of two-dimensional
!          array parameters as declared in the calling program
!          dimension statement.
!
!        n is the order of the matrix.
!
!        low and igh are integers determined by the balancing
!          subroutine  balanc.  if  balanc  has not been used,
!          set low=1, igh=n.
!
!        a contains information about the orthogonal trans-
!          formations used in the reduction by  orthes
!          in its strict lower triangle.
!
!        ort contains further information about the trans-
!          formations used in the reduction by  orthes.
!          only elements low through igh are used.
!
!     on output
!
!        z contains the transformation matrix produced in the
!          reduction by  orthes.
!
!        ort has been altered.
!
!     questions and comments should be directed to burton s. garbow,
!     mathematics and computer science div, argonne national laboratory
!
!     this version dated august 1983.
!
!     ------------------------------------------------------------------
!
!     .......... initialize z to identity matrix ..........
      do 80 j = 1, n
!
         do 60 i = 1, n
   60    z(i,j) = 0.0d0
!
         z(j,j) = 1.0d0
   80 continue
!
      kl = igh - low - 1
      if (kl .lt. 1) go to 200
!     .......... for mp=igh-1 step -1 until low+1 do -- ..........
      do 140 mm = 1, kl
         mp = igh - mm
         if (a(mp,mp-1) .eq. 0.0d0) go to 140
         mp1 = mp + 1
!
         do 100 i = mp1, igh
  100    ort(i) = a(i,mp-1)
!
         do 130 j = mp, igh
            g = 0.0d0
!
            do 110 i = mp, igh
  110       g = g + ort(i) * z(i,j)
!     .......... divisor below is negative of h formed in orthes.
!                double division avoids possible underflow ..........
            g = (g / ort(mp)) / a(mp,mp-1)
!
            do 120 i = mp, igh
  120       z(i,j) = z(i,j) + g * ort(i)
!
  130    continue
!
  140 continue
!
  200 return
      end
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

