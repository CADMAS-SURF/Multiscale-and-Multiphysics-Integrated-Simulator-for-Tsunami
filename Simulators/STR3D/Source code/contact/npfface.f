      SUBROUTINE NPFFACE(MODE,FN,AVR,RKF,EPS0R,FRCI,VR,FT,NS,IELC,IFCQ
     &                  ,IELQ,U0,RL0,ISTK,NCONV,MODEL,RMU0,AVR0,RMUD,UG
     &                  ,POS,F,DT,IFMDL,MITER1,MITERD0,MITERD1,EPS0,EPSD
     &                  ,IDYN,ITER)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DU(3,4),UG(6,*),U0(3,4),IELC(3),RL0(3),VR(3),FT(3)
     &         ,FRCI(3,*),POS(3,*),V1(3),V2(3),RN(3),F(3),KN(4)
     &         ,IELQ(4,*)
C-----------------------------------------------------------------------
      V1(:) = POS(:,IELC(3)) - POS(:,IELC(1))
      V2(:) = POS(:,IELC(2)) - POS(:,IELC(1))
C
      CALL CROSS2(V1,V2,RN)
C
      CALL VECML1(FN,F,RN,3)
C
C     FN = DABS(FN)
      FN = DMAX1(FN,0.D0)
C
      DU(:,1:3) = UG(1:3,IELC(:)) - U0(:,1:3)
      DU(:,4)   = UG(1:3,NS) - U0(:,4)
C
      VR(:) = DU(:,4)
      DO I = 1, 3
        VR(:) = VR(:) - RL0(I) * DU(:,I)
      ENDDO
      VR(:) = VR(:) / DT
C
      CALL VECML2(AVR,VR,3)
C
      FT(:) = 0.
C
      IF( IFMDL == 1 ) THEN
C
        IF( AVR / AVR0 < 1.D-10 ) THEN
          FT(:) = RMU0 / DASIN(1.D0) * FN / AVR0 * VR(:)
        ELSE
          RMU = RMU0 / DASIN(1.D0) * DATAN( AVR / AVR0 )
          FT(:) = RMU * FN * VR(:) / AVR
        ENDIF
C
      ELSEIF( IFMDL == 2 ) THEN
C
        IF( IDYN == 1 ) THEN
C
          IF( ISTK == 1 ) THEN
C
            IF( MODE <= 2 ) THEN
C
              IF( AVR < EPS0 .OR.
     &            ( MODE == 1 .AND. RMU0 * FN > RKF * AVR ) ) THEN
                RKF = RMU0 * FN / EPS0
                MODE = 1
              ELSE
                RKF = RMU0 * FN / AVR
                IF( ITER > MITER1 .AND. NCONV == 1 ) THEN
                  EPS0R = AVR
                  MODE = 3
                ELSE
                  MODE = 2
                ENDIF
              ENDIF
C
            ELSEIF( MODE == 3 ) THEN
C
              RKF = RMU0 * FN / EPS0R
C
            ENDIF
C
          ELSEIF( ISTK == 0 ) THEN
C
            IF( MODE <= 2 ) THEN
C
              IF( MODEL == 1 ) THEN
C
                IF( MODE == 1 ) THEN
                  IF( AVR < 1.D-10 ) THEN
                    RKF = RMUD * FN / EPSD
                  ELSE
                    RKF = RMUD * FN / AVR
                    MODE = 2
                    IF( ITER > MITERD0 .AND. NCONV == 1 ) MODEL = 2
                  ENDIF
                ELSEIF( MODE == 2 ) THEN
                  RKF = RMUD * FN / AVR
                  IF( ITER > MITERD0 .AND. NCONV == 1 ) MODEL = 2
                ENDIF
C
              ELSEIF( MODEL == 2 ) THEN
C
                IF( AVR < EPSD ) THEN
                  RKF = RMUD * FN / EPSD
                  MODE = 1
                ELSE
                  RKF = RMUD * FN / AVR
                  IF( ITER > MITERD1 .AND. NCONV == 1 ) THEN
                    EPS0R = AVR
                    MODE = 3
                  ELSE
                    MODE = 2
                  ENDIF
                ENDIF
C
              ENDIF
C
            ELSEIF( MODE == 3 ) THEN
C
              RKF = RMUD * FN / EPS0R
C
            ENDIF
C
          ENDIF
C
          FT(:) = RKF * VR(:)
C
        ELSEIF( IDYN == 0 ) THEN
C
          RKF = RMUD * FN / AVR0 * 1.D-1
C
          FT(:) = RKF * VR(:)
C
        ENDIF
C
      ENDIF
C
      IF( IFCQ == 0 ) THEN
C
        KN(1:3) = IELC(:)
C
        DO I = 1, 3
          FRCI(:,KN(I)) = FRCI(:,KN(I)) - RL0(I) * FT(:)
        ENDDO
C
        FRCI(:,NS) = FRCI(:,NS) + FT(:)
C
      ELSE
C
        KN(1:2) = IELC(1:2)
C
        IF( IELC(1) == IELQ(1,IFCQ) ) THEN
          KN(3)=IELQ(3,IFCQ)
          KN(4)=IELQ(4,IFCQ)
        ELSEIF( IELC(1) == IELQ(2,IFCQ) ) THEN
          KN(3)=IELQ(1,IFCQ)
          KN(4)=IELQ(4,IFCQ)
        ELSEIF( IELC(1) == IELQ(3,IFCQ) ) THEN
          KN(3)=IELQ(1,IFCQ)
          KN(4)=IELQ(2,IFCQ)
        ELSEIF( IELC(1) == IELQ(4,IFCQ) ) THEN
          KN(3)=IELQ(2,IFCQ)
          KN(4)=IELQ(3,IFCQ)
        ENDIF
C
        FRCI(:,KN(1)) = FRCI(:,KN(1)) - RL0(1) * FT(:)
     &                    - .25D0*RL0(3)*FT(:)
        FRCI(:,KN(2)) = FRCI(:,KN(2)) - RL0(2) * FT(:)
     &                    - .25D0*RL0(3)*FT(:)
        FRCI(:,KN(3)) = FRCI(:,KN(3)) - .25D0*RL0(3)*FT(:)
        FRCI(:,KN(4)) = FRCI(:,KN(4)) - .25D0*RL0(3)*FT(:)
C
        FRCI(:,NS) = FRCI(:,NS) + FT(:)
C
      ENDIF
C
      END
