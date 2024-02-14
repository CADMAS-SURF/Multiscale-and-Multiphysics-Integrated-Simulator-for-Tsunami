      SUBROUTINE ESTFFACE(ND,KN,ESTF,NS,IELC,IFCQ,IELQ,U0,RL0,MODE,ISTK
     &                   ,MODEL,RMU0,AVR0,FN,RKF,UG,DT,IFMDL,MITER0
     &                   ,IDYN,ITER)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KN(5),DU(3,4),UG(6,*),U0(3,4),IELC(3),RL0(3),VR(3)
     &         ,A(3,3),RK(12,12),ESTF(*),TR(12,15),WK(15,12),IELQ(4,*)
C-----------------------------------------------------------------------
      ND = 4
C
      KN(1:3) = IELC(:)
      KN(4) = NS
C
      DU(:,:) = UG(1:3,KN(1:4)) - U0(:,:)
C
      VR(:) = DU(:,4)
      DO I = 1, 3
        VR(:) = VR(:) - RL0(I) * DU(:,I)
      ENDDO
      VR(:) = VR(:) / DT
C
      CALL VECML2(AVR,VR,3)
C
      IF( IFMDL == 1 ) THEN
C
        IF( AVR / AVR0 < 1.D-10 ) THEN
C
          A(:,:) = 0.
C
          DO I = 1, 3
            A(I,I) = RMU0 / DASIN(1.D0) * FN / AVR0
          ENDDO
C
        ELSE
C
          C1 = RMU0 / DASIN(1.D0) * FN / AVR
          C2 = ( 1.D0 / ( 1.D0 + ( AVR / AVR0 ) * ( AVR / AVR0 ) ) /AVR0
     &           - DATAN( AVR / AVR0 ) / AVR ) / AVR
          C3 = DATAN( AVR / AVR0 )
C
          DO I = 1, 3
            DO J = 1, 3
              A(I,J) = VR(I) * VR(J) * C2
              IF( I == J ) A(I,J) = A(I,J) + C3
            ENDDO
          ENDDO
C
          A(:,:) = C1 * A(:,:)
C
        ENDIF
C
      ELSEIF( IFMDL == 2 ) THEN
C
        IF( IDYN == 1 ) THEN
C
          A(:,:) = 0.
C
          DO I = 1, 3
            A(I,I) = RKF
          ENDDO
C
          IF( ( ISTK == 1 .AND. MODE == 2 .AND. ITER <= MITER0 ) .OR.
     &        ( ISTK == 0 .AND. MODE == 2 .AND. MODEL == 1 ) ) THEN
C
            DO I = 1, 3
              DO J = 1, 3
                A(I,J) = A(I,J) -  RKF * VR(I) * VR(J) / ( AVR * AVR )
              ENDDO
            ENDDO
C
          ENDIF
C
        ELSEIF( IDYN == 0 ) THEN
C
          A(:,:) = 0.
C
          DO I = 1, 3
            A(I,I) = RKF
          ENDDO
C
        ENDIF
C
      ENDIF
C
      DO I = 1, 4
C
        IP = 3*(I-1) + 1
C
        IF( I == 4 ) THEN
          FI = 1.D0
        ELSE
          FI = -RL0(I)
        ENDIF
C
        DO J = I, 4
C
          JP = 3*(J-1) + 1
C
          IF( J == 4 ) THEN
            FJ = 1.D0
          ELSE
            FJ = -RL0(J)
          ENDIF       
C   
          RK( IP:IP+2, JP:JP+2 ) = FI * FJ * A(:,:)
C
        ENDDO
C
      ENDDO
C
      RK(:,:) = RK(:,:) / DT
C
      IP = 0
C
      DO I = 1, 12
        DO J = I, 12
          IP = IP + 1
          ESTF(IP) =  RK(I,J)
        ENDDO
      ENDDO
C
      IF( IFCQ == 0 ) RETURN
C
      ND = 5
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
      KN(5) = NS
C
      CALL TRFACE(TR)
C
      CALL MATML(WK,2,TR,3,ESTF,1,15,12,12)
      CALL MATML(ESTF,1,WK,2,TR,2,15,15,12)
C
      END
