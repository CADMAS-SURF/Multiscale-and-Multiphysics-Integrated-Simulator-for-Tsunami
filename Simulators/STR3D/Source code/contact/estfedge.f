      SUBROUTINE ESTFEDGE(ND,KN,ESTF,NS,IG,IEDQ,IELQ,U0,RL0,RMU0,AVR0
     &                   ,FN,RKF,UG,DT,IFMDL)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KN(5),DU(3,3),UG(6,*),U0(3,3),IG(2),RL0(2),VR(3)
     &         ,A(3,3),RK(9,9),ESTF(*),TR(9,15),WK(15,9),IELQ(4,*)
C-----------------------------------------------------------------------
      ND = 3
C
      KN(1:2) = IG(:)
      KN(3) = NS
C
      DU(:,:) = UG(1:3,KN(1:3)) - U0(:,:)
C
      VR(:) = DU(:,3)
      DO I = 1, 2
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
        A(:,:) = 0.
C
        DO I = 1, 3
          A(I,I) = RKF
        ENDDO
C
      ENDIF
C
      DO I = 1, 3
C
        IP = 3*(I-1) + 1
C
        IF( I == 3 ) THEN
          FI = 1.D0
        ELSE
          FI = -RL0(I)
        ENDIF
C
        DO J = I, 3
C
          JP = 3*(J-1) + 1
C
          IF( J == 3 ) THEN
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
      DO I = 1, 9
        DO J = I, 9
          IP = IP + 1
          ESTF(IP) =  RK(I,J)
        ENDDO
      ENDDO
C
      IF( IEDQ == 0 ) RETURN
C
      ND = 5
C
      KN(1) = IG(1)
C
      IF( IG(1) == IELQ(1,IEDQ) ) THEN
        KN(2)=IELQ(2,IEDQ)
        KN(3)=IELQ(3,IEDQ)
        KN(4)=IELQ(4,IEDQ)
      ELSEIF( IG(1) == IELQ(2,IEDQ) ) THEN
        KN(2)=IELQ(1,IEDQ)
        KN(3)=IELQ(3,IEDQ)
        KN(4)=IELQ(4,IEDQ)
      ELSEIF( IG(1) == IELQ(3,IEDQ) ) THEN
        KN(2)=IELQ(1,IEDQ)
        KN(3)=IELQ(2,IEDQ)
        KN(4)=IELQ(4,IEDQ)
      ELSEIF( IG(1) == IELQ(4,IEDQ) ) THEN
        KN(2)=IELQ(1,IEDQ)
        KN(3)=IELQ(2,IEDQ)
        KN(4)=IELQ(3,IEDQ)
      ENDIF
C
      KN(5) = NS
C
      CALL TREDGE(TR)
C
      CALL MATML(WK,2,TR,3,ESTF,1,15,9,9)
      CALL MATML(ESTF,1,WK,2,TR,2,15,15,9)
C
      END
