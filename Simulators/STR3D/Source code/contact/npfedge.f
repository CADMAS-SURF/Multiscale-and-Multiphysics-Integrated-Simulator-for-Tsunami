      SUBROUTINE NPFEDGE(MODE,FN,RKF,FRCI,NS,IG,IEDQ,IELQ,U0,RL0,RMU0
     &                  ,AVR0,RMUD,UG,POS,F,DT,IFMDL,EPSD,IDYN)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DU(3,3),UG(6,*),U0(3,3),IG(2),RL0(2),VR(3),FT(3)
     &         ,FRCI(3,*),V(3),POS(3,*),E(3),F(3),KN(4),IELQ(4,*)
C-----------------------------------------------------------------------
      V(:) = POS(:,IG(2)) - POS(:,IG(1))
      CALL DIRCOS(E,V,3)
C
      CALL CROSS1(E,F,FN)
C
      DU(:,1:2) = UG(1:3,IG(:)) - U0(:,1:2)
      DU(:,3)   = UG(1:3,NS) - U0(:,3)
C
      VR(:) = DU(:,3)
      DO I = 1, 2
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
        IF( MODE == 1 ) THEN
C
          IF( IDYN == 1 ) THEN
            RKF = RMUD * FN / EPSD
          ELSEIF( IDYN == 0 ) THEN
            RKF = RMUD * FN / AVR0 * 1.D-1
          ENDIF
C
          MODE = 2
C
        ENDIF
C
        FT(:) = RKF * VR(:)
C
      ENDIF
C
      IF( IEDQ == 0 ) THEN
C
        KN(1:2) = IG(:)
C
        DO I = 1, 2
          FRCI(:,KN(I)) = FRCI(:,KN(I)) - RL0(I) * FT(:)
        ENDDO
C
        FRCI(:,NS) = FRCI(:,NS) + FT(:)
C
      ELSE
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
        FRCI(:,KN(1)) = FRCI(:,KN(1)) - RL0(1) * FT(:)
     &                    - .25D0*RL0(2)*FT(:)
        FRCI(:,KN(2)) = FRCI(:,KN(2)) - .25D0*RL0(2)*FT(:)
        FRCI(:,KN(3)) = FRCI(:,KN(3)) - .25D0*RL0(2)*FT(:)
        FRCI(:,KN(4)) = FRCI(:,KN(4)) - .25D0*RL0(2)*FT(:)
C
        FRCI(:,NS) = FRCI(:,NS) + FT(:)
C
      ENDIF
C
      END
