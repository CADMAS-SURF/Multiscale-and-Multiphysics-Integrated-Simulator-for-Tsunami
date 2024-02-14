      SUBROUTINE SF_ADJOIN(IP,JP,KP,G,X1,X2,Y1,Y2,Z1,Z2)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION DX(-1:1),DY(-1:1),DZ(-1:1),DIST(-1:1,-1:1,-1:1)
     &         ,ISKIP(-1:1,-1:1,-1:1),IP(7),JP(7),KP(7),G(3)
!-----------------------------------------------------------------------
      DX(-1) = G(1) - X1
      DX( 0) = 0.
      DX( 1) = X2 - G(1)
      DY(-1) = G(2) - Y1
      DY( 0) = 0.
      DY( 1) = Y2 - G(2)
      DZ(-1) = G(3) - Z1
      DZ( 0) = 0.
      DZ( 1) = Z2 - G(3)

      IF( DX(-1) < DX(1) ) THEN
        IS = -1
        IE = 0
      ELSE
        IS = 0
        IE = 1
      ENDIF

      IF( DY(-1) < DY(1) ) THEN
        JS = -1
        JE = 0
      ELSE
        JS = 0
        JE = 1
      ENDIF

      IF( DZ(-1) < DZ(1) ) THEN
        KS = -1
        KE = 0
      ELSE
        KS = 0
        KE = 1
      ENDIF

      DO I = IS, IE
        DO J = JS, JE
          DO K = KS, KE
            DIST(I,J,K) = DSQRT(DX(I)*DX(I) + DY(J)*DY(J) + DZ(K)*DZ(K))
          ENDDO
        ENDDO
      ENDDO

      ISKIP(:,:,:) = 0

      ISKIP(0,0,0) = 1

      DO IR = 1, 7

        DIST_MIN = 1.D20

        DO I = IS, IE
          DO J = JS, JE
            DO K = KS, KE
              IF( ISKIP(I,J,K) == 1 ) CYCLE
              IF( DIST(I,J,K) < DIST_MIN ) THEN
                DIST_MIN = DIST(I,J,K)
                I0 = I
                J0 = J
                K0 = K
              ENDIF
            ENDDO
          ENDDO
        ENDDO

        IP(IR) = I0
        JP(IR) = J0
        KP(IR) = K0
        ISKIP(I0,J0,K0) = 1

      ENDDO

      END