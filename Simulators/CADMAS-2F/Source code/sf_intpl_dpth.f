      SUBROUTINE SF_INTPL_DPTH(DELZ,DZ,LNDC,I,IM,IP,J,JM,JP,XL,X,YL,Y)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'

      DIMENSION DZ(NUMI,NUMJ),LNDC(NUMI,NUMJ),RL(3),H(4)

      IF( IM == I - 1 ) THEN
        IO = 0
      ELSE
        IO = 1
      ENDIF

      IF( JM == J - 1 ) THEN
        JO = 0
      ELSE
        JO = 1
      ENDIF

      IJO = 1 * IO + 2 * JO

      N = 0

      DO K = IM, IP
        DO L = JM, JP
          IF( LNDC(K,L) == 2 ) N = N + 1
        ENDDO
      ENDDO

      IF( LNDC(I,J) < 2 ) THEN
        DELZ = 0.D0
        RETURN
      ENDIF

      SELECT CASE( N )
      CASE( 1 )
        DELZ = DZ(I,J)
      CASE( 2 )
        IF( IO == 0 .AND. LNDC(IM,J) == 2 .OR.
     &      IO == 1 .AND. LNDC(IP,J) == 2 ) THEN
          DELZ = ( ( XL - X ) * DZ(IM,J) + X * DZ(IP,J) ) / XL
        ELSEIF( JO == 0 .AND. LNDC(I,JM) == 2 .OR.
     &          JO == 1 .AND. LNDC(I,JP) == 2 ) THEN
          DELZ = ( ( YL - Y ) * DZ(I,JM) + Y * DZ(I,JP) ) / YL
        ELSE
          DELZ = DZ(I,J)
        ENDIF
      CASE( 3 )
        IF( IJO==0 .AND. LNDC(IM,J)==2 .AND. LNDC(I,JM)==2 ) THEN
          RL(1) = ( XL - X ) / XL
          RL(2) = ( YL - Y ) / YL
          RL(3) = 1.D0 - RL(1) - RL(2)
          DELZ = RL(1) * DZ(IM,J) + RL(2) * DZ(I,JM) + RL(3) * DZ(I,J)
        ELSEIF( IJO==1 .AND. LNDC(IP,J)==2 .AND. LNDC(I,JM)==2 ) THEN
          RL(1) = ( YL - Y ) / YL
          RL(2) = X / XL
          RL(3) = 1.D0 - RL(1) - RL(2)
          DELZ = RL(1) * DZ(I,JM) + RL(2) * DZ(IP,J) + RL(3) * DZ(I,J)
        ELSEIF( IJO==2 .AND. LNDC(IM,J)==2 .AND. LNDC(I,JP)==2 ) THEN
          RL(1) = Y / YL
          RL(2) = ( XL - X ) / XL
          RL(3) = 1.D0 - RL(1) - RL(2)
          DELZ = RL(1) * DZ(I,JP) + RL(2) * DZ(IM,J) + RL(3) * DZ(I,J)
        ELSEIF( IJO==3 .AND. LNDC(IP,J)==2 .AND. LNDC(I,JP)==2 ) THEN
          RL(1) = X / XL
          RL(2) = Y / YL
          RL(3) = 1.D0 - RL(1) - RL(2)
          DELZ = RL(1) * DZ(IP,J) + RL(2) * DZ(I,JP) + RL(3) * DZ(I,J)
        ELSE
          IF( IO == 0 .AND. LNDC(IM,J) == 2 .OR.
     &        IO == 1 .AND. LNDC(IP,J) == 2 ) THEN
            DELZ = ( ( XL - X ) * DZ(IM,J) + X * DZ(IP,J) ) / XL
          ELSEIF( JO == 0 .AND. LNDC(I,JM) == 2 .OR.
     &            JO == 1 .AND. LNDC(I,JP) == 2 ) THEN
            DELZ = ( ( YL - Y ) * DZ(I,JM) + Y * DZ(I,JP) ) / YL
          ENDIF
        ENDIF
      CASE( 4 )
        CALL SF_HQU1(H,XL,YL,X,Y)
        DELZ = H(1) * DZ(IM,JM) + H(2) * DZ(IP,JM)
     &       + H(3) * DZ(IP,JP) + H(4) * DZ(IM,JP)
      END SELECT

      END
