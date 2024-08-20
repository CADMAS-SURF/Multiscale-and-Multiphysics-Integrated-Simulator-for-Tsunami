      SUBROUTINE SF_RESET_FF(FF,NFP,NF,DBUF)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION NFP(NUMI,NUMJ,NUMK),NF(NUMI,NUMJ,NUMK)
     &         ,FF(NUMI,NUMJ,NUMK),DBUF(*)

      DO K = 2, NUMK - 1
        DO J = MYJS, MYJE
          DO I = MYIS, MYIE

!           IF( NF(I,J,K) == -2 ) THEN
            IF( NF(I,J,K) < 0 ) THEN  ! FOR CADMAS-STM

              FF(I,J,K) = 0.

!           ELSEIF( NFP(I,J,K) == -2 .AND. NF(I,J,K) == 0 ) THEN
            ELSEIF( NFP(I,J,K) < 0 .AND. NF(I,J,K) == 0 ) THEN  ! FOR CADMAS-STM

              DO IDR = 1, 3

                IF( IDR /= 2 ) THEN
                  IC = 0
                  FFA = 0.
                ENDIF

                DO IP = -1, 1, 2

                  SELECT CASE( IDR )
                  CASE( 1 )
                    II = I + IP
                    JJ = J
                    KK = K
                  CASE( 2 )
                    II = I
                    JJ = J + IP
                    KK = K
                  CASE( 3 )
                    II = I
                    JJ = J
                    KK = K + IP
                  END SELECT
        
                  IF( NFP(II,JJ,KK) >= 0 .AND. NF(II,JJ,KK) >= 0 ) THEN
                    IC = IC + 1
                    FFA = FFA + FF(II,JJ,KK)
                  ENDIF

                ENDDO

                IF( IDR >= 2 .AND. IC > 0 ) THEN
                  FF(I,J,K) = FFA / DBLE(IC)
                  EXIT
                ENDIF

              ENDDO

              IF( IDR == 4 ) FF(I,J,K) = 1.D0

            ENDIF

          ENDDO
        ENDDO
      ENDDO

      CALL VF_P3SRD2(FF,DBUF,0)

      END
