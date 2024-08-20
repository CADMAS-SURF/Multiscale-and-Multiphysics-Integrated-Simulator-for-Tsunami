      SUBROUTINE SF_RESET_UU(UU,INDXP,INDX,IXYZ,DBUF)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION INDXP(NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
     &         ,UU(NUMI,NUMJ,NUMK),DBUF(*)

      DO K = 2, NUMK - 1
        DO J = MYJS, MYJE
          DO I = MYIS, MYIE

            IF( INDX(I,J,K) == -1 ) THEN

              UU(I,J,K) = 0.D0

            ELSEIF( INDXP(I,J,K) /= 0 .AND. INDX(I,J,K) == 0 ) THEN

              UU(I,J,K) = 0.D0

              CYCLE

              DO IDR = 1, 3

                IC = 0
                UUA = 0.

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
        
                  IF( INDXP(II,JJ,KK)==0 .AND. INDX(II,JJ,KK)==0 ) THEN
                    IC = IC + 1
                    UUA = UUA + UU(II,JJ,KK)
                  ENDIF

                ENDDO

                IF( IC > 0 ) THEN
                  UU(I,J,K) = UUA / DBLE(IC)
                  EXIT
                ENDIF

              ENDDO

              IF( IDR == 4 ) UU(I,J,K) = 0.D0

            ENDIF

          ENDDO
        ENDDO
      ENDDO

      CALL VF_P3SRD2(UU,DBUF,IXYZ)

      END
