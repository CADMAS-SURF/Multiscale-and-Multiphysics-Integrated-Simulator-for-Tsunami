      SUBROUTINE TREDGE(TR)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION TR(9,15)

      TR(:,:) = 0.

      DO I = 1, 3
        TR(I,I) = 1.D0
      ENDDO

      DO J = 1, 4
        JP = 3 * ( J - 1 )
        DO I = 1, 3
          TR(3+I,JP+I) = .25D0
        ENDDO
      ENDDO

      DO I = 7, 9
        TR(I,6+I) = 1.D0
      ENDDO

      END
