      SUBROUTINE TRFACE(TR)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION TR(12,15)

      TR(:,:) = 0.

      DO I = 1, 6
        TR(I,I) = 1.D0
      ENDDO

      DO J = 1, 4
        JP = 3 * ( J - 1 )
        DO I = 1, 3
          TR(6+I,JP+I) = .25D0
        ENDDO
      ENDDO

      DO I = 10, 12
        TR(I,3+I) = 1.D0
      ENDDO

      END
