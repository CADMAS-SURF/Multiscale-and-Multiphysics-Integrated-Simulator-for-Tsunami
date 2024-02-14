      SUBROUTINE VELTE1(VEL,VELG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION VEL(3,4),VELG(3)

      DO I = 1, 4
        VEL(:,I) = VELG(:)
      ENDDO

      END
