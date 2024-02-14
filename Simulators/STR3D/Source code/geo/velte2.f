      SUBROUTINE VELTE2(VEL,VELG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION VEL(3,10),VELG(3,5),II(2,10)
      DATA II / 8*0,  1,2,  2,3,  3,1,  1,4,  2,4,  3,4 /

      DO I = 1, 4
        VEL(:,I) = 3.D0 * VELG(:,I) - 2.D0 * VELG(:,5)
      ENDDO

      DO I = 5, 10
        VEL(:,I) = 1.5D0 * ( VELG(:,II(1,I)) + VELG(:,II(2,I)) )
     &           - 2.D0 * VELG(:,5)
      ENDDO

      END
