      SUBROUTINE STRHX2(EPS,SIG,EPSG,SIGG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EPSG(6,3,3,3),SIGG(6,3,3,3),EPS(6,20),SIG(6,20),IG(3,20)
      INCLUDE 'gauss_ln_3.h'
      DATA IG / 1,1,1, 3,1,1, 3,3,1, 1,3,1,
     &          1,1,3, 3,1,3, 3,3,3, 1,3,3,
     &          2,1,1, 3,2,1, 2,3,1, 1,2,1,
     &          1,1,2, 3,1,2, 3,3,2, 1,3,2,
     &          2,1,3, 3,2,3, 2,3,3, 1,2,3 /

      FAC = 1.D0 / XG(3,3)

      DO I = 1, 20
        II = IG(1,I)
        JJ = IG(2,I)
        KK = IG(3,I)
        EPS(:,I) = (1.D0 - FAC) * EPSG(:,2,2,2) + FAC * EPSG(:,II,JJ,KK)
        SIG(:,I) = (1.D0 - FAC) * SIGG(:,2,2,2) + FAC * SIGG(:,II,JJ,KK)
      ENDDO

      END
