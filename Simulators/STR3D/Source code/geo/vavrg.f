      SUBROUTINE VAVRG(VG,VE,KN,ND,N)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KN(ND),N(*),VG(3,*),VE(3,ND)

      DO I = 1, ND
        NODE = KN(I)
        N(NODE) = N(NODE) + 1
        A = 1.D0 / DBLE( N(NODE) )
        VG(:,NODE) = VG(:,NODE) * ( 1.D0 - A ) + VE(:,I) * A
      ENDDO

      END
