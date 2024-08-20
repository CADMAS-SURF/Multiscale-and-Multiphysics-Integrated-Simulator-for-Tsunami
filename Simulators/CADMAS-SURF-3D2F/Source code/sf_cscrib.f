      SUBROUTINE SF_CSCRIB(XG,RMAX,X)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(2,3),XG(2)

      XG(:) = ( X(:,1) + X(:,2) + X(:,3) ) / 3.D0

      RMAX = 0.

      DO I = 1, 3
        CALL SF_LENGTH(R,X(1,I),XG,2)
        IF( R > RMAX ) RMAX = R
      ENDDO

      END
