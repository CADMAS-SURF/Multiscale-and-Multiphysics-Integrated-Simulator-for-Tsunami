      FUNCTION SF_RINT(X1,X2,I,J,X)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X1(*), X2(*)

!-----------------------------------------------------------------------
      SF_RINT = ( ( X2(I) - X ) * X1(J) + ( X - X1(I) ) * X2(J) )
     &       / ( X2(I) - X1(I) )

      END