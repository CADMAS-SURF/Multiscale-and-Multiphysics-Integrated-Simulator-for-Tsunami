      SUBROUTINE SF_LENGTH(RLEN,GRID1,GRID2,N)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION GRID1(N),GRID2(N)
C----&------------------------------------------------------------------
      RLEN = 0.
C
      DO 100 I=1,N
        RLEN = RLEN + ( GRID1(I)-GRID2(I) ) * ( GRID1(I)-GRID2(I) )
  100 CONTINUE
C
      RLEN = DSQRT( RLEN )
C
      RETURN
      END
