      SUBROUTINE MEAN2(RMEAN,A,INDX,N)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(*),INDX(N)
C-----------------------------------------------------------------------
      RMEAN=0.
C
      DO I=1,N
        RMEAN = RMEAN + A(INDX(I))
      ENDDO
      RMEAN=RMEAN/N
C
      RETURN
      END
