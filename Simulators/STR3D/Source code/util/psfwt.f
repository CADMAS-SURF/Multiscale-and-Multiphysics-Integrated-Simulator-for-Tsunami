      SUBROUTINE PSFWT(IST,P1,P2,ISTEP,NSTEP,ITER,IFL)
C
      IMPLICIT REAL*8(A-H,O-Z)
C-----------------------------------------------------------------------
      RETURN
C
      REWIND(IFL)
C
      WRITE(IFL,1000) IST,P1,P2,ISTEP,NSTEP,ITER
 1000 FORMAT(I1,',',F5.2,',',F5.2,',',I8,',',I8,',',I8)
C
      END
