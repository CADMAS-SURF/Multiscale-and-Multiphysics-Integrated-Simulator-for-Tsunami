      SUBROUTINE ENPFADDG(FT,GE,FC,NDF,ND,KN)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION FT(6,*),KN(ND),FC(NDF,*)
C-----------------------------------------------------------------------
      FT(1:NDF,KN(:)) = FT(1:NDF,KN(:)) + GE * FC(:,1:ND)

      END