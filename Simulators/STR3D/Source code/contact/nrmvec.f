      SUBROUTINE NRMVEC(RN,X)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(3,3),V12(3),V13(3),RN(3)

      V12(:) = X(:,2) - X(:,1)
      V13(:) = X(:,3) - X(:,1)
      CALL CROSS2(V12,V13,RN)

      END
