      SUBROUTINE SF_TRNS1(X,EZ,X0,TR)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X(3,3),X0(3),DX(3,3),TR(3,3),V12(3),V13(3),E(3)

      DO I = 1, 3
        DX(:,I) = X(:,I) - X0(:)
      ENDDO

      CALL SF_AXB(X,TR,DX,3,3,3)

      V12(:) = X(:,2) - X(:,1)
      V13(:) = X(:,3) - X(:,1)

      CALL SF_CROSS2(V12,V13,E)

      EZ = E(3)

      END
