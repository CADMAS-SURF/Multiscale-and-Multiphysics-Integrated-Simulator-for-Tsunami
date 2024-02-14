      SUBROUTINE SF_TRNS2(X,X0,TR)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION DX(2,3),X(2,3),X0(2),TR(2,2)

      DO I = 1, 3
        DX(:,I) = X(:,I) - X0(:)
      ENDDO

      CALL SF_AXB(X,TR,DX,2,2,3)

      END
