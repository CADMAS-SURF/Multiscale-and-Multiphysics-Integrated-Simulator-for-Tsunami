      SUBROUTINE SF_TRM1(TR,X0,X2,X)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION V12(3),V13(3),X(3,3),E(3,3),TR(3,3),X0(3),X2(2,3)

      V12(:) = X(:,2) - X(:,1)
      V13(:) = X(:,3) - X(:,1)

      CALL SF_DIRCOS(E(1,2),V12,3)
      CALL SF_CROSS2(V13,V12,E(1,3))
      CALL SF_CROSS2(E(1,2),E(1,3),E(1,1))

      DO I = 1, 3
        TR(I,:) = E(:,I)
      ENDDO

      X0(:) = X(:,1)

      X2(:,:) = 0.

      CALL SF_VECML(X2(2,2),V12,E(1,2),3)
      CALL SF_VECML(X2(1,3),V13,E(1,1),3)
      CALL SF_VECML(X2(2,3),V13,E(1,2),3)

      END
