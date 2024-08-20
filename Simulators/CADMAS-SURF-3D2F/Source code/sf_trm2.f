      SUBROUTINE SF_TRM2(TR,X0,X)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION V(2,2),X(2,3),E(2,2),TR(2,2,2),T(2,2),X0(2,2)

      V(:,1) = X(:,3) - X(:,2)
      V(:,2) = X(:,1) - X(:,3)
      
      DO I = 1, 2

        CALL SF_DIRCOS(E(1,2),V(1,I),2)
      
        E(1,1) =  E(2,2)
        E(2,1) = -E(1,2)

        TR(1,:,I) = E(:,1)
        TR(2,:,I) = E(:,2)

      ENDDO

      CALL SF_AXBT(T,TR(1,1,2),TR(1,1,1),2,2,2)

      TR(:,:,2) = T(:,:)

      CALL SF_LENGTH(Y2,X(1,1),X(1,2),2)
      CALL SF_LENGTH(Y3,X(1,2),X(1,3),2)

      X0(1,1) = 0.
      X0(2,1) = Y2

      X0(1,2) = 0.
      X0(2,2) = Y3

      END
