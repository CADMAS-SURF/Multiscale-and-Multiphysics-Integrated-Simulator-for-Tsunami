      SUBROUTINE MEAN3(A,B,N,M)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N),B(N,M)

      A(:) = 0.

      DO I = 1, M
        A(:) = A(:) + B(:,I)
      ENDDO

      A(:) = A(:) / DBLE(M)

      END
