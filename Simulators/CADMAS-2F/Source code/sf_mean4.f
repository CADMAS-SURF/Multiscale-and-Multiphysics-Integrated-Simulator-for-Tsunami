      SUBROUTINE SF_MEAN4(A,B,N,IDX,M)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N),B(N,*),IDX(M)

      A(:) = 0.

      DO I = 1, M
        A(:) = A(:) + B(:,IDX(I))
      ENDDO

      A(:) = A(:) / DBLE(M)

      END
