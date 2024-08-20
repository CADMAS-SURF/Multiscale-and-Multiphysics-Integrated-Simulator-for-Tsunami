      SUBROUTINE SF_MEAN1(XM,X,N)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X(N)

      DO I = 2, N
        XX = X(I)
        DO J = 1, I - 1
          IF( XX < X(J) ) THEN
            X(J+1:I) = X(J:I-1)
            X(J) = XX
            EXIT
          ENDIF
        ENDDO
      ENDDO

      XM = 0.

      DO I = 1, N
        XM = XM + X(I)
      ENDDO

      XM = XM / DBLE(N)

      END