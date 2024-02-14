      SUBROUTINE RMULT5(A,RB,B,RC,C,N)
C
C     {A}=RB*{B}+RC*{C}
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(*),B(*),C(*)
C
      DO I=1,N
        A(I)=RB*B(I)+RC*C(I)
      ENDDO
C
      RETURN
      END
