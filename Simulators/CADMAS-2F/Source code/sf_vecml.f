      SUBROUTINE SF_VECML(D3,D1,D2,N)
C
C 1.PURPOSE
C         INTERIOR PRODUCT FOR TWO VECTOR
C
C 3.ARGUMENT
C   I D1      D VECTOR
C   I D2      D VECTOR
C   O D3      D CALCULATED VECTOR
C   I N       I DIMENSION FOR VECTOR (=3)
C
C 8.PROGRAM
C    KITAGAWA KAZUSHI (1988-03-10)  MI(910708)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION D1(N),D2(N)
C
      D3        =  0.
      DO  100   I  =  1 , N
          D3    =  D3  +  D1(I)  *  D2(I)
  100 CONTINUE
C
      RETURN
      END
