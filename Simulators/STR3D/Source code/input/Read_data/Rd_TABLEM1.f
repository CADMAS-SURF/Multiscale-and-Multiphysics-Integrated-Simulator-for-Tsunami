      SUBROUTINE RD_TABLEM1( ITM1, TBM1, ITBM1R, I1, I2, CHAR, ITI )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      DIMENSION TBM1(2,*), ITBM1R(*), X(4), Y(4)
C
      READ(CHAR,'(BN,8X,I8)') ID
      ITBM1R(ID) = I1
C
      DO
        READ(ITI,'(A80)') CHAR
        CALL TB_END( N, CHAR )
        READ(CHAR,'(BN,8X,8F8.0)') ( X(I), Y(I), I = 1, N )
        TBM1(1,I2+1:I2+N) = X(1:N)
        TBM1(2,I2+1:I2+N) = Y(1:N)
        I2 = I2 + N
        IF( N < 4 ) EXIT
      ENDDO
C
      ITM1 = I2
C
      END