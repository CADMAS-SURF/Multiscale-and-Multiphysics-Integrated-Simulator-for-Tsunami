      SUBROUTINE CT_TABLEM1( N2, ITI )
C
      CHARACTER*80 CHAR
C
      DO
        READ(ITI,'(A80)') CHAR
        CALL TB_END( N, CHAR )
        N2 = N2 + N
        IF( N < 4 ) EXIT
      ENDDO
C
      END
