      SUBROUTINE CT_TSTEP( N2_TSTP, ITI )
C
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
C
      N2_TSTP = N2_TSTP + 1
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
          N2_TSTP = N2_TSTP + 1
        ELSE
          BACKSPACE(ITI)
          RETURN
        ENDIF
C
      ENDDO
C
      END
