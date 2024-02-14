      SUBROUTINE CT_BCTSET( N_BCTS, ITI )
C
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
C
      N_BCTS = N_BCTS + 1
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
          N_BCTS = N_BCTS + 1
        ELSE
          BACKSPACE(ITI)
          RETURN
        ENDIF
C
      ENDDO
C
      END
