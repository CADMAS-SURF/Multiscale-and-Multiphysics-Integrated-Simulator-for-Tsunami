      SUBROUTINE RD_SOLID( J_SOL, CHAR, ITI )
C
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION J_SOL(22)
C
      IP = 0
      IS = IP + 1
      IE = IP + 8
      READ(CHAR,'(BN,8X,8I8)') J_SOL(IS:IE)
C
      DO
        READ(ITI,'(A80)') CHAR
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
          IP = IP + 8
          IS = IP + 1
          IE = IP + 8
          IF( IE > 22 ) IE = 22
          READ(CHAR,'(BN,8X,8I8)') J_SOL(IS:IE)
        ELSE
          BACKSPACE(ITI)
          RETURN
        ENDIF
      ENDDO
C
      END
