      SUBROUTINE RD_SUBCASE( ISUB, ITI )
C
      CHARACTER*80 CHAR
      DIMENSION ISUB(13)
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:7) == 'SUBCASE' .OR. CHAR(1:10) == 'BEGIN BULK' )THEN
          BACKSPACE(ITI)
          RETURN
        ENDIF
C
        CALL GET_COMM( IP, IS, IE, CHAR )
C
        IF( IP == 0 ) CYCLE
C
        IF( CHAR(IS:IE) == 'LOAD' ) THEN
          READ(CHAR(IP+1:80),*) ISUB(1)
C
        ELSEIF( CHAR(IS:IE) == 'SPC' ) THEN
          READ(CHAR(IP+1:80),*) ISUB(2)
C
        ELSEIF( CHAR(IS:IE) == 'MPC' ) THEN
          READ(CHAR(IP+1:80),*) ISUB(3)
C
        ELSEIF( CHAR(IS:IE) == 'METHOD' ) THEN
          READ(CHAR(IP+1:80),*) ISUB(4)
C
        ELSEIF( CHAR(IS:IE) == 'TEMPERATURE(LOAD)' ) THEN
          READ(CHAR(IP+1:80),*) ISUB(5)
C
        ELSEIF( CHAR(IS:IE) == 'TEMPERATURE(BOTH)' .OR.
     &          CHAR(IS:IE) == 'TEMP' ) THEN
          READ(CHAR(IP+1:80),*) ISUB(5)
          ISUB(10) = ISUB(5)
C
        ELSEIF( CHAR(IS:IE) == 'DLOAD' ) THEN
          READ(CHAR(IP+1:80),*) ISUB(7)
C
        ELSEIF( CHAR(IS:IE) == 'FREQUENCY' ) THEN
          READ(CHAR(IP+1:80),*) ISUB(9)
C
        ELSEIF( CHAR(IS:IE) == 'NLPARM' ) THEN
          READ(CHAR(IP+1:80),*) ISUB(11)
C
        ELSEIF( CHAR(IS:IE) == 'TSTEP' ) THEN
          READ(CHAR(IP+1:80),*) ISUB(12)
C
        ELSEIF( CHAR(IS:IE) == 'BCSET' ) THEN
          READ(CHAR(IP+1:80),*) ISUB(13)
C
        ENDIF
C
      ENDDO
C
      END
