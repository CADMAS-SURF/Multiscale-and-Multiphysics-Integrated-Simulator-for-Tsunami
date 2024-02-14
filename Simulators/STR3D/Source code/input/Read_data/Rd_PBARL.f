      SUBROUTINE RD_PBARL( J_PBAR, R_PBAR, CHAR, ITI, ITO )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      DIMENSION J_PBAR(3), R_PBAR(6)
C
      READ(CHAR,'(BN,8X,2I8)') J_PBAR(1:2)
C
      DO I = 33, 40
        IF( CHAR(I:I+2) == 'ROD' ) THEN
          J_PBAR(3) = 1
          GOTO 10
        ELSEIF( CHAR(I:I+2) == 'BAR' ) THEN
          J_PBAR(3) = 2
          GOTO 10
        ELSEIF( CHAR(I:I+3) == 'TUBE' ) THEN
          J_PBAR(3) = 3
          GOTO 10
        ENDIF
      ENDDO
C
      WRITE(ITO,*) 'TYPE ERROR, STOP IN SUB. RD_PBARL.'
      CALL ERRSTP(90,ITO)
C
   10 READ(ITI,'(BN,8X,2F8.0)') R_PBAR(1:2)
C
      END
