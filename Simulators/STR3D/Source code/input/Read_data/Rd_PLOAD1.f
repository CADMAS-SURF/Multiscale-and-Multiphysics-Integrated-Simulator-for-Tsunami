      SUBROUTINE RD_PLOAD1( J_PL1, R_PL1, CHAR, ITO )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      DIMENSION J_PL1(3)
C
      READ(CHAR,'(BN,8X,2I8,24X,F8.0)') J_PL1(1:2), R_PL1
C
      DO I = 25, 31
        IF( CHAR(I:I+1) == 'FX' ) THEN
          J_PL1(3) = 1
          RETURN
        ELSEIF( CHAR(I:I+1) == 'FY' ) THEN
          J_PL1(3) = 2
          RETURN
        ELSEIF( CHAR(I:I+1) == 'FZ' ) THEN
          J_PL1(3) = 3
          RETURN
        ENDIF
      ENDDO
C
      WRITE(ITO,*) 'TYPE ERROR, STOP IN SUB. PLOAD1.'
      CALL ERRSTP(90,ITO)
C
      END