      SUBROUTINE RD_PBAR( J_PBAR, R_PBAR, CHAR, ITI )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION J_PBAR(3), R_PBAR(6)
C
      READ(CHAR,'(BN,8X,2I8,4F8.0)') J_PBAR(1:2), R_PBAR(1:4)
C
      READ(ITI,'(A80)') CHAR
C
      IF( CHAR(1:8) /= BLK ) THEN
        BACKSPACE( ITI )
        RETURN
      ENDIF
C
      READ(ITI,'(A80)') CHAR
C
      IF( CHAR(1:8) /= BLK ) THEN
        BACKSPACE( ITI )
        RETURN
      ENDIF
C
      READ(CHAR,'(BN,8X,2F8.0)') R_PBAR(5:6)
C
      END
