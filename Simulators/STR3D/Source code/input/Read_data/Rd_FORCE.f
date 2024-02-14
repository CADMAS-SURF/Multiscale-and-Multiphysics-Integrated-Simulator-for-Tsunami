      SUBROUTINE RD_FORCE( J_FRC, R_FRC, CHAR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      DIMENSION J_FRC(3), R_FRC(6), RN(3)
C
      READ(CHAR,'(BN,8X,3I8,4F8.0)') IDS, IG, IDC, F, RN(:)
C
      J_FRC(1) = IDS
      J_FRC(2) = IG
      J_FRC(3) = IDC
C
      IF( CHAR(1:8) == 'FORCE   ' ) THEN
        R_FRC(1:3) = F * RN(:)
      ELSEIF( CHAR(1:8) == 'MOMENT  ' ) THEN
        R_FRC(4:6) = F * RN(:)
      ENDIF
C
      END
