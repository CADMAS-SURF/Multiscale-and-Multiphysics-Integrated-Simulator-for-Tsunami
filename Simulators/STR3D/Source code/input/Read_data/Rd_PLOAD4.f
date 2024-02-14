      SUBROUTINE RD_PLOAD4( J_PL4, R_PL4, CHAR, ITI )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      LOGICAL THRU
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION J_PL4(6), R_PL4(4), RN(3)
C
      IF( THRU( CHAR(57:64) ) ) THEN
        READ(CHAR,'(BN,8X,2I8,F8.0,32X,I8)') IDS, IDE1, P1, IDE2
        IG1 = 0
        IG3 = 0
      ELSE
        READ(CHAR,'(BN,8X,2I8,F8.0,24X,2I8)') IDS, IDE1, P1, IG1, IG3
        IDE2 = IDE1
      ENDIF
C
      READ(ITI,'(A80)') CHAR
C
      IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
        READ(CHAR,'(BN,8X,I8,3F8.0)') IDC, RN(:)
      ELSE
        BACKSPACE(ITI)
        IDC = 0
        RN(:) = 0.
      ENDIF
C
      J_PL4(1) = IDS
      J_PL4(2) = IDE1
      J_PL4(3) = IDE2
      J_PL4(4) = IG1
      J_PL4(5) = IG3
      J_PL4(6) = IDC
C
      R_PL4(1) = P1
      R_PL4(2:4) = RN(:)
C
      END