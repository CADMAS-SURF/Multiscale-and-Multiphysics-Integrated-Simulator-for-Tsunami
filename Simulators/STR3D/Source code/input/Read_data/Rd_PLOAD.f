      SUBROUTINE RD_PLOAD( J_PL, R_PL, CHAR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      DIMENSION J_PL(5), IG(4)
C
      READ(CHAR,'(BN,8X,I8,F8.0,4I8)') IDS, P, IG(:)
C
      J_PL(1) = IDS
      J_PL(2:5) = IG(:)
      R_PL = P
C
      END