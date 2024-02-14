      SUBROUTINE RD_TEMPD( J_TMPD, R_TMPD, IP, CHAR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      DIMENSION J_TMPD(*), R_TMPD(*), ID(3), T(3)
C
      READ(CHAR,'(BN,8X,3(I8,F8.0))') ( ID(I), T(I), I = 1, 3 )
C
      DO I = 1, 3
        IF( ID(I) == 0 ) EXIT
        IP = IP + 1
        J_TMPD(IP) = ID(I)
        R_TMPD(IP) = T(I)
      ENDDO
C
      END