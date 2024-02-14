      SUBROUTINE RD_TEMP( J_TMP, R_TMP, IP, CHAR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      DIMENSION J_TMP(2,*), R_TMP(*), IG(3), T(3)
C
      READ(CHAR,'(BN,8X,I8,3(I8,F8.0))') IDS, ( IG(I), T(I), I = 1, 3 )
C
      DO I = 1, 3
        IF( IG(I) == 0 ) EXIT
        IP = IP + 1
        J_TMP(1,IP) = IDS
        J_TMP(2,IP) = IG(I)
        R_TMP(IP) = T(I)
      ENDDO
C
      END