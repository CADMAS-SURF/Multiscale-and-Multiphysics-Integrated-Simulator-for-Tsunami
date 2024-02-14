      SUBROUTINE RD_PLOAD2( J_PL4, R_PL4, I_PL4, CHAR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      LOGICAL THRU
      CHARACTER*80 CHAR
      DIMENSION J_PL4(6,*), R_PL4(4,*), IDE(6)
C
      IF( THRU( CHAR(33:40) ) ) THEN
        READ(CHAR,'(BN,8X,I8,F8.0,I8,8X,I8)') IDS, P, IDE1, IDE2
        I_PL4 = I_PL4 + 1
        J_PL4(1,I_PL4) = IDS
        J_PL4(2,I_PL4) = IDE1
        J_PL4(3,I_PL4) = IDE2
        R_PL4(1,I_PL4) = P
      ELSE
        READ(CHAR,'(BN,8X,I8,F8.0,6I8)') IDS, P, IDE(:)
        DO I = 1, 6
          IF( IDE(I) == 0 ) EXIT
          I_PL4 = I_PL4 + 1
          J_PL4(1,I_PL4) = IDS
          J_PL4(2,I_PL4) = IDE(I)
          J_PL4(3,I_PL4) = IDE(I)
          R_PL4(1,I_PL4) = P
        ENDDO
      ENDIF
C
      END