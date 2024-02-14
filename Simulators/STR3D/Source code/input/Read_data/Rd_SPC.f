      SUBROUTINE RD_SPC( J_SPC, R_SPC, IP, CHAR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      CHARACTER*8  C(2)
      DIMENSION J_SPC(8,*), R_SPC(*), IG(2), D(2)
C
      READ(CHAR,'(BN,8X,I8,2(I8,A8,F8.0))') 
     &  IDS, ( IG(I), C(I), D(I), I = 1, 2 )
C
      DO I = 1, 2
        IF( IG(I) == 0 ) EXIT
        IP = IP + 1
        J_SPC(1,IP) = IDS
        J_SPC(2,IP) = IG(I)
        CALL RESFR( J_SPC(3,IP), C(I) )
        R_SPC(IP) = D(I)
      ENDDO
C
      END