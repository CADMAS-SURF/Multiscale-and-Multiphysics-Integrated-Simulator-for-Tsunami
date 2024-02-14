      SUBROUTINE RD_MATS1( J_MATS1, R_MATS1, CHAR, ITO )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      DIMENSION J_MATS1(2), R_MATS1(3)
C
      READ(CHAR,'(BN,8X,I8,16X,F8.0,I8,8X,2F8.0)') MID, HD, IYF, SY, ALP
C
      J_MATS1(1) = MID
C
      SELECT CASE( IYF )
      CASE( 1 )
        J_MATS1(2) = 1
      CASE( 2, 3 )
        WRITE(ITO,*) 'YIELD FUNCTION ERROR, STOP IN SUB. RD_MATS1.'
        CALL ERRSTP(90,ITO)
      CASE( 4 )
        J_MATS1(2) = 2
      CASE DEFAULT
        J_MATS1(2) = 1
      END SELECT
C
      R_MATS1(1) = HD
      R_MATS1(2) = SY
C
      IF( ALP == 0.D0 ) ALP = 7.D-2
      R_MATS1(3) = ALP
C
      END
