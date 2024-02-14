      SUBROUTINE GETI(IVAL)
C======================================================================
C     入力データから1つの整数型データを読み込む
C
C     IVAL: 整数型配列
C======================================================================
      IMPLICIT NONE
C
      INTEGER,INTENT(INOUT)::IVAL
C
      INTEGER::ITMP(1)
      INTEGER::NTMP
C
C
      CALL MGETI(ITMP,NTMP,1)
      IVAL = ITMP(1)
C
      RETURN
      END
