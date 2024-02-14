      SUBROUTINE GETC(CVAL,ISIZ)
C======================================================================
C     入力データから1つの文字型データを読み込む
C
C     CVAL: 文字型配列
C     ISIZ: 文字列の最大長
C======================================================================
      IMPLICIT NONE
C
      CHARACTER(*),INTENT(INOUT)::CVAL
      INTEGER,INTENT(INOUT)::ISIZ
C
      CHARACTER(132)::CTMP(1)
      INTEGER::NTMP
C
C
      CALL MGETC(CTMP,ISIZ,NTMP,1)
      CVAL = CTMP(1)(1:ISIZ)
C
      RETURN
      END
