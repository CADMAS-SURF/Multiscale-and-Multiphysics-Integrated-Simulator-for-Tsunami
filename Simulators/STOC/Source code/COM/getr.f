      SUBROUTINE GETR(RVAL)
C======================================================================
C     入力データから1つの実数型データを読み込む
C
C     RVAL: 実数型配列
C======================================================================
      IMPLICIT NONE
C
      REAL(8),INTENT(INOUT)::RVAL
C
      REAL(8)::RTMP(1)
      INTEGER::NTMP
C
C
      CALL MGETR(RTMP,NTMP,1)
      RVAL = RTMP(1)
C
      RETURN
      END
