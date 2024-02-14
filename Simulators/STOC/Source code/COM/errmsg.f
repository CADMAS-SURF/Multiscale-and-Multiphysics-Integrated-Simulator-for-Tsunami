      SUBROUTINE ERRMSG(CROUT,ID)
C======================================================================
C     エラーメッセージの共通フォーマット部分を出力する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
C
      CHARACTER(*),INTENT(INOUT)::CROUT
      INTEGER,INTENT(INOUT)::ID
C
      WRITE(LP,9000) ID, CROUT
C
      RETURN
 9000 FORMAT(/1X,'## ERROR ',I4.4,' : ROUTINE = ',A)
      END
