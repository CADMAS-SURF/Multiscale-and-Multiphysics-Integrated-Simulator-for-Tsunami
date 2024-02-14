      SUBROUTINE GETC2(CVAL,ILEN,ISIZ)
C======================================================================
C     MGETCと同様の内容だが、
C     CASE名読み取り用に特化し、読み込み文字列数を一つに限定し、
C     ＜GET1の替わりにGET2を呼び出す＞ように変更。
C
C     CVAL:  文字型配列
C     ILEN:  文字列長
C     ISIZ:  文字列の最大長
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
C
      CHARACTER(*),INTENT(INOUT)::CVAL
      INTEGER,INTENT(INOUT)::ILEN,ISIZ
C
      INTEGER::IE,IERR,IS
C
C
C----------------------------------------------------------------------
C     (1) '='を読み込む
C----------------------------------------------------------------------
C
      CALL GET1(IS,IE,IERR)
      IF( IERR.GT.0 ) GO TO 900
C
      IF( CLINE(IS:IE) .NE. '=' ) THEN
         CALL ERRMSG('GETC2',6320)
         WRITE(LP,*) 'INPUT DATA FORMAT IS INCORRECT'
         WRITE(LP,*) 'EQUAL SYMBOL IS NEEDED',
     $               ' BETWEEN VARIABLE AND VALUES'
         WRITE(LP,*) 'LINE=',CLINE
         CALL ABORT1('')
      END IF
C
C
C----------------------------------------------------------------------
C     (2) "値"(右辺値)を読み込む
C----------------------------------------------------------------------
C
      CLINE(1:IE) = CNUL(1:IE)
      CALL GET2(IS,IE,IERR)
      IF( IERR.GT.0 ) GO TO 900
C
      IF( IE-IS+1.GT.ISIZ ) THEN
         CALL ERRMSG('GETC2',6321)
         WRITE(LP,*) 'STRING LENGTH MAY ',
     $               'NOT BE OVER ',ISIZ,' HERE'
         WRITE(LP,*) 'ERROR DATA=',CLINE(IS:IE)
         WRITE(LP,*) 'LINE=',CLINE
         CALL ABORT1('')
      END IF
      CVAL = CNUL(1:ISIZ)
      CVAL = CLINE(IS:IE)
      ILEN = IE-IS+1
      CLINE(IS:IE) = CNUL(IS:IE)
C
c      write(*,*) 'debug:getc2:value=',cval(1:isiz)
      RETURN
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('GETC2',6322)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
      END
