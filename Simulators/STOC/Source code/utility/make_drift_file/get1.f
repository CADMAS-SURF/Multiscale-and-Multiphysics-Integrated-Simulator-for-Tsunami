      SUBROUTINE GET1(IS,IE,IERR)
C======================================================================
C     入力データファイルから文字列CLINE(IS:IE)を一つ取り出し、
C     開始位置ISと終了位置IEを返す
C
C     文字列は1つ以上の空白文字または改行によって区切られる。
C     なお、'='と'('と')'は特殊文字とし、1文字だけの文字列として扱う。
C
C     またアルファベット小文字は大文字に変換する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
C
      INTEGER,INTENT(INOUT)::IS,IE,IERR
C
      CHARACTER(1)::CH
C
      CHARACTER(26)::CUP ='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      CHARACTER(26)::CLOW='abcdefghijklmnopqrstuvwxyz'
C
C ... CLINEの中の読みとり開始位置
      INTEGER::I1=0
C
      INTEGER::I,J
C
C
      IERR = 0
      IF(I1.NE.0) I1=IABS(I1DD)
      IF(I1.NE.0.AND.I1DD.LE.0) I1=IABS(I1DD)
C
      DO 100 J=1,100000
         IF( I1 .EQ. 0 ) THEN
            READ(INP,'(A132)',ERR=910,END=900) CLINE
c            write(*,*) 'debug:get1:read line:$',cline,'$'
            I1 = 1
         END IF
C
         IS = 0
         IE = 0
C
         DO 200 I=I1,132
            CH = CLINE(I:I)
C
C ......... 文字列の開始位置を検索
            IF( IS.EQ.0 ) THEN
               IF( CH.EQ.'#' ) THEN
C                 コメント行のためスキップ
                  I1 = 0
                  GO TO 100
               ELSE IF( CH.EQ.'=' .OR. CH.EQ.'(' .OR.CH.EQ.')' ) THEN
C                 '=()'のいずれかの場合、1文字だけの文字列として取り出す
                  I1 = I+1
                  IS = I
                  IE = I
                  GO TO 300
               ELSE IF( CH.NE.' ' ) THEN
C                 文字列の開始位置を設定
                  IS = I
               END IF
C
C ......... 文字列の終了位置を検索
            ELSE
               IF( CH.EQ.'#' ) THEN
C                 文字列の終了位置その1('#')
                  I1 = 0
                  IE = I - 1
                  GO TO 300
               ELSE IF( CH.EQ.'=' .OR. CH.EQ.'(' .OR.CH.EQ.')' ) THEN
C                 文字列の終了位置その2(特殊文字'=()')
                  I1 = I
                  IE = I - 1
                  GO TO 300
               ELSE IF( CH.EQ.' ' ) THEN
C                 文字列の終了位置その3(' ')
                  I1 = I
                  IE = I - 1
                  GO TO 300
               END IF
            END IF
  200    CONTINUE
         IF( IS.NE.0 ) THEN
C           文字列の終了位置をその4(行末)
            I1 = 0
            IE = 132
            GO TO 300
         END IF
         I1 = 0
  100 CONTINUE
C
  300 CONTINUE
C
C
C ... 小文字→大文字に変換
      DO 400 I=1,IS,IE
         CH = CLINE(I:I)
         DO 500 J=1,26
            IF( CH .EQ. CLOW(J:J) ) THEN
               CLINE(I:I) = CUP(J:J)
               GO TO 400
            END IF
  500    CONTINUE
  400 CONTINUE
C
c      write(*,*) 'debug:get1:string:$',cline(is:ie),'$'
      IF(I1DD.LT.0) THEN
        I1DD = -I1
      ELSE
        I1DD = I1
      END IF
C
      RETURN
C
  900 CONTINUE
C     ファイルの終端
      IERR = 1
      RETURN
C
  910 CONTINUE
C     読み込みエラー
      CALL ERRMSG('GET1',6300)
      WRITE(LP,*) 'READ ERROR'
      CALL ABORT1('')
      END
