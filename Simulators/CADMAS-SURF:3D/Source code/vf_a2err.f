      SUBROUTINE VF_A2ERR(CNAME,CMSG)

CD=== 概要 ===========================================================

CDT   VF_A2ERR:エラーメッセージの出力と実行終了

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    CNAME : IN : C*(*) : エラーの発生したルーチン名
CD    CMSG  : IN : C*(*) : エラーメッセージ
      CHARACTER*(*) CNAME,CMSG

CD    -- 局所変数 --
      CHARACTER*5 TEXTP

C==== 実行 ===========================================================

CD    -- エラーメッセージの出力 --
      IF (MGPROC.EQ.1) THEN
        IF (ILPFIL.NE.0) THEN
          WRITE(ILPFIL,9510) CNAME,CMSG
          WRITE(ILPFIL,9520)
        ENDIF
        WRITE(*     ,9510) CNAME,CMSG
        WRITE(*     ,9520)
      ELSE
        WRITE(TEXTP,'(I5.5)') MGRANK
        IF (ILPFIL.NE.0) THEN
          WRITE(ILPFIL,9530) TEXTP,CNAME,CMSG
          WRITE(ILPFIL,9540) TEXTP
        ENDIF
        WRITE(*     ,9530) TEXTP,CNAME,CMSG
        WRITE(*     ,9540) TEXTP
      ENDIF

CD    -- ファイルのクローズ --
      CALL VF_A2CLOS()

CD    -- 並列環境の異常終了 --
      CALL VF_P0ENDA()

CD    -- 実行を終了 --
      GOTO 9000

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(/' ','>>>>> ERROR. [',A,'] : ',A)
 9520 FORMAT(/' ','##### ABNORMAL END. #########################'/)
 9530 FORMAT(/' ','>>>>> ERROR(MG',A,'). [',A,'] : ',A)
 9540 FORMAT(/' ','##### ABNORMAL END(MG',A,'). #################'/)

C==== 終了 ===========================================================

 9999 CONTINUE
      STOP
      END
