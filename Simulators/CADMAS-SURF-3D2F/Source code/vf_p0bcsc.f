      SUBROUTINE VF_P0BCSC(CBUF,N,IROOT)

CD=== 概要 ===========================================================

CDT   VF_P0BCSC:irootからその他へ送信する/文字

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    CBUF  : I/O : C*(*) : 転送するデータ
CD    N     : IN  : I*4   : データの要素数
CD    IROOT : IN  : I*4   : 送信元のランク
      CHARACTER*(*) CBUF

C==== 実行 ===========================================================

      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

C     -- irootからその他へ送信する --
      IF (MGPROC.NE.1) CALL VF_ZXMG_BCASTC(CBUF,N,IROOT,IERR)

C     -- 実行文の終了 --
      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
