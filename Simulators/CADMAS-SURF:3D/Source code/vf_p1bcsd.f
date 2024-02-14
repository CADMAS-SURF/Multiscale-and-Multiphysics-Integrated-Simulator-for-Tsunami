      SUBROUTINE VF_P1BCSD(BUF,N,IROOT)

CD=== 概要 ===========================================================

CDT   VF_P1BCSD:irootからその他へ送信する/実数

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    BUF(N)  : I/O : R*8 : 転送するデータ
CD    N       : IN  : I*4 : データの要素数
CD    IROOT   : IN  : I*4 : 送信元のランク
      DIMENSION BUF(N)

C==== 実行 ===========================================================

      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

C     -- irootからその他へ送信する --
      IF (NPROCS.NE.1) CALL VF_ZXMP_BCASTD(BUF,N,IROOT,IERR)

C     -- 実行文の終了 --
      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
