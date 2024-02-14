      SUBROUTINE GF_BCD(N)

CD=== 概要 ===========================================================

CDT   GF_BCD:境界条件の読込/実数

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'GF_CONV.h'

CD    -- 引数 --
CD    N : IN  : I*4 : 自分のランク+1

CD    -- 局所変数 --
      CHARACTER*13 FILENM

C==== 実行 ===========================================================

CD    -- 入力ファイルのファイル名 --
      IFL=IFLIN+N-1
      WRITE(FILENM,'(I5.5)') N-1
      FILENM='data.grp'//FILENM

CD    -- データの範囲 --
      NB=NBX(N)+NBY(N)+NBZ(N)

CD    -- 境界のインデックス --
      IF (NB.GT.0) THEN
        READ(IFL,ERR=9010) (D1,I=1,NB)
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== エラー処理 =====================================================

 9010 CONTINUE
      WRITE(*,*) 'GF_BCD : READ ERROR (',FILENM,').'
      STOP

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
