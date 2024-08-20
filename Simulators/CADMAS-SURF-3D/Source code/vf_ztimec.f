      SUBROUTINE VF_ZTIMEC(TALL,TUSER,TSYS)

CD=== 概要 ===========================================================

CDT   VF_ZTIMEC:経過時間を計測
CD      (1)ユーザ時間とシステム時間の和が経過時間
CD      (2)機種依存性有り

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

CD    -- 引数 --
CD    TALL  : OUT : R*8 : 経過時間
CD    TUSER : OUT : R*8 : ユーザ時間
CD    TSYS  : OUT : R*8 : システム時間

CD    -- 局所変数 --
      REAL E,T(2),ETIME

C==== 実行 ===========================================================

CD    -- エラーチェッカーをごまかすため --
      T(1)=0.0E0
      T(2)=0.0E0

CD    -- 経過時間を計測 --
C     * for Compaq Tru64 UNIX V5.1
C     E=ETIME(T)
C     TALL =DBLE(E   )
C     TUSER=DBLE(T(1))
C     TSYS =DBLE(T(2))

C     * for Intel/LINUX
C     CALL CPU_TIME(E)
C     TALL =0.0D0
C     TUSER=DBLE(E)
C     TSYS =0.0D0

C     * for MPI
      CALL VF_P0TIME(PT)
      TALL =0.0D0
      TUSER=PT
      TSYS =0.0D0

C     * ダミーの場合
C     TALL =0.0D0
C     TUSER=0.0D0
C     TSYS =0.0D0

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
