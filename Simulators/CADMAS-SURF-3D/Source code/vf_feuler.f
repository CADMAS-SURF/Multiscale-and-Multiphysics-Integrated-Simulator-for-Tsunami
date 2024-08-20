      SUBROUTINE VF_FEULER(XX,YY,ZZ,FF,GGV,GGV0,DBUF,
     &                     FLFU,FLFV,FLFW,QF,NF)

CD=== 概要 ===========================================================

CDT   VF_FEULER:VOF関数Fの時間積分を計算(Euler法)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    FF(@FOR-3D@)     : I/O : R*8 : VOF関数F
CD    GGV(@FOR-3D@)    : IN  : R*8 : 空隙率
CD    GGV0(@FOR-3D@)   : IN  : R*8 : 空隙率(時間依存用)
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    FLFU(@FOR-3D@)   : IN  : R*8 : VOF関数Fのx方向フラックス
CD    FLFV(@FOR-3D@)   : IN  : R*8 : VOF関数Fのy方向フラックス
CD    FLFW(@FOR-3D@)   : IN  : R*8 : VOF関数Fのz方向フラックス
CD    QF(@FOR-3D@)     : IN  : R*8 : VOF関数Fの生成消滅
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION FF  (NUMI,NUMJ,NUMK),GGV (NUMI,NUMJ,NUMK)
      DIMENSION GGV0(NUMI,NUMJ,NUMK),DBUF(NUMBUF*MAXBUF)
      DIMENSION FLFU(NUMI,NUMJ,NUMK),FLFV(NUMI,NUMJ,NUMK)
      DIMENSION FLFW(NUMI,NUMJ,NUMK),QF  (NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 時間積分 --
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
            IF (NF(I,J,K).NE.-1) THEN
              G0=GGV(I,J,K)-(GGV(I,J,K)-GGV0(I,J,K))*DBLE(LOOPS)
              DF=( XX(4,I)*(FLFU(I+1,J,K)-FLFU(I,J,K))
     &            +YY(4,J)*(FLFV(I,J+1,K)-FLFV(I,J,K))
     &            +ZZ(4,K)*(FLFW(I,J,K+1)-FLFW(I,J,K))
     &            +DTNOW*QF(I,J,K)                    )
              FF(I,J,K)=(G0*FF(I,J,K)+DF)/GGV(I,J,K)
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

      CALL VF_P3SRD2(FF,DBUF,0)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
