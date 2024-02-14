      SUBROUTINE VF_SEULER(XX,YY,ZZ,CC,GGV,GGV0,DBUF,
     &                     FLCU,FLCV,FLCW,QC,NF)

CD=== 概要 ===========================================================

CDT   VF_SEULER:スカラ量の時間積分を計算(Euler法)

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
CD    CC(@FOR-3D@)     : I/O : R*8 : スカラ量
CD    GGV(@FOR-3D@)    : IN  : R*8 : 空隙率
CD    GGV0(@FOR-3D@)   : IN  : R*8 : 空隙率(時間依存用)
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    FLCU(@FOR-3D@)   : IN  : R*8 : x方向フラックス
CD    FLCV(@FOR-3D@)   : IN  : R*8 : y方向フラックス
CD    FLCW(@FOR-3D@)   : IN  : R*8 : z方向フラックス
CD    QC(@FOR-3D@)     : IN  : R*8 : 生成消滅
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION CC  (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGV0(NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION FLCU(NUMI,NUMJ,NUMK),FLCV(NUMI,NUMJ,NUMK)
      DIMENSION FLCW(NUMI,NUMJ,NUMK),QC  (NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 時間積分 --
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
            IF (NF(I,J,K).EQ.0) THEN
              G0=GGV(I,J,K)-(GGV(I,J,K)-GGV0(I,J,K))*DBLE(LOOPS)
              DS=( XX(4,I)*(FLCU(I+1,J,K)-FLCU(I,J,K))
     &            +YY(4,J)*(FLCV(I,J+1,K)-FLCV(I,J,K))
     &            +ZZ(4,K)*(FLCW(I,J,K+1)-FLCW(I,J,K))+QC(I,J,K))
              CC(I,J,K)=(G0*CC(I,J,K)+DTNOW*DS)/GGV(I,J,K)
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

      CALL VF_P3SRD2(CC,DBUF,0)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
