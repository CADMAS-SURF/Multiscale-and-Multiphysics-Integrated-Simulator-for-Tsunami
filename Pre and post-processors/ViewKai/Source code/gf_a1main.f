      PROGRAM GF_A1MAIN

CD=== 概要 ===========================================================

CDT   GF_A1MAIN:CADMAS-SURF/3D-MPの並列用グラフィックデータを変換する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'GF_CONV.h'

CD    -- 局所変数 --
CD    XX(NUMI0)  : R*8 : x方向格子座標
CD    YY(NUMJ0)  : R*8 : y方向格子座標
CD    ZZ(NUMK0)  : R*8 : z方向格子座標
CD    VAL (@@@@) : R*8 : 物理量
CD    IVAL(@@@@) : I*4 : NF等
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: XX ,YY ,ZZ
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: VAL
      INTEGER         , DIMENSION(:,:,:), ALLOCATABLE :: IVAL

C==== 実行 ===========================================================

CD    -- 開始コメントの出力 --
      WRITE(*,9510)

CD    -- 初期部分の読込と出力 --
      CALL GF_A2INIT(1)
      DO 100 N=2,NPROCS
        CALL GF_A2INIT(N)
 100  CONTINUE
      WRITE(*,*) 'NPROCS=',NPROCS
      WRITE(*,*) 'NUMI0 =',NUMI0
      WRITE(*,*) 'NUMJ0 =',NUMJ0
      WRITE(*,*) 'NUMK0 =',NUMK0
      DO 110 N=1,NPROCS
        WRITE(*,*) 'LCL   =',MYGIS(N)+MYMIS(N)-1,
     &                       MYGJS(N)+MYMJS(N)-1,
     &                       MYGIE(N)-MYMIE(N)-1,
     &                       MYGJE(N)-MYMJE(N)-1
 110  CONTINUE

C     -- 配列のアロケート --
      IERR = 0
      ALLOCATE(XX  (NUMI0)            ,STAT=IERR)
      ALLOCATE(YY  (NUMJ0)            ,STAT=IERR)
      ALLOCATE(ZZ  (NUMK0)            ,STAT=IERR)
      ALLOCATE(VAL (NUMI0,NUMJ0,NUMK0),STAT=IERR)
      ALLOCATE(IVAL(NUMI0,NUMJ0,NUMK0),STAT=IERR)
      IF (IERR.NE.0) GOTO 9010

CD    -- 格子座標等の読込と出力 --
      DO 200 N=1,NPROCS
        CALL GF_A3GRID(N,XX,YY,ZZ,VAL)
 200  CONTINUE

CD    -- 時間毎のデータの読込と出力 --
 300  CONTINUE
        CALL GF_A4TRAN(VAL,IVAL,IE)
        IF (IE.EQ.0) GOTO 300
 390  CONTINUE

CD    -- 終了コメントを出力 --
      WRITE(*,9990)

C     -- 実行文の終了 --
      GOTO 9999

C==== エラー処理 =====================================================

 9010 CONTINUE
      WRITE(*,*) 'GF_A1MAIN : CAN NOT ALLOC.'
      STOP

C==== フォーマット文 =================================================

 9510 FORMAT(/' ','##### CADMAS-SURF/3D-MP GF_CONV START. ######' )
 9990 FORMAT(/' ','##### NORMAL END. ###########################'/)

C==== 終了 ===========================================================

 9999 CONTINUE
      STOP
      END
