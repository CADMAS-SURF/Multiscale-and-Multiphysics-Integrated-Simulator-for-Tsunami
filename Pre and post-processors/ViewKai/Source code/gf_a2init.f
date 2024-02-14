      SUBROUTINE GF_A2INIT(N)

CD=== 概要 ===========================================================

CDT   GF_A2INIT:初期部分の読込と出力

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'GF_CONV.h'

CD    -- 引数 --
CD    N : CNS : I*4 : 自分のランク+1

CD    -- 局所変数 --
      CHARACTER*13 FILENM

C==== 実行 ===========================================================

CD    -- 入力ファイルのオープンとメッセージの出力 --
      IFL=IFLIN+N-1
      WRITE(FILENM,'(I5.5)') N-1
      FILENM='data.grp'//FILENM
      WRITE(*,*) 'IN  :',FILENM,N-1
      OPEN(IFL,ERR=9010,FILE=FILENM,
     &     STATUS='OLD',FORM='UNFORMATTED' )

CD    -- 出力ファイルのオープンとメッセージの出力 --
      IF (N.EQ.NPROCS) THEN
        OPEN(IFLOU,ERR=9040,FILE='data.grp',
     &       STATUS='NEW',FORM='UNFORMATTED' )
        WRITE(*,*) 'OUT :','data.grp'
      ENDIF

CD    -- バージョン --
      READ (IFL  ,ERR=9020) I1,I2
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9050) I1,I2
      ENDIF

CD    -- 解析領域 --
      READ (IFL  ,ERR=9020) NUMI0,NUMJ0,NUMK0
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9050) NUMI0,NUMJ0,NUMK0
      ENDIF
      READ (IFL  ,ERR=9020) D1,D2,D3
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9050) D1,D2,D3
      ENDIF
      READ (IFL  ,ERR=9020) D1,D2,D3
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9050) D1,D2,D3
      ENDIF

CD    -- 出力領域 --
      READ (IFL  ,ERR=9020) IG1,JG1,KG1,IG2,JG2,KG2
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9050) IG1,JG1,KG1,IG2,JG2,KG2
      ENDIF
      READ (IFL  ,ERR=9020) NBX(N),NBY(N),NBZ(N)
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9050) 0     ,0     ,0
      ENDIF
      IG1=IG1-1
      JG1=JG1-1
      KG1=KG1-1
      IG2=IG2-1
      JG2=JG2-1
      KG2=KG2-1

CD    -- 並列情報 --
      READ(IFL,ERR=9020) NPROCS,NUMI(N),NUMJ(N)
      READ(IFL,ERR=9020) MYIS (N),MYIE (N),MYJS (N),MYJE (N)
      READ(IFL,ERR=9020) MYMIS(N),MYMIE(N),MYMJS(N),MYMJE(N)
      READ(IFL,ERR=9020) MYGIS(N),MYGIE(N),MYGJS(N),MYGJE(N)
      IF (NPROCS.GT.MPROCS) GOTO 9030

CD    -- 時間毎に出力する物理量のフラグ --
      READ (IFL  ,ERR=9020) ISWLN,ISWLV,ISWLP,ISWLF,ISWLK,
     &                      ISWLT,ISWLS,ISWLG,ISWL1,ISWL2
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9050) ISWLN,ISWLV,ISWLP,ISWLF,ISWLK,
     &                        ISWLT,ISWLS,ISWLG,ISWL1,ISWL2
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== エラー処理 =====================================================

 9010 CONTINUE
      WRITE(*,*) 'GF_A2INIT : CAN NOT OPEN (',FILENM,').'
      STOP

 9020 CONTINUE
      WRITE(*,*) 'GF_A2INIT : READ ERROR (',FILENM,').'
      STOP

 9030 CONTINUE
      WRITE(*,*) 'GF_A2INIT : AREA IS FULL.'
      STOP

 9040 CONTINUE
      WRITE(*,*) 'GF_A2INIT : CAN NOT OPEN (data.grp).'
      STOP

 9050 CONTINUE
      WRITE(*,*) 'GF_A2INIT : WRITE ERROR (data.grp).'
      STOP

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
