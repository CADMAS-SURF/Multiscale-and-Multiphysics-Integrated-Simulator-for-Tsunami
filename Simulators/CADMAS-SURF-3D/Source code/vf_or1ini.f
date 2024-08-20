      SUBROUTINE VF_OR1INI(GGV,ETIME)

CD=== 概要 ===========================================================

CDT   VF_OR1INI:詳細ファイルに格子数等を出力する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'

CD    -- 引数 --
CD    GGV(@FOR-3D@) : IN  : R*8 : 空隙率
      DIMENSION GGV(NUMI,NUMJ,NUMK)

CD    -- 局所変数 --
      CHARACTER*5 TEXTP

C==== 実行 ===========================================================

CD    -- 出力指定がなければ抜ける --
      IF (IRSTYP.EQ.0.AND.ETIME.EQ.1.0D30) GOTO 9000

CD    -- 詳細ファイルのオープンとメッセージの出力 --
      IRSFIL=0
      IF (NPROCS.EQ.1) THEN
        OPEN(MFILRS,ERR=9010,
     &       FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.rsl',
     &       STATUS='NEW',FORM='UNFORMATTED' )
      ELSE
        WRITE(TEXTP,'(I5.5)') MYRANK
        OPEN(MFILRS,ERR=9010,
     &       FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.rsl'//TEXTP,
     &       STATUS='NEW',FORM='UNFORMATTED' )
      ENDIF
      IRSFIL=MFILRS
      WRITE(ILPFIL,9510)

CD    -- バージョンを出力 --
      WRITE(IRSFIL,ERR=9020) IVR001,IVR002

CD    -- 格子数等を出力 --
      WRITE(IRSFIL,ERR=9020) NUMI,NUMJ,NUMK,NUMB

CD    -- 並列時にはその情報を出力 --
      IF (NPROCS.NE.1) THEN
        WRITE(IRSFIL,ERR=9020) NPROCS
        WRITE(IRSFIL,ERR=9020) MYIS ,MYIE ,MYJS ,MYJE
        WRITE(IRSFIL,ERR=9020) MYMIS,MYMIE,MYMJS,MYMJE
        WRITE(IRSFIL,ERR=9020) MYGIS,MYGIE,MYGJS,MYGJE
      ENDIF

CD    -- 時間毎に出力する物理量のフラグを出力 --
      LN=1
      LV=1
      LP=1
      LF=1
      LK=0
      IF (LEQK .NE.0) LK=1
      LT=0
      IF (LEQT .NE.0) LT=1
      LS=0
      IF (LEQC .GE.1) LS=LEQC
      LG=0
      IF (IPRNT.GT.1) LG=1
      LD=0
      IF (IDROP.GE.1) LD=1
      L1=0
      WRITE(IRSFIL,ERR=9020) LN,LV,LP,LF,LK,LT,LS,LG,LD,L1

CD    -- 空隙率を出力 --
      IF (IPRNT.LE.1) THEN
        WRITE(IRSFIL,ERR=9020)
     &          (((GGV(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
      ENDIF

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      CALL VF_A2ERR('VF_OR1INI','CAN NOT OPEN (*****.rsl).')
      GOTO 9999

 9020 CONTINUE
      CALL VF_A2ERR('VF_OR1INI','WRITE ERROR (*****.rsl).')
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT( ' ','>> FILE-RSL : OUT : INITIAL')

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
