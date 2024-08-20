      SUBROUTINE VF_OG1INI(XX,YY,ZZ,GGV,INDB,NWKBC)

CD=== 概要 ===========================================================

CDT   VF_OG1INI:図化ファイルに格子数等を出力する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APARAR.h'

CD    -- 引数 --
CD    XX   (MAXG1,NUMI) : IN  : R*8 : x方向格子座標等
CD    YY   (MAXG1,NUMJ) : IN  : R*8 : y方向格子座標等
CD    ZZ   (MAXG1,NUMK) : IN  : R*8 : z方向格子座標等
CD    GGV(@FOR-3D@)     : IN  : R*8 : 空隙率
CD    INDB (MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
CD    NWKBC(NUMB)       : OUT : I*4 : ワーク配列
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION GGV(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB),NWKBC(NUMB)

CD    -- 局所変数 --
      CHARACTER*5 TEXTP

C==== 実行 ===========================================================

CD    -- 出力指定がなければ抜ける --
      IF (IGRTYP.EQ.0) GOTO 9000

CD    -- 図化用の境界の数を数える --
      CALL VF_OGBCNM(INDB,NBX,NBY,NBZ)
      NB=NBX+NBY+NBZ

CD    -- 図化ファイルのオープンとメッセージの出力 --
      IGRFIL=0
      IF (NPROCS.EQ.1) THEN
        OPEN(MFILGR,ERR=9010,
     &       FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.grp',
     &       STATUS='NEW',FORM='UNFORMATTED' )
      ELSE
        WRITE(TEXTP,'(I5.5)') MYRANK
        OPEN(MFILGR,ERR=9010,
     &       FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.grp'//TEXTP,
     &       STATUS='NEW',FORM='UNFORMATTED' )
      ENDIF
      IGRFIL=MFILGR
      WRITE(ILPFIL,9510)

CD    -- バージョンを出力 --
      WRITE(IGRFIL,ERR=9020) IVR001,IVR002

CD    -- 解析領域を出力 --
      WRITE(IGRFIL,ERR=9020) NUMI0-1,NUMJ0-1,NUMK-1
      WRITE(IGRFIL,ERR=9020) GLXMIN,GLYMIN,ZZ(1,   2)
      WRITE(IGRFIL,ERR=9020) GLXMAX,GLYMAX,ZZ(1,NUMK)

CD    -- 出力領域を出力 --
      I1=IGRARA(1)
      J1=IGRARA(2)
      K1=IGRARA(3)
      I2=IGRARA(4)
      J2=IGRARA(5)
      K2=IGRARA(6)
      WRITE(IGRFIL,ERR=9020) I1,J1,K1,I2,J2,K2
      WRITE(IGRFIL,ERR=9020) NBX,NBY,NBZ

CD    -- 並列時にはその情報を出力 --
      IF (NPROCS.NE.1) THEN
        WRITE(IGRFIL,ERR=9020) NPROCS,NUMI-1,NUMJ-1
        WRITE(IGRFIL,ERR=9020) MYIS ,MYIE ,MYJS ,MYJE
        WRITE(IGRFIL,ERR=9020) MYMIS,MYMIE,MYMJS,MYMJE
        WRITE(IGRFIL,ERR=9020) MYGIS,MYGIE,MYGJS,MYGJE
      ENDIF
      I1=MAX(I1-(MYGIS-1),MYIS)
      J1=MAX(J1-(MYGJS-1),MYJS)
      I2=MIN(I2-(MYGIS-1),MYIE)
      J2=MIN(J2-(MYGJS-1),MYJE)

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
      IF (LEQC .GT.0) LS=LEQC
      IF (IGRVOR.NE.0) LS=LS+3
      LG=0
      IF (IPRNT.GT.1) LG=1
      L1=0
      L2=0
      WRITE(IGRFIL,ERR=9020) LN,LV,LP,LF,LK,LT,LS,LG,L1,L2

CD    -- 格子座標を出力 --
      WRITE(IGRFIL,ERR=9020) (XX(1,I),I=2,NUMI)
      WRITE(IGRFIL,ERR=9020) (YY(1,J),J=2,NUMJ)
      WRITE(IGRFIL,ERR=9020) (ZZ(1,K),K=2,NUMK)

CD    -- 境界のインデックスを出力する --
      IF (NB.GT.0) THEN
        CALL VF_OGBCIN(INDB,NWKBC,NBX,NBY,NBZ)
        WRITE(IGRFIL,ERR=9020) (NWKBC(I),I=1,NB)
      ENDIF

CD    -- 空隙率を出力 --
      IF (IPRNT.LE.1) THEN
        WRITE(IGRFIL,ERR=9020) (((GGV(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2)
      ENDIF

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      CALL VF_A2ERR('VF_OG1INI','CAN NOT OPEN (*****.grp).')
      GOTO 9999

 9020 CONTINUE
      CALL VF_A2ERR('VF_OG1INI','WRITE ERROR (*****.grp).')
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT( ' ','>> FILE-GRP : OUT : INITIAL')

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
