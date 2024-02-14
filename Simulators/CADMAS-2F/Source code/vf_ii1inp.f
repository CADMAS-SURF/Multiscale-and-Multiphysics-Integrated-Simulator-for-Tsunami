      SUBROUTINE VF_II1INP(XX,YY,ZZ,CM0,CD0,GGV,GGX,GGY,GGZ,
C-------------------------------------------------------for MG/2FC coupling
     &                     XPF,YPF,ZPF,IPF,JPF,KPF,
C-------------------------------------------------------for MG/2FC coupling
     &                     BCU,BCV,BCW,BCP,BCF,BCVI,
     &                     BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                     NF,INDX,INDY,INDZ,INDB,
     &                     INDBK,INDBE,INDBT,INDBC,
     &                     DBUF,IBUF)

CD=== 概要 ===========================================================

CDT   VF_II1INP:入力ファイルを読み込む
CD      (1)1行の最大文字数はMAXCHR
CD      (2)1行の最大単語数はMAXWDS
CD      (3)区切りは1つ以上の空白
CD      (4)空白行はスキップ
CD      (5)「#」以降はコメント
CD      (6)文法上有効な最後の単語以降は無視
CD      (7)ダブ等の特殊文字は判定していない(注意)

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : I/O : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : I/O : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : I/O : R*8 : z方向格子座標等
CD    CM0(@FOR-3D@)    : I/O : R*8 : 慣性力係数
CD    CD0(@FOR-3D@)    : I/O : R*8 : 抵抗係数
CD    GGV(@FOR-3D@)    : I/O : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : I/O : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : I/O : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : I/O : R*8 : z方向面積透過率
C-------------------------------------------------------for MG/2FC coupling
CD    XPF(NUMI)        : OUT : R*8   :x方向の親格子に対する補間係数
CD    YPF(NUMJ)        : OUT : R*8   :y方向の親格子に対する補間係数
CD    ZPF(NUMK)        : OUT : R*8   :z方向の親格子に対する補間係数
CD    IPF(MGPINF(1))   : OUT : I*4   :x方向の親格子1に対する格子の数
CD    JPF(MGPINF(2))   : OUT : I*4   :y方向の親格子1に対する格子の数
CD    KPF(MGPINF(3))   : OUT : I*4   :z方向の親格子1に対する格子の数
C-------------------------------------------------------for MG/2FC coupling
CD    BCU(NUMB,3)      : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB,3)      : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB,3)      : I/O : R*8 : z方向流速の境界値
CD    BCP(NUMB,3)      : I/O : R*8 : 圧力の境界値
CD    BCF(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    BCVI(NUMB)       : I/O : R*8 : 流速の境界条件(壁面の粗さ)
CD    BCK(NUMB,3)      : I/O : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB,3)      : I/O : R*8 : 乱流エネルギ散逸の境界値
CD    BCT(NUMB)        : I/O : R*8 : 温度の境界値
CD    BCTI(2,NUMB)     : I/O : R*8 : 温度の境界条件
CD    BCC(NUMB,LEQC)   : I/O : R*8 : 濃度の境界値
CD    BCCI(2,NUMB,LEQC): I/O : R*8 : 濃度の境界条件
CD    NF(@FOR-3D@)     : I/O : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : I/O : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : I/O : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : I/O : I*4 : z面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : I/O : I*4 : 境界面のインデックス
CD    INDBK(MAXBK1,NUMB) : I/O : I*4 : 乱流エネルギの境界条件
CD    INDBE(MAXBK1,NUMB) : I/O : I*4 : 乱流エネルギ散逸の境界条件
CD    INDBT(NUMB)      : I/O : I*4 : 温度の境界条件
CD    INDBC(NUMB,LEQC) : I/O : I*4 : 濃度の境界条件
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    IBUF(NUMBUF*MAXBUF) : OUT :I*4 : 並列用のバッファ
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION CM0 (NUMI,NUMJ,NUMK),CD0 (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
C-------------------------------------------------------for MG/2FC coupling
      DIMENSION XPF(NUMI),YPF(NUMJ),ZPF(NUMK)
      DIMENSION IPF(0:MGPINF(1)),JPF(0:MGPINF(2)),KPF(0:MGPINF(3))
C-------------------------------------------------------for MG/2FC coupling
      DIMENSION BCU(NUMB,3),BCV(NUMB,3),BCW(NUMB,3)
      DIMENSION BCP(NUMB,3),BCF(NUMB),BCVI(NUMB)
      DIMENSION BCK(NUMB,3),BCE(NUMB,3),BCT(NUMB),BCTI(2,NUMB)
      DIMENSION BCC(NUMB,LEQC),BCCI(2,NUMB,LEQC)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB),INDBK(MAXBK1,NUMB),INDBE(MAXBE1,NUMB)
      DIMENSION INDBT(NUMB),INDBC(NUMB,LEQC)
      DIMENSION DBUF(NUMBUF*MAXBUF),IBUF(NUMBUF*MAXBUF)

CD    -- 局所変数 --
CD    TEXT       : C*(MAXCHR) : 入力した文字列
CD    IS(MAXWDS) : I*4        : n番目の単語の開始位置
CD    IE(MAXWDS) : I*4        : n番目の単語の終了位置
      CHARACTER*(MAXCHR) TEXT
      DIMENSION IS(MAXWDS),IE(MAXWDS)

C==== 実行 ===========================================================

CD    -- 読み込みレベルの設定 --
C     * LEVEL=<0:格子数を読み込む(格子数を決定する)
C     * LEVEL= 1:格子座標と障害物データを読み込む(境界面数を決定する)
C     * LEVEL>=2:その他のデータを読み込む
      LEVEL=0
      IF (NUMI0.GT.0) LEVEL=1
      IF (NUMB .GT.0) LEVEL=2
      WRITE(ILPFIL,9510) '## INPUT-LEVEL=',LEVEL

CD    -- NFにデフォルト値を設定 --
      IF (LEVEL.EQ.1) CALL VF_CNFDFL(NF)

CD    -- 境界面のインデックスを設定 --
      IF (LEVEL.EQ.2) CALL VF_CINDB(NF,INDX,INDY,INDZ,INDB)

CD    -- ローカルな格子数のクリア --
      NI=0
      NJ=0
      NK=0

CD    -- 入力ファイルのオープン --
      IEOF  =0
      IINFIL=0
      IF (MYRANK.EQ.0) THEN
C----------------------------------------------------for MG/2FC coupling
C       OPEN(MFILIN,ERR=9010,FILE='data.in',
        OPEN(MFILIN,ERR=9010,
     &       FILE=TRIM(MGNAME(MGRANK+1))//'.in',
C----------------------------------------------------for MG/2FC coupling
     &       STATUS='OLD',FORM='FORMATTED'  )
        IINFIL=MFILIN
      ENDIF

CD    -- ファイルの読み込み --
C     ** 前判定反復:入力ファイルが終了するまで **
 100  CONTINUE
        IF (IEOF.NE.0) GOTO 5010

CD      -- 1行を読み込み単語に分解する --
        CALL VF_ZGETLN(IS,IE,MAXWDS,NWD,IINFIL,IEOF,TEXT)
        IF (NWD.LE.0) GOTO 5000

CD      -- PARALLELを解釈(LEVEL=<0) --
        IF     (TEXT(IS(1):IE(1)).EQ.'PARALLEL') THEN
          IF (LEVEL.LE.0) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IIPARA(IS,IE,NWD,TEXT)
          ENDIF

CD      -- PHIDEMを解釈(LEVEL=<0) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'HIDEM') THEN
          IF (LEVEL.LE.0) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IIHIDM(IS,IE,NWD,TEXT)
          ENDIF

CD      -- EQUATIONを解釈(LEVEL=<0) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'EQUATION') THEN
          IF (LEVEL.LE.0) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IIEQUA(IS,IE,NWD,TEXT)
          ENDIF

CD      -- GRIDを解釈(LEVEL=<0で格子数、LEVEL=1で座標値) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'GRID') THEN
          IF (LEVEL.LE.0) WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
          IF (NWD.LT.2) CALL VF_A2ERR('VF_II1INP','SYNTAX ERROR.')
C         * x方向格子数を入力
          IF     (TEXT(IS(2):IE(2)).EQ.'X') THEN
            CALL VF_IIGRID(1,NI,NUMI,XX,IEOF,LEVEL,IS,IE,NWD,TEXT)
C         * y方向格子数を入力
          ELSEIF (TEXT(IS(2):IE(2)).EQ.'Y') THEN
            CALL VF_IIGRID(2,NJ,NUMJ,YY,IEOF,LEVEL,IS,IE,NWD,TEXT)
C         * z方向格子数を入力
          ELSEIF (TEXT(IS(2):IE(2)).EQ.'Z') THEN
            CALL VF_IIGRID(3,NK,NUMK,ZZ,IEOF,LEVEL,IS,IE,NWD,TEXT)
C         * 解釈できない単語によるエラー
          ELSE
            CALL VF_A2ERR('VF_II1INP','UNKNOWN WORD.')
          ENDIF

CD      -- OBSTを解釈(LEVEL=1) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'OBST') THEN
          IF (LEVEL.EQ.1) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IIOBST(NF,IS,IE,NWD,TEXT)
          ENDIF

CD      -- POROUSを解釈(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'POROUS') THEN
          IF (LEVEL.GE.2) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IIPORO(CM0,CD0,GGV,GGX,GGY,GGZ,
     &                     NF,INDX,INDY,INDZ,IS,IE,NWD,TEXT)
          ENDIF

CD      -- B.C.を解釈(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'B.C.') THEN
          IF (LEVEL.GE.2) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            IF (NWD.LT.2) CALL VF_A2ERR('VF_II1INP','SYNTAX ERROR.')
CD          * デフォルトを入力
            IF     (TEXT(IS(2):IE(2)).EQ.'D') THEN
              CALL VF_IIBOUN(BCU,BCV,BCW,BCP,BCF,BCVI,
     &                       BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                       -1,INDX,INDB,INDBK,INDBE,INDBT,INDBC,
     &                       IS,IE,NWD,TEXT)
              CALL VF_IIBOUN(BCU,BCV,BCW,BCP,BCF,BCVI,
     &                       BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                       -2,INDY,INDB,INDBK,INDBE,INDBT,INDBC,
     &                       IS,IE,NWD,TEXT)
              CALL VF_IIBOUN(BCU,BCV,BCW,BCP,BCF,BCVI,
     &                       BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                       -3,INDZ,INDB,INDBK,INDBE,INDBT,INDBC,
     &                       IS,IE,NWD,TEXT)
CD          * x面を入力
            ELSEIF (TEXT(IS(2):IE(2)).EQ.'X') THEN
              CALL VF_IIBOUN(BCU,BCV,BCW,BCP,BCF,BCVI,
     &                       BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                       +1,INDX,INDB,INDBK,INDBE,INDBT,INDBC,
     &                       IS,IE,NWD,TEXT)
CD          * y面を入力
            ELSEIF (TEXT(IS(2):IE(2)).EQ.'Y') THEN
              CALL VF_IIBOUN(BCU,BCV,BCW,BCP,BCF,BCVI,
     &                       BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                       +2,INDY,INDB,INDBK,INDBE,INDBT,INDBC,
     &                       IS,IE,NWD,TEXT)
CD          * z面を入力
            ELSEIF (TEXT(IS(2):IE(2)).EQ.'Z') THEN
              CALL VF_IIBOUN(BCU,BCV,BCW,BCP,BCF,BCVI,
     &                       BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                       +3,INDZ,INDB,INDBK,INDBE,INDBT,INDBC,
     &                       IS,IE,NWD,TEXT)
CD          * 解釈できない単語によるエラー
            ELSE
              CALL VF_A2ERR('VF_II1INP','UNKNOWN WORD.')
            ENDIF
          ENDIF

CD      -- TIMEを解釈(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'TIME') THEN
          IF (LEVEL.GE.2) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IITIME(IS,IE,NWD,TEXT)
          ENDIF

CD      -- MATEを解釈(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'MATE') THEN
          IF (LEVEL.GE.2) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IIMATE(IS,IE,NWD,TEXT)
          ENDIF

CD      -- MODELを解釈(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'MODEL') THEN
          IF (LEVEL.GE.2) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IIMDL(IS,IE,NWD,TEXT)
          ENDIF

CD      -- COMPを解釈(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'COMP') THEN
          IF (LEVEL.GE.2) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IICOMP(IS,IE,NWD,TEXT)
          ENDIF

CD      -- FILEを解釈(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'FILE') THEN
          IF (LEVEL.GE.2) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IIFILE(IS,IE,NWD,TEXT)
          ENDIF

CAKIY   @@@@@@@@@@@@@@@@@@@@@@
CD      -- OBST.TBLを解釈 --
CD      -- POROUS.TBLを解釈 --
CAKIY   @@@@@@@@@@@@@@@@@@@@@@

CD      -- OPTIONを解釈(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'OPTION') THEN
          IF (LEVEL.GE.2) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IIOPT(IS,IE,NWD,TEXT)
          ENDIF

CD      -- DEBUGを解釈:デバッグ用(LEVEL>=2) --
        ELSEIF (TEXT(IS(1):IE(1)).EQ.'DEBUG') THEN
          IF (LEVEL.GE.2) THEN
            WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
            CALL VF_IIDBG(IS,IE,NWD,TEXT)
          ENDIF

CD      -- 解釈できないヘッダーによるエラー --
        ELSE
          WRITE(ILPFIL,9520) (TEXT(IS(I):IE(I)),I=1,NWD)
          CALL VF_A2ERR('VF_II1INP','UNKNOWN HEADER.')
        ENDIF

C     ** 反復終了 **
 5000   GOTO 100
 5010 CONTINUE

CD    -- 入力ファイルのクローズ --
      IF (MYRANK.EQ.0) THEN
        CLOSE(IINFIL)
        IINFIL=0
      ENDIF

CD    -- 必須項目のチェック --
      IF (NI.LE.0) CALL VF_A2ERR('VF_II1INP','NOT FOUND (GRID X).')
      IF (NJ.LE.0) CALL VF_A2ERR('VF_II1INP','NOT FOUND (GRID Y).')
      IF (NK.LE.0) CALL VF_A2ERR('VF_II1INP','NOT FOUND (GRID Z).')

CD    -- 並列制御データのチェックと設定 --
      IF (LEVEL.LE.0) CALL VF_CPARA()

CD    -- MGとの親子関係のチェックと設定 --
C-------------------------------------------------------for MG/2FC coupling
C     IF (LEVEL.EQ.1) CALL VF_PMGSET(XX,YY,ZZ,NF)
cmod20160803(s)
c      IF (LEVEL.EQ.1)
      IF (LEVEL.ge.1)
     &  CALL VF_PMGSET(XX,YY,ZZ,GGV,GGX,GGY,GGZ,XPF,YPF,ZPF,NF,
     &                 dbuf,ibuf,level)
cmod20160803(e)
      IF (LEVEL.EQ.2) CALL VF_PMGST2(XPF,YPF,ZPF,IPF,JPF,KPF)
C-------------------------------------------------------for MG/2FC coupling

CD    -- 面の状態を示すインデックスを設定 --
      IF (LEVEL.EQ.1) CALL VF_CINDX(NF,INDX,INDY,INDZ)

CD    -- 気相と液相の境界条件の整合性を調整 --
      IF (LEVEL.GE.2) THEN
        CALL VF_CSET2F(BCU,BCV,BCW,BCP,BCK,BCE,INDB,INDBK,INDBE)
C-------------------------------------------------------for MG/2FC coupling
        CALL VF_PMGBC0(BCU,BCV,BCW,BCP,BCF,INDX,INDY,INDB)
C-------------------------------------------------------for MG/2FC coupling
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
C----------------------------------------------------for MG/2FC coupling
C     CALL VF_A2ERR('VF_II1INP','CAN NOT OPEN (data.in).')
      CALL VF_A2ERR('VF_II1INP','CAN NOT OPEN ('
     &                  //TRIM(MGNAME(MGRANK+1))//'.in).')
C----------------------------------------------------for MG/2FC coupling
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(/' ',A,100(I2:))
 9520 FORMAT( ' ',100('[',A,']':))

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
