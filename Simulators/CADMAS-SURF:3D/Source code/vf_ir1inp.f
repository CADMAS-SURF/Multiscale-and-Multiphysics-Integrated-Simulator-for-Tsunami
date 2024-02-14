      SUBROUTINE VF_IR1INP(UU,VV,WW,PP,FF,ANU,GGV,BCU,BCV,BCW,BCP,BCF,
     &                     ANUT,AK,AE,BCK,BCE,TT,ALM,BCT,CC,DD,BCC,
     &                     TBUB,DROPTX,DROPTY,DROPTZ,
     &                     DROPUU,DROPVV,DROPWW,
     &                     FFLXX,FFLXY,DELH,
     &                     NF,INDC,INDS)

CD=== 概要 ===========================================================

CDT   VF_IR1INP:リスタートファイルを読み込む

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'
      INCLUDE 'VF_ASTOCI.h'
      INCLUDE 'VF_ASEABT.h'

CD    -- 引数 --
CD    UU(@FOR-3D@)     : OUT : R*8 : x方向流速
CD    VV(@FOR-3D@)     : OUT : R*8 : y方向流速
CD    WW(@FOR-3D@)     : OUT : R*8 : z方向流速
CD    PP(@FOR-3D@)     : OUT : R*8 : 圧力
CD    FF(@FOR-3D@)     : OUT : R*8 : VOF関数F
CD    ANU(@FOR-3D@)    : OUT : R*8 : 分子動粘性係数と渦動粘性係数の和
CD    GGV(@FOR-3D@)    : OUT : R*8 : 空隙率
CD    BCU(NUMB)        : OUT : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : OUT : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : OUT : R*8 : z方向流速の境界値
CD    BCP(NUMB)        : OUT : R*8 : 圧力の境界値
CD    BCF(NUMB)        : OUT : R*8 : VOF関数Fの境界値
CD    ANUT(@FOR-3D@)   : OUT : R*8 : 渦動粘性係数νt
CD    AK(@FOR-3D@)     : OUT : R*8 : 乱流エネルギ
CD    AE(@FOR-3D@)     : OUT : R*8 : 乱流エネルギ散逸
CD    BCK(NUMB)        : OUT : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB)        : OUT : R*8 : 乱流エネルギ散逸の境界値
CD    TT(@FOR-3D@)     : OUT : R*8 : 温度
CD    ALM(@FOR-3D@)    : OUT : R*8 : 熱伝導率と乱流熱伝導率の和
CD    BCT(NUMB)        : OUT : R*8 : 温度の境界値
CD    CC(@FOR-3D@,LEQC) : OUT : R*8 : スカラ量
CD    DD(@FOR-3D@,LEQC) : OUT : R*8 : 拡散係数と乱流拡散係数の和
CD    BCC(NUMB,LEQC)    : OUT : R*8 : 濃度の境界値
CD    TBUB(NUMK)       : OUT : R*8 : 気泡上昇処理を最後に行った時間
CD    DROPTX(@FOR-3D@) : OUT : R*8 : 自由落下処理を最後に行った時間(x)
CD    DROPTY(@FOR-3D@) : OUT : R*8 : 自由落下処理を最後に行った時間(y)
CD    DROPTZ(@FOR-3D@) : OUT : R*8 : 自由落下処理を最後に行った時間(z)
CD    DROPUU(@FOR-3D@) : OUT : R*8 : 自由落下のx方向速度
CD    DROPVV(@FOR-3D@) : OUT : R*8 : 自由落下のy方向速度
CD    DROPWW(@FOR-3D@) : OUT : R*8 : 自由落下のz方向速度
CD    NF(@FOR-3D@)     : OUT : I*4 : セルの状態を示すインデックス
CD    INDC(@FOR-3D@)   : OUT : I*4 : セルの計算状態を示すインデックス
CD    INDS(@FOR-1D@)   : OUT : I*4 : 表面セルのI,J,K座標
      DIMENSION UU    (NUMI,NUMJ,NUMK),VV    (NUMI,NUMJ,NUMK)
      DIMENSION WW    (NUMI,NUMJ,NUMK),PP    (NUMI,NUMJ,NUMK)
      DIMENSION FF    (NUMI,NUMJ,NUMK),ANU   (NUMI,NUMJ,NUMK)
      DIMENSION GGV   (NUMI,NUMJ,NUMK)
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB),BCP(NUMB),BCF(NUMB)
      DIMENSION ANUT  (NUMI,NUMJ,NUMK)
      DIMENSION AK    (NUMI,NUMJ,NUMK),AE    (NUMI,NUMJ,NUMK)
      DIMENSION BCK   (NUMB)          ,BCE   (NUMB)
      DIMENSION TT    (NUMI,NUMJ,NUMK),ALM   (NUMI,NUMJ,NUMK)
      DIMENSION BCT(NUMB),TBUB(NUMK)
      DIMENSION CC(NUMI,NUMJ,NUMK,LEQC),DD(NUMI,NUMJ,NUMK,LEQC)
      DIMENSION BCC(NUMB,LEQC)
      DIMENSION DROPTX(NUMI,NUMJ,NUMK),DROPTY(NUMI,NUMJ,NUMK)
      DIMENSION DROPTZ(NUMI,NUMJ,NUMK),DROPUU(NUMI,NUMJ,NUMK)
      DIMENSION DROPVV(NUMI,NUMJ,NUMK),DROPWW(NUMI,NUMJ,NUMK)
      DIMENSION FFLXX(NUMI,NUMJ,NUMK),FFLXY(NUMI,NUMJ,NUMK)
      DIMENSION DELH(NUMI0,NUMJ0)
      DIMENSION NF    (NUMI,NUMJ,NUMK),INDC  (NUMI,NUMJ,NUMK)
      DIMENSION INDS  (NUMI*NUMJ*NUMK)

CD    -- 局所変数 --
      CHARACTER*5 TEXTP

C==== 実行 ===========================================================

CD    -- 入力指定がなければ抜ける --
      IF (IRETYP.LT.0) GOTO 9000

CD    -- TimerDoor法のための設定 --
      DO 10 K=1,NUMK
        TBUB(K)=0.0D0
 10   CONTINUE
      DO 40 K=1,NUMK
        DO 30 J=1,NUMJ
          DO 20 I=1,NUMI
            DROPTX(I,J,K)=-1.0D10
            DROPTY(I,J,K)= 0.0D0
            DROPTZ(I,J,K)= 0.0D0
            DROPUU(I,J,K)= 0.0D0
            DROPVV(I,J,K)= 0.0D0
            DROPWW(I,J,K)= 0.0D0
 20       CONTINUE
 30     CONTINUE
 40   CONTINUE

CD    -- リスタートファイルのオープンとメッセージの出力 --
      IREFIL=0
      IF (NPROCS.EQ.1) THEN
        OPEN(MFILRE,ERR=9010,
     &       FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.res',
     &       STATUS='OLD',FORM='UNFORMATTED' )
      ELSE
        WRITE(TEXTP,'(I5.5)') MYRANK
        OPEN(MFILRE,ERR=9010,
     &       FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.res'//TEXTP,
     &       STATUS='OLD',FORM='UNFORMATTED' )
      ENDIF
      IREFIL=MFILRE

CD    -- バージョンを読み込む --
      READ(IREFIL,ERR=9020) IV1,IV2

CD    -- 格子数等を読み込む --
      READ(IREFIL,ERR=9020) I1,I2,I3,I4
      IF (NUMI.NE.I1 .OR. NUMJ.NE.I2 .OR. NUMK.NE.I3)
     &    CALL VF_A2ERR('VF_IR1INP','INVALID NUMBER OF GRID.')
      IF (NUMB.NE.I4)
     &    CALL VF_A2ERR('VF_IR1INP','INVALID NUMBER OF B.C.')

CD    -- 並列時にはその情報を読み込む --
      IF (NPROCS.NE.1) THEN
        READ(IREFIL,ERR=9020) I1
        IF (NPROCS.NE.I1)
     &    CALL VF_A2ERR('VF_IR1INP','INVALID VALUE FOR PARALLEL(1).')
        READ(IREFIL,ERR=9020) I1,I2,I3,I4
        IF (MYIS .NE.I1 .OR. MYIE .NE.I2 .OR.
     &      MYJS .NE.I3 .OR. MYJE .NE.I4     )
     &    CALL VF_A2ERR('VF_IR1INP','INVALID VALUE FOR PARALLEL(2).')
        READ(IREFIL,ERR=9020) I1,I2,I3,I4
        IF (MYMIS.NE.I1 .OR. MYMIE.NE.I2 .OR.
     &      MYMJS.NE.I3 .OR. MYMJE.NE.I4     )
     &    CALL VF_A2ERR('VF_IR1INP','INVALID VALUE FOR PARALLEL(3).')
        READ(IREFIL,ERR=9020) I1,I2,I3,I4
        IF (MYGIS.NE.I1 .OR. MYGIE.NE.I2 .OR.
     &      MYGJS.NE.I3 .OR. MYGJE.NE.I4     )
     &    CALL VF_A2ERR('VF_IR1INP','INVALID VALUE FOR PARALLEL(4).')
      ENDIF

CD    -- 時間毎に出力する物理量のフラグを読み込む --
      READ(IREFIL,ERR=9020) LN,LV,LP,LF,LK,LT,LS,LG,LD,L1
      IF (LK.NE.LEQK)
     &    CALL VF_A2ERR('VF_IR1INP','INVALID EQUATION (K-EPS).')
      IF (LT.NE.LEQT)
     &    CALL VF_A2ERR('VF_IR1INP','INVALID EQUATION (TEMP).')
      IF (LS.NE.LEQC)
     &    CALL VF_A2ERR('VF_IR1INP','INVALID EQUATION (CONC).')

CD    -- 空隙率を読み込む --
      IF (LG.EQ.0) THEN
        IF (IPRNT.GT.1) THEN
          READ(IREFIL,ERR=9020)
     &            (((GGV(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
        ELSE
          READ(IREFIL,ERR=9020)
     &            (((DUMMY     ,I=1,NUMI),J=1,NUMJ),K=1,NUMK)
        ENDIF
      ENDIF

CD    -- INITIAL部分読み込み終了のエコー --
      WRITE(ILPFIL,9510)

CD    -- フラグの初期設定 --
      ION=0

CD    -- ファイルの読み込み --
CD    ** 前判定反復:詳細ファイルが終了するまで **
 100  CONTINUE

CD      -- 計算情報を読み込む(ファイルの終了判定) --
        READ(IREFIL,END=1000,ERR=9020) NNOW,TNOW,DTNOW
        READ(IREFIL,ERR=9020) FSUM,FCUT,CGBNRM,CGXNRM,ICGITR

CD      -- セルの状態を示すインデックスを読み込む --
        READ(IREFIL,ERR=9020)
     &          (((NF(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)

CD      -- 流速を読み込む --
        READ(IREFIL,ERR=9020)
     &          (((UU(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
        READ(IREFIL,ERR=9020) (BCU(L),L=1,NUMB)
        READ(IREFIL,ERR=9020)
     &          (((VV(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
        READ(IREFIL,ERR=9020) (BCV(L),L=1,NUMB)
        READ(IREFIL,ERR=9020)
     &          (((WW(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
        READ(IREFIL,ERR=9020) (BCW(L),L=1,NUMB)

CD      -- 圧力を読み込む --
        READ(IREFIL,ERR=9020)
     &          (((PP(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
        READ(IREFIL,ERR=9020) (BCP(L),L=1,NUMB)

CD      -- VOF関数Fを読み込む --
        READ(IREFIL,ERR=9020)
     &          (((FF(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
        READ(IREFIL,ERR=9020) (BCF(L),L=1,NUMB)

CD      -- 乱流エネルギを読み込む --
        IF (LEQK.NE.0) THEN
          READ(IREFIL,ERR=9020)
     &               (((AK(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
          READ(IREFIL,ERR=9020) (BCK(L),L=1,NUMB)
        ENDIF

CD      -- 乱流エネルギ散逸を読み込む --
        IF (LEQK.NE.0) THEN
          READ(IREFIL,ERR=9020)
     &               (((AE(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
          READ(IREFIL,ERR=9020) (BCE(L),L=1,NUMB)
        ENDIF

CD      -- 温度を読み込む --
        IF (LEQT.NE.0) THEN
          READ(IREFIL,ERR=9020)
     &               (((TT(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
          READ(IREFIL,ERR=9020) (BCT(L),L=1,NUMB)
        ENDIF

CD      -- スカラー量を読み込む --
        DO 200 LC=1,LEQC
          READ(IREFIL,ERR=9020)
     &            (((CC(I,J,K,LC),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
          READ(IREFIL,ERR=9020) (BCC(L,LC),L=1,NUMB)
 200    CONTINUE

CD      -- STOC連成用データを読み込む --
        IF (NB_SC.GT.0) THEN
C ..... WEST
        I=MIST(2)
        READ(IREFIL,ERR=9020)
     $          ((((FFLXX(I,J,K),J=MJST(JST),MJST(JST+1)-1),
     $              K=MKST(KST),MKST(KST+1)-1),JST=1,NJST),KST=1,NKST)
C ..... EAST
        I=MIST(NIST)
        READ(IREFIL,ERR=9020)
     $          ((((FFLXX(I,J,K),J=MJST(JST),MJST(JST+1)-1),
     $              K=MKST(KST),MKST(KST+1)-1),JST=1,NJST),KST=1,NKST)
C ..... SOUTH
        J=MJST(2)
        READ(IREFIL,ERR=9020)
     $          ((((FFLXY(I,J,K),I=MIST(IST),MIST(IST+1)-1),
     $              K=MKST(KST),MKST(KST+1)-1),IST=1,NIST),KST=1,NKST)
C ..... NORTH
        J=MJST(NJST)
        READ(IREFIL,ERR=9020)
     $          ((((FFLXY(I,J,K),I=MIST(IST),MIST(IST+1)-1),
     $              K=MKST(KST),MKST(KST+1)-1),IST=1,NIST),KST=1,NKST)
C
        IF(ISEABT.NE.0) THEN
        I1=MYIS + MYGIS - 1
        I2=MYIE + MYGIS - 1
        J1=MYJS + MYGJS - 1
        J2=MYJE + MYGJS - 1
        READ(IREFIL,ERR=9020) ((DELH(I,J),I=I1,I2),J=J1,J2)
        ENDIF
        ENDIF

CD      -- 時間依存型空隙率を読み込む --
        IF (LG.NE.0) THEN
          IF (IPRNT.GT.1) THEN
            READ(IREFIL,ERR=9020)
     &              (((GGV(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
          ELSE
            READ(IREFIL,ERR=9020)
     &              (((DUMMY     ,I=1,NUMI),J=1,NUMJ),K=1,NUMK)
          ENDIF
        ENDIF

CD      -- Timer Door用データを読み込む --
        READ(IREFIL,ERR=9020) (TBUB(L),L=1,NUMK)
        IF (IDROP.GE.1 .AND. LD.GE.1 .AND. NNOW.EQ.IRETYP) THEN
          READ(IREFIL,ERR=9020) ND
          DO 300 N=1,ND
            READ(IREFIL,ERR=9020) I,J,K,DTX,DTY,DTZ,DUU,DVV,DWW
            DROPTX(I,J,K)=DTX
            DROPTY(I,J,K)=DTY
            DROPTZ(I,J,K)=DTZ
            DROPUU(I,J,K)=DUU
            DROPVV(I,J,K)=DVV
            DROPWW(I,J,K)=DWW
 300      CONTINUE
        ELSEIF (LD.GE.1) THEN
          READ(IREFIL,ERR=9020) ND
          DO 400 N=1,ND
            READ(IREFIL,ERR=9020) I,J,K,DTX,DTY,DTZ,DUU,DVV,DWW
 400      CONTINUE
        ENDIF

CD      -- STEP毎部分読み込み終了のエコー --
        WRITE(ILPFIL,9520) NNOW,TNOW

CD      -- 指定したステップの情報であるかを判定 --
        IF (NNOW.EQ.IRETYP) THEN
          ION=1
          GOTO 1000
        ENDIF

CD    ** 反復終了 **
        GOTO 100
 1000 CONTINUE

CD    -- 指定したステップの情報がなければエラー --
      IF (ION.EQ.0) CALL VF_A2ERR('VF_IR1RES','INVALID STEP.')

CD    -- セルの計算状態を示すインデックスの設定 --
      CALL VF_ZSETI3(INDC,-1,NUMI,NUMJ,NUMK)
      CALL VF_CINDC(NF,INDC)

CD    -- 表面セルのI,J,K座標の設定 --
      NUMS=0
      DO 2120 K=2,NUMK-1
        DO 2110 J=MYJS,MYJE
          DO 2100 I=MYIS,MYIE
            IF (NF(I,J,K).EQ.1 .OR. NF(I,J,K).EQ.2 .OR.
     &          NF(I,J,K).EQ.3 .OR. NF(I,J,K).EQ.4 .OR.
     &          NF(I,J,K).EQ.5 .OR. NF(I,J,K).EQ.6     ) THEN
              NUMS=NUMS+1
              INDS(NUMS)=I+NUMI*(J-1)+NUMI*NUMJ*(K-1)
            ENDIF
 2100     CONTINUE
 2110   CONTINUE
 2120 CONTINUE

CD    -- 動粘性係数等の設定 --
      IF (LEQK.NE.0) CALL VF_CNUT0(AK,AE,ANUT,NF)
      CALL VF_CNU00(ANUT,ANU,NF)
      IF (LEQT.NE.0) CALL VF_CLM00(ANUT ,ALM ,NF)
      IF (LEQC.GT.0) CALL VF_CDD00(ANUT ,DD,  NF)

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      CALL VF_A2ERR('VF_IR1INI','CAN NOT OPEN (*****.res).')
      GOTO 9999

 9020 CONTINUE
      CALL VF_A2ERR('VF_IR1INI','READ ERROR (*****.res).')
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT( ' ','>> FILE-RES : IN : INITIAL')
 9520 FORMAT( ' ','>> FILE-RES : IN : STEP=',I6,' : TIME= ',1PE12.5)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
