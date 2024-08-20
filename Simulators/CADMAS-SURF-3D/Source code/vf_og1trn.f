      SUBROUTINE VF_OG1TRN(DT ,XX ,YY ,ZZ ,
     &                     UU ,VV ,WW ,PP ,FF ,GGV,
     &                     BCU,BCV,BCW,BCP,BCF,
     &                     AK,AE,BCK,BCE,TT,BCT,CC,BCC,
     &                     WK01,WK02,WK03,WKBC,
     &                     NF ,INDX,INDY,INDZ,INDB)

CD=== 概要 ===========================================================

CDT   VF_OG1TRN:解析結果を図化ファイルに出力する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    DT               : IN  : R*8 : 次のステップの時間刻み幅
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)     : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    PP(@FOR-3D@)     : IN  : R*8 : 圧力
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
CD    GGV(@FOR-3D@)    : IN  : R*8 : 空隙率
CD    BCU(NUMB)        : IN  : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : IN  : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : IN  : R*8 : z方向流速の境界値
CD    BCP(NUMB)        : IN  : R*8 : 圧力の境界値
CD    BCF(NUMB)        : IN  : R*8 : VOF関数Fの境界値
CD    AK(@FOR-3D@)     : IN  : R*8 : 乱流エネルギ
CD    AE(@FOR-3D@)     : IN  : R*8 : 乱流エネルギ散逸
CD    BCK(NUMB)        : IN  : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB)        : IN  : R*8 : 乱流エネルギ散逸の境界値
CD    TT(@FOR-3D@)     : IN  : R*8 : 温度
CD    BCT(NUMB)        : IN  : R*8 : 温度の境界値
CD    CC(@FOR-3D@,LEQC) : IN : R*8 : 濃度
CD    BCC(NUMB,LEQC)    : IN : R*8 : 濃度の境界値
CD    WK01(@FOR-3D@)   : OUT : I*4 : ワーク配列
CD    WK02(@FOR-3D@)   : OUT : I*4 : ワーク配列
CD    WK03(@FOR-3D@)   : OUT : I*4 : ワーク配列
CD    WKBC(NUMB)       : OUT : I*4 : ワーク配列
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU (NUMI,NUMJ,NUMK),VV (NUMI,NUMJ,NUMK)
      DIMENSION WW (NUMI,NUMJ,NUMK),PP (NUMI,NUMJ,NUMK)
      DIMENSION FF (NUMI,NUMJ,NUMK),GGV(NUMI,NUMJ,NUMK)
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB),BCP(NUMB)
      DIMENSION BCF(NUMB)
      DIMENSION AK (NUMI,NUMJ,NUMK),AE (NUMI,NUMJ,NUMK)
      DIMENSION BCK(NUMB)          ,BCE(NUMB)
      DIMENSION TT (NUMI,NUMJ,NUMK),BCT(NUMB)
      DIMENSION CC (NUMI,NUMJ,NUMK,LEQC),BCC(NUMB,LEQC)
      DIMENSION WK01(NUMI,NUMJ,NUMK),WK02(NUMI,NUMJ,NUMK)
      DIMENSION WK03(NUMI,NUMJ,NUMK),WKBC(NUMB)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    -- 出力の判定 --
      IO=0
C     * ステップ間隔出力の場合
      IF     (IGRTYP.EQ.1) THEN
        IF (NNOW.GE.IGRTRN(1) .AND. NNOW.LE.IGRTRN(2)) THEN
          IF (MOD(NNOW-IGRTRN(1),IGRTRN(3)).EQ.0) IO=1
        ENDIF
C     * 時間間隔出力の場合
      ELSEIF (IGRTYP.EQ.2) THEN
Cmod20130605        IF (TNOW.GE.RGRTRN(1)-ZERO .AND. TNOW.LE.RGRTRN(2)+ZERO) THEN
        IF (TNOW.GE.RGRTRN(1)-0.5D0*DT .AND.
     $      TNOW.LE.RGRTRN(2)+0.5D0*DT) THEN
          W=(TNOW+0.5D0*DT)-RGRTRN(4)
          IF (W.GE.0.0D0) THEN
            IO=1
            RGRTRN(4)=RGRTRN(4)+DBLE(INT(W/RGRTRN(3))+1)*RGRTRN(3)
          ENDIF
        ENDIF
      ENDIF

CD    -- 非出力ならば抜ける --
      IF (IO.EQ.0) GOTO 9000

CD    -- STOC用ファイルの出力 --
CAKIY CALL DB_TRN(UU,VV,WW,PP,FF,NF,88,NUMI,NUMJ,NUMK,NNOW,TNOW)

CD    -- メッセージの出力 --
      WRITE(ILPFIL,9510) NNOW,TNOW

CD    -- 定数の設定 --
      I1=MAX(IGRARA(1)-(MYGIS-1),MYIS)
      J1=MAX(IGRARA(2)-(MYGJS-1),MYJS)
      K1=IGRARA(3)
      I2=MIN(IGRARA(4)-(MYGIS-1),MYIE)
      J2=MIN(IGRARA(5)-(MYGJS-1),MYJE)
      K2=IGRARA(6)
      CALL VF_OGBCNM(INDB,NBX,NBY,NBZ)
      NB=NBX+NBY+NBZ

CD    -- 計算情報を出力 --
      WRITE(IGRFIL,ERR=9010) NNOW,TNOW

CD    -- セルの状態を示すインデックスを出力 --
      WRITE(IGRFIL,ERR=9010) (((NF(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2)

CD    -- 流速を出力 --
      WRITE(IGRFIL,ERR=9010) (((UU(I,J,K),I=I1,I2+1),J=J1,J2),K=K1,K2)
      WRITE(IGRFIL,ERR=9010) (((VV(I,J,K),I=I1,I2),J=J1,J2+1),K=K1,K2)
      WRITE(IGRFIL,ERR=9010) (((WW(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2+1)
      IF (NB.GT.0) THEN
        CALL VF_OGBCVL(BCU,WKBC,INDB,NBX,NBY,NBZ)
        WRITE(IGRFIL,ERR=9010) (WKBC(I),I=1,NB)
        CALL VF_OGBCVL(BCV,WKBC,INDB,NBX,NBY,NBZ)
        WRITE(IGRFIL,ERR=9010) (WKBC(I),I=1,NB)
        CALL VF_OGBCVL(BCW,WKBC,INDB,NBX,NBY,NBZ)
        WRITE(IGRFIL,ERR=9010) (WKBC(I),I=1,NB)
      ENDIF

CD    -- 圧力を出力 --
      WRITE(IGRFIL,ERR=9010) (((PP(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2)
      IF (NB.GT.0) THEN
        CALL VF_OGBCVL(BCP,WKBC,INDB,NBX,NBY,NBZ)
        WRITE(IGRFIL,ERR=9010) (WKBC(I),I=1,NB)
      ENDIF

CD    -- VOF関数Fを出力 --
      WRITE(IGRFIL,ERR=9010) (((FF(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2)
      IF (NB.GT.0) THEN
        CALL VF_OGBCVL(BCF,WKBC,INDB,NBX,NBY,NBZ)
        WRITE(IGRFIL,ERR=9010) (WKBC(I),I=1,NB)
      ENDIF

CD    -- 乱流量を出力 --
      IF (LEQK.NE.0) THEN
        WRITE(IGRFIL,ERR=9010) (((AK(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2)
        WRITE(IGRFIL,ERR=9010) (((AE(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2)
        IF (NB.GT.0) THEN
          CALL VF_OGBCVL(BCK,WKBC,INDB,NBX,NBY,NBZ)
          WRITE(IGRFIL,ERR=9010) (WKBC(I),I=1,NB)
          CALL VF_OGBCVL(BCE,WKBC,INDB,NBX,NBY,NBZ)
          WRITE(IGRFIL,ERR=9010) (WKBC(I),I=1,NB)
        ENDIF
      ENDIF

CD    -- 温度を出力 --
      IF (LEQT.NE.0) THEN
        WRITE(IGRFIL,ERR=9010) (((TT(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2)
        IF (NB.GT.0) THEN
          CALL VF_OGBCVL(BCT,WKBC,INDB,NBX,NBY,NBZ)
          WRITE(IGRFIL,ERR=9010) (WKBC(I),I=1,NB)
        ENDIF
      ENDIF

CD    -- 濃度を出力 --
      DO 100 LC=1,LEQC
        WRITE(IGRFIL,ERR=9010)
     &                       (((CC(I,J,K,LC),I=I1,I2),J=J1,J2),K=K1,K2)
 100  CONTINUE
      IF (IGRVOR.NE.0) THEN
        DO 130 K=K1,K2
          DO 120 J=J1,J2
            DO 110 I=I1,I2
              CALL VF_CVORT(XX,YY,ZZ,UU,VV,WW,BCU,BCV,BCW,
     &                      NF,INDX,INDY,INDZ,
     &                      0,1,I,J,K,WK01(I,J,K))
              CALL VF_CVORT(XX,YY,ZZ,UU,VV,WW,BCU,BCV,BCW,
     &                      NF,INDX,INDY,INDZ,
     &                      0,2,I,J,K,WK02(I,J,K))
              CALL VF_CVORT(XX,YY,ZZ,UU,VV,WW,BCU,BCV,BCW,
     &                      NF,INDX,INDY,INDZ,
     &                      0,3,I,J,K,WK03(I,J,K))
 110        CONTINUE
 120      CONTINUE
 130    CONTINUE
        WRITE(IGRFIL,ERR=9010) (((WK01(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2)
        WRITE(IGRFIL,ERR=9010) (((WK02(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2)
        WRITE(IGRFIL,ERR=9010) (((WK03(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2)
      ENDIF
      IF (NB.GT.0) THEN
        DO 200 LC=1,LEQC
          CALL VF_OGBCVL(BCC(1,LC),WKBC,INDB,NBX,NBY,NBZ)
          WRITE(IGRFIL,ERR=9010) (WKBC(I),I=1,NB)
 200    CONTINUE
        IF (IGRVOR.NE.0) THEN
          DO 210 I=1,NB
            WKBC(I)=0.0D0
 210      CONTINUE
          WRITE(IGRFIL,ERR=9010) (WKBC(I),I=1,NB)
          WRITE(IGRFIL,ERR=9010) (WKBC(I),I=1,NB)
          WRITE(IGRFIL,ERR=9010) (WKBC(I),I=1,NB)
        ENDIF
      ENDIF

CD    -- 空隙率を出力 --
      IF (IPRNT.GT.1) THEN
        WRITE(IGRFIL,ERR=9010) (((GGV(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2)
      ENDIF

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      CALL VF_A2ERR('VF_OG1TRN','WRITE ERROR (*****.grp).')
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT( ' ','>> FILE-GRP : OUT : STEP=',I6,' : TIME= ',1PE12.5)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
