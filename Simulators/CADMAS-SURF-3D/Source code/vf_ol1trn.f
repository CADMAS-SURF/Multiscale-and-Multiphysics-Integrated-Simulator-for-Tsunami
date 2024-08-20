      SUBROUTINE VF_OL1TRN(DT ,
     &                     UU ,VV ,WW ,PP ,FF ,ANU,
     &                     GGV,GGX,GGY,GGZ,GLV,GLX,GLY,GLZ,
     &                     BCU,BCV,BCW,BCP,BCF,
     &                     ANUT,AK,AE,BCK,BCE,
     &                     TT ,ALM,BCT,BCTI,CC,DD,BCC,BCCI,PPPVC,
     &                     NF ,INDB,INDBK,INDBE,INDBT,INDBC,IPVC)

CD=== 概要 ===========================================================

CDT   VF_OL1TRN:解析結果をリストファイルに出力する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    DT               : IN : R*8 : 次のステップの時間刻み幅
CD    UU(@FOR-3D@)     : IN : R*8 : x方向流速
CD    VV(@FOR-3D@)     : IN : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN : R*8 : z方向流速
CD    PP(@FOR-3D@)     : IN : R*8 : 圧力
CD    FF(@FOR-3D@)     : IN : R*8 : VOF関数F
CD    ANU(@FOR-3D@)    : IN : R*8 : 分子動粘性係数と渦動粘性係数の和
CD    GGV(@FOR-3D@)    : IN : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : IN : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : IN : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : IN : R*8 : z方向面積透過率
CD    GLV(@FOR-3D@)    : IN : R*8 : =GGV+(1-GGV)*CM
CD    GLX(@FOR-3D@)    : IN : R*8 : =GGX+(1-GGX)*CM
CD    GLY(@FOR-3D@)    : IN : R*8 : =GGY+(1-GGY)*CM
CD    GLZ(@FOR-3D@)    : IN : R*8 : =GGZ+(1-GGZ)*CM
CD    BCU(NUMB)        : IN : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : IN : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : IN : R*8 : z方向流速の境界値
CD    BCP(NUMB)        : IN : R*8 : 圧力の境界値
CD    BCF(NUMB)        : IN : R*8 : VOF関数Fの境界値
CD    ANUT(@FOR-3D@)   : IN : R*8 : 渦動粘性係数νt
CD    AK(@FOR-3D@)     : IN : R*8 : 乱流エネルギ
CD    AE(@FOR-3D@)     : IN : R*8 : 乱流エネルギ散逸
CD    BCK(NUMB)        : IN : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB)        : IN : R*8 : 乱流エネルギ散逸の境界値
CD    TT(@FOR-3D@)     : IN : R*8 : 温度
CD    ALM(@FOR-3D@)    : IN : R*8 : 熱伝導率と乱流熱伝導率の和
CD    BCT(NUMB)        : IN : R*8 : 温度の境界値
CD    BCTI(2,NUMB)     : IN : R*8 : 温度の境界条件
CD    CC(@FOR-3D@,LEQC) : IN : R*8 : 濃度
CD    DD(@FOR-3D@,LEQC) : IN : R*8 : 拡散係数と乱流拡散係数の和
CD    BCC(NUMB,LEQC)    : IN : R*8 : 濃度の境界値
CD    BCCI(2,NUMB,LEQC) : IN : R*8 : 濃度の境界条件
CD    PPPVC(@FOR-3D@)  : IN : R*8 : 空気圧の計算用の圧力
CD    NF(@FOR-3D@)     : IN : I*4 : セルの状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN : I*4 : 境界面のインデックス
CD    INDBK(NUMB)      : IN  : I*4 : 乱流エネルギの境界条件
CD    INDBE(NUMB)      : IN  : I*4 : 乱流エネルギ散逸の境界条件
CD    INDBT(NUMB)      : IN : I*4 : 温度の境界条件
CD    INDBC(NUMB,LEQC) : IN : I*4 : 濃度の境界条件
CD    IPVC(@FOR-3D)    : IN : I*4 : 空気圧の計算用インデックス
      DIMENSION UU (NUMI,NUMJ,NUMK),VV (NUMI,NUMJ,NUMK)
      DIMENSION WW (NUMI,NUMJ,NUMK),PP (NUMI,NUMJ,NUMK)
      DIMENSION FF (NUMI,NUMJ,NUMK),ANU(NUMI,NUMJ,NUMK)
      DIMENSION GGV(NUMI,NUMJ,NUMK),GGX(NUMI,NUMJ,NUMK)
      DIMENSION GGY(NUMI,NUMJ,NUMK),GGZ(NUMI,NUMJ,NUMK)
      DIMENSION GLV(NUMI,NUMJ,NUMK),GLX(NUMI,NUMJ,NUMK)
      DIMENSION GLY(NUMI,NUMJ,NUMK),GLZ(NUMI,NUMJ,NUMK)
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB),BCP(NUMB),BCF(NUMB)
      DIMENSION ANUT(NUMI,NUMJ,NUMK),AK  (NUMI,NUMJ,NUMK)
      DIMENSION AE  (NUMI,NUMJ,NUMK),BCK(NUMB),BCE(NUMB)
      DIMENSION TT (NUMI,NUMJ,NUMK),ALM(NUMI,NUMJ,NUMK)
      DIMENSION BCT(NUMB),BCTI(2,NUMB)
      DIMENSION CC(NUMI,NUMJ,NUMK,LEQC),DD(NUMI,NUMJ,NUMK,LEQC)
      DIMENSION BCC(NUMB,LEQC),BCCI(2,NUMB,LEQC),PPPVC(NUMI,NUMJ,NUMK)
      DIMENSION NF (NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)
      DIMENSION INDBK(NUMB),INDBE(NUMB),INDBT(NUMB),INDBC(NUMB,LEQC)
      DIMENSION IPVC(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 出力の判定 --
      IO=0
C     * ステップ間隔出力の場合
      IF     (ILPTYP.EQ.1) THEN
        IF (NNOW.GE.ILPTRN(1) .AND. NNOW.LE.ILPTRN(2)) THEN
          IF (MOD(NNOW-ILPTRN(1),ILPTRN(3)).EQ.0) IO=1
        ENDIF
C     * 時間間隔出力の場合
      ELSEIF (ILPTYP.EQ.2) THEN
Cmod20130605        IF (TNOW.GE.RLPTRN(1)-ZERO .AND. TNOW.LE.RLPTRN(2)+ZERO) THEN
        IF (TNOW.GE.RLPTRN(1)-0.5D0*DT .AND.
     $      TNOW.LE.RLPTRN(2)+0.5D0*DT) THEN
          W=(TNOW+0.5D0*DT)-RLPTRN(4)
          IF (W.GE.0.0D0) THEN
            IO=1
            RLPTRN(4)=RLPTRN(4)+DBLE(INT(W/RLPTRN(3))+1)*RLPTRN(3)
          ENDIF
        ENDIF
      ENDIF

CD    -- 非出力ならば抜ける --
      IF (IO.EQ.0) GOTO 9000

CD    -- NFを出力する --
      IF (ILPON(7).NE.0) THEN
        WRITE(ILPFIL,9510) 'NF',NNOW,TNOW
        CALL VF_OL3DNF(NF)
      ENDIF

CD    -- 空気圧の計算用インデックスを出力する --
      IF (ILPON(20).NE.0 .AND. PVCP0.GE.ZERO) THEN
        WRITE(ILPFIL,9510) 'IPVC',NNOW,TNOW
        CALL VF_OL3DI(IPVC,0)
      ENDIF

CD    -- VOF関数Fを出力する --
      IF (ILPON(8).NE.0) THEN
        WRITE(ILPFIL,9510) 'F',NNOW,TNOW
        CALL VF_OL3DR(FF,0)
      ENDIF

CD    -- 流速を出力する --
      IF (ILPON(9).NE.0) THEN
        WRITE(ILPFIL,9510) 'VELOCITY-X',NNOW,TNOW
        CALL VF_OL3DR(UU,1)
        WRITE(ILPFIL,9510) 'VELOCITY-Y',NNOW,TNOW
        CALL VF_OL3DR(VV,1)
        WRITE(ILPFIL,9510) 'VELOCITY-Z',NNOW,TNOW
        CALL VF_OL3DR(WW,1)
      ENDIF

CD    -- 圧力を出力する --
      IF (ILPON(10).NE.0) THEN
        WRITE(ILPFIL,9510) 'PRESSURE',NNOW,TNOW
        CALL VF_OL3DR(PP,0)
      ENDIF

CD    -- 空気圧の計算用の圧力を出力する --
      IF (ILPON(21).NE.0 .AND. PVCP0.GE.ZERO) THEN
        WRITE(ILPFIL,9510) 'PPPVC',NNOW,TNOW
        CALL VF_OL3DR(PPPVC,0)
      ENDIF

CD    -- 乱流エネルギを出力する --
      IF (ILPON(17).NE.0 .AND. LEQK.NE.0) THEN
        WRITE(ILPFIL,9510) 'TURB-K',NNOW,TNOW
        CALL VF_OL3DR(AK,0)
      ENDIF

CD    -- 乱流エネルギ散逸を出力する --
      IF (ILPON(18).NE.0 .AND. LEQK.NE.0) THEN
        WRITE(ILPFIL,9510) 'TURB-E',NNOW,TNOW
        CALL VF_OL3DR(AE,0)
      ENDIF

CD    -- 渦動粘性係数を出力する --
      IF (ILPON(19).NE.0 .AND. LEQK.NE.0) THEN
        WRITE(ILPFIL,9510) 'VISC-T',NNOW,TNOW
        CALL VF_OL3DR(ANUT,0)
      ENDIF

CD    -- 分子動粘性係数と渦動粘性係数の和を出力する --
      IF (ILPON(11).NE.0) THEN
        WRITE(ILPFIL,9510) 'VISC',NNOW,TNOW
        CALL VF_OL3DR(ANU,0)
      ENDIF

CD    -- 温度を出力する --
      IF (ILPON(13).NE.0 .AND. LEQT.NE.0) THEN
        WRITE(ILPFIL,9510) 'TEMPERATURE',NNOW,TNOW
        CALL VF_OL3DR(TT,0)
      ENDIF

CD    -- 熱伝導率を出力する --
      IF (ILPON(14).NE.0 .AND. LEQT.NE.0) THEN
        WRITE(ILPFIL,9510) 'T-COND',NNOW,TNOW
        CALL VF_OL3DR(ALM,0)
      ENDIF

CD    -- 濃度を出力する --
      IF (ILPON(15).NE.0) THEN
        DO 100 LC=1,LEQC
          WRITE(ILPFIL,9530) 'CONCENTRATION',LC,NNOW,TNOW
          CALL VF_OL3DR(CC(1,1,1,LC),0)
 100    CONTINUE
      ENDIF

CD    -- 拡散係数を出力する --
      IF (ILPON(16).NE.0) THEN
        DO 200 LC=1,LEQC
          WRITE(ILPFIL,9530) 'DIFFUS',LC,NNOW,TNOW
          CALL VF_OL3DR(DD(1,1,1,LC),0)
 200    CONTINUE
      ENDIF

CD    -- ポーラス関連を出力する --
      IF (ILPON(5).NE.0 .AND. IPRNT.GT.1) THEN
        WRITE(ILPFIL,9510) 'VOLUME POROSITY',NNOW,TNOW
        CALL VF_OL3DR(GGV,0)
        WRITE(ILPFIL,9510) 'SURFACE PERMEABILITY-X',NNOW,TNOW
        CALL VF_OL3DR(GGX,1)
        WRITE(ILPFIL,9510) 'SURFACE PERMEABILITY-Y',NNOW,TNOW
        CALL VF_OL3DR(GGY,1)
        WRITE(ILPFIL,9510) 'SURFACE PERMEABILITY-Z',NNOW,TNOW
        CALL VF_OL3DR(GGZ,1)
      ENDIF
      IF (ILPON(6).NE.0 .AND. IPRNT.GT.1) THEN
        WRITE(ILPFIL,9510) 'GLV',NNOW,TNOW
        CALL VF_OL3DR(GLV,0)
        WRITE(ILPFIL,9510) 'GLX',NNOW,TNOW
        CALL VF_OL3DR(GLX,1)
        WRITE(ILPFIL,9510) 'GLY',NNOW,TNOW
        CALL VF_OL3DR(GLY,1)
        WRITE(ILPFIL,9510) 'GLZ',NNOW,TNOW
        CALL VF_OL3DR(GLZ,1)
      ENDIF

CD    -- 境界値を出力する --
      IF (ILPON(12).NE.0) THEN
        WRITE(ILPFIL,9510) 'BOUNDARY',NNOW,TNOW
        CALL VF_OLBC(BCU,BCV,BCW,BCP,BCF,BCK,BCE,BCT,BCTI,BCC,BCCI,
     &               INDB,INDBK,INDBE,INDBT,INDBC)
      ENDIF

CD    -- 空行の出力 --
      WRITE(ILPFIL,9520)

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(/' ','<<',A,'>> : STEP=',I6,' : TIME= ',1PE12.5)
 9520 FORMAT( ' '/)
 9530 FORMAT(/' ','<<',A,'(',I6,')>> : STEP=',I6,' : TIME= ',1PE12.5)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
