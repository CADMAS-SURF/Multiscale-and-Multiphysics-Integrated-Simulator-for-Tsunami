      SUBROUTINE VF_OL1INI(XX,YY,ZZ,
     &                     CM0,CD0,GGV,GGX,GGY,GGZ,GLV,GLX,GLY,GLZ,
     &                     BCU,BCV,BCW,BCP,BCF,
     &                     BCK,BCE,BCT,BCTI,BCC,BCCI,
     &                     NF,INDX,INDY,INDZ,INDB,
     &                     INDBK,INDBE,INDBT,INDBC)

CD=== 概要 ===========================================================

CDT   VF_OL1INI:解析条件をリストファイルに出力する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN : R*8 : z方向格子座標等
CD    CM0(@FOR-3D@)    : IN : R*8 : 慣性力係数
CD    CD0(@FOR-3D@)    : IN : R*8 : 抵抗係数
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
CD    BCK(NUMB)        : IN : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB)        : IN : R*8 : 乱流エネルギ散逸の境界値
CD    BCT(NUMB)        : IN : R*8 : 温度の境界値
CD    BCTI(2,NUMB)     : IN : R*8 : 温度の境界条件
CD    BCC(NUMB,LEQC)    : IN : R*8 : 濃度の境界値
CD    BCCI(2,NUMB,LEQC) : IN : R*8 : 濃度の境界条件
CD    NF(@FOR-3D@)     : IN : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN : I*4 : z面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN : I*4 : 境界面のインデックス
CD    INDBK(NUMB)      : IN : I*4 : 乱流エネルギの境界条件
CD    INDBE(NUMB)      : IN : I*4 : 乱流エネルギ散逸の境界条件
CD    INDBT(NUMB)      : IN : I*4 : 温度の境界条件
CD    INDBC(NUMB,LEQC) : IN : I*4 : 濃度の境界条件
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION CM0(NUMI,NUMJ,NUMK),CD0(NUMI,NUMJ,NUMK)
      DIMENSION GGV(NUMI,NUMJ,NUMK),GGX(NUMI,NUMJ,NUMK)
      DIMENSION GGY(NUMI,NUMJ,NUMK),GGZ(NUMI,NUMJ,NUMK)
      DIMENSION GLV(NUMI,NUMJ,NUMK),GLX(NUMI,NUMJ,NUMK)
      DIMENSION GLY(NUMI,NUMJ,NUMK),GLZ(NUMI,NUMJ,NUMK)
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB),BCP(NUMB),BCF(NUMB)
      DIMENSION BCK(NUMB),BCE(NUMB),BCT(NUMB),BCTI(2,NUMB)
      DIMENSION BCC(NUMB,LEQC),BCCI(2,NUMB,LEQC)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB),INDBK(NUMB),INDBE(NUMB)
      DIMENSION INDBT(NUMB),INDBC(NUMB,LEQC)

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- 並列制御関連情報を出力 --
      WRITE(ILPFIL,9510) '-- PARALLEL --'
      WRITE(ILPFIL,9530) 'NPROCS      [-] =',NPROCS
      WRITE(ILPFIL,9530) '    NPI,NPJ [-] =',NUMNPI,NUMNPJ
      WRITE(ILPFIL,9530) 'MYRANK      [-] =',MYRANK
      WRITE(ILPFIL,9530) '    MYI,MYJ [-] =',MYRI,MYRJ
      WRITE(ILPFIL,9530) 'CELL-X(ALL) [-] =',NUMI0-2
      WRITE(ILPFIL,9530) '    ICS,ICE [-] =',MYGIS+MYMIS-1,MYGIE-MYMIE-1
      WRITE(ILPFIL,9530) 'CELL-Y(ALL) [-] =',NUMJ0-2
      WRITE(ILPFIL,9530) '    JCS,JCE [-] =',MYGJS+MYMJS-1,MYGJE-MYMJE-1
      WRITE(ILPFIL,9530) 'CELL-Z(ALL) [-] =',NUMK -2
      WRITE(ILPFIL,9530) '    KCS,KCE [-] =',1,NUMK-2

CD    -- 方程式制御関連情報を出力 --
      WRITE(ILPFIL,9510) '-- EQUATION --'
      IF (LEQK.EQ.0) THEN
        WRITE(ILPFIL,9520) 'K-EPS           = NOCALC'
      ELSE
        WRITE(ILPFIL,9520) 'K-EPS           = CALC'
      ENDIF
      IF (LEQT.EQ.0) THEN
        WRITE(ILPFIL,9520) 'TEMPERATURE     = NOCALC'
      ELSE
        WRITE(ILPFIL,9520) 'TEMPERATURE     = CALC'
      ENDIF
      IF (LEQC.EQ.0) THEN
        WRITE(ILPFIL,9520) 'CONCENTRATION   = NOCALC'
      ELSE
        WRITE(ILPFIL,9530) 'CONCENTRATION[-]=',LEQC
      ENDIF

CD    -- 時間制御関連情報を出力 --
      WRITE(ILPFIL,9510) '-- TIME CONTROL --'
      IF (IDTTYP.EQ.0) THEN
        WRITE(ILPFIL,9520) 'DT     TYPE     = CONSTANT'
        WRITE(ILPFIL,9540) '       DELTA[S] =',DTCNST
      ELSE
        WRITE(ILPFIL,9520) 'DT     TYPE     = AUTO'
        WRITE(ILPFIL,9540) '       SAFE [-] =',DTSAFE
        WRITE(ILPFIL,9540) '       INIT [S] =',DTINIT
        WRITE(ILPFIL,9540) '       MIN  [S] =',DTMIN
        WRITE(ILPFIL,9540) '       MAX  [S] =',DTMAX
      ENDIF
      WRITE(ILPFIL,9530) 'END    STEP [-] =',NEND
      WRITE(ILPFIL,9540) '       TIME [S] =',TEND

CD    -- 物性値関連情報を出力 --
      WRITE(ILPFIL,9510) '-- MATERIAL --'
      WRITE(ILPFIL,9540) 'W-LEVEL     [M] =',WVLVL
      WRITE(ILPFIL,9540) 'DENSITY [KG/M3] =',RHO0
      WRITE(ILPFIL,9540) 'K-VISC   [M2/S] =',ANU0
      WRITE(ILPFIL,9540) 'GRAVITY  [M/S2] =',GRZ0
      IF (LEQT.NE.0) THEN
        WRITE(ILPFIL,9540) 'S-HEAT [J/K/kg] =',TCP0
        WRITE(ILPFIL,9540) 'T-COND  [W/M/K] =',TCN0
        WRITE(ILPFIL,9540) 'T-D T0      [K] =',TDT0
        WRITE(ILPFIL,9540) 'T-D DR[KG/M3/K] =',TDR0
      ENDIF
      DO 100 LC=1,LEQC
        WRITE(ILPFIL,9540) 'DIFFUS   [M2/S] =',CDF0(LC)
        IF (CDF0(LC).LT.ZERO)
     &               CALL VF_A2ERR('VF_OL1INI','INVALID VALUE.')
 100  CONTINUE
      DO 110 LC=1,LEQC
        WRITE(ILPFIL,9540) 'C-D C0      [-] =',CDC0(LC)
 110  CONTINUE
      DO 120 LC=1,LEQC
        WRITE(ILPFIL,9540) 'C-D DR  [KG/M3] =',CDR0(LC)
 120  CONTINUE
      WRITE(ILPFIL,9540) 'I.C.   U  [M/S] =',UINI
      WRITE(ILPFIL,9540) '       V  [M/S] =',VINI
      WRITE(ILPFIL,9540) '       W  [M/S] =',WINI
      IF (LEQK.NE.0) THEN
        WRITE(ILPFIL,9540) '       K[M2/S2] =',AKINIK
        WRITE(ILPFIL,9540) '       E[M2/S3] =',AKINIE
      ENDIF
      IF (LEQT.NE.0) THEN
        WRITE(ILPFIL,9540) '       T    [K] =',TINI
      ENDIF
      DO 130 LC=1,LEQC
        WRITE(ILPFIL,9540) '       C    [-] =',CINI(LC)
 130  CONTINUE

CD    -- モデル関連情報を出力 --
      WRITE(ILPFIL,9510) '-- MODEL --'
      IF (ISCTYP(1).NE.0) THEN
        IF (ISCTYP(1).GT.0) THEN
          WRITE(ILPFIL,9530) 'WAVE-SRC X      =', ISCTYP(1)-1
        ELSE
          WRITE(ILPFIL,9530) 'WAVE-SRC Y      =',-ISCTYP(1)-1
        ENDIF
        IF     (ISCTYP(2).EQ.-2) THEN
          WRITE(ILPFIL,9520) '  FUNC          = STOKES(5TH)'
        ELSEIF (ISCTYP(2).EQ.-1) THEN
          WRITE(ILPFIL,9520) '  FUNC          = CNOIDAL(3RD)'
        ELSEIF (ISCTYP(2).GE. 1) THEN
          WRITE(ILPFIL,9520) '  FUNC          = STREAM'
          WRITE(ILPFIL,9530) '            N[-]=',ISCTYP(2)
        ELSEIF (ISCTYP(2).EQ.-3) THEN
          WRITE(ILPFIL,9520) '  FUNC          = MATRIX'
        ENDIF
        WRITE(ILPFIL,9540) '  DEPTH      [M]=',SCTYP(1)
        WRITE(ILPFIL,9540) '  HEIGHT     [M]=',SCTYP(2)
        WRITE(ILPFIL,9540) '  PERIOD     [S]=',SCTYP(3)
        WRITE(ILPFIL,9540) '  AMPL       [-]=',SCTYP(8)
        WRITE(ILPFIL,9540) '  LENGTH     [M]=',SCTYP(4)
        WRITE(ILPFIL,9540) '  URSELL NUMB[-]=',SCTYP(5)
        WRITE(ILPFIL,9540) '  0.0=WAVE(x)[-]=',SCTYP(6)
      ENDIF
      DO 200 JD=1,4
        IF (IBCTYP(1,JD).EQ.1) THEN
          IF     (JD.EQ.1) THEN
            WRITE(ILPFIL,9520) 'WAVE-BC X-'
          ELSEIF (JD.EQ.2) THEN
            WRITE(ILPFIL,9520) 'WAVE-BC X+'
          ELSEIF (JD.EQ.3) THEN
            WRITE(ILPFIL,9520) 'WAVE-BC Y-'
          ELSEIF (JD.EQ.4) THEN
            WRITE(ILPFIL,9520) 'WAVE-BC Y+'
          ENDIF
          IF     (IBCTYP(2,JD).EQ.-2) THEN
            WRITE(ILPFIL,9520) '  FUNC          = STOKES(5TH)'
          ELSEIF (IBCTYP(2,JD).EQ.-1) THEN
            WRITE(ILPFIL,9520) '  FUNC          = CNOIDAL(3RD)'
          ELSEIF (IBCTYP(2,JD).GE. 1) THEN
            WRITE(ILPFIL,9520) '  FUNC          = STREAM'
            WRITE(ILPFIL,9530) '            N[-]=',IBCTYP(2,JD)
          ELSEIF (IBCTYP(2,JD).EQ.-3) THEN
            WRITE(ILPFIL,9520) '  FUNC          = MATRIX'
          ELSEIF (IBCTYP(2,JD).EQ.-4) THEN
            WRITE(ILPFIL,9520) '  FUNC          = MATRIX2'
          ENDIF
          WRITE(ILPFIL,9540) '  DEPTH      [M]=',BCTYP(1,JD)
          WRITE(ILPFIL,9540) '  HEIGHT     [M]=',BCTYP(2,JD)
          WRITE(ILPFIL,9540) '  PERIOD     [S]=',BCTYP(3,JD)
          WRITE(ILPFIL,9540) '  AMPL       [-]=',BCTYP(8,JD)
          IF (ABS(BCTYP(9,JD)).GE.ZERO) THEN
            WRITE(ILPFIL,9540) '  ANGLE    [DEG]=',BCTYP(9,JD)
            WRITE(ILPFIL,9540) '  X0         [M]=',BCTYP(10,JD)
            WRITE(ILPFIL,9540) '  Y0         [M]=',BCTYP(11,JD)
          ENDIF
          WRITE(ILPFIL,9540) '  LENGTH     [M]=',BCTYP(4,JD)
          WRITE(ILPFIL,9540) '  URSELL NUMB[-]=',BCTYP(5,JD)
          WRITE(ILPFIL,9540) '  0.0=WAVE(x)[-]=',BCTYP(6,JD)
        ENDIF
 200  CONTINUE
      DO 210 JD=1,4
        IF (IBCTYP(1,JD).EQ.2) THEN
          IF     (JD.EQ.1) THEN
            WRITE(ILPFIL,9520) 'OPEN-BC X-'
          ELSEIF (JD.EQ.2) THEN
            WRITE(ILPFIL,9520) 'OPEN-BC X+'
          ELSEIF (JD.EQ.3) THEN
            WRITE(ILPFIL,9520) 'OPEN-BC Y-'
          ELSEIF (JD.EQ.4) THEN
            WRITE(ILPFIL,9520) 'OPEN-BC Y+'
          ENDIF
          IF     (IBCTYP(2,JD).EQ.0) THEN
            WRITE(ILPFIL,9520) '  FUNC          = TYPE1'
          ENDIF
          WRITE(ILPFIL,9540) '  DEPTH      [M]=',BCTYP(1,JD)
          WRITE(ILPFIL,9540) '  PERIOD     [S]=',BCTYP(3,JD)
          WRITE(ILPFIL,9540) '  LENGTH     [M]=',BCTYP(4,JD)
          WRITE(ILPFIL,9540) '  VELOCITY   [-]=',BCTYP(6,JD)
        ENDIF
 210  CONTINUE
      DO 220 JD=1,4
        IF (IDAMP(JD).GE.0) THEN
          IF     (JD.EQ.1) THEN
            WRITE(ILPFIL,9520) 'DAMP    X-'
          ELSEIF (JD.EQ.2) THEN
            WRITE(ILPFIL,9520) 'DAMP    X+'
          ELSEIF (JD.EQ.3) THEN
            WRITE(ILPFIL,9520) 'DAMP    Y-'
          ELSEIF (JD.EQ.4) THEN
            WRITE(ILPFIL,9520) 'DAMP    Y+'
          ENDIF
          WRITE(ILPFIL,9530) '  DEGREE     [-]=',IDAMP(  JD)
          WRITE(ILPFIL,9540) '  PARAM-XY   [-]=', DAMP(1,JD)
          WRITE(ILPFIL,9540) '  PARAM-Z    [-]=', DAMP(2,JD)
          WRITE(ILPFIL,9540) '  WIDTH      [M]=', DAMP(3,JD)
          WRITE(ILPFIL,9540) '  DEPTH      [M]=', DAMP(4,JD)
        ENDIF
 220  CONTINUE
      IF (LEQK.NE.0) THEN
        WRITE(ILPFIL,9540) 'K-EPS KMN[M2/S2]=',AKMINK
        WRITE(ILPFIL,9540) '      EMN[M2/S3]=',AKMINE
        WRITE(ILPFIL,9540) '      CMU    [-]=',AKCMU
        WRITE(ILPFIL,9540) '      SGK    [-]=',AKSGK
        WRITE(ILPFIL,9540) '      SGE    [-]=',AKSGE
        WRITE(ILPFIL,9540) '      C1     [-]=',AKC1
        WRITE(ILPFIL,9540) '      C2     [-]=',AKC2
        WRITE(ILPFIL,9540) '      C3     [-]=',AKC3
        WRITE(ILPFIL,9540) '      LG-K   [-]=',AKK0
        WRITE(ILPFIL,9540) '      LG-A   [-]=',AKA0
        IF (LEQT.NE.0) THEN
          WRITE(ILPFIL,9540) '      PRANDTL[-]=',AKPR
        ENDIF
        DO 230 LC=1,LEQC
          WRITE(ILPFIL,9540) '      SCHMIDT[-]=',AKSM(LC)
 230    CONTINUE
      ENDIF

CD    -- 数値解法関連情報を出力 --
      WRITE(ILPFIL,9510) '-- COMPUTATION --'
      IF (ISCMVP.EQ.0) THEN
        WRITE(ILPFIL,9540) 'SCHM  VP-DNR[-] =',SCMVP
      ENDIF
      IF (ISCMFF.EQ.0) THEN
        WRITE(ILPFIL,9520) 'SCHM  FF-DN-AC'
      ELSE
        WRITE(ILPFIL,9520) 'SCHM  FF-SLOPE'
      ENDIF
      IF (LEQK.NE.0) THEN
        IF (ISCMK.EQ.0) THEN
          WRITE(ILPFIL,9540) '      KE-DNR[-] =',SCMK
        ENDIF
      ENDIF
      IF (LEQT.NE.0) THEN
        IF (ISCMT.EQ.0) THEN
          WRITE(ILPFIL,9540) '      T-DNR [-] =',SCMT
        ENDIF
      ENDIF
      DO 300 LC=1,LEQC
        IF (ISCMC(LC).EQ.0) THEN
          WRITE(ILPFIL,9540) '      C-DNR [-] =',SCMC(LC)
        ENDIF
 300  CONTINUE
      IF (ICGTYP.EQ.0) THEN
        WRITE(ILPFIL,9520) 'MTRX  TYPE      = ILU-BCGSTAB'
      ELSE
        WRITE(ILPFIL,9520) 'MTRX  TYPE      = MILU-BCGSTAB'
        WRITE(ILPFIL,9540) '      PARAM [-] =',CGPARA
      ENDIF
      WRITE(ILPFIL,9530) '      I-MAX [-] =',ICGMAX
      WRITE(ILPFIL,9540) '      A-ERR [-] =',CGEPSA
      WRITE(ILPFIL,9540) '      R-ERR [-] =',CGEPSR

CD    -- ファイル制御関連情報を出力 --
      WRITE(ILPFIL,9510) '-- FILE CONTROL --'
      IF (IRETYP.LT.0) THEN
        WRITE(ILPFIL,9520) 'RES             = NOT READ'
      ELSE
        WRITE(ILPFIL,9520) 'RES   TYPE      = BY STEP'
        WRITE(ILPFIL,9530) '      START [-] =',IRETYP
      ENDIF
      IF (ILPTYP.EQ.0) THEN
        WRITE(ILPFIL,9520) 'L/P   TYPE      = NOT WRITE'
      ELSE
        IF (ILPTYP.EQ.1) THEN
          WRITE(ILPFIL,9520) 'L/P   TYPE      = BY STEP'
          WRITE(ILPFIL,9530) '      START [-] =',ILPTRN(1)
          WRITE(ILPFIL,9530) '      END   [-] =',ILPTRN(2)
          WRITE(ILPFIL,9530) '      DELTA [-] =',ILPTRN(3)
        ELSE
          WRITE(ILPFIL,9520) 'L/P   TYPE      = BY TIME'
          WRITE(ILPFIL,9540) '      START [S] =',RLPTRN(1)
          WRITE(ILPFIL,9540) '      END   [S] =',RLPTRN(2)
          WRITE(ILPFIL,9540) '      DELTA [S] =',RLPTRN(3)
        ENDIF
        IF     (ILPARA(1).EQ.1) THEN
          WRITE(ILPFIL,9530) '      XY    [-] =',(ILPARA(I)-1,I=2,3)
        ELSEIF (ILPARA(1).EQ.2) THEN
          WRITE(ILPFIL,9530) '      XZ    [-] =',(ILPARA(I)-1,I=2,3)
        ELSEIF (ILPARA(1).EQ.3) THEN
          WRITE(ILPFIL,9530) '      YZ    [-] =',(ILPARA(I)-1,I=2,3)
        ELSEIF (ILPARA(1).EQ.4) THEN
          WRITE(ILPFIL,9530) '      YX    [-] =',(ILPARA(I)-1,I=2,3)
        ELSEIF (ILPARA(1).EQ.5) THEN
          WRITE(ILPFIL,9530) '      ZX    [-] =',(ILPARA(I)-1,I=2,3)
        ELSEIF (ILPARA(1).EQ.6) THEN
          WRITE(ILPFIL,9530) '      ZY    [-] =',(ILPARA(I)-1,I=2,3)
        ENDIF
      ENDIF
      IF (IGRTYP.EQ.0) THEN
        WRITE(ILPFIL,9520) 'GRP   TYPE      = NOT WRITE'
      ELSE
        IF (IGRTYP.EQ.1) THEN
          WRITE(ILPFIL,9520) 'GRP   TYPE      = BY STEP'
          WRITE(ILPFIL,9530) '      START [-] =',IGRTRN(1)
          WRITE(ILPFIL,9530) '      END   [-] =',IGRTRN(2)
          WRITE(ILPFIL,9530) '      DELTA [-] =',IGRTRN(3)
        ELSE
          WRITE(ILPFIL,9520) 'GRP   TYPE      = BY TIME'
          WRITE(ILPFIL,9540) '      START [S] =',RGRTRN(1)
          WRITE(ILPFIL,9540) '      END   [S] =',RGRTRN(2)
          WRITE(ILPFIL,9540) '      DELTA [S] =',RGRTRN(3)
        ENDIF
        IF (IGRARA(1).LE.1) IGRARA(1)=2
        IF (IGRARA(2).LE.1) IGRARA(2)=2
        IF (IGRARA(3).LE.1) IGRARA(3)=2
        IF (IGRARA(4).LE.1) IGRARA(4)=NUMI0-1
        IF (IGRARA(5).LE.1) IGRARA(5)=NUMJ0-1
        IF (IGRARA(6).LE.1) IGRARA(6)=NUMK -1
        WRITE(ILPFIL,9530) '      AREA1 [-] =',(IGRARA(I)-1,I=1,3)
        WRITE(ILPFIL,9530) '      AREA2 [-] =',(IGRARA(I)-1,I=4,6)
        L=0
        IF (IGRARA(1).EQ.IGRARA(4)) L=L+1
        IF (IGRARA(2).EQ.IGRARA(5)) L=L+1
        IF (IGRARA(3).EQ.IGRARA(6)) L=L+1
        IF (L.GE.2) CALL VF_A2ERR('VF_OL1INI','INVALID VALUE.')
      ENDIF
      IF (IRSTYP.EQ.0) THEN
        WRITE(ILPFIL,9520) 'RSL   TYPE      = NOT WRITE'
      ELSE
        IF (IRSTYP.EQ.1) THEN
          WRITE(ILPFIL,9520) 'RSL   TYPE      = BY STEP'
          WRITE(ILPFIL,9530) '      START [-] =',IRSTRN(1)
          WRITE(ILPFIL,9530) '      END   [-] =',IRSTRN(2)
          WRITE(ILPFIL,9530) '      DELTA [-] =',IRSTRN(3)
        ELSE
          WRITE(ILPFIL,9520) 'RSL   TYPE      = BY TIME'
          WRITE(ILPFIL,9540) '      START [S] =',RRSTRN(1)
          WRITE(ILPFIL,9540) '      END   [S] =',RRSTRN(2)
          WRITE(ILPFIL,9540) '      DELTA [S] =',RRSTRN(3)
        ENDIF
      ENDIF
      IF (ITRTYP.EQ.0) THEN
        WRITE(ILPFIL,9520) 'TRN   TYPE      = NOT WRITE'
      ELSE
        IF (ITRTYP.EQ.1) THEN
          WRITE(ILPFIL,9520) 'TRN   TYPE      = BY STEP'
          WRITE(ILPFIL,9530) '      START [-] =',ITRTRN(1)
          WRITE(ILPFIL,9530) '      END   [-] =',ITRTRN(2)
          WRITE(ILPFIL,9530) '      DELTA [-] =',ITRTRN(3)
        ELSE
          WRITE(ILPFIL,9520) 'TRN   TYPE      = BY TIME'
          WRITE(ILPFIL,9540) '      START [S] =',RTRTRN(1)
          WRITE(ILPFIL,9540) '      END   [S] =',RTRTRN(2)
          WRITE(ILPFIL,9540) '      DELTA [S] =',RTRTRN(3)
        ENDIF
      ENDIF
      IF     (IPRNT.EQ.0) THEN
        WRITE(ILPFIL,9520) 'PORO  TYPE      = NOT READ'
      ELSEIF (IPRNT.EQ.1) THEN
        WRITE(ILPFIL,9520) 'PORO  TYPE      = CONST'
      ELSE
        WRITE(ILPFIL,9520) 'PORO  TYPE      = TRN'
        WRITE(ILPFIL,9530) '      NTIME [-] =',IPRNT
        WRITE(ILPFIL,9530) '      NBLOCK[-] =',IPRNB
        WRITE(ILPFIL,9530) '      NCELL [-] =',IPRNP
        DO 400 IB=1,IPRNB
          WRITE(ILPFIL,9530) '      AREA1 [-] =',(IPRARA(I,IB)-1,I=1,3)
          WRITE(ILPFIL,9530) '      AREA2 [-] =',(IPRARA(I,IB)-1,I=4,6)
 400    CONTINUE
      ENDIF

CD    -- オプション関連情報を出力 --
      WRITE(ILPFIL,9510) '-- OPTION --'
      WRITE(ILPFIL,9530) 'SUB-LOOP    [-] =',LOOPS
      IF (IBSUW0.EQ.0) THEN
        WRITE(ILPFIL,9520) 'S-CELL-VEL      = D2U=0'
      ELSE
        WRITE(ILPFIL,9520) 'S-CELL-VEL      = DU=0'
      ENDIF
      IF (WBUB.LT.ZERO) THEN
        WRITE(ILPFIL,9520) 'T-DOOR BUB      = NOT USE'
      ELSE
        WRITE(ILPFIL,9540) 'T-DOOR BUB [M/S]=',WBUB
      ENDIF
      IF (IDROP.EQ.0) THEN
        WRITE(ILPFIL,9520) 'T-DOOR DROP     = NOT USE'
      ELSEIF (IDROP.EQ.1) THEN
        WRITE(ILPFIL,9520) 'T-DOOR DROP     = FREE-RUNDOWN'
      ENDIF
      IF (PVCP0.LT.ZERO) THEN
        WRITE(ILPFIL,9520) 'PV=CONST        = NOT USE'
      ELSE
        WRITE(ILPFIL,9540) 'PV=CONST PP[PA] =',PVCP0
        WRITE(ILPFIL,9540) '         GM [-] =',PVCGM
      ENDIF
      IF (IDRGN.LE.0) THEN
        WRITE(ILPFIL,9520) 'DRAG-DF         = NOT USE'
      ELSE
        WRITE(ILPFIL,9540) 'DRAG-DF YU[M2/S]=',DRGYU
        WRITE(ILPFIL,9570) 'L','DR','AP','BT'
        DO 450 L=1,IDRGN
          WRITE(ILPFIL,9560) L,DRGDR(L),DRGAP(L),DRGBT(L)
 450    CONTINUE
      ENDIF

CD    -- 格子関連情報を出力 --
      WRITE(ILPFIL,9510) '-- GRID DATA --'
      WRITE(ILPFIL,9530) 'NUMBER OF GRID-X =',NUMI-1
      WRITE(ILPFIL,9550) 'I','X','DX','CX'
      DO 500 I=1,NUMI
        WRITE(ILPFIL,9560) I-1+IP,(XX(L,I),L=1,3)
 500  CONTINUE
      WRITE(ILPFIL,9530) 'NUMBER OF GRID-Y =',NUMJ-1
      WRITE(ILPFIL,9550) 'J','Y','DY','CY'
      DO 510 J=1,NUMJ
        WRITE(ILPFIL,9560) J-1+JP,(YY(L,J),L=1,3)
 510  CONTINUE
      WRITE(ILPFIL,9530) 'NUMBER OF GRID-Z =',NUMK-1
      WRITE(ILPFIL,9550) 'K','Z','DZ','CZ'
      DO 520 K=1,NUMK
        WRITE(ILPFIL,9560) K-1   ,(ZZ(L,K),L=1,3)
 520  CONTINUE

CD    -- 障害物関連情報を出力 --
      WRITE(ILPFIL,9510) '-- OBSTACLE --'
      IF (ILPON(1).NE.0) THEN
        WRITE(ILPFIL,9510) '<<OBSTACLE>>'
        CALL VF_OL3DNF(NF)
      ENDIF
      IF (ILPON(3).NE.0) THEN
        WRITE(ILPFIL,9510) '<<CM0>>'
        CALL VF_OL3DR(CM0,0)
      ENDIF
      IF (ILPON(4).NE.0) THEN
        WRITE(ILPFIL,9510) '<<CD0>>'
        CALL VF_OL3DR(CD0,0)
      ENDIF
      IF (ILPON(5).NE.0 .AND. IPRNT.LE.1) THEN
        WRITE(ILPFIL,9510) '<<VOLUME POROSITY>>'
        CALL VF_OL3DR(GGV,0)
        WRITE(ILPFIL,9510) '<<SURFACE PERMEABILITY-X>>'
        CALL VF_OL3DR(GGX,1)
        WRITE(ILPFIL,9510) '<<SURFACE PERMEABILITY-Y>>'
        CALL VF_OL3DR(GGY,1)
        WRITE(ILPFIL,9510) '<<SURFACE PERMEABILITY-Z>>'
        CALL VF_OL3DR(GGZ,1)
      ENDIF
      IF (ILPON(6).NE.0 .AND. IPRNT.LE.1) THEN
        WRITE(ILPFIL,9510) '<<GLV>>'
        CALL VF_OL3DR(GLV,0)
        WRITE(ILPFIL,9510) '<<GLX>>'
        CALL VF_OL3DR(GLX,1)
        WRITE(ILPFIL,9510) '<<GLY>>'
        CALL VF_OL3DR(GLY,1)
        WRITE(ILPFIL,9510) '<<GLZ>>'
        CALL VF_OL3DR(GLZ,1)
      ENDIF

CD    -- 境界条件関連情報を出力 --
      WRITE(ILPFIL,9510) '-- BOUNDARY --'
      IF (ILPON(2).NE.0) THEN
        WRITE(ILPFIL,9510) '<<INDX>>'
        CALL VF_OL3DI(INDX,1)
        WRITE(ILPFIL,9510) '<<INDY>>'
        CALL VF_OL3DI(INDY,1)
        WRITE(ILPFIL,9510) '<<INDZ>>'
        CALL VF_OL3DI(INDZ,1)
        WRITE(ILPFIL,9510) '<<INDB>>'
        CALL VF_OLBC(BCU,BCV,BCW,BCP,BCF,BCK,BCE,BCT,BCTI,BCC,BCCI,
     &               INDB,INDBK,INDBE,INDBT,INDBC)
      ENDIF
      DO 610 L=1,NUMB
        IF (INDB(3,L).EQ.0 .OR. INDB(4,L).EQ.0)
     &                 CALL VF_A2ERR('VF_OL1INI','UNDEFINED.')
        IF (LEQK.NE.0) THEN
          IF (INDBK (L).EQ.0) CALL VF_A2ERR('VF_OL1INI','UNDEFINED.')
        ENDIF
        IF (LEQK.NE.0) THEN
          IF (INDBE (L).EQ.0) CALL VF_A2ERR('VF_OL1INI','UNDEFINED.')
        ENDIF
        IF (LEQT.NE.0) THEN
          IF (INDBT (L).EQ.0) CALL VF_A2ERR('VF_OL1INI','UNDEFINED.')
        ENDIF
        DO 600 LC=1,LEQC
          IF (INDBC(L,LC).EQ.0)
     &                 CALL VF_A2ERR('VF_OL1INI','UNDEFINED.')
 600    CONTINUE
 610  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(/' ',A)
 9520 FORMAT( ' ',A)
 9530 FORMAT( ' ',A ,100(' ',I6:))
 9540 FORMAT( ' ',A ,100(' ',1PE12.5:))
 9550 FORMAT( ' ',A6,100(' ',A12:))
 9560 FORMAT( ' ',I6,100(' ',1PE12.5:))
 9570 FORMAT( ' ',A6,100(A13:))

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
