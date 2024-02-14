      PROGRAM VF_A1MAIN

CD=== 概要 ===========================================================

CDT   VF_A1MAIN:CADMAS-SURF/3D-2Fのメインルーチン
CD      (1)解析対象:自由表面を含む気液2相3次元非圧縮性流体
CD      (2)解析方法:差分法,SMAC法,VOF法
CD      (3)座標系  :デカルト座標系
CD      (4)言語    :Fortran90(配列の動的アロケートのため)

C==== 宣言 ===========================================================

      use vf_a2array
      use mod_comm,only: init_mpmd,comm_mlicdsmg2fc,comm_2fc_dem
      use mod_dem

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'
      INCLUDE  'mpif.h'
      INCLUDE 'SF_STRUCT.h'
C----------------------------------------------------------2016.09 start
      INCLUDE 'VF_ASEABT.h'
C----------------------------------------------------------2016.09 end

CD    -- 局所変数 --
CD    XX(MAXG1,NUMI)    : R*8 : x方向格子座標等
CD                              ( 1,I):格子座標x(I)
CD                              ( 2,I):dx=x(I+1)-x(I)
CD                              ( 3,I):cx=(dx(I)+dx(I-1))/2.0
CD                              ( 4,I):1.0/dx
CD                              ( 5,I):1.0/cx
CD                              ( 6,I):1.0/(dx(I)+dx(I-1))
CD    YY(MAXG1,NUMJ)    : R*8 : y方向格子座標等
CD                              ( 1,J):格子座標y(J)
CD                              ( 2,J):dy=y(J+1)-y(J)
CD                              ( 3,J):cy=(dy(J)+dy(J-1))/2.0
CD                              ( 4,J):1.0/dy
CD                              ( 5,J):1.0/cy
CD                              ( 6,J):1.0/(dy(J)+dy(J-1))
CD    ZZ(MAXG1,NUMK)    : R*8 : z方向格子座標等
CD                              ( 1,K):格子座標z(K)
CD                              ( 2,K):dz=z(K+1)-z(K)
CD                              ( 3,K):cz=(dz(K)+dz(K-1))/2.0
CD                              ( 4,K):1.0/dz
CD                              ( 5,K):1.0/cz
CD                              ( 6,K):1.0/(dz(K)+dz(K-1))
CD    UU(@FOR-3D@)      : R*8 : x方向流速
CD    VV(@FOR-3D@)      : R*8 : y方向流速
CD    WW(@FOR-3D@)      : R*8 : z方向流速
CD    PP(@FOR-3D@)      : R*8 : 圧力
CD    FF(@FOR-3D@)      : R*8 : VOF関数F
CD    FX(@FOR-3D@)      : R*8 : x方向スタッガードセルでのVOF関数Fx
CD    FY(@FOR-3D@)      : R*8 : y方向スタッガードセルでのVOF関数Fy
CD    FZ(@FOR-3D@)      : R*8 : z方向スタッガードセルでのVOF関数Fz
CD    ANU(@FOR-3D@)     : R*8 : 分子動粘性係数と渦動粘性係数の和
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CD    RHOG(@FOR-3D@)    : R*8 : 気相密度
CD    DRHODP(@FOR-3D@)  : R*8 : (dρ/dp)/ρ [1/Pa]
CD    DRHODT(@FOR-3D@)  : R*8 : (dρ/dt)/ρ [1/s]
CD    UUO(@FOR-3D@)     : R*8 : 前時刻のx方向流速
CD    VVO(@FOR-3D@)     : R*8 : 前時刻のy方向流速
CD    WWO(@FOR-3D@)     : R*8 : 前時刻のz方向流速
CD    PPO(@FOR-3D@)     : R*8 : 前時刻の圧力
CD    RHOGO(@FOR-3D@)   : R*8 : 前時刻の気相密度
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CD    CM0(@FOR-3D@)     : R*8 : 慣性力係数
CD    CD0(@FOR-3D@)     : R*8 : 抵抗係数
CD    GGV(@FOR-3D@)     : R*8 : 空隙率
CD    GGX(@FOR-3D@)     : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)     : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)     : R*8 : z方向面積透過率
CD    GLV(@FOR-3D@)     : R*8 : =GGV+(1-GGV)*CM
CD    GLX(@FOR-3D@)     : R*8 : =GGX+(1-GGX)*CM
CD    GLY(@FOR-3D@)     : R*8 : =GGY+(1-GGY)*CM
CD    GLZ(@FOR-3D@)     : R*8 : =GGZ+(1-GGZ)*CM
CD    BCU(NUMB,3)       : R*8 : x方向流速の境界値
CD    BCV(NUMB,3)       : R*8 : y方向流速の境界値
CD    BCW(NUMB,3)       : R*8 : z方向流速の境界値
CD    BCP(NUMB,3)       : R*8 : 圧力の境界値
CD    BCF(NUMB)         : R*8 : VOF関数Fの境界値
CD    BCVI(NUMB)        : R*8 : 流速の境界条件(壁面の粗さ)
CD    TBUB(NUMK)        : R*8 : 気泡上昇処理を最後に行った時間
CD    DROPTX(@FOR-3D@)  : R*8 : 自由落下処理を最後に行った時間(x)
CD    DROPTY(@FOR-3D@)  : R*8 : 自由落下処理を最後に行った時間(y)
CD    DROPTZ(@FOR-3D@)  : R*8 : 自由落下処理を最後に行った時間(z)
CD    DROPUU(@FOR-3D@)  : R*8 : 自由落下のx方向速度
CD    DROPVV(@FOR-3D@)  : R*8 : 自由落下のy方向速度
CD    DROPWW(@FOR-3D@)  : R*8 : 自由落下のz方向速度
CD    GGVOLD(IPRNP)     : R*8 : 前の時刻ブロックの空隙率
CD    GGVNOW(IPRNP)     : R*8 : 現在の時刻ブロックの空隙率
CD    GGVEL(3,IPRNP)    : R*8 : 障害物移動速度（時間依存用）
CD    GGVELO(3,IPRNP)   : R*8 : 前の時刻の障害物移動速度
CD    GGVELN(3,IPRNP)   : R*8 : 現在の時刻の障害物移動速度
CD    GGVLO(3,IPRNB)    : R*8 : 前の時刻ブロックの障害物移動速度
CD    GGVLN(3,IPRNB)    : R*8 : 現在の時刻ブロックの障害物移動速度
CD    GGV0(@FOR-3D@)    : R*8 : 空隙率(時間依存用)
CD    GLV0(@FOR-3D@)    : R*8 : =GGV+(1-GGV)*CM(時間依存用)
CD    GVL0(@FOR-3D@)    : R*8 : 障害物移動速度(時間依存用)
CD    ANUT(@FOR-3D@)    : R*8 : 渦動粘性係数νt
CD    AK(@FOR-3D@)      : R*8 : 乱流エネルギ
CD    AE(@FOR-3D@)      : R*8 : 乱流エネルギ散逸
CD    BCK(NUMB,3)       : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB,3)       : R*8 : 乱流エネルギ散逸の境界値
CD    TT(@FOR-3D@)      : R*8 : 温度
CD    ALM(@FOR-3D@)     : R*8 : 熱伝導率と乱流熱伝導率の和
CD    BCT(NUMB)         : R*8 : 温度の境界値
CD    BCTI(2,NUMB)      : R*8 : 温度の境界条件
CD                              (1,L):熱伝達係数or熱流束
CD                              (2,L):外部温度
CD    CC(@FOR-3D@,LEQC) : R*8 : 濃度
CD    DD(@FOR-3D@,LEQC) : R*8 : 拡散係数と乱流拡散係数の和
CD    BCC(NUMB,LEQC)    : R*8 : 濃度の境界値
CD    BCCI(2,NUMB,LEQC) : R*8 : 濃度の境界条件
CD                              (1,L):物質移動係数or拡散流束
CD                              (2,L):外部濃度
CD    DMTBTT(MTBTT)     : R*8 : マトリクスデータの無次元位相
CD    DMTBZZ(MTBZZ)     : R*8 : マトリクスデータのz座標(平均水位をゼロ)
CD    DMTBHH(MTBTT)     : R*8 : マトリクスデータの水位
CD    DMTBUN(MTBZZ,MTBTT) : R*8 : マトリクスデータの水平方向流速
CD    DMTBUT(MTBZZ,MTBTT) : R*8 : マトリクスデータの鉛直方向流速
CD    DBUF(NUMBUF*MAXBUF) : R*8 : 並列用のバッファ
CD    RBUF(NUMBUF*MAXBUF) : R*8 : 並列用のバッファ
CD    WK01-18(@FOR-3D@) : R*8 : ワーク配列
CD    WKBC(NUMB*3)      : R*8 : ワーク配列
CD    NF(@FOR-3D@)      : I*4 : セルの状態を示すインデックス
CD                              =-2:移動障害物セル
CD                              =-1:障害物セル
CD                              = 0:流体セル
CD                              = 1:表面セル:x負方向に流体
CD                              = 2:表面セル:x正方向に流体
CD                              = 3:表面セル:y負方向に流体
CD                              = 4:表面セル:y正方向に流体
CD                              = 5:表面セル:z負方向に流体
CD                              = 6:表面セル:z正方向に流体
CD                              = 8:気体セル
CD    INDX(@FOR-3D@)    : I*4 : x面の状態を示すインデックス
CD                              =-2  :移動障害物面
CD                              =-1  :障害物面
CD                              = 0  :通常面
CD                              >=1  :境界面(INDBへのポインタ)
CD                              > NUMB0 :移動障害物の境界面(INDBへのポインタ)
CD    INDY(@FOR-3D@)    : I*4 : y面の状態を示すインデックス
CD                              =-2  :移動障害物面
CD                              =-1  :障害物面
CD                              = 0  :通常面
CD                              >=1  :境界面(INDBへのポインタ)
CD                              > NUMB0 :移動障害物の境界面(INDBへのポインタ)
CD    INDZ(@FOR-3D@)    : I*4 : z面の状態を示すインデックス
CD                              =-2  :移動障害物面
CD                              =-1  :障害物面
CD                              = 0  :通常面
CD                              >=1  :境界面(INDBへのポインタ)
CD                              > NUMB0 :移動障害物の境界面(INDBへのポインタ)
CD    INDC(@FOR-3D@)    : I*4 : セルの計算状態を示すインデックス
CD                              =-1:非計算セル(障害物または気体)
CD                              = 0:計算セル(表面)
CD                              = 1:計算セル(流体)
CD                              = 2:計算セル(気体)
CD    INDB(MAXB1,NUMB)  : I*4 : 境界面のインデックス
CD                              (1,L):境界面のI,J,K座標の1次元表記
CD                                    (I+NUMI*(J-1)+NUMI*NUMJ*(K-1))
CD                              (2,L):境界面の向き
CD                                =1:x方向負側に構造物
CD                                =2:x方向正側に構造物
CD                                =3:y方向負側に構造物
CD                                =4:y方向正側に構造物
CD                                =5:z方向負側に構造物
CD                                =6:z方向正側に構造物
CD                              (3,L):流速・圧力の境界条件(液相)
CD                                =0:未定義
CD                                =1:スリップ
CD                                =2:ノンスリップ
CD                                =3:流速固定
CD                                =4:フリー
CD                                =5:造波境界
CD                                =6:対数則
CD                                =7:放射境界
CD                                =8:完全粗面境界
CD                              (4,L):VOF関数Fの境界条件
CD                                =0:未定義
CD                                =1:値固定
CD                                =2:フリー
CD                                =5:造波境界
CD                                =7:放射境界
CD                              (5,L):流速・圧力の境界条件(気相)
CD                                =0:未定義
CD                                =1:スリップ
CD                                =2:ノンスリップ
CD                                =3:流速固定
CD                                =4:フリー
CD                                =5:造波境界
CD                                =6:対数則
CD                                =7:放射境界
CD                                =8:完全粗面境界
CD    INDS(@FOR-1D@)    : I*4 : 表面セルのI,J,K座標
CD                                (I+NUMI*(J-1)+NUMI*NUMJ*(K-1))
CD    INDBK(MAXBK1,NUMB): I*4 : 乱流エネルギの境界条件
CD                             (1,*):液相
CD                             (2,*):気相
CD                                =-2:勾配ゼロ(移流項を評価しない)
CD                                =-1:値固定(移流項を評価しない)
CD                                = 0:未定義
CD                                = 1:値固定(移流項を評価する)
CD                                = 2:勾配ゼロ移流項を評価する)
CD                                = 6:対数則
CD                                = 8:完全粗面境界
CD    INDBE(MAXBE1,NUMB): I*4 : 乱流エネルギ散逸の境界条件
CD                             (1,*):液相
CD                             (2,*):気相
CD                                =-2:勾配ゼロ(移流項を評価しない)
CD                                =-1:値固定(移流項を評価しない)
CD                                = 0:未定義
CD                                = 1:値固定(移流項を評価する)
CD                                = 2:勾配ゼロ移流項を評価する)
CD                                = 6:対数則
CD                                = 8:完全粗面境界
CD    INDBT(NUMB)       : I*4 : 温度の境界条件
CD                                =-4:熱伝達(移流項を評価しない)
CD                                =-3:熱流束(移流項を評価しない)
CD                                =-2:断熱(移流項を評価しない)
CD                                =-1:温度固定(移流項を評価しない)
CD                                = 0:未定義
CD                                = 1:温度固定(移流項を評価する)
CD                                = 2:断熱(移流項を評価する)
CD                                = 3:熱流束(移流項を評価する)
CD                                = 4:熱伝達(移流項を評価する)
CD    INDBC(NUMB,LEQC)  : I*4 : 濃度の境界条件
CD                                =-4:物質移動(移流項を評価しない)
CD                                =-3:拡散流束(移流項を評価しない)
CD                                =-2:勾配ゼロ(移流項を評価しない)
CD                                =-1:濃度固定(移流項を評価しない)
CD                                = 0:未定義
CD                                = 1:濃度固定(移流項を評価する)
CD                                = 2:勾配ゼロ(移流項を評価する)
CD                                = 3:拡散流束(移流項を評価する)
CD                                = 4:物質移動(移流項を評価する)
CD    INDX0(@FOR-3D@)   : I*4 : 移動障害物がない状態でのINDX(移動障害物処理用)
CD    INDY0(@FOR-3D@)   : I*4 : 移動障害物がない状態でのINDY(移動障害物処理用)
CD    INDZ0(@FOR-3D@)   : I*4 : 移動障害物がない状態でのINDZ(移動障害物処理用)
CD    INDB0(MAXB1,NUMB) : I*4 : 移動障害物がない状態でのINDB(移動障害物処理用)
CD    IBUF(NUMBUF*MAXBUF) : I*4 : 並列用のバッファ
CD    NWK1(@FOR-3D@)    : I*4 : ワーク配列
CD    NWKBC(NUMB)       : I*4 : ワーク配列
C----------------------------------------------------for MG/2FC coupling
CD    XPF(NUMI)                      :x方向の親格子に対する補間係数
CD    YPF(NUMJ)                      :y方向の親格子に対する補間係数
CD    ZPF(NUMK)                      :z方向の親格子に対する補間係数
CD    IPF(MGPINF(1))                 :x方向の親格子1に対する格子の数
CD    JPF(MGPINF(2))                 :y方向の親格子1に対する格子の数
CD    KPF(MGPINF(3))                 :z方向の親格子1に対する格子の数
C----------------------------------------------------for MG/2FC coupling
C----------------------------------------------------------2016.09 start
CD    DELH(@FOR-2D@)    : R*8 : 水位の増分[m]
CD    DELH_IN(@FOR-2D@) : R*8 : 水位の増分(ファイル読み込み値)[m]
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: DELH
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: DELH_IN
C----------------------------------------------------------2016.09 end
      CHARACTER*5 TEXTP
      INTEGER IWORK(2)
      INTEGER IEND(2),IEND1(2)

C==== 実行 ===========================================================

      call init_mpmd

CD    -- 経過時間の計測(開始時) --
      CALL VF_P0TIME(WTM1)

CD    -- 並列環境の初期化 --
      CALL VF_P0INIT()
      IHIDEM = 0
      MYHOST = 0

CD    -- タイマーの初期化と開始 --
      CALL VF_A2CPUT(0,ICPUIN,0     )
      CALL VF_A2CPUT(0,ICPUST,KCP0AL)
      CALL VF_A2CPUT(0,ICPUST,KCP1PR)

CD    -- マルチグリッド環境ファイルの読み込み
      IF (MGRANK.EQ.0) WRITE(*,9510) IVR001,IVR002
      IF (MGRANK.EQ.0) WRITE(*,*) '*** 2fc ver5.3.1 ***'
C----------------------------------------------------for MG/2FC coupling
      CALL VF_PMGINP()
C----------------------------------------------------for MG/2FC coupling

CD    -- 構造解析との連成フラッグ設定 --
      CALL SF_COMM_INIT()

CD    -- 土砂移動解析との連成フラッグ設定 --
      CALL SF_STM_COMM_INIT()

CD    -- リストファイルのオープンと開始コメントの出力 --
      ILPFIL=0
      IF (NPROCS.EQ.1) THEN
C----------------------------------------------------for MG/2FC coupling
C       OPEN(MFILLP,ERR=9010,FILE='data.list',
        OPEN(MFILLP,ERR=9010,
     &       FILE=TRIM(MGNAME(MGRANK+1))//'.list',
C----------------------------------------------------for MG/2FC coupling
     &       STATUS='NEW',FORM='FORMATTED'    )
      ELSE
        WRITE(TEXTP,'(I5.5)') MYRANK
C----------------------------------------------------for MG/2FC coupling
C       OPEN(MFILLP,ERR=9010,FILE='data.list'//TEXTP,
        OPEN(MFILLP,ERR=9010,
     &       FILE=TRIM(MGNAME(MGRANK+1))//'.list'//TEXTP,
C----------------------------------------------------for MG/2FC coupling
     &       STATUS='NEW',FORM='FORMATTED'    )
      ENDIF
      ILPFIL=MFILLP
      WRITE(ILPFIL,9510) IVR001,IVR002
      WRITE(ILPFIL,9570) MYRANK,NPROCS

CD    -- コモン変数へのデフォルト値の設定 --
      IF (MYRANK.EQ.0) WRITE(*,9520) 'DEFAULT.'
      WRITE(ILPFIL,9520) 'DEFAULT.'
      CALL VF_A2DFLT()

CD    -- STOCとの通信環境の初期化 --
      IERR = 0
      CALL VF_STOC_INIT(IERR)

CD    -- 入力ファイルの読み込みと配列のアロケート等 --
C     Fortran90のALLOCATE文だけではきれいに配列のアロケート
C     ができないので、あえて、3度読みを行う
      IF (MYRANK.EQ.0) WRITE(*,9520) 'INPUT-DATA.'
      WRITE(ILPFIL,9520) 'INPUT-DATA.'
C     * 読み込み(1)
      CALL VF_II1INP(XX,YY,ZZ,CM0,CD0,GGV,GGX,GGY,GGZ,
C----------------------------------------------------for MG/2FC coupling
     &               XPF,YPF,ZPF,IPF,JPF,KPF,
C----------------------------------------------------for MG/2FC coupling
     &               BCU,BCV,BCW,BCP,BCF,BCVI,BCK,BCE,BCT,BCTI,BCC,BCCI,
     &               NF,INDX,INDY,INDZ,INDB,INDBK,INDBE,INDBT,INDBC,
     &               DBUF,IBUF)
C     * 配列のアロケートとデフォルト値の設定(1)
      IERR = 0
      ALLOCATE(XX  (MAXG1,NUMI)    ,STAT=IERR)
      ALLOCATE(YY  (MAXG1,NUMJ)    ,STAT=IERR)
      ALLOCATE(ZZ  (MAXG1,NUMK)    ,STAT=IERR)
      ALLOCATE(UU  (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(VV  (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WW  (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(PP  (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(PPBK(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(FF  (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(FX  (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(FY  (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(FZ  (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(ANU (NUMI,NUMJ,NUMK),STAT=IERR)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ALLOCATE(RHOG(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(DRHODP(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(DRHODT(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(RHOGO(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(UUO(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(VVO(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WWO(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(PPO(NUMI,NUMJ,NUMK),STAT=IERR)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ALLOCATE(CM0 (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(CD0 (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(GGV (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(GGX (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(GGY (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(GGZ (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(GLV (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(GLX (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(GLY (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(GLZ (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(TBUB(NUMK)          ,STAT=IERR)
      ALLOCATE(DROPTX(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(DROPTY(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(DROPTZ(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(DROPUU(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(DROPVV(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(DROPWW(NUMI,NUMJ,NUMK),STAT=IERR)
      IF (LEQK.NE.0) THEN
        ALLOCATE(ANUT(NUMI,NUMJ,NUMK),STAT=IERR)
        ALLOCATE(AK  (NUMI,NUMJ,NUMK),STAT=IERR)
        ALLOCATE(AE  (NUMI,NUMJ,NUMK),STAT=IERR)
      ENDIF
      IF (LEQT.NE.0) THEN
        ALLOCATE(TT  (NUMI,NUMJ,NUMK),STAT=IERR)
        ALLOCATE(ALM (NUMI,NUMJ,NUMK),STAT=IERR)
      ENDIF
      IF (LEQC.GT.0) THEN
        ALLOCATE(CC(NUMI,NUMJ,NUMK,LEQC),STAT=IERR)
        ALLOCATE(DD(NUMI,NUMJ,NUMK,LEQC),STAT=IERR)
      ENDIF
      ALLOCATE(DBUF(NUMBUF*MAXBUF) ,STAT=IERR)
      ALLOCATE(WK01(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK02(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK03(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK04(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK05(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK06(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK07(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK08(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK09(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK10(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK11(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK12(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK13(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK14(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK15(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK16(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK17(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK18(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(NF  (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(INDX(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(INDY(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(INDZ(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(INDC(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(INDS(NUMI*NUMJ*NUMK),STAT=IERR)
      ALLOCATE(IBUF(NUMBUF*MAXBUF) ,STAT=IERR)
      ALLOCATE(NWK1(NUMI,NUMJ,NUMK),STAT=IERR)
C----------------------------------------------------for MG/2FC coupling
      ALLOCATE(XPF(NUMI),STAT=IERR)
      ALLOCATE(YPF(NUMJ),STAT=IERR)
      ALLOCATE(ZPF(NUMK),STAT=IERR)
C----------------------------------------------------for MG/2FC coupling
C----------------------------------------------------for 2FC/STM coupling
      ALLOCATE(GGW(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(GGR(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(FLFU(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(FLFV(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(LNDC(NUMI,NUMJ),STAT=IERR)
      ALLOCATE(LNDC0(NUMI,NUMJ),STAT=IERR)
      ALLOCATE(LNDC_0(NUMI,NUMJ),STAT=IERR)
      ALLOCATE(KST(NUMI,NUMJ),STAT=IERR)
      ALLOCATE(DZ(NUMI,NUMJ),STAT=IERR)
      ALLOCATE(SUMZ(NUMI,NUMJ),STAT=IERR)
      ALLOCATE(WK19(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WK20(NUMI,NUMJ,NUMK),STAT=IERR)
C----------------------------------------------------for 2FC/STM coupling
      IF (IERR.NE.0) CALL VF_A2ERR('VF_A1MAIN','CAN NOT ALLOC.')
      CALL VF_ZSETR2(XX  ,0.0D0,MAXG1,NUMI)
      CALL VF_ZSETR2(YY  ,0.0D0,MAXG1,NUMJ)
      CALL VF_ZSETR2(ZZ  ,0.0D0,MAXG1,NUMK)
      CALL VF_ZSETR3(UU  ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(VV  ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WW  ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(PP  ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(PPBK ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(FF  ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(FX  ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(FY  ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(FZ  ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(ANU ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(CM0 ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(CD0 ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(GGV ,1.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(GGX ,1.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(GGY ,1.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(GGZ ,1.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(GLV ,1.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(GLX ,1.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(GLY ,1.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(GLZ ,1.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR1(TBUB,0.0D0,NUMK)
      CALL VF_ZSETR3(DROPTX,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(DROPTY,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(DROPTZ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(DROPUU,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(DROPVV,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(DROPWW,0.0D0,NUMI,NUMJ,NUMK)
      IF (LEQK.NE.0) THEN
        CALL VF_ZSETR3(ANUT,0.0D0,NUMI,NUMJ,NUMK)
        CALL VF_ZSETR3(AK  ,0.0D0,NUMI,NUMJ,NUMK)
        CALL VF_ZSETR3(AE  ,0.0D0,NUMI,NUMJ,NUMK)
      ENDIF
      IF (LEQT.NE.0) THEN
        CALL VF_ZSETR3(TT ,0.0D0,NUMI,NUMJ,NUMK)
        CALL VF_ZSETR3(ALM,0.0D0,NUMI,NUMJ,NUMK)
      ENDIF
      DO 100 LC=1,LEQC
        CALL VF_ZSETR3(CC(1,1,1,LC),0.0D0,NUMI,NUMJ,NUMK)
        CALL VF_ZSETR3(DD(1,1,1,LC),0.0D0,NUMI,NUMJ,NUMK)
 100  CONTINUE
      CALL VF_ZSETR1(DBUF,0.0D0,NUMBUF*MAXBUF )
      CALL VF_ZSETR3(WK01,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK02,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK03,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK04,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK05,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK06,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK07,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK08,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK09,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK10,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK11,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK12,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK13,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK14,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK15,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK16,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK17,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK18,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETI3(NF  ,    0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETI3(INDX,    0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETI3(INDY,    0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETI3(INDZ,    0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETI3(INDC,    0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETI1(INDS,    0,NUMI*NUMJ*NUMK)
      CALL VF_ZSETI1(IBUF,    0,NUMBUF*MAXBUF )
      CALL VF_ZSETI3(NWK1,    0,NUMI,NUMJ,NUMK)
C----------------------------------------------------for MG/2FC coupling
      CALL VF_ZSETR1(XPF,0.0D0,NUMI)
      CALL VF_ZSETR1(YPF,0.0D0,NUMJ)
      CALL VF_ZSETR1(ZPF,0.0D0,NUMK)
C----------------------------------------------------for MG/2FC coupling
C----------------------------------------------------for 2FC/STM coupling
      CALL VF_ZSETR3(GGW,1.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(GGR,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(FLFU,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(FLFV,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETI2(LNDC,0,NUMI,NUMJ)
      CALL VF_ZSETI2(LNDC0,0,NUMI,NUMJ)
      CALL VF_ZSETI2(LNDC_0,0,NUMI,NUMJ)
      CALL VF_ZSETI2(KST,0,NUMI,NUMJ)
      CALL VF_ZSETR2(DZ,0.0D0,NUMI,NUMJ)
      CALL VF_ZSETR2(SUMZ,0.0D0,NUMI,NUMJ)
      CALL VF_ZSETR3(WK19,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WK20,0.0D0,NUMI,NUMJ,NUMK)
C----------------------------------------------------for 2FC/STM coupling
C     * 読み込み(2)
      CALL VF_II1INP(XX,YY,ZZ,CM0,CD0,GGV,GGX,GGY,GGZ,
C----------------------------------------------------for MG/2FC coupling
     &               XPF,YPF,ZPF,IPF,JPF,KPF,
C----------------------------------------------------for MG/2FC coupling
     &               BCU,BCV,BCW,BCP,BCF,BCVI,BCK,BCE,BCT,BCTI,BCC,BCCI,
     &               NF,INDX,INDY,INDZ,INDB,INDBK,INDBE,INDBT,INDBC,
     &               DBUF,IBUF)
      IERR = 0
      ALLOCATE(BCU  (NUMB,3    ),STAT=IERR)
      ALLOCATE(BCV  (NUMB,3    ),STAT=IERR)
      ALLOCATE(BCW  (NUMB,3    ),STAT=IERR)
      ALLOCATE(BCP  (NUMB,3    ),STAT=IERR)
      ALLOCATE(BCF  (NUMB      ),STAT=IERR)
      ALLOCATE(BCVI (NUMB      ),STAT=IERR)
      IF (LEQK.NE.0) THEN
        ALLOCATE(BCK (  NUMB,3 ),STAT=IERR)
        ALLOCATE(BCE (  NUMB,3 ),STAT=IERR)
      ENDIF
      IF (LEQT.NE.0) THEN
        ALLOCATE(BCT (  NUMB   ),STAT=IERR)
        ALLOCATE(BCTI(2,NUMB   ),STAT=IERR)
      ENDIF
      IF (LEQC.GT.0) THEN
        ALLOCATE(BCC (  NUMB,LEQC),STAT=IERR)
        ALLOCATE(BCCI(2,NUMB,LEQC),STAT=IERR)
      ENDIF
      ALLOCATE(WKBC (NUMB*3    ),STAT=IERR)
      ALLOCATE(INDB (MAXB1,NUMB),STAT=IERR)
      IF (LEQK.NE.0) THEN
        ALLOCATE(INDBK(MAXBK1,NUMB),STAT=IERR)
        ALLOCATE(INDBE(MAXBE1,NUMB),STAT=IERR)
      ENDIF
      IF (LEQT.NE.0) THEN
        ALLOCATE(INDBT(NUMB     ),STAT=IERR)
      ENDIF
      IF (LEQC.GT.0) THEN
        ALLOCATE(INDBC(NUMB,LEQC),STAT=IERR)
      ENDIF
      ALLOCATE(NWKBC(NUMB      ),STAT=IERR)
C----------------------------------------------------for MG/2FC coupling
      IF(MGPARE(MGRANK+1).GT.0) THEN
        ALLOCATE(IPF(0:MGPINF(1)),STAT=IERR)
        ALLOCATE(JPF(0:MGPINF(2)),STAT=IERR)
        ALLOCATE(KPF(0:MGPINF(3)),STAT=IERR)
      END IF
C----------------------------------------------------for MG/2FC coupling
      IF (IERR.NE.0) CALL VF_A2ERR('VF_A1MAIN','CAN NOT ALLOC.')
      CALL VF_ZSETR2(BCU  ,0.0D0,NUMB,3)
      CALL VF_ZSETR2(BCV  ,0.0D0,NUMB,3)
      CALL VF_ZSETR2(BCW  ,0.0D0,NUMB,3)
      CALL VF_ZSETR2(BCP  ,0.0D0,NUMB,3)
      CALL VF_ZSETR1(BCF  ,0.0D0,NUMB)
      CALL VF_ZSETR1(BCVI ,0.0D0,NUMB)
      IF (LEQK.NE.0) THEN
        CALL VF_ZSETR2(BCK ,0.0D0,NUMB,3)
        CALL VF_ZSETR2(BCE ,0.0D0,NUMB,3)
      ENDIF
      IF (LEQT.NE.0) THEN
        CALL VF_ZSETR1(BCT ,0.0D0,  NUMB)
        CALL VF_ZSETR2(BCTI,0.0D0,2,NUMB)
      ENDIF
      DO 110 LC=1,LEQC
        CALL VF_ZSETR1(BCC (  1,LC),0.0D0,  NUMB)
        CALL VF_ZSETR2(BCCI(1,1,LC),0.0D0,2,NUMB)
 110  CONTINUE
      CALL VF_ZSETR1(WKBC ,0.0D0,NUMB*3)
      CALL VF_ZSETI2(INDB ,    0,MAXB1,NUMB)
      IF (LEQK.NE.0) THEN
        CALL VF_ZSETI2(INDBK,  0,MAXBK1,NUMB)
        CALL VF_ZSETI2(INDBE,  0,MAXBE1,NUMB)
      ENDIF
      IF (LEQT.NE.0) THEN
        CALL VF_ZSETI1(INDBT,  0,NUMB)
      ENDIF
      DO 120 LC=1,LEQC
        CALL VF_ZSETI1(INDBC(1,LC),0,NUMB)
 120  CONTINUE
      CALL VF_ZSETI1(NWKBC,    0,NUMB)
C----------------------------------------------------for MG/2FC coupling
      IF(MGPARE(MGRANK+1).GT.0) THEN
        CALL VF_ZSETI1(IPF,0,MGPINF(1)+1)
        CALL VF_ZSETI1(JPF,0,MGPINF(2)+1)
        CALL VF_ZSETI1(KPF,0,MGPINF(3)+1)
      END IF
C----------------------------------------------------for MG/2FC coupling
C     * 読み込み(3)
      CALL VF_II1INP(XX,YY,ZZ,CM0,CD0,GGV,GGX,GGY,GGZ,
C----------------------------------------------------for MG/2FC coupling
     &               XPF,YPF,ZPF,IPF,JPF,KPF,
C----------------------------------------------------for MG/2FC coupling
     &               BCU,BCV,BCW,BCP,BCF,BCVI,BCK,BCE,BCT,BCTI,BCC,BCCI,
     &               NF,INDX,INDY,INDZ,INDB,INDBK,INDBE,INDBT,INDBC,
     &               DBUF,IBUF)
C----------------------------------------------------------2016.09 start
      IF(ISEABT.NE.0) THEN
        ALLOCATE(DELH   (NUMI0,NUMJ0),STAT=IERR)
        ALLOCATE(DELH_IN(NUMI0,NUMJ0),STAT=IERR)
        CALL VF_ZSETR2(DELH   ,0.0D0,NUMI0,NUMJ0)
        CALL VF_ZSETR2(DELH_IN,0.0D0,NUMI0,NUMJ0)
      END IF
C----------------------------------------------------------2016.09 end

CD    -- 時間依存型空隙率ファイルの読み込みと配列のアロケート等 --
C     * 読み込み(1)
      IF (IHIDEM.EQ.0) THEN
        CALL VF_IP1INP(0.0D0,GGVOLD,GGVNOW,GGVLO,GGVLN)
      ELSE
        CALL VF_JPHDM1(XX,YY,ZZ)
      ENDIF
      ALLOCATE(PPPVC(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(IPVC (NUMI,NUMJ,NUMK),STAT=IERR)
      CALL VF_ZSETR3(PPPVC,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETI3(IPVC ,    0,NUMI,NUMJ,NUMK)

C     * 配列のアロケートとデフォルト値の設定(1)
      IF (IPRNP.GE.1) THEN
        ALLOCATE(GGVOLD(IPRNP),STAT=IERR)
        ALLOCATE(GGVNOW(IPRNP),STAT=IERR)
        IF (IHIDEM.EQ.0) THEN
          ALLOCATE(GGVLO(3,IPRNB),STAT=IERR)
          ALLOCATE(GGVLN(3,IPRNB),STAT=IERR)
        ELSE
          ALLOCATE(GGVELO(3,IPRNP),STAT=IERR)
          ALLOCATE(GGVELN(3,IPRNP),STAT=IERR)
          ALLOCATE(RBUF(IPRNP),STAT=IERR)
        ENDIF
        ALLOCATE(GGVEL (3,IPRNP),STAT=IERR)
        IF (IERR.NE.0) CALL VF_A2ERR('VF_A1MAIN','CAN NOT ALLOC.')
        CALL VF_ZSETR1(GGVOLD,1.0D0,IPRNP)
        CALL VF_ZSETR1(GGVNOW,1.0D0,IPRNP)
        IF (IHIDEM.EQ.0) THEN
          CALL VF_ZSETR2(GGVLO,0.0D0,3,IPRNB)
          CALL VF_ZSETR2(GGVLN,0.0D0,3,IPRNB)
        ELSE
          CALL VF_ZSETR2(GGVELO,0.0D0,3,IPRNP)
          CALL VF_ZSETR2(GGVELN,0.0D0,3,IPRNP)
          CALL VF_ZSETR1(RBUF,0.0D0,IPRNP)
        ENDIF
        CALL VF_ZSETR2(GGVEL ,0.0D0,3,IPRNP)
      ENDIF
      IF (IPRNT.GT.1) THEN
        ALLOCATE(GGV0(NUMI,NUMJ,NUMK),STAT=IERR)
        ALLOCATE(GLV0(NUMI,NUMJ,NUMK),STAT=IERR)
        ALLOCATE(GVL0(NUMI,NUMJ,NUMK),STAT=IERR)
        IF (IERR.NE.0) CALL VF_A2ERR('VF_A1MAIN','CAN NOT ALLOC.')
        CALL VF_ZSETR3(GGV0,1.0D0,NUMI,NUMJ,NUMK)
        CALL VF_ZSETR3(GLV0,1.0D0,NUMI,NUMJ,NUMK)
        CALL VF_ZSETR3(GVL0,1.0D0,NUMI,NUMJ,NUMK)
      ENDIF
C     * 読み込み（1-1）ファイルから1回だけ読み込む場合のみ
      IF (IPRNT.EQ.1) THEN
        CALL VF_IP1INP(0.0D0,GGVOLD,GGVNOW,GGVLO,GGVLN)
        CALL VF_CGGV  (0,0.0D0,0.0D0,0.0D0,GGV,GGV0,GGVOLD,GGVNOW,
     &                 GGVEL,GGVLO,GGVLN,NF)
        CALL VF_CGGXYZ(0,XX,YY,ZZ,GGV,GGX,GGY,GGZ,GGV0,DBUF,
     &                 NF,INDX,INDY,INDZ)
      ENDIF
      CALL VF_CGLV  (CM0,GGV,GLV,NF)
      CALL VF_CGLXYZ(XX,YY,ZZ,CM0,GGX,GGY,GGZ,GLX,GLY,GLZ,DBUF,
     &               NF,INDX,INDY,INDZ)

CD    -- 構造物との連成解析用配列のアロケート --
      IF(ICPL.GT.0 .OR. ISTM.EQ.1) THEN
        ALLOCATE(GGV0(NUMI,NUMJ,NUMK),STAT=IERR)
        ALLOCATE(GLV0(NUMI,NUMJ,NUMK),STAT=IERR)
        ALLOCATE(DGGV(NUMI,NUMJ,NUMK),STAT=IERR)
        ALLOCATE(DGLV(NUMI,NUMJ,NUMK),STAT=IERR)
        IF (IERR.NE.0) CALL VF_A2ERR('VF_A1MAIN','CAN NOT ALLOC.')
        CALL VF_ZSETR3(GGV0,1.0D0,NUMI,NUMJ,NUMK)
        CALL VF_ZSETR3(GLV0,1.0D0,NUMI,NUMJ,NUMK)
        CALL VF_ZSETR3(DGGV,0.0D0,NUMI,NUMJ,NUMK)
        CALL VF_ZSETR3(DGLV,0.0D0,NUMI,NUMJ,NUMK)
      ENDIF

CD    -- マトリクスデータファイルの読み込みと配列のアロケート等 --
      IF (MTBTYP.GE.1) THEN
C       * 読み込み(1)
        CALL VF_IM1INP(DMTBTT,DMTBZZ,DMTBHH,DMTBUN,DMTBUT)
C       * 配列のアロケートとデフォルト値の設定
        ALLOCATE(DMTBTT(MTBTT)      ,STAT=IERR)
        IF (MTBTYP.NE.2) THEN
          ALLOCATE(DMTBHH(MTBTT)      ,STAT=IERR)
        ENDIF
        IF (MTBTYP.NE.3) THEN
          ALLOCATE(DMTBZZ(MTBZZ)      ,STAT=IERR)
          ALLOCATE(DMTBUN(MTBZZ,MTBTT),STAT=IERR)
          ALLOCATE(DMTBUT(MTBZZ,MTBTT),STAT=IERR)
        ENDIF
        IF (IERR.NE.0) CALL VF_A2ERR('VF_A1MAIN','CAN NOT ALLOC.')
        CALL VF_ZSETR1(DMTBTT,0.0D0,MTBTT)
        IF (MTBTYP.NE.2) THEN
          CALL VF_ZSETR1(DMTBHH,0.0D0,MTBTT)
        ENDIF
        IF (MTBTYP.NE.3) THEN
          CALL VF_ZSETR1(DMTBZZ,0.0D0,MTBZZ)
          CALL VF_ZSETR2(DMTBUN,0.0D0,MTBZZ,MTBTT)
          CALL VF_ZSETR2(DMTBUT,0.0D0,MTBZZ,MTBTT)
        ENDIF
C       * 読み込み(2)
        CALL VF_IM1INP(DMTBTT,DMTBZZ,DMTBHH,DMTBUN,DMTBUT)
      ENDIF

CD    -- 解析条件から各種情報を構築する --
      IF (MYRANK.EQ.0) WRITE(*,9520) 'SETUP.'
      WRITE(ILPFIL,9520) 'SETUP.'
      CALL VF_CSETUP(XX,YY,ZZ,BCU,BCV,BCW,BCP,BCF,BCVI,BCK,BCE,
     &               NF,INDX,INDY,INDZ,INDB,INDBK,INDBE)

CD    -- 構造物による解析条件の変更 --
      IF(ICPL.GT.0) THEN
        CALL SF_STR0()
      ELSEIF(ISTM.EQ.1) THEN
        CALL SF_STM0()
      ENDIF

CD    -- リストファイルに解析条件を出力 --
      IF (MYRANK.EQ.0) WRITE(*,9520) 'CONDITION.'
      WRITE(ILPFIL,9520) 'CONDITION.'
      CALL VF_OL1INI(XX,YY,ZZ,
     &               CM0,CD0,GGV,GGX,GGY,GGZ,GLV,GLX,GLY,GLZ,
     &               BCU,BCV,BCW,BCP,BCF,BCK,BCE,BCT,BCTI,BCC,BCCI,
     &               NF,INDX,INDY,INDZ,INDB,INDBK,INDBE,INDBT,INDBC)

CD    -- 初期条件の設定 --
      IF (IRETYP.LT.0) THEN
        IF (MYRANK.EQ.0) WRITE(*,9520) 'INITIAL.'
        WRITE(ILPFIL,9520) 'INITIAL.'
        NNOW     =0
        DTNOW    =0.0D0
        TNOW     =0.0D0
        DT       =0.0D0
        DTOLD    =0.0D0
        RLPTRN(4)=RLPTRN(1)
        RGRTRN(4)=RGRTRN(1)
        RRSTRN(4)=RRSTRN(1)
        RTRTRN(4)=RTRTRN(1)
        IF (IPRNT.GT.1.AND.IHIDEM.EQ.0) THEN
C         * 読み込み(2)
          CALL VF_IP1INP(TNOW,GGVOLD,GGVNOW,GGVLO,GGVLN)
          CALL VF_CGGV  (0,0.0D0,0.0D0,0.0D0,GGV,GGV0,GGVOLD,GGVNOW,
     &                   GGVEL,GGVLO,GGVLN,NF)
          CALL VF_CGGXYZ(0,XX,YY,ZZ,GGV,GGX,GGY,GGZ,GGV0,DBUF,
     &                   NF,INDX,INDY,INDZ)
          CALL VF_CGLV  (CM0,GGV,GLV,NF)
          CALL VF_CGLXYZ(XX,YY,ZZ,CM0,GGX,GGY,GGZ,GLX,GLY,GLZ,DBUF,
     &                   NF,INDX,INDY,INDZ)
        ENDIF
        CALL VF_CINIT(XX ,YY ,ZZ, 
     &                UU ,VV ,WW ,PP ,FF ,FX ,FY ,FZ ,
     &                ANU,GGV,GGX,GGY,GGZ,
     &                BCU,BCV,BCW,BCP,BCF,BCVI,TBUB,
     &                DROPTX,DROPTY,DROPTZ,DROPUU,DROPVV,DROPWW,
     &                ANUT,AK,AE,BCK,BCE,
     &                TT,ALM,BCT,BCTI,CC,DD,BCC,BCCI,
     &                DMTBTT,DMTBZZ,DMTBHH,DMTBUN,DMTBUT,DBUF,
     &                WK01,WK02,WK03,
C----------------------------------------------------------2016.09 start
     &                DELH,DELH_IN,
C----------------------------------------------------------2016.09 end
     &                NF,INDX,INDY,INDZ,INDC,INDB,INDS,
     &                INDBK,INDBE,INDBT,INDBC,IBUF,NWK1)

        IF (IPRNT.GT.1.AND.IHIDEM.NE.0) THEN
C         * 読み込み(2)
          CALL VF_JPHDM2(0,DT,XX,YY,ZZ,UU,VV,WW,PP,PPBK,WK18,
     &                   GGVOLD,GGVNOW,GGVELO,GGVELN,
     &                   NF,INDX,INDY,INDZ,INDB,
     &                   WK01,WK02,WK03,WK04,WK05,
     &                   WK06,WK07,WK08,WK09,WK10,
     &                   WK11,WK12,WK13,WK14,WK15,
     &                   WK16,WK17,DBUF,RBUF,NWK1)
          CALL VF_CGGVH(0,0.0D0,0.0D0,GGV,GGV0,GGVOLD,GGVNOW,
     &                  GGVEL,GGVELO,GGVELN,NF)
          CALL VF_CGGXYZ(0,XX,YY,ZZ,GGV,GGX,GGY,GGZ,GGV0,DBUF,
     &                   NF,INDX,INDY,INDZ)
          CALL VF_CGLV  (CM0,GGV,GLV,NF)
          CALL VF_CGLXYZ(XX,YY,ZZ,CM0,GGX,GGY,GGZ,GLX,GLY,GLZ,DBUF,
     &                   NF,INDX,INDY,INDZ)
        ENDIF

CD    -- リスタートファイルのオープンと読み込み --
      ELSEIF(ICPL.EQ.0 .AND. ISTM.EQ.0) THEN
        IF (MYRANK.EQ.0) WRITE(*,9520) 'RESTART.'
        WRITE(ILPFIL,9520) 'RESTART.'
        CALL VF_IR1INP(UU,VV,WW,PP,FF,ANU,GGV,BCU,BCV,BCW,BCP,BCF,
     &                 ANUT,AK,AE,BCK,BCE,TT,ALM,BCT,CC,DD,BCC,
     &                 TBUB,DROPTX,DROPTY,DROPTZ,
     &                 DROPUU,DROPVV,DROPWW,
     &                 NF,INDC,INDS)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        CALL VF_CFXYZ(XX,YY,ZZ,FF,FX,FY,FZ,GGV,
     &                DBUF,NF,INDX,INDY,INDZ)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C@      IF (IPRNT.GT.1) THEN
        IF (IPRNT.GT.1.AND.IHIDEM.EQ.0) THEN
          CALL VF_CGGXYZ(0,XX,YY,ZZ,GGV,GGX,GGY,GGZ,GGV0,DBUF,
     &                   NF,INDX,INDY,INDZ)
          CALL VF_CGLV  (CM0,GGV,GLV,NF)
          CALL VF_CGLXYZ(XX,YY,ZZ,CM0,GGX,GGY,GGZ,GLX,GLY,GLZ,DBUF,
     &                   NF,INDX,INDY,INDZ)
        ENDIF
        IF (IPRNT.GT.1.AND.IHIDEM.NE.0) THEN
          CALL VF_JPHDM2(0,DT,XX,YY,ZZ,UU,VV,WW,PP,PPBK,WK18,
     &                   GGVOLD,GGVNOW,GGVELO,GGVELN,
     &                   NF,INDX,INDY,INDZ,INDB,
     &                   WK01,WK02,WK03,WK04,WK05,
     &                   WK06,WK07,WK08,WK09,WK10,
     &                   WK11,WK12,WK13,WK14,WK15,
     &                   WK16,WK17,DBUF,RBUF,NWK1)
          CALL VF_CGGVH(0,0.0D0,0.0D0,GGV,GGV0,GGVOLD,GGVNOW,
     &                  GGVEL,GGVELO,GGVELN,NF)
          CALL VF_CGGXYZ(0,XX,YY,ZZ,GGV,GGX,GGY,GGZ,GGV0,DBUF,
     &                   NF,INDX,INDY,INDZ)
          CALL VF_CGLV  (CM0,GGV,GLV,NF)
          CALL VF_CGLXYZ(XX,YY,ZZ,CM0,GGX,GGY,GGZ,GLX,GLY,GLZ,DBUF,
     &                   NF,INDX,INDY,INDZ)
        ENDIF
      ENDIF

CD    -- 移動障害物機能の前処理 --
      IF (IPRNT.GE.1) THEN
C       -- 移動障害物用にインデックスデータを二重化 --
        ALLOCATE(INDX0(NUMI,NUMJ,NUMK),STAT=IERR)
        ALLOCATE(INDY0(NUMI,NUMJ,NUMK),STAT=IERR)
        ALLOCATE(INDZ0(NUMI,NUMJ,NUMK),STAT=IERR)
        ALLOCATE(INDB0(MAXB1,NUMB0),STAT=IERR)
        CALL VF_CGIND0(INDX,INDY,INDZ,INDB,INDX0,INDY0,INDZ0,INDB0)
        CALL VF_CGINDX(UU,VV,WW,GGVEL,NF,INDB0,INDC,INDS,
     &                 INDX,INDY,INDZ,INDX0,INDY0,INDZ0,
     &                 GGV,FF,BCF,IBUF,NWK1,IPVC)
        DEALLOCATE(INDB,STAT=IERR)
        ALLOCATE(INDB(MAXB1,NUMB),STAT=IERR)
        CALL VF_CGINDB(NF,INDX,INDY,INDZ,INDB0,INDB,NWK1)
C       IF (LEQK.NE.0) THEN
C         ALLOCATE(INDBK0(MAXBK1,NUMB0),STAT=IERR)
C         ALLOCATE(INDBE0(MAXBE1,NUMB0),STAT=IERR)
C         CALL VF_CGINKE(...
C         DEALLOCATE(INDBK,STAT=IERR)
C         DEALLOCATE(INDBE,STAT=IERR)
C         ALLOCATE(INDBK (MAXBK1,NUMB ),STAT=IERR)
C         ALLOCATE(INDBE (MAXBE1,NUMB ),STAT=IERR)
C       ENDIF
C       IF (LEQT.NE.0) THEN
C         ALLOCATE(INDBT0(NUMB0),STAT=IERR)
C         DEALLOCATE(INDBT,STAT=IERR)
C         CALL VF_CGINT(...
C         ALLOCATE(INDBT (NUMB ),STAT=IERR)
C       ENDIF
C       IF (LEQC.GT.0) THEN
C         ALLOCATE(INDBC0(NUMB0,LEQC),STAT=IERR)
C         CALL VF_CGINC(...
C         DEALLOCATE(INDBC,STAT=IERR)
C         ALLOCATE(INDBC (NUMB ,LEQC),STAT=IERR)
C       ENDIF
C       -- 配列サイズ変更 --        
        IF (NUMB.GT.NUMB0) THEN
C         -- 作業エリア --
          DEALLOCATE(WKBC ,STAT=IERR)
          DEALLOCATE(NWKBC,STAT=IERR)
          IF (2*LEQC.GT.3) THEN
            ALLOCATE(WKBC(2*NUMB*LBQC),STAT=IERR)
          ELSE
            ALLOCATE(WKBC(NUMB*3),STAT=IERR)
          ENDIF
          ALLOCATE(NWKBC(NUMB  ),STAT=IERR)
C         -- BCU --
          CALL VF_CGCPR2(WKBC,BCU,NUMB,NUMB0)
          DEALLOCATE(BCU,STAT=IERR)
          ALLOCATE(BCU(NUMB,3),STAT=IERR)
          CALL VF_CGCPR2(BCU,WKBC,NUMB,NUMB )
C         -- BCV --
          CALL VF_CGCPR2(WKBC,BCV,NUMB,NUMB0)
          DEALLOCATE(BCV,STAT=IERR)
          ALLOCATE(BCV(NUMB,3),STAT=IERR)
          CALL VF_CGCPR2(BCV,WKBC,NUMB,NUMB )
C         -- BCW --
          CALL VF_CGCPR2(WKBC,BCW,NUMB,NUMB0)
          DEALLOCATE(BCW,STAT=IERR)
          ALLOCATE(BCW(NUMB,3),STAT=IERR)
          CALL VF_CGCPR2(BCW,WKBC,NUMB,NUMB )
C         -- BCP --
          CALL VF_CGCPR2(WKBC,BCP,NUMB,NUMB0)
          DEALLOCATE(BCP,STAT=IERR)
          ALLOCATE(BCP(NUMB,3),STAT=IERR)
          CALL VF_CGCPR2(BCP,WKBC,NUMB,NUMB )
C         -- BCF --
          CALL VF_CGCPR1(WKBC,BCF,NUMB,NUMB0)
          DEALLOCATE(BCF,STAT=IERR)
          ALLOCATE(BCF(NUMB),STAT=IERR)
          CALL VF_CGCPR1(BCF,WKBC,NUMB,NUMB )
C         -- BCVI --
          CALL VF_CGCPR1(WKBC,BCVI,NUMB,NUMB0)
          DEALLOCATE(BCVI,STAT=IERR)
          ALLOCATE(BCVI(NUMB),STAT=IERR)
          CALL VF_CGCPR1(BCVI,WKBC,NUMB,NUMB )
C         -- BCK & BCE --
          IF (LEQK.NE.0) THEN
            CALL VF_CGCPR2(WKBC,BCK,NUMB,NUMB0)
            DEALLOCATE(BCK,STAT=IERR)
            ALLOCATE(BCK(NUMB,3 ),STAT=IERR)
            CALL VF_CGCPR2(BCK,WKBC,NUMB,NUMB )
            CALL VF_CGCPR2(WKBC,BCE,NUMB,NUMB0)
            DEALLOCATE(BCE,STAT=IERR)
            ALLOCATE(BCE(NUMB,3 ),STAT=IERR)
            CALL VF_CGCPR2(BCE,WKBC,NUMB,NUMB )
          ENDIF
C         -- BCT & BCTI --
C         IF (LEQT.NE.0) THEN
C           CALL VF_CGCPR1(WKBC,BCT,NUMB,NUMB0)
C           DEALLOCATE(BCT,STAT=IERR)
C           ALLOCATE(BCT(NUMB),STAT=IERR)
C           CALL VF_CGCPR1(BCT,WKBC,NUMB,NUMB )
C           CALL VF_CGCPRT(WKBC,BCTI,NUMB,NUMB0)
C           DEALLOCATE(BCTI,STAT=IERR)
C           ALLOCATE(BCTI(2,NUMB),STAT=IERR)
C           CALL VF_CGCPRT(BCTI,WKBC,NUMB,NUMB )
C         ENDIF
C         -- BCC & BCCI --
          IF (LEQC.GT.0) THEN
C           CALL VF_CGCPCC(WKBC,BCC,NUMB,NUMB0)
C           DEALLOCATE(BCC,STAT=IERR)
C           ALLOCATE(BCC(NUMB,LEQC),STAT=IERR)
C           CALL VF_CGCPCC(BCC,WKBC,NUMB,NUMB )
C           CALL VF_CGCPCI(WKBC,BCCI,NUMB,NUMB0)
C           DEALLOCATE(BCCI,STAT=IERR)
C           ALLOCATE(BCCI(2,NUMB,LEQC),STAT=IERR)
C           CALL VF_CGCPCI(BCCI,WKBC,NUMB,NUMB )
            DEALLOCATE(WKBC,STAT=IERR)
            ALLOCATE(WKBC(NUMB*3),STAT=IERR)
          ENDIF
          CALL VF_ZSETR1(WKBC ,0.0D0,NUMB*3)
          CALL VF_ZSETI1(NWKBC,    0,NUMB)
        ENDIF
      ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DTNOW = 1.0D0
      RHOGO(:,:,:) = 0.0D0
      CALL VF_V1EOS(RHOG,DRHODP,DRHODT,DBUF
     &             ,UU,VV,WW,PP,RHOGO
     &             ,GGV,GGX,GGY,GGZ,GLV,XX,YY,ZZ,NF,INDC)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

CD    -- 図化ファイルに格子数等を出力 --
      CALL VF_OG1INI(XX,YY,ZZ,GGV,INDB,NWKBC)

CD    -- 結果ファイルのオープンと格子数等の出力 --
      CALL VF_OR1INI(GGV,ETIME)

CD    -- マルチエージェントファイルのオープンと格子数等の出力 --
      CALL VF_OM1INI(XX,YY,ZZ,GGV,WK01,NF)

CD    -- 時系列ファイルのオープンと出力対象の出力 --
      CALL VF_OT1INI()

CD    -- 構造物の変形による解析条件の変更，連成解析に関わるデータ出力 --
      IF(ICPL.GT.0) THEN
        CALL SF_STR1(DT)
      ELSEIF(ISTM.EQ.1) THEN
        CALL SF_STM1(DT)
      ENDIF

CD    -- タイマーの切替 --
      CALL VF_A2CPUT(0,ICPUEN,KCP1PR)
      CALL VF_A2CPUT(0,ICPUST,KCP1CL)

C@@@@@@@@@
      DO 813 K=1,NUMK
      DO 812 J=1,NUMJ
      DO 811 I=1,NUMI
        PPBK(I,J,K)=PP(I,J,K)
 811  CONTINUE
 812  CONTINUE
 813  CONTINUE
C@@@@@@@@@

CD    -- SMAC法およびVOF法の計算ループ --
      IF (MYRANK.EQ.0) WRITE(*,9520) 'CALCULATION.'
      WRITE(ILPFIL,9520) 'CALCULATION.'
CD    ** 中判定反復 **
 500  CONTINUE

C----------------------------------------------------for MG/2FC coupling
CD      -- MGとのデータ転送 --
        CALL VF_PMGP2C(XX,YY,ZZ,UU,VV,WW,FF,
     &                 GGV,GGX,GGY,XPF,YPF,ZPF,IPF,JPF,KPF,
     &                 BCU,BCV,BCW,BCF,DBUF,NF,INDX,INDY,INDB)
        CALL VF_PMGC2P(XX,YY,ZZ,UU,VV,WW,FF,
     &                 GGV,GGX,GGY,XPF,YPF,ZPF,IPF,JPF,KPF,
     &                 BCU,BCV,BCW,BCF,DBUF,NF,INDX,INDY,INDB)
C----------------------------------------------------for MG/2FC coupling

CD      -- 構造物との連成解析時，MGからの固定流速条件をコピー --
        IF(ICPL.EQ.2 .OR. ISTM.EQ.1)
     &    CALL SF_PMGBC0(BCU0,BCV0,BCW0,BCP0,BCF0,INDX0,INDY0
     &                  ,BCU,BCV,BCW,BCP,BCF,INDX,INDY)

CD      -- 次のステップの時間刻み幅を計算 --
        IF (DT.GT.0.0D0 ) DTOLD=DT
        IF (IDTTYP.EQ.0) THEN
          DT=DTCNST
        ELSE
          CALL VF_CDTCAL(DT,XX,YY,ZZ,UU,VV,WW,FF,ANU,
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     &                   RHOG,
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     &                   GGV,GGX,GGY,GGZ,BCF,ALM,DD,WK01,
     &                   NF,INDX,INDY,INDZ)
        ENDIF
        IF (DTOLD.EQ.0.0D0 ) DTOLD=DT

CD      -- 終了条件の判定 --
        IEND(1) = 0
        IF (NNOW.GE.NEND .OR. (TNOW+0.5D0*DT).GE.TEND) IEND(1) = 1
C----------------------------------------------------for MG/2FC coupling
CCC        CALL VF_P0SUMI(IE,IES)
CCC        IE = IES
C----------------------------------------------------for MG/2FC coupling

CD      -- 解析結果をリストファイルに出力 --
        CALL VF_A2CPUT(0,ICPUST,KCP2FL)
CD      * ステップ情報の出力
        CALL VF_CDIV00(DV,XX,YY,ZZ,UU,VV,WW,GGX,GGY,GGZ,INDC)
        IF (MYRANK.EQ.0) WRITE(*,9530) NNOW,TNOW,DTNOW
        WRITE(ILPFIL,9540) NNOW,TNOW,DTNOW,FSUM,FCUT,DV
        WRITE(ILPFIL,9550) CGBNRM,CGXNRM,ICGITR
        CALL VF_OL1TRN(DT ,
     &                 UU ,VV ,WW ,PP ,FF ,ANU,
     &                 GGV,GGX,GGY,GGZ,GLV,GLX,GLY,GLZ,
     &                 BCU,BCV,BCW,BCP,BCF,
     &                 ANUT,AK,AE,BCK,BCE,
     &                 TT,ALM,BCT,BCTI,CC,DD,BCC,BCCI,
     &                 NF ,INDB,INDBK,INDBE,INDBT,INDBC)

CD      -- 解析結果を図化ファイルに出力 --
        CALL VF_OG1TRN(IOG,DT ,XX ,YY ,ZZ ,
     &                 UU ,VV ,WW ,PP ,FF ,GGV ,
     &                 BCU,BCV,BCW,BCP,BCF,
     &                 AK,AE,BCK,BCE,TT,BCT,CC,BCC,
     &                 WK01,WK02,WK03,WKBC,
     &                 NF,INDX,INDY,INDZ,INDB,NWK1)

CD      -- 解析結果をマルチエージェントファイルに出力 --
        CALL VF_OM1TRN(DT,ZZ,UU,VV,FF,GGV,WK01,NF,0)

CD      -- 解析結果を詳細ファイルに出力 --
        CALL VF_OR1TRN(DT,UU,VV,WW,PP,FF,GGV,BCU,BCV,BCW,BCP,BCF,
     &                 AK,AE,BCK,BCE,TT,BCT,CC,BCC,
     &                 TBUB,DROPTX,DROPTY,DROPTZ,
     &                 DROPUU,DROPVV,DROPWW,
     &                 NF)

CD      -- 解析結果を時系列ファイルに出力 --
        CALL VF_OT1TRN(DT,XX,YY,ZZ,UU,VV,WW,PP,FF,GGV,AK,AE,TT,CC,
     &                 BCU,BCV,BCW,NF,INDX,INDY,INDZ)

CD      -- 構造物との連成解析に関わるデータ出力 --
        IF(ICPL.GT.0) THEN
          CALL SF_STR3(DT)
        ELSEIF(ISTM.EQ.1) THEN
          CALL SF_STM3(DT)
        ENDIF

        CALL VF_A2CPUT(0,ICPUEN,KCP2FL)

CD      ** 終了条件を満たすまで **
CC        IF (IE.NE.0) GOTO 1000
CD      -- 終了条件の判定 --
        IEND1(1)=IEND(1)
C
CD      -- 経過時間の計測(毎ステップ) --
        CALL VF_P0TIME(WTM2)
        IEND1(2)=0
        IF(WTM2-WTM1.GT.ETIME) IEND1(2)=1
        CALL MPI_ALLREDUCE(IEND1,IEND,2,MPI_INTEGER,MPI_MAX,
     $                     comm_mlicdsmg2fc,IERR)
        IE = IEND(1)
        IEE= IEND(2)

        IF (IHIDEM.EQ.1.AND.MYRANK.EQ.0) THEN
          IWORK(1) = IOG
          IWORK(2) = MAX(IE,IEE)
          CALL MPI_SEND(IWORK,2,MPI_INTEGER,IHIDM,0,comm_2fc_dem,IERR)
        ENDIF

        IF (IE.EQ.1 .OR. IEE.EQ.1) GOTO 1000

CD      -- 解析時刻の更新(1) --
        DT0 = DT
        CALL MPI_ALLREDUCE( DT0,DT,1,MPI_DOUBLE_PRECISION,MPI_MIN,
     $                      comm_mlicdsmg2fc,IERR )

CD      -- 構造物との連成解析における解析終了時刻の調整 --
        IF(ICPL.EQ.2.AND.TNOW+DT.GT.TEND-1.D-8) DT=TEND-1.D-8-TNOW

        IF(ICPL.GT.0 .OR. ISTM.EQ.1) THEN
          GGV0=GGV
          GLV0=GLV
        ENDIF

CD    -- 構造物の変形による解析条件の変更 --
        IF(ICPL.EQ.2) THEN
          CALL SF_STR2(DT,DTOLD)
        ELSEIF(ISTM.EQ.1) THEN
          CALL SF_STM2(DT,DTOLD)
        ENDIF

        DTNOW=DT/DBLE(LOOPS)
        NNOW =NNOW+1
        IF (IPRNT.GT.1) THEN
          T0=TNOW+DT
          IF (IHIDEM.EQ.0) THEN
            CALL VF_IP1INP(T0,GGVOLD,GGVNOW,GGVLO,GGVLN)
          ELSE
            CALL VF_JPHDM2(1,DT,XX,YY,ZZ,UU,VV,WW,PP,PPBK,WK18,
     &                     GGVOLD,GGVNOW,GGVELO,GGVELN,
     &                     NF,INDX,INDY,INDZ,INDB,
     &                     WK01,WK02,WK03,WK04,WK05,
     &                     WK06,WK07,WK08,WK09,WK10,
     &                     WK11,WK12,WK13,WK14,WK15,
     &                     WK16,WK17,DBUF,RBUF,NWK1)
          ENDIF
        ENDIF
C@@@@@@@@@
      DO 823 K=1,NUMK
      DO 822 J=1,NUMJ
      DO 821 I=1,NUMI
        PPBK(I,J,K)=PP(I,J,K)
 821  CONTINUE
 822  CONTINUE
 823  CONTINUE
C@@@@@@@@@

CBUG!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CBUG        RHOGO(:,:,:) = RHOG(:,:,:)
CBUG        UUO(:,:,:) = UU(:,:,:)
CBUG        VVO(:,:,:) = VV(:,:,:)
CBUG        WWO(:,:,:) = WW(:,:,:)
CBUG        PPO(:,:,:) = PP(:,:,:)
        if(nnow.ge.100) ICOMP = 1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CD      -- 流速・圧力計算のサブループ --
        CALL VF_A2CPUT(0,ICPUST,KCP2VL)
        DO 600 ILOOP=1,LOOPS
          RHOGO(:,:,:) = RHOG(:,:,:)
          UUO(:,:,:) = UU(:,:,:)
          VVO(:,:,:) = VV(:,:,:)
          WWO(:,:,:) = WW(:,:,:)
          PPO(:,:,:) = PP(:,:,:)

CD        -- 解析時刻の更新(2) --
          TNOW =TNOW+DTNOW
          IF (LOOPS.NE.1) THEN
            IF (MYRANK.EQ.0) WRITE(*,9560) ILOOP,TNOW
            WRITE(ILPFIL,9560) ILOOP,TNOW
          ENDIF

CD        -- 移動障害物データの更新 --
          IF (IPRNT.GT.1) THEN
            IF (IHIDEM.EQ.0) THEN
              CALL VF_CGGV(1,T0,TNOW,DTNOW,GGV,GGV0,GGVOLD,GGVNOW,
     &                     GGVEL,GGVLO,GGVLN,NF)
            ELSE
              W=DTNOW*DBLE(ILOOP)/DT
              CALL VF_CGGVH(1,W,DTOLD,
     &                      GGV,GGV0,GGVOLD,GGVNOW,
     &                      GGVEL,GGVELO,GGVELN,NF)
            ENDIF
            CALL VF_CGLV(CM0,GGV0,GLV0,NF)

C          -- 移動障害物用インデックスデータを更新 --
            NUMB1=NUMB
            CALL VF_CGINDX(UU,VV,WW,GGVEL,NF,INDB0,INDC,INDS,
     &                     INDX,INDY,INDZ,INDX0,INDY0,INDZ0,
     &                     GGV0,FF,BCF,IBUF,NWK1,IPVC)
            IF (NUMB.NE.NUMB1) THEN
              DEALLOCATE(INDB,STAT=IERR1)
              ALLOCATE(INDB(MAXB1,NUMB),STAT=IERR)
            ENDIF
            CALL VF_CGINDB(NF,INDX,INDY,INDZ,INDB0,INDB,NWK1)

C           IF (LEQK.NE.0) THEN
C             IF (NUMB.NE.NUMB1) THEN
C               DEALLOCATE(INDBK,STAT=IERR)
C               DEALLOCATE(INDBE,STAT=IERR)
C               ALLOCATE(INDBK(MAXBK1,NUMB),STAT=IERR)
C               ALLOCATE(INDBE(MAXBE1,NUMB),STAT=IERR)
C             ENDIF
C             CALL VF_CGSTKE(...
C           ENDIF
C           IF (LEQT.NE.0) THEN
C             IF (NUMB.NE.NUMB1) THEN
C               DEALLOCATE(INDBT,STAT=IERR)
C               ALLOCATE(INDBT(NUMB ),STAT=IERR)
C             ENDIF
C             CALL VF_CGSTT(...
C           ENDIF
C           IF (LEQC.GT.0) THEN
C             IF (NUMB.NE.NUMB1) THEN
C               DEALLOCATE(INDBC,STAT=IERR)
C               ALLOCATE(INDBC(NUMB,LEQC),STAT=IERR)
C             ENDIF
C             CALL VF_CGSTC(...
C           ENDIF
C           -- 配列サイズ変更 --        
            IF (NUMB.NE.NUMB1) THEN
C             -- 作業エリア --
              DEALLOCATE(WKBC ,STAT=IERR)
              DEALLOCATE(NWKBC,STAT=IERR)
              IF (2*LEQC.GT.3) THEN
                ALLOCATE(WKBC(2*NUMB*LEQC),STAT=IERR)
              ELSE
                ALLOCATE(WKBC(NUMB*3),STAT=IERR)
              ENDIF
              ALLOCATE(NWKBC(NUMB),STAT=IERR)
C             -- BCU --
              CALL VF_CGCPR2(WKBC,BCU,NUMB,NUMB1)
              DEALLOCATE(BCU,STAT=IERR)
              ALLOCATE(BCU(NUMB,3),STAT=IERR)
              CALL VF_CGCPR2(BCU,WKBC,NUMB,NUMB )
C             -- BCV --
              CALL VF_CGCPR2(WKBC,BCV,NUMB,NUMB1)
              DEALLOCATE(BCV,STAT=IERR)
              ALLOCATE(BCV(NUMB,3),STAT=IERR)
              CALL VF_CGCPR2(BCV,WKBC,NUMB,NUMB )
C             -- BCW --
              CALL VF_CGCPR2(WKBC,BCW,NUMB,NUMB1)
              DEALLOCATE(BCW,STAT=IERR)
              ALLOCATE(BCW(NUMB,3),STAT=IERR)
              CALL VF_CGCPR2(BCW,WKBC,NUMB,NUMB )
C             -- BCP --
              CALL VF_CGCPR2(WKBC,BCP,NUMB,NUMB1)
              DEALLOCATE(BCP,STAT=IERR)
              ALLOCATE(BCP(NUMB,3),STAT=IERR)
              CALL VF_CGCPR2(BCP,WKBC,NUMB,NUMB )
C             -- BCF --
              CALL VF_CGCPR1(WKBC,BCF,NUMB,NUMB1)
              DEALLOCATE(BCF,STAT=IERR)
              ALLOCATE(BCF(NUMB),STAT=IERR)
              CALL VF_CGCPR1(BCF,WKBC,NUMB,NUMB )
C             -- BCVI --
              CALL VF_CGCPR1(WKBC,BCVI,NUMB,NUMB1)
              DEALLOCATE(BCVI,STAT=IERR)
              ALLOCATE(BCVI(NUMB),STAT=IERR)
              CALL VF_CGCPR1(BCVI,WKBC,NUMB,NUMB )
C             -- BCK & BCE --
              IF (LEQK.NE.0) THEN
                CALL VF_CGCPR2(WKBC,BCK,NUMB,NUMB1)
                DEALLOCATE(BCK,STAT=IERR)
                ALLOCATE(BCK(NUMB,3 ),STAT=IERR)
                CALL VF_CGCPR2(BCK,WKBC,NUMB,NUMB )
                CALL VF_CGCPR2(WKBC,BCE,NUMB,NUMB1)
                DEALLOCATE(BCE,STAT=IERR)
                ALLOCATE(BCE(NUMB,3 ),STAT=IERR)
                CALL VF_CGCPR2(BCE,WKBC,NUMB,NUMB )
              ENDIF
C             -- BCT & BCTI --
C             IF (LEQT.NE.0) THEN
C               CALL VF_CGCPR1(WKBC,BCT,NUMB,NUMB1)
C               DEALLOCATE(BCT,STAT=IERR)
C               ALLOCATE(BCT(NUMB),STAT=IERR)
C               CALL VF_CGCPR1(BCT,WKBC,NUMB,NUMB )
C               CALL VF_CGCPRT(WKBC,BCTI,NUMB,NUMB1)
C               DEALLOCATE(BCTI,STAT=IERR)
C               ALLOCATE(BCTI(2,NUMB),STAT=IERR)
C               CALL VF_CGCPRT(BCTI,WKBC,NUMB,NUMB )
C             ENDIF
C             -- BCC & BCCI --
              IF (LEQC.GT.0) THEN
C               CALL VF_CGCPCC(WKBC,BCC,NUMB,NUMB1)
C               DEALLOCATE(BCC,STAT=IERR)
C               ALLOCATE(BCC(NUMB,LEQC),STAT=IERR)
C               CALL VF_CGCPCC(BCC,WKBC,NUMB,NUMB )
C               CALL VF_CGCPCI(WKBC,BCCI,NUMB,NUMB1)
C               DEALLOCATE(BCCI,STAT=IERR)
C               ALLOCATE(BCCI(2,NUMB,LEQC),STAT=IERR)
C               CALL VF_CGCPCI(BCCI,WKBC,NUMB,NUMB )
                DEALLOCATE(WKBC,STAT=IERR)
                ALLOCATE(WKBC(NUMB*3),STAT=IERR)
              ENDIF
              CALL VF_ZSETR1(WKBC ,0.0D0,NUMB*3)
              CALL VF_ZSETI1(NWKBC,    0,NUMB)
C             -- 境界条件の再設定 --
C             CALL VF_BWUWT(XX,YY,ZZ,UU,VV,WW,ANU,
C    &                      BCU,BCV,BCW,BCF,BCVI,INDB)
C             CALL VF_BWUWN(XX,YY,ZZ,UU,VV,WW,FF,BCU,BCV,BCW,BCF,
C    &                      DMTBTT,DMTBZZ,DMTBHH,DMTBUN,DMTBUT,DBUF,
C    &                      WK13,WK14,WK15,NF,INDX,INDY,INDB)
              CALL VF_BWPP(PP,BCP,INDB)
              CALL VF_BWFF(FF,BCF,INDB)
            ENDIF
          ENDIF

          IF(ICPL.GT.0 .OR. ISTM.EQ.1) THEN
            IF (ILOOP.EQ.1) THEN
              DGGV=(GGV-GGV0)/DBLE(LOOPS)
              DGLV=(GLV-GLV0)/DBLE(LOOPS)
              GGV=GGV0+DGGV
              GLV=GLV0+DGLV
            ELSE
              GGV0=GGV
              GLV0=GLV
              GGV=GGV0+DGGV
              GLV=GLV0+DGLV
            ENDIF
          ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          DO LL=1,MAXITR

CD        -- 気相密度の計算 --
          WK01(:,:,:) = RHOG(:,:,:)
          CALL VF_V1EOS(RHOG,DRHODP,DRHODT,DBUF
     &                 ,UU,VV,WW,PP,RHOGO
     &                 ,GGV,GGX,GGY,GGZ,GLV,XX,YY,ZZ,NF,INDC)

          SERR = 0.0D0
          DO K=2,NUMK-1
            DO J=MYJS,MYJE
              DO I=MYIS,MYIE
                IF(NF(I,J,K).EQ.8) THEN
                SERR = MAX(SERR
     &                    ,ABS(WK01(I,J,K) - RHOG(I,J,K))/RHOG(I,J,K))
                END IF
                RHOG(I,J,K) =        SRELAX *RHOG(I,J,K)
     &                      + (1.0D0-SRELAX)*WK01(I,J,K)
              END DO
            END DO
          END DO
          W = SERR
          CALL VF_P1MAXD(W,SERR)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

CD        -- 流速・圧力の計算 --
          CALL VF_V1CAL(ILOOP,XX,YY,ZZ,UU,VV,WW,PP,FF,FX,FY,FZ,ANU,
     &                  CM0,CD0,GGV,GGX,GGY,GGZ,GLV,GLX,GLY,GLZ,GGVEL,
     &                  BCU,BCV,BCW,BCP,BCF,BCVI,GGV0,GLV0,AK,TT,CC,
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     &                  RHOG,RHOGO,UUO,VVO,WWO,PPO,DRHODP,DRHODT,
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     &                  DMTBTT,DMTBZZ,DMTBHH,DMTBUN,DMTBUT,DBUF,
     &                  WK01,WK02,WK03,WK04,WK05,
     &                  WK06,WK07,WK08,WK09,WK10,
     &                  WK11,WK12,WK13,WK14,WK15,WK16,WK17,
     &                  NF,INDX,INDY,INDZ,INDC,INDB,INDS)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          IF(SERR.LE.SERROR) EXIT
          END DO
          IF (MYRANK.EQ.0) 
     &      WRITE(6,'(A,I5,A,1PE15.5)') 'ITER=',LL,' SERR=',SERR
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 600    CONTINUE
        CALL VF_A2CPUT(0,ICPUEN,KCP2VL)

CD      -- 解析時刻の更新(3) --
        DTNOW=DT
        IF (IPRNT.GT.1) THEN
          CALL VF_BWUWG(0,XX,YY,ZZ,UU,VV,WW,ANU,GGVEL,
     &                  BCU,BCV,BCW,BCF,BCVI,INDB)
          CALL VF_BWUWG(1,XX,YY,ZZ,UU,VV,WW,ANU,GGVEL,
     &                  BCU,BCV,BCW,BCF,BCVI,INDB)
        ENDIF

CD      -- 温度の計算 --
        IF (LEQT.NE.0) THEN
          CALL VF_A2CPUT(0,ICPUST,KCP2TT)
          CALL VF_T1CAL(XX,YY,ZZ,UU,VV,WW,GGV,GGX,GGY,GGZ,GGV0,
     &                  TT,ALM,BCT,BCTI,DBUF,WK01,WK02,WK03,WK04,
     &                  NF,INDX,INDY,INDZ,INDC,INDB,INDS,INDBT)
          CALL VF_A2CPUT(0,ICPUEN,KCP2TT)
        ENDIF

CD      -- スカラー量の計算 --
        IF (LEQC.GT.0) THEN
          CALL VF_A2CPUT(0,ICPUST,KCP2SS)
          CALL VF_S1CAL(XX,YY,ZZ,UU,VV,WW,GGV,GGX,GGY,GGZ,GGV0,
     &                  CC,DD,BCC,BCCI,DBUF,WK01,WK02,WK03,WK04,
     &                  NF,INDX,INDY,INDZ,INDC,INDB,INDS,INDBC)
          CALL VF_A2CPUT(0,ICPUEN,KCP2SS)
        ENDIF

CD      -- k-ε2方程式モデルの計算 --
        IF (LEQK.NE.0) THEN
          CALL VF_A2CPUT(0,ICPUST,KCP2KE)
          CALL VF_K1CAL(XX,YY,ZZ,UU,VV,WW,FF,GGV,GGX,GGY,GGZ,GGV0,
     &                  BCU,BCV,BCW,BCVI,ANUT,AK,AE,BCK,BCE,BCF,DBUF,
     &                  WK01,WK02,WK03,WK04,WK05,WK06,WK07,WK08,WK09,
     &                  NF,INDX,INDY,INDZ,INDC,INDB,INDS,INDBK,INDBE)
          CALL VF_A2CPUT(0,ICPUEN,KCP2KE)
        ENDIF

CD      -- VOF関数Fの計算およびNFの設定 --
        CALL VF_A2CPUT(0,ICPUST,KCP2FF)
        CALL VF_F1CAL(XX,YY,ZZ,UU,VV,WW,PP,FF,FX,FY,FZ,ANU,
     &                GGV,GGX,GGY,GGZ,BCU,BCV,BCW,BCP,BCF,BCVI,TBUB,
     &                DROPTX,DROPTY,DROPTZ,DROPUU,DROPVV,DROPWW,
     &                GGV0,DMTBTT,DMTBHH,DBUF,WK01,WK02,WK03,WK04,WK05,
     &                WK06,WK07,WK08,WK09,WK10,WK11,
     &                NF,INDX,INDY,INDZ,INDC,INDB,INDS,IBUF,NWK1)
        CALL VF_A2CPUT(0,ICPUEN,KCP2FF)

        IF(ISTM.EQ.1) THEN
          FLFU = -WK01/DTNOW
          FLFV = -WK02/DTNOW
        ENDIF

CD      -- NFの変更に伴う、スカラー量の境界値等の再設定 --
        IF (LEQK.NE.0) THEN
          CALL VF_BWKELG(XX,YY,ZZ,UU,VV,WW,FF,BCVI,AK,AE,DBUF,
     &                   NF,INDX,INDY,INDZ,INDB)
C2F       CALL VF_BSSS (AK,DBUF,NF)
C2F       CALL VF_BSSS (AE,DBUF,NF)
          CALL VF_CNUT0(AK,AE,ANUT,NF)
          CALL VF_CNU00(ANUT,ANU,NF,FF)
          CALL VF_BWKE (AK,AE,BCK,BCE,BCF,INDB,INDBK,INDBE)
          CALL VF_BWUWN(XX,YY,ZZ,UU,VV,WW,FF,BCU,BCV,BCW,BCF,
     &                  DMTBTT,DMTBZZ,DMTBHH,DMTBUN,DMTBUT,DBUF,
     &                  WK13,WK14,WK15,NF,INDX,INDY,INDB)
          CALL VF_BWUWT(XX,YY,ZZ,UU,VV,WW,ANU,BCU,BCV,BCW,BCF,BCVI,INDB)
          IF (IPRNT.GT.1) THEN
            CALL VF_BWUWG(0,XX,YY,ZZ,UU,VV,WW,ANU,GGVEL,
     &                    BCU,BCV,BCW,BCF,BCVI,INDB)
            CALL VF_BWUWG(1,XX,YY,ZZ,UU,VV,WW,ANU,GGVEL,
     &                    BCU,BCV,BCW,BCF,BCVI,INDB)
          ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSE
          CALL VF_CNU00(ANUT,ANU,NF,FF)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ENDIF
        IF (LEQT.NE.0) THEN
C2F       CALL VF_BSSS(TT,DBUF,NF)
          IF (LEQK.NE.0) CALL VF_CLM00(ANUT,ALM,NF,FF)
          CALL VF_BWSS(XX,YY,ZZ,GGX,GGY,GGZ,TT,ALM,BCT,BCTI,
     &                 NF,INDB,INDBT)
        ENDIF
        DO 700 LC=1,LEQC
C2F       CALL VF_BSSS(CC(1,1,1,LC),DBUF,NF)
 700    CONTINUE
        IF (LEQK.NE.0) CALL VF_CDD00(ANUT,DD,NF)
        DO 710 LC=1,LEQC
          CALL VF_BWSS(XX,YY,ZZ,GGX,GGY,GGZ,CC(1,1,1,LC),DD(1,1,1,LC),
     &                 BCC(1,LC),BCCI(1,1,LC),NF,INDB,INDBC(1,LC))
 710    CONTINUE

         write(*,*) 'End CADM ==============',nnow

CD    ** 反復終了 **
        GOTO 500
 1000 CONTINUE

CD    -- MA側にCADMASの送信終了を伝える（MPIの場合） --
      CALL VF_OM1TRN(DT,ZZ,UU,VV,FF,GGV,WK01,NF,-1)

CD    -- STMにCADMASの計算終了を伝える --
      IF(ISTM.GT.0) CALL SF_STM_FINALIZE()

CD    -- タイマーの終了とCPU時間の出力 --
      CALL VF_A2CPUT(0     ,ICPUEN,KCP1CL)
      CALL VF_A2CPUT(0     ,ICPUEN,KCP0AL)
      CALL VF_A2CPUT(ILPFIL,ICPUOU,0     )

CD    -- 終了コメントをリストファイルに出力 --
      IF (MYRANK.EQ.0) WRITE(*,9990)
      WRITE(ILPFIL,9990)

CD    -- ファイルのクローズ --
      CALL VF_A2CLOS()

CD    -- 並列環境の終了 --
      CALL VF_P1END()

C     -- 実行文の終了 --
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      CALL VF_A2ERR('VF_A1MAIN','CAN NOT OPEN (*****.list).')

      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(/' ',
     &       '##### CADMAS-SURF/3D-2F Ver.',I1,'.',I1,' START. ######')
 9520 FORMAT(/' ','##### ',A)
 9530 FORMAT( ' ','STEP='    ,I6,
     &            ' : TIME= ',1PE12.5,
     &            ' : DT  = ',1PE12.5 )
 9540 FORMAT( ' ','STEP='    ,I6,
     &            ' : TIME= ',1PE12.5,
     &            ' : DT  = ',1PE12.5,
     &            ' : FSUM= ',1PE12.5,
     &            ' : FCUT= ',1PE12.5,
     &            ' : !VD!= ',1PE12.5 )
 9550 FORMAT( ' ',11X,
     &            ' : !B! = ',1PE12.5,
     &            ' : !R! = ',1PE12.5,
     &            ' : ITR = ',I6      )
 9560 FORMAT( ' ','  SUBLOOP=',I6,
     &            ' : TIME= ',1PE12.5 )
 9570 FORMAT(/' ','##### MYRANK=',I5,' /',I5)
 9990 FORMAT(/' ','##### NORMAL END. ###########################'/)

C==== 終了 ===========================================================

 9999 CONTINUE
      STOP
      END
