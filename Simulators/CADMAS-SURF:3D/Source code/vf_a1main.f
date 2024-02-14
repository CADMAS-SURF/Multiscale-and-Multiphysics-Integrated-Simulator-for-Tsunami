      PROGRAM VF_A1MAIN

CD=== 概要 ===========================================================

CDT   VF_A1MAIN:CADMAS-SURF/3D-MGのメインルーチン
CD      (1)解析対象:自由表面を含む3次元非圧縮性流体
CD      (2)解析方法:差分法,SMAC法,VOF法
CD      (3)座標系  :デカルト座標系
CD      (4)言語    :Fortran90(配列の動的アロケートのため)

C==== 宣言 ===========================================================

      use mod_comm,only: init_mpmd,comm_mlicdsmg2fc
C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'mpif.h'
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'
      INCLUDE 'VF_ASTOCI.h'
      INCLUDE 'VF_ASTOCR.h'
C----------------------------------------------------------2012.03 start
      INCLUDE 'VF_ASEABT.h'
C----------------------------------------------------------2012.03 end

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
CD    ANU(@FOR-3D@)     : R*8 : 分子動粘性係数と渦動粘性係数の和
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
CD    BCU(NUMB)         : R*8 : x方向流速の境界値
CD    BCV(NUMB)         : R*8 : y方向流速の境界値
CD    BCW(NUMB)         : R*8 : z方向流速の境界値
CD    BCP(NUMB)         : R*8 : 圧力の境界値
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
CD    GGV0(@FOR-3D@)    : R*8 : 空隙率(時間依存用)
CD    GLV0(@FOR-3D@)    : R*8 : =GGV+(1-GGV)*CM(時間依存用)
CD    ANUT(@FOR-3D@)    : R*8 : 渦動粘性係数νt
CD    AK(@FOR-3D@)      : R*8 : 乱流エネルギ
CD    AE(@FOR-3D@)      : R*8 : 乱流エネルギ散逸
CD    BCK(NUMB)         : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB)         : R*8 : 乱流エネルギ散逸の境界値
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
CD    DMTBTT2(MTBTT2)     : R*8 : マトリクスデータの無次元位相
CD    DMTBZZ2(MTBZZ2)     : R*8 : マトリクスデータのz座標(平均水位をゼロ)
CD    DMTBHH2(MTBTT2)     : R*8 : マトリクスデータの水位
CD    DMTBUN2(MTBZZ2,MTBTT2) : R*8 : マトリクスデータの水平方向流速
CD    DMTBUT2(MTBZZ2,MTBTT2) : R*8 : マトリクスデータの鉛直方向流速
CD    DBUF(NUMBUF*MAXBUF) : R*8 : 並列用のバッファ
CD    SRCUV(NUMIJ,NUMK) : R*8 : 造波ソース用水平方向流速
CD    PPPVC(@FOR-3D@)   : R*8 : 空気圧の計算用の圧力（E,Sセルに気圧）
CD    WK01-17(@FOR-3D@) : R*8 : ワーク配列
CD    WKBC(NUMB)        : R*8 : ワーク配列
CD    NF(@FOR-3D@)      : I*4 : セルの状態を示すインデックス
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
CD                              =-1:障害物面
CD                              = 0:通常面
CD                              >=1:境界面(INDBへのポインタ)
CD    INDY(@FOR-3D@)    : I*4 : y面の状態を示すインデックス
CD                              =-1:障害物面
CD                              = 0:通常面
CD                              >=1:境界面(INDBへのポインタ)
CD    INDZ(@FOR-3D@)    : I*4 : z面の状態を示すインデックス
CD                              =-1:障害物面
CD                              = 0:通常面
CD                              >=1:境界面(INDBへのポインタ)
CD    INDC(@FOR-3D@)    : I*4 : セルの計算状態を示すインデックス
CD                              =-1:非計算セル(障害物または気体)
CD                              = 0:計算セル(流体または表面)
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
CD                              (3,L):流速・圧力の境界条件
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
CD    INDS(@FOR-1D@)    : I*4 : 表面セルのI,J,K座標
CD                                (I+NUMI*(J-1)+NUMI*NUMJ*(K-1))
CD    INDBK(NUMB)       : I*4 : 乱流エネルギの境界条件
CD                                =-2:勾配ゼロ(移流項を評価しない)
CD                                =-1:値固定(移流項を評価しない)
CD                                = 0:未定義
CD                                = 1:値固定(移流項を評価する)
CD                                = 2:勾配ゼロ移流項を評価する)
CD                                = 6:対数則
CD                                = 8:完全粗面境界
CD    INDBE(NUMB)       : I*4 : 乱流エネルギ散逸の境界条件
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
CD    IPVC(@FOR-3D@)    : I*4 : 空気圧の計算用インデックス
CD                                = 0:非気泡セル
CD                                >=1:気泡番号
CD    IBUF(NUMBUF*MAXBUF) : I*4 : 並列用のバッファ
CD    NWK1(@FOR-3D@)    : I*4 : ワーク配列
CD    NWKBC(NUMB)       : I*4 : ワーク配列
C----------------------------------------------------------2011.04 start
CD    XPF(NUMI)                      :x方向の親格子に対する補間係数
CD    YPF(NUMJ)                      :y方向の親格子に対する補間係数
CD    ZPF(NUMK)                      :z方向の親格子に対する補間係数
CD    IPF(MGPINF(1))                 :x方向の親格子1に対する格子の数
CD    JPF(MGPINF(2))                 :y方向の親格子1に対する格子の数
CD    KPF(MGPINF(3))                 :z方向の親格子1に対する格子の数
C----------------------------------------------------------2011.04 end
C----------------------------------------------------------2012.03 start
CD    DELH(@FOR-2D@)    : R*8 : 水位の増分[m]
CD    DELH_IN(@FOR-2D@) : R*8 : 水位の増分(ファイル読み込み値)[m]
C----------------------------------------------------------2012.03 end
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: XX ,YY ,ZZ
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: UU ,VV ,WW
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: PP ,FF ,ANU
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: CM0,CD0
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: GGV,GGX,GGY,GGZ
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: GLV,GLX,GLY,GLZ
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: BCU,BCV,BCW
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: BCP,BCF,BCVI
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: TBUB
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: DROPTX,DROPTY
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: DROPTZ,DROPUU
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: DROPVV,DROPWW
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: GGVOLD,GGVNOW
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: GGV0,GLV0
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: ANUT,AK,AE
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: BCK,BCE
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: TT,ALM
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: BCT
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: BCTI
      DOUBLE PRECISION, DIMENSION(:,:,:,:), ALLOCATABLE :: CC,DD
      DOUBLE PRECISION, DIMENSION(:    ,:), ALLOCATABLE :: BCC
      DOUBLE PRECISION, DIMENSION(:,:  ,:), ALLOCATABLE :: BCCI
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: DMTBTT
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: DMTBZZ
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: DMTBHH
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: DMTBUN
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: DMTBUT
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: DMTBTT2
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: DMTBZZ2
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: DMTBHH2
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: DMTBUN2
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: DMTBUT2
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: DBUF
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: SRCUV
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: PPPVC
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: WK01,WK02,WK03
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: WK04,WK05,WK06
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: WK07,WK08,WK09
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: WK10,WK11,WK12
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: WK13,WK14,WK15
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: WK16,WK17
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: WKBC
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: STBUF
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: XXWK,YYWK,ZZWK
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: FFLXX,FFLXY
      INTEGER         , DIMENSION(:,:,:), ALLOCATABLE :: NF  ,INDX
      INTEGER         , DIMENSION(:,:,:), ALLOCATABLE :: INDY,INDZ,INDC
      INTEGER         , DIMENSION(:,:  ), ALLOCATABLE :: INDB
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: INDS,INDBK
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: INDBE,INDBT
      INTEGER         , DIMENSION(:,:  ), ALLOCATABLE :: INDBC
      INTEGER         , DIMENSION(:,:,:), ALLOCATABLE :: IPVC
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: IBUF
      INTEGER         , DIMENSION(:,:,:), ALLOCATABLE :: NWK1
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: NWKBC
C----------------------------------------------------------2011.04 start
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: XPF, YPF, ZPF
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: IPF, JPF, KPF
C----------------------------------------------------------2011.04 end
C----------------------------------------------------------2012.03 start
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: DELH
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: DELH_IN
C----------------------------------------------------------2012.03 end
C-FOR-DBG-START
C      DOUBLE PRECISION, DIMENSION(MAXG1,10)         :: XX ,YY ,ZZ
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: UU ,VV ,WW
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: PP ,FF ,ANU
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: CM0,CD0
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: GGV,GGX,GGY,GGZ
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: GLV,GLX,GLY,GLZ
C      DOUBLE PRECISION, DIMENSION(10      )         :: BCU,BCV,BCW
C      DOUBLE PRECISION, DIMENSION(10      )         :: BCP,BCF,BCVI
C      DOUBLE PRECISION, DIMENSION(10      )         :: TBUB
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: DROPTX,DROPTY
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: DROPTZ,DROPUU
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: DROPVV,DROPWW
C      DOUBLE PRECISION, DIMENSION(10      )         :: GGVOLD,GGVNOW
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: GGV0,GLV0
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: ANUT,AK,AE
C      DOUBLE PRECISION, DIMENSION(10      )         :: BCK,BCE
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: TT,ALM
C      DOUBLE PRECISION, DIMENSION(10      )         :: BCT
C      DOUBLE PRECISION, DIMENSION( 2,10   )         :: BCTI
C      DOUBLE PRECISION, DIMENSION(10,10,10,5)       :: CC,DD
C      DOUBLE PRECISION, DIMENSION(10      ,5)       :: BCC
C      DOUBLE PRECISION, DIMENSION( 2,10   ,5)       :: BCCI
C      DOUBLE PRECISION, DIMENSION(10      )         :: DMTBTT
C      DOUBLE PRECISION, DIMENSION(10      )         :: DMTBZZ
C      DOUBLE PRECISION, DIMENSION(10      )         :: DMTBHH
C      DOUBLE PRECISION, DIMENSION(10,10   )         :: DMTBUN
C      DOUBLE PRECISION, DIMENSION(10,10   )         :: DMTBUT
C      DOUBLE PRECISION, DIMENSION(10      )         :: DBUF
C      DOUBLE PRECISION, DIMENSION(10,10   )         :: SRCUV
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: PPPVC
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: WK01,WK02,WK03
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: WK04,WK05,WK06
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: WK07,WK08,WK09
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: WK10,WK11,WK12
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: WK13,WK14,WK15
C      DOUBLE PRECISION, DIMENSION(10,10,10)         :: WK16,WK17
C      DOUBLE PRECISION, DIMENSION(10      )         :: WKBC
C      INTEGER         , DIMENSION(10,10,10)         :: NF  ,INDX
C      INTEGER         , DIMENSION(10,10,10)         :: INDY,INDZ,INDC
C      INTEGER         , DIMENSION(MAXB1,10)         :: INDB
C      INTEGER         , DIMENSION(10      )         :: INDS,INDBK
C      INTEGER         , DIMENSION(10      )         :: INDBE,INDBT
C      INTEGER         , DIMENSION(10      ,5)       :: INDBC
C      INTEGER         , DIMENSION(10,10,10)         :: IPVC
C      INTEGER         , DIMENSION(10      )         :: IBUF
C      INTEGER         , DIMENSION(10,10,10)         :: NWK1
C      INTEGER         , DIMENSION(10      )         :: NWKBC
C-FOR-DBG-END
      CHARACTER*5 TEXTP
      INTEGER     IERR,M,N,IRANK,ISIZE,ISTT(MPI_STATUS_SIZE)
      INTEGER     IREQ,ITAG,NWK(8)
      INTEGER     I1G,I2G,J1G,J2G,IS,IE,JS,JE,IE1
      INTEGER     IEND(2),IEND1(2)

C==== 実行 ===========================================================

      call init_mpmd

CD    -- 経過時間の計測(開始時) --
      CALL VF_P0TIME(WTM1)

CD    -- 並列環境の初期化 --
      CALL VF_P0INIT()

CD    -- タイマーの初期化と開始 --
      CALL VF_A2CPUT(0,ICPUIN,0     )
      CALL VF_A2CPUT(0,ICPUST,KCP0AL)
      CALL VF_A2CPUT(0,ICPUST,KCP1PR)

CD    -- マルチグリッド環境ファイルの読み込み --
      IF (MGRANK.EQ.0) WRITE(*,9510) IVR001,IVR002
      IF (MGRANK.EQ.0) WRITE(*,*) '*** ver4.9.1 ***'
      CALL VF_PMGINP()

CD    -- リストファイルのオープンと開始コメントの出力 --
      ILPFIL=0
      IF (NPROCS.EQ.1) THEN
        OPEN(MFILLP,ERR=9010,
     &       FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.list',
     &       STATUS='NEW',FORM='FORMATTED'    )
      ELSE
        WRITE(TEXTP,'(I5.5)') MYRANK
        OPEN(MFILLP,ERR=9010,
     &       FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.list'//TEXTP,
     &       STATUS='NEW',FORM='FORMATTED'    )
      ENDIF
      ILPFIL=MFILLP
      WRITE(ILPFIL,9510) IVR001,IVR002
      WRITE(ILPFIL,9570) MGRANK,MGPROC
      WRITE(ILPFIL,9580) MGAREA(MGRANK+1),MGARAN
      WRITE(ILPFIL,9590) MYRANK,NPROCS

CD    -- コモン変数へのデフォルト値の設定 --
      IF (MGRANK.EQ.0) WRITE(*,9520) 'DEFAULT.'
      WRITE(ILPFIL,9520) 'DEFAULT.'
      CALL VF_A2DFLT()

CD    -- STOCとの通信環境の初期化 --
      IERR = 0
      CALL VF_STOC_INIT(IERR)

CD    -- 入力ファイルの読み込みと配列のアロケート等 --
C     Fortran90のALLOCATE文だけではきれいに配列のアロケート
C     ができないので、あえて、3度読みを行う
      IF (MGRANK.EQ.0) WRITE(*,9520) 'INPUT-DATA.'
      WRITE(ILPFIL,9520) 'INPUT-DATA.'
C     * 読み込み(1)
      CALL VF_II1INP(XX,YY,ZZ,CM0,CD0,GGV,GGX,GGY,GGZ,
C----------------------------------------------------------2011.04 start
     &               XPF,YPF,ZPF,IPF,JPF,KPF,
C----------------------------------------------------------2011.04 end
     &               BCU,BCV,BCW,BCP,BCF,BCVI,BCK,BCE,BCT,BCTI,BCC,BCCI,
     &               NF,INDX,INDY,INDZ,INDB,INDBK,INDBE,INDBT,INDBC,
     &               DBUF,IBUF,XXWK,YYWK,ZZWK)

C     * 配列のアロケートとデフォルト値の設定(1)
      IERR = 0
      ALLOCATE(XX  (MAXG1,NUMI)    ,STAT=IERR)
      ALLOCATE(YY  (MAXG1,NUMJ)    ,STAT=IERR)
      ALLOCATE(ZZ  (MAXG1,NUMK)    ,STAT=IERR)
      ALLOCATE(UU  (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(VV  (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(WW  (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(PP  (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(FF  (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(ANU (NUMI,NUMJ,NUMK),STAT=IERR)
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
cmod20160721(s)
c      IF (LB_CADMAS.EQ.1) THEN
        ALLOCATE(XXWK(NUMI0),STAT=IERR)
        ALLOCATE(YYWK(NUMJ0),STAT=IERR)
        ALLOCATE(ZZWK(NUMK ),STAT=IERR)
c      ENDIF
cmod20160721(e)
      ALLOCATE(FFLXX(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(FFLXY(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(NF  (NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(INDX(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(INDY(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(INDZ(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(INDC(NUMI,NUMJ,NUMK),STAT=IERR)
      ALLOCATE(INDS(NUMI*NUMJ*NUMK),STAT=IERR)
      ALLOCATE(IBUF(NUMBUF*MAXBUF) ,STAT=IERR)
      ALLOCATE(NWK1(NUMI,NUMJ,NUMK),STAT=IERR)
C----------------------------------------------------------2011.04 start
      ALLOCATE(XPF(NUMI),STAT=IERR)
      ALLOCATE(YPF(NUMJ),STAT=IERR)
      ALLOCATE(ZPF(NUMK),STAT=IERR)
C----------------------------------------------------------2011.04 end
      IF (IERR.NE.0) CALL VF_A2ERR('VF_A1MAIN','CAN NOT ALLOC.')
      CALL VF_ZSETR2(XX  ,0.0D0,MAXG1,NUMI)
      CALL VF_ZSETR2(YY  ,0.0D0,MAXG1,NUMJ)
      CALL VF_ZSETR2(ZZ  ,0.0D0,MAXG1,NUMK)
      CALL VF_ZSETR3(UU  ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(VV  ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(WW  ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(PP  ,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(FF  ,0.0D0,NUMI,NUMJ,NUMK)
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
      CALL VF_ZSETR3(FFLXX,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETR3(FFLXY,0.0D0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETI3(NF  ,    0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETI3(INDX,    0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETI3(INDY,    0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETI3(INDZ,    0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETI3(INDC,    0,NUMI,NUMJ,NUMK)
      CALL VF_ZSETI1(INDS,    0,NUMI*NUMJ*NUMK)
      CALL VF_ZSETI1(IBUF,    0,NUMBUF*MAXBUF )
      CALL VF_ZSETI3(NWK1,    0,NUMI,NUMJ,NUMK)
C----------------------------------------------------------2011.04 start
      CALL VF_ZSETR1(XPF,0.0D0,NUMI)
      CALL VF_ZSETR1(YPF,0.0D0,NUMJ)
      CALL VF_ZSETR1(ZPF,0.0D0,NUMK)
C----------------------------------------------------------2011.04 end
C     * 読み込み(2)
      CALL VF_II1INP(XX,YY,ZZ,CM0,CD0,GGV,GGX,GGY,GGZ,
C----------------------------------------------------------2011.04 start
     &               XPF,YPF,ZPF,IPF,JPF,KPF,
C----------------------------------------------------------2011.04 end
     &               BCU,BCV,BCW,BCP,BCF,BCVI,BCK,BCE,BCT,BCTI,BCC,BCCI,
     &               NF,INDX,INDY,INDZ,INDB,INDBK,INDBE,INDBT,INDBC,
     &               DBUF,IBUF,XXWK,YYWK,ZZWK)
      IF( NB_SC.GT.0 ) THEN
         NBUF = MAX(NIST,NJST)*NKST*6
         NBUF = MAX(NBUF,1)
         ALLOCATE(STBUF(NBUF),STAT=IERR)
      ENDIF
C     * 配列のアロケートとデフォルト値の設定(2)
      IERR = 0
      ALLOCATE(BCU  (NUMBX     ),STAT=IERR)
      ALLOCATE(BCV  (NUMBX     ),STAT=IERR)
      ALLOCATE(BCW  (NUMBX     ),STAT=IERR)
      ALLOCATE(BCP  (NUMBX     ),STAT=IERR)
      ALLOCATE(BCF  (NUMBX     ),STAT=IERR)
      ALLOCATE(BCVI (NUMBX     ),STAT=IERR)
      IF (LEQK.NE.0) THEN
        ALLOCATE(BCK (  NUMBX  ),STAT=IERR)
        ALLOCATE(BCE (  NUMBX  ),STAT=IERR)
      ENDIF
      IF (LEQT.NE.0) THEN
        ALLOCATE(BCT (  NUMBX  ),STAT=IERR)
        ALLOCATE(BCTI(2,NUMBX  ),STAT=IERR)
      ENDIF
      IF (LEQC.GT.0) THEN
        ALLOCATE(BCC (  NUMBX,LEQC),STAT=IERR)
        ALLOCATE(BCCI(2,NUMBX,LEQC),STAT=IERR)
      ENDIF
      ALLOCATE(WKBC (NUMBX      ),STAT=IERR)
      ALLOCATE(INDB (MAXB1,NUMBX),STAT=IERR)
      IF (LEQK.NE.0) THEN
        ALLOCATE(INDBK(NUMBX    ),STAT=IERR)
        ALLOCATE(INDBE(NUMBX    ),STAT=IERR)
      ENDIF
      IF (LEQT.NE.0) THEN
        ALLOCATE(INDBT(NUMBX    ),STAT=IERR)
      ENDIF
      IF (LEQC.GT.0) THEN
        ALLOCATE(INDBC(NUMBX,LEQC),STAT=IERR)
      ENDIF
      ALLOCATE(NWKBC(NUMBX     ),STAT=IERR)
C----------------------------------------------------------2011.04 start
      IF(MGPARE(MGRANK+1).GT.0) THEN
        ALLOCATE(IPF(0:MGPINF(1)),STAT=IERR)
        ALLOCATE(JPF(0:MGPINF(2)),STAT=IERR)
        ALLOCATE(KPF(0:MGPINF(3)),STAT=IERR)
      END IF
C----------------------------------------------------------2011.04 end
      IF (IERR.NE.0) CALL VF_A2ERR('VF_A1MAIN','CAN NOT ALLOC.')
      CALL VF_ZSETR1(BCU  ,0.0D0,NUMBX)
      CALL VF_ZSETR1(BCV  ,0.0D0,NUMBX)
      CALL VF_ZSETR1(BCW  ,0.0D0,NUMBX)
      CALL VF_ZSETR1(BCP  ,0.0D0,NUMBX)
      CALL VF_ZSETR1(BCF  ,0.0D0,NUMBX)
      CALL VF_ZSETR1(BCVI ,0.0D0,NUMBX)
      IF (LEQK.NE.0) THEN
        CALL VF_ZSETR1(BCK ,0.0D0,  NUMBX)
        CALL VF_ZSETR1(BCE ,0.0D0,  NUMBX)
      ENDIF
      IF (LEQT.NE.0) THEN
        CALL VF_ZSETR1(BCT ,0.0D0,  NUMBX)
        CALL VF_ZSETR2(BCTI,0.0D0,2,NUMBX)
      ENDIF
      DO 110 LC=1,LEQC
        CALL VF_ZSETR1(BCC (  1,LC),0.0D0,  NUMBX)
        CALL VF_ZSETR2(BCCI(1,1,LC),0.0D0,2,NUMBX)
 110  CONTINUE
      CALL VF_ZSETR1(WKBC ,0.0D0,NUMBX)
      CALL VF_ZSETI2(INDB ,    0,MAXB1,NUMBX)
      IF (LEQK.NE.0) THEN
        CALL VF_ZSETI1(INDBK,  0,NUMBX)
        CALL VF_ZSETI1(INDBE,  0,NUMBX)
      ENDIF
      IF (LEQT.NE.0) THEN
        CALL VF_ZSETI1(INDBT,  0,NUMBX)
      ENDIF
      DO 120 LC=1,LEQC
        CALL VF_ZSETI1(INDBC(1,LC),0,NUMBX)
 120  CONTINUE
      CALL VF_ZSETI1(NWKBC,    0,NUMBX)
C----------------------------------------------------------2011.04 start
      IF(MGPARE(MGRANK+1).GT.0) THEN
        CALL VF_ZSETI1(IPF,0,MGPINF(1)+1)
        CALL VF_ZSETI1(JPF,0,MGPINF(2)+1)
        CALL VF_ZSETI1(KPF,0,MGPINF(3)+1)
      END IF
C----------------------------------------------------------2011.04 end
C     * 読み込み(3)
      CALL VF_II1INP(XX,YY,ZZ,CM0,CD0,GGV,GGX,GGY,GGZ,
C----------------------------------------------------------2011.04 start
     &               XPF,YPF,ZPF,IPF,JPF,KPF,
C----------------------------------------------------------2011.04 end
     &               BCU,BCV,BCW,BCP,BCF,BCVI,BCK,BCE,BCT,BCTI,BCC,BCCI,
     &               NF,INDX,INDY,INDZ,INDB,INDBK,INDBE,INDBT,INDBC,
     &               DBUF,IBUF,XXWK,YYWK,ZZWK)
cmod20160803(s)
cout20160721      IF (LB_CADMAS.EQ.1) THEN
        DEALLOCATE(XXWK)
        DEALLOCATE(YYWK)
        DEALLOCATE(ZZWK)
cout20160721      ENDIF
cmod20160803(e)
C     * 配列のアロケートとデフォルト値の設定(3)
      IERR = 0
      NUMIJ=1
      IF     (ISCTYP(1).GT.0) THEN
        NUMIJ=NUMJ
      ELSEIF (ISCTYP(1).LT.0) THEN
        NUMIJ=NUMI
      ENDIF
      ALLOCATE(SRCUV(NUMIJ,NUMK),STAT=IERR)
      IF (PVCP0.GE.ZERO) THEN
        ALLOCATE(PPPVC(NUMI,NUMJ,NUMK),STAT=IERR)
        ALLOCATE(IPVC (NUMI,NUMJ,NUMK),STAT=IERR)
      ENDIF
      IF (IERR.NE.0) CALL VF_A2ERR('VF_A1MAIN','CAN NOT ALLOC.')
      CALL VF_ZSETR2(SRCUV,0.0D0,NUMIJ,NUMK)
      IF (PVCP0.GE.ZERO) THEN
        CALL VF_ZSETR3(PPPVC,0.0D0,NUMI,NUMJ,NUMK)
        CALL VF_ZSETI3(IPVC ,    0,NUMI,NUMJ,NUMK)
      ENDIF
C----------------------------------------------------------2012.03 start
      IF(ISEABT.NE.0) THEN
        ALLOCATE(DELH   (NUMI0,NUMJ0),STAT=IERR)
        ALLOCATE(DELH_IN(NUMI0,NUMJ0),STAT=IERR)
        CALL VF_ZSETR2(DELH   ,0.0D0,NUMI0,NUMJ0)
        CALL VF_ZSETR2(DELH_IN,0.0D0,NUMI0,NUMJ0)
      END IF
C----------------------------------------------------------2012.03 end

CD    -- 時間依存型空隙率ファイルの読み込みと配列のアロケート等 --
C     * 読み込み(1)
      CALL VF_IP1INP(0.0D0,GGVOLD,GGVNOW)
C     * 配列のアロケートとデフォルト値の設定(1)
      IF (IPRNP.GE.1) THEN
        ALLOCATE(GGVOLD(IPRNP),STAT=IERR)
        ALLOCATE(GGVNOW(IPRNP),STAT=IERR)
        IF (IERR.NE.0) CALL VF_A2ERR('VF_A1MAIN','CAN NOT ALLOC.')
        CALL VF_ZSETR1(GGVOLD,1.0D0,IPRNP)
        CALL VF_ZSETR1(GGVNOW,1.0D0,IPRNP)
      ENDIF
      IF (IPRNT.GT.1) THEN
        ALLOCATE(GGV0(NUMI,NUMJ,NUMK),STAT=IERR)
        ALLOCATE(GLV0(NUMI,NUMJ,NUMK),STAT=IERR)
        IF (IERR.NE.0) CALL VF_A2ERR('VF_A1MAIN','CAN NOT ALLOC.')
        CALL VF_ZSETR3(GGV0,1.0D0,NUMI,NUMJ,NUMK)
        CALL VF_ZSETR3(GLV0,1.0D0,NUMI,NUMJ,NUMK)
      ENDIF
      IF (IPRNT.EQ.1) THEN
        CALL VF_IP1INP(0.0D0,GGVOLD,GGVNOW)
        CALL VF_CGGV  (0,0.0D0,0.0D0,0.0D0,GGV,GGV0,GGVOLD,GGVNOW,NF)
        CALL VF_CGGXYZ(0,XX,YY,ZZ,GGV,GGX,GGY,GGZ,GGV0,DBUF,
     &                 NF,INDX,INDY,INDZ)
      ENDIF
      CALL VF_CGLV  (CM0,GGV,GLV,NF)
      CALL VF_CGLXYZ(XX,YY,ZZ,CM0,GGX,GGY,GGZ,GLX,GLY,GLZ,DBUF,
     &               NF,INDX,INDY,INDZ)

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
      IF (MTBTYP2.GE.1) THEN
C       * 読み込み(1)
        CALL VF_IM2INP(DMTBTT2,DMTBZZ2,DMTBHH2,DMTBUN2,DMTBUT2)
C       * 配列のアロケートとデフォルト値の設定
        ALLOCATE(DMTBTT2(MTBTT2)      ,STAT=IERR)
        IF (MTBTYP2.NE.2) THEN
          ALLOCATE(DMTBHH2(MTBTT2)      ,STAT=IERR)
        ENDIF
        IF (MTBTYP2.NE.3) THEN
          ALLOCATE(DMTBZZ2(MTBZZ2)       ,STAT=IERR)
          ALLOCATE(DMTBUN2(MTBZZ2,MTBTT2),STAT=IERR)
          ALLOCATE(DMTBUT2(MTBZZ2,MTBTT2),STAT=IERR)
        ENDIF
        IF (IERR.NE.0) CALL VF_A2ERR('VF_A1MAIN','CAN NOT ALLOC.')
        CALL VF_ZSETR1(DMTBTT2,0.0D0,MTBTT2)
        IF (MTBTYP2.NE.2) THEN
          CALL VF_ZSETR1(DMTBHH2,0.0D0,MTBTT2)
        ENDIF
        IF (MTBTYP2.NE.3) THEN
          CALL VF_ZSETR1(DMTBZZ2,0.0D0,MTBZZ2)
          CALL VF_ZSETR2(DMTBUN2,0.0D0,MTBZZ2,MTBTT2)
          CALL VF_ZSETR2(DMTBUT2,0.0D0,MTBZZ2,MTBTT2)
        ENDIF
C       * 読み込み(2)
        CALL VF_IM2INP(DMTBTT2,DMTBZZ2,DMTBHH2,DMTBUN2,DMTBUT2)
      ENDIF

CD    -- 解析条件から各種情報を構築する --
      IF (MGRANK.EQ.0) WRITE(*,9520) 'SETUP.'
      WRITE(ILPFIL,9520) 'SETUP.'
      CALL VF_CSETUP(XX,YY,ZZ,BCU,BCV,BCW,BCP,BCF,BCVI,BCK,BCE,
     &               NF,INDX,INDY,INDZ,INDB,INDBK,INDBE)

CD    -- リストファイルに解析条件を出力 --
      IF (MGRANK.EQ.0) WRITE(*,9520) 'CONDITION.'
      WRITE(ILPFIL,9520) 'CONDITION.'
      CALL VF_OL1INI(XX,YY,ZZ,
     &               CM0,CD0,GGV,GGX,GGY,GGZ,GLV,GLX,GLY,GLZ,
     &               BCU,BCV,BCW,BCP,BCF,BCK,BCE,BCT,BCTI,BCC,BCCI,
     &               NF,INDX,INDY,INDZ,INDB,INDBK,INDBE,INDBT,INDBC)

CD    -- 初期条件の設定 --
      IF (IRETYP.LT.0) THEN
        IF (MGRANK.EQ.0) WRITE(*,9520) 'INITIAL.'
        WRITE(ILPFIL,9520) 'INITIAL.'
        NNOW     =0
        DTNOW    =0.0D0
        TNOW     =0.0D0
        RLPTRN(4)=RLPTRN(1)
        RGRTRN(4)=RGRTRN(1)
        RRSTRN(4)=RRSTRN(1)
        RTRTRN(4)=RTRTRN(1)
        IF (IPRNT.GT.1) THEN
          CALL VF_IP1INP(TNOW,GGVOLD,GGVNOW)
          CALL VF_CGGV  (0,0.0D0,0.0D0,0.0D0,GGV,GGV0,GGVOLD,GGVNOW,NF)
          CALL VF_CGGXYZ(0,XX,YY,ZZ,GGV,GGX,GGY,GGZ,GGV0,DBUF,
     &                   NF,INDX,INDY,INDZ)
          CALL VF_CGLV  (CM0,GGV,GLV,NF)
          CALL VF_CGLXYZ(XX,YY,ZZ,CM0,GGX,GGY,GGZ,GLX,GLY,GLZ,DBUF,
     &                   NF,INDX,INDY,INDZ)
        ENDIF
        CALL VF_CINIT(XX ,YY ,ZZ, 
     &                UU ,VV ,WW ,PP ,FF ,
     &                ANU,GGV,GGX,GGY,GGZ,
     &                BCU,BCV,BCW,BCP,BCF,BCVI,TBUB,
     &                DROPTX,DROPTY,DROPTZ,DROPUU,DROPVV,DROPWW,
     &                ANUT,AK,AE,BCK,BCE,
     &                TT,ALM,BCT,BCTI,CC,DD,BCC,BCCI,
     &                DMTBTT ,DMTBZZ ,DMTBHH ,DMTBUN ,DMTBUT ,
     &                DMTBTT2,DMTBZZ2,DMTBHH2,DMTBUN2,DMTBUT2,
     &                DBUF,PPPVC,WK01,WK02,WK03,
C----------------------------------------------------------2012.03 start
     &                DELH,DELH_IN,
C----------------------------------------------------------2012.03 end
     &                NF,INDX,INDY,INDZ,INDC,INDB,INDS,
     &                INDBK,INDBE,INDBT,INDBC,IPVC,IBUF,NWK1)

CD    -- リスタートファイルのオープンと読み込み --
      ELSE
        IF (MGRANK.EQ.0) WRITE(*,9520) 'RESTART.'
        WRITE(ILPFIL,9520) 'RESTART.'
        CALL VF_IR1INP(UU,VV,WW,PP,FF,ANU,GGV,BCU,BCV,BCW,BCP,BCF,
     &                 ANUT,AK,AE,BCK,BCE,TT,ALM,BCT,CC,DD,BCC,
     &                 TBUB,DROPTX,DROPTY,DROPTZ,
     &                 DROPUU,DROPVV,DROPWW,
     &                 FFLXX,FFLXY,DELH,
     &                 NF,INDC,INDS)
        IF (IPRNT.GT.1) THEN
          CALL VF_CGGXYZ(0,XX,YY,ZZ,GGV,GGX,GGY,GGZ,GGV0,DBUF,
     &                   NF,INDX,INDY,INDZ)
          CALL VF_CGLV  (CM0,GGV,GLV,NF)
          CALL VF_CGLXYZ(XX,YY,ZZ,CM0,GGX,GGY,GGZ,GLX,GLY,GLZ,DBUF,
     &                   NF,INDX,INDY,INDZ)
        ENDIF
      ENDIF
      IF (NB_SC.GT.0) THEN
CD    * STOCから情報を得る
        CALL VF_STOC_RECV(STBUF,NBUF)
CD    * STOCへ情報を渡す
        CALL VF_STOC_SEND(XX,YY,ZZ,UU,VV,WW,FF,NF,STBUF,NBUF,
     &                    GGV,FFLXX,FFLXY)
      ENDIF

CD    -- 図化ファイルに格子数等を出力 --
      CALL VF_OG1INI(XX,YY,ZZ,GGV,INDB,NWKBC)
CAKIY CALL DB_INI(XX,YY,ZZ,GGV,88,NUMI,NUMJ,NUMK,MYRANK)

CD    -- 結果ファイルのオープンと格子数等の出力 --
      CALL VF_OR1INI(GGV,ETIME)

CD    -- マルチエージェントファイルのオープンと格子数等の出力 --
      CALL VF_OM1INI(XX,YY,ZZ,GGV,WK01,NF)

CD    -- 時系列ファイルのオープンと出力対象の出力 --
      CALL VF_OT1INI()

C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C      IF (MGRANK.EQ.0) WRITE(*,*) '#################### STOP1 BY AKIY'
C      CALL VF_P0BARR()
C      WRITE(ILPFIL,*)
C      WRITE(ILPFIL,*) 'MGPRNK     =',MGPRNK
C      WRITE(ILPFIL,*) ' PR NI     =',MGPINF(1)
C      WRITE(ILPFIL,*) '    NJ     =',MGPINF(2)
C      WRITE(ILPFIL,*) '    NK     =',MGPINF(3)
C      WRITE(ILPFIL,*) '    SEND X-=',MGPINF(4)
C      WRITE(ILPFIL,*) '    SEND Y-=',MGPINF(5)
C      WRITE(ILPFIL,*) '    SEND Z-=',MGPINF(6)
C      WRITE(ILPFIL,*) '    SEND X+=',MGPINF(7)
C      WRITE(ILPFIL,*) '    SEND Y+=',MGPINF(8)
C      WRITE(ILPFIL,*) '    SEND Z+=',MGPINF(9)
C      WRITE(ILPFIL,*)
C      WRITE(ILPFIL,*) 'MGCNUM     =',MGCNUM
C      DO 990 IC=1,MGCNUM
C        WRITE(ILPFIL,*) ' CH RANK   =',MGCRNK(IC)
C        WRITE(ILPFIL,*) '    NI     =',MGCINF(1,IC)
C        WRITE(ILPFIL,*) '    NJ     =',MGCINF(2,IC)
C        WRITE(ILPFIL,*) '    NK     =',MGCINF(3,IC)
C        WRITE(ILPFIL,*) '    SEND X-=',MGCINF(4,IC)
C        WRITE(ILPFIL,*) '    SEND Y-=',MGCINF(5,IC)
C        WRITE(ILPFIL,*) '    SEND Z-=',MGCINF(6,IC)
C        WRITE(ILPFIL,*) '    SEND X+=',MGCINF(7,IC)
C        WRITE(ILPFIL,*) '    SEND Y+=',MGCINF(8,IC)
C        WRITE(ILPFIL,*) '    SEND Z+=',MGCINF(9,IC)
C        WRITE(ILPFIL,*) '    POS  X-=',MGCPOS(1,IC)
C        WRITE(ILPFIL,*) '    POS  Y-=',MGCPOS(2,IC)
C        WRITE(ILPFIL,*) '    POS  Z-=',MGCPOS(3,IC)
C        WRITE(ILPFIL,*) '    POS  X+=',MGCPOS(4,IC)
C        WRITE(ILPFIL,*) '    POS  Y+=',MGCPOS(5,IC)
C        WRITE(ILPFIL,*) '    POS  Z+=',MGCPOS(6,IC)
C 990  CONTINUE
C      IF (MGRANK.EQ.0) WRITE(*,*) '#################### STOP2 BY AKIY'
C      CALL VF_P0BARR()
C      CALL VF_A2CLOS()
C      CALL VF_P0END()
C      STOP
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

CD    -- タイマーの切替 --
      CALL VF_A2CPUT(0,ICPUEN,KCP1PR)
      CALL VF_A2CPUT(0,ICPUST,KCP1CL)

CD    -- SMAC法およびVOF法の計算ループ --
      IF (MGRANK.EQ.0) WRITE(*,9520) 'CALCULATION.'
      WRITE(ILPFIL,9520) 'CALCULATION.'
CD    ** 中判定反復 **
 500  CONTINUE

CD      -- マルチグリッドデータを転送 --
c        write(100+mgrank,*)'call pmgp2c,nnow=',nnow
        CALL VF_PMGP2C(XX,YY,ZZ,UU,VV,WW,FF,
C----------------------------------------------------------2011.04 start
     &                 GGV,GGX,GGY,XPF,YPF,ZPF,IPF,JPF,KPF,
C----------------------------------------------------------2011.04 end
     &                 BCU,BCV,BCW,BCF,DBUF,NF,INDX,INDY,INDB)
c        write(100+mgrank,*)'call pmgc2p,nnow=',nnow
        CALL VF_PMGC2P(XX,YY,ZZ,UU,VV,WW,FF,
C----------------------------------------------------------2011.04 start
     &                 GGV,GGX,GGY,XPF,YPF,ZPF,IPF,JPF,KPF,
C----------------------------------------------------------2011.04 end
     &                 BCU,BCV,BCW,BCF,DBUF,NF,INDX,INDY,INDB)
c        write(100+mgrank,*)'called pmgc2p,nnow=',nnow

CD      -- 次のステップの時間刻み幅を計算 --
        IF (IDTTYP.EQ.0) THEN
          DT=DTCNST
        ELSE
          CALL VF_CDTCAL(DT,XX,YY,ZZ,UU,VV,WW,FF,ANU,
     &                   GGV,GGX,GGY,GGZ,BCF,ALM,DD,WK01,
     &                   NF,INDX,INDY,INDZ)
        ENDIF

CD      -- 終了条件の判定 --
        IEND(1)=0
        IF (NNOW.GE.NEND .OR. (TNOW+0.5D0*DT).GE.TEND) IEND(1)=1
CCC        CALL VF_P0SUMI(IEND(1),IES)
CCC        IEND(1)=IES

CD      -- 解析結果をリストファイルに出力 --
        CALL VF_A2CPUT(0,ICPUST,KCP2FL)
CD      * ステップ情報の出力
        CALL VF_CDIV00(DV,XX,YY,ZZ,UU,VV,WW,GGX,GGY,GGZ,INDC)
        IF (MGRANK.EQ.0) WRITE(*,9530) NNOW,TNOW,DTNOW
        WRITE(ILPFIL,9540) NNOW,TNOW,DTNOW,FSUM,FCUT,DV
        IF (PVCP0.LT.ZERO) THEN
          WRITE(ILPFIL,9550) CGBNRM,CGXNRM,ICGITR
        ELSE
          WRITE(ILPFIL,9555) CGBNRM,CGXNRM,ICGITR,NPVCB
        ENDIF
        CALL VF_OL1TRN(DT ,
     &                 UU ,VV ,WW ,PP ,FF ,ANU,
     &                 GGV,GGX,GGY,GGZ,GLV,GLX,GLY,GLZ,
     &                 BCU,BCV,BCW,BCP,BCF,
     &                 ANUT,AK,AE,BCK,BCE,
     &                 TT,ALM,BCT,BCTI,CC,DD,BCC,BCCI,PPPVC,
     &                 NF ,INDB,INDBK,INDBE,INDBT,INDBC,IPVC)

CD      -- 解析結果を図化ファイルに出力 --
        CALL VF_OG1TRN(DT ,XX ,YY ,ZZ ,
     &                 UU ,VV ,WW ,PP ,FF ,GGV ,
     &                 BCU,BCV,BCW,BCP,BCF,
     &                 AK,AE,BCK,BCE,TT,BCT,CC,BCC,
     &                 WK01,WK02,WK03,WKBC,
     &                 NF,INDX,INDY,INDZ,INDB)

CD      -- 解析結果をマルチエージェントファイルに出力 --
        CALL VF_OM1TRN(DT,ZZ,UU,VV,FF,GGV,WK01,NF,0)

CD      -- 解析結果を詳細ファイルに出力 --
        CALL VF_OR1TRN(DT,UU,VV,WW,PP,FF,GGV,BCU,BCV,BCW,BCP,BCF,
     &                 AK,AE,BCK,BCE,TT,BCT,CC,BCC,
     &                 TBUB,DROPTX,DROPTY,DROPTZ,
     &                 DROPUU,DROPVV,DROPWW,
     &                 FFLXX,FFLXY,DELH,
     &                 NF)

CD      -- 解析結果を時系列ファイルに出力 --
        CALL VF_OT1TRN(DT,XX,YY,ZZ,UU,VV,WW,PP,FF,GGV,AK,AE,TT,CC,
     &                 BCU,BCV,BCW,NF,INDX,INDY,INDZ)
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
        IF (IE.EQ.1 .OR. IEE.EQ.1) GOTO 1000

CD      -- STOCと情報交換 --
        IF (NB_SC.GT.0) THEN
          CALL VF_STOC_RECV(STBUF,NBUF)
          CALL VF_STOC_SEND(XX,YY,ZZ,UU,VV,WW,FF,NF,STBUF,NBUF,
     &                      GGV,FFLXX,FFLXY)
        ENDIF

CD      -- 解析時刻の更新(1) --
        DT0 = DT
        CALL MPI_ALLREDUCE( DT0,DT,1,MPI_DOUBLE_PRECISION,MPI_MIN,
     $                      comm_mlicdsmg2fc,IERR )
        IF(NB_SC.GT.0) THEN
          CALL VF_BWUWN(XX,YY,ZZ,UU,VV,WW,FF,BCU,BCV,BCW,
     &                 DMTBTT ,DMTBZZ ,DMTBHH ,DMTBUN ,DMTBUT ,
     &                 DMTBTT2,DMTBZZ2,DMTBHH2,DMTBUN2,DMTBUT2,DBUF,
     &                 WK01,WK02,WK03,NF,INDX,INDY,INDB)
          CALL VF_BWUWT(XX,YY,ZZ,UU,VV,WW,ANU,BCU,BCV,BCW,BCVI,INDB)
          CALL VF_BWFF (ZZ,FF,BCF,INDX,INDY,INDB)
        END IF

        DTNOW=DT/DBLE(LOOPS)
        NNOW =NNOW+1
        IF (IPRNT.GT.1) THEN
          T0=TNOW+DT
          CALL VF_IP1INP(T0,GGVOLD,GGVNOW)
        ENDIF
        IF (ISCTYP(1).NE.0) THEN
          T0=TNOW+DT
          CALL VF_CWMSRC(T0,ZZ,FF,SRCUV,
     &                   DMTBTT,DMTBZZ,DMTBHH,DMTBUN,DMTBUT,
     &                   NF)
        ENDIF

CD      -- 流速・圧力計算のサブループ --
        CALL VF_A2CPUT(0,ICPUST,KCP2VL)
        DO 600 ILOOP=1,LOOPS

CD        -- 解析時刻の更新(2) --
          TNOW =TNOW+DTNOW
          IF (LOOPS.NE.1) THEN
            IF (MGRANK.EQ.0) WRITE(*,9560) ILOOP,TNOW
            WRITE(ILPFIL,9560) ILOOP,TNOW
          ENDIF
          IF (IPRNT.GT.1) THEN
            CALL VF_CGGV(1,T0,TNOW,DTNOW,GGV,GGV0,GGVOLD,GGVNOW,NF)
            CALL VF_CGLV(CM0,GGV0,GLV0,NF)
          ENDIF

CD        -- 流速・圧力の計算 --
          CALL VF_V1CAL(ILOOP,XX,YY,ZZ,UU,VV,WW,PP,FF,ANU,CM0,CD0,
     &                  GGV,GGX,GGY,GGZ,GLV,GLX,GLY,GLZ,
     &                  BCU,BCV,BCW,BCP,BCVI,GGV0,GLV0,AK,TT,CC,
     &                  DMTBTT ,DMTBZZ ,DMTBHH ,DMTBUN ,DMTBUT ,
     &                  DMTBTT2,DMTBZZ2,DMTBHH2,DMTBUN2,DMTBUT2,
     &                  DBUF,SRCUV,WK01,WK02,WK03,WK04,WK05,
     &                  WK06,WK07,WK08,WK09,WK10,
     &                  WK11,WK12,WK13,WK14,WK15,WK16,WK17,
     &                  NF,INDX,INDY,INDZ,INDC,INDB,INDS)

 600    CONTINUE
        CALL VF_A2CPUT(0,ICPUEN,KCP2VL)

CD      -- 解析時刻の更新(3) --
        DTNOW=DT

CD      -- 温度の計算 --
        IF (LEQT.NE.0) THEN
          CALL VF_A2CPUT(0,ICPUST,KCP2TT)
          CALL VF_T1CAL(XX,YY,ZZ,UU,VV,WW,GGV,GGX,GGY,GGZ,GGV0,
     &                  TT,ALM,BCT,BCTI,DBUF,WK01,WK02,WK03,WK04,
     &                  NF,INDX,INDY,INDZ,INDB,INDS,INDBT)
          CALL VF_A2CPUT(0,ICPUEN,KCP2TT)
        ENDIF

CD      -- スカラー量の計算 --
        IF (LEQC.GT.0) THEN
          CALL VF_A2CPUT(0,ICPUST,KCP2SS)
          CALL VF_S1CAL(XX,YY,ZZ,UU,VV,WW,GGV,GGX,GGY,GGZ,GGV0,
     &                  CC,DD,BCC,BCCI,DBUF,WK01,WK02,WK03,WK04,
     &                  NF,INDX,INDY,INDZ,INDB,INDS,INDBC)
          CALL VF_A2CPUT(0,ICPUEN,KCP2SS)
        ENDIF

CD      -- k-ε2方程式モデルの計算 --
        IF (LEQK.NE.0) THEN
          CALL VF_A2CPUT(0,ICPUST,KCP2KE)
          CALL VF_K1CAL(XX,YY,ZZ,UU,VV,WW,GGV,GGX,GGY,GGZ,GGV0,
     &                  BCU,BCV,BCW,BCVI,ANUT,AK,AE,BCK,BCE,DBUF,
     &                  WK01,WK02,WK03,WK04,WK05,WK06,WK07,WK08,WK09,
     &                  NF,INDX,INDY,INDZ,INDB,INDS,INDBK,INDBE)
          CALL VF_A2CPUT(0,ICPUEN,KCP2KE)
        ENDIF

CD      -- VOF関数Fの計算およびNFの設定 --
        CALL VF_A2CPUT(0,ICPUST,KCP2FF)
        CALL VF_F1CAL(XX,YY,ZZ,UU,VV,WW,PP,FF,ANU,GGV,GGX,GGY,GGZ,
     &                BCU,BCV,BCW,BCP,BCF,BCVI,TBUB,
     &                DROPTX,DROPTY,DROPTZ,DROPUU,DROPVV,DROPWW,
     &                GGV0,DMTBTT,DMTBHH,DMTBTT2,DMTBHH2,
     &                DBUF,SRCUV,PPPVC,
     &                WK01,WK02,WK03,WK04,WK05,
     &                WK06,WK07,WK08,WK09,WK10,WK11,
     &                NF,INDX,INDY,INDZ,INDC,INDB,INDS,IPVC,IBUF,NWK1,
C----------------------------------------------------------2012.03 start
C    &                FFLXX,FFLXY)
     &                FFLXX,FFLXY,DELH,DELH_IN)
C----------------------------------------------------------2012.03 end
        CALL VF_A2CPUT(0,ICPUEN,KCP2FF)

CD      -- NFの変更に伴う、スカラー量の境界値等の再設定 --
        IF (LEQK.NE.0) THEN
          CALL VF_BWKELG(XX,YY,ZZ,UU,VV,WW,BCVI,AK,AE,DBUF,
     &                   NF,INDX,INDY,INDZ,INDB)
          CALL VF_BSSS (AK,DBUF,NF)
          CALL VF_BSSS (AE,DBUF,NF)
          CALL VF_CNUT0(AK,AE,ANUT,NF)
          CALL VF_CNU00(ANUT,ANU,NF)
          CALL VF_BWKE (AK,AE,BCK,BCE,INDB,INDBK,INDBE)
          CALL VF_BWUWT(XX,YY,ZZ,UU,VV,WW,ANU,BCU,BCV,BCW,BCVI,INDB)
        ENDIF
        IF (LEQT.NE.0) THEN
          CALL VF_BSSS(TT,DBUF,NF)
          IF (LEQK.NE.0) CALL VF_CLM00(ANUT,ALM,NF)
          CALL VF_BWSS(XX,YY,ZZ,GGX,GGY,GGZ,TT,ALM,BCT,BCTI,
     &                 NF,INDB,INDBT)
        ENDIF
        DO 700 LC=1,LEQC
          CALL VF_BSSS(CC(1,1,1,LC),DBUF,NF)
 700    CONTINUE
        IF (LEQK.NE.0) CALL VF_CDD00(ANUT,DD,NF)
        DO 710 LC=1,LEQC
          CALL VF_BWSS(XX,YY,ZZ,GGX,GGY,GGZ,CC(1,1,1,LC),DD(1,1,1,LC),
     &                 BCC(1,LC),BCCI(1,1,LC),NF,INDB,INDBC(1,LC))
 710    CONTINUE

CD    ** 反復終了 **
        GOTO 500
 1000 CONTINUE

CD    -- 自動リスタート用のデータを出力 --
      IF ( IE.EQ.1.AND.ETIME.LT.1.0D30 ) IEE=1
      IF ( IEE.EQ.1 ) THEN
        WRITE(ILPFIL,*) 'ETIME=',WTM2-WTM1
        IRSTYP=1
        IRSTRN(1)=NNOW
        IRSTRN(2)=NNOW
        IRSTRN(3)=1
        IF (MYRANK.EQ.0) THEN
          OPEN(MFILAR,
     &         FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.ars',
     &         STATUS='UNKNOWN',FORM='FORMATTED'  )
          IF (IE.EQ.1)THEN
            WRITE(MFILAR,'(I10,1X,F10.2)') -999,TNOW
            CLOSE(MFILAR)
            OPEN(IFLAR,FILE='stop_ars',STATUS='UNKNOWN')
            CLOSE(IFLAR)
          ELSE
            WRITE(MFILAR,'(I10,1X,F10.2)') NNOW,TNOW
            CLOSE(MFILAR)
          ENDIF
        ENDIF
        CALL VF_OR1TRN(DT,UU,VV,WW,PP,FF,GGV,BCU,BCV,BCW,BCP,BCF,
     &                 AK,AE,BCK,BCE,TT,BCT,CC,BCC,
     &                 TBUB,DROPTX,DROPTY,DROPTZ,
     &                 DROPUU,DROPVV,DROPWW,
     &                 FFLXX,FFLXY,DELH,
     &                 NF)
      ENDIF

CD    -- MA側にCADMASの送信終了を伝える（MPIの場合） --
      CALL VF_OM1TRN(DT,ZZ,UU,VV,FF,GGV,WK01,NF,-1)

CD    -- タイマーの終了とCPU時間の出力 --
      CALL VF_A2CPUT(0     ,ICPUEN,KCP1CL)
      CALL VF_A2CPUT(0     ,ICPUEN,KCP0AL)
      CALL VF_A2CPUT(ILPFIL,ICPUOU,0     )

CD    -- 終了コメントをリストファイルに出力 --
      IF (MGRANK.EQ.0) WRITE(*,9990)
      WRITE(ILPFIL,9990)

CD    -- ファイルのクローズ --
      CALL VF_A2CLOS()

CD    -- 並列環境の終了 --
      CALL VF_P0END()

C     -- 実行文の終了 --
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      CALL VF_A2ERR('VF_A1MAIN','CAN NOT OPEN (*****.list).')

      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(/' ',
     &       '##### CADMAS-SURF/3D-MG Ver.',I1,'.',I1,' START. ######')
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
 9555 FORMAT( ' ',11X,
     &            ' : !B! = ',1PE12.5,
     &            ' : !R! = ',1PE12.5,
     &            ' : ITR = ',I6     ,
     &            ' : NPVC= ',I6      )
 9560 FORMAT( ' ','  SUBLOOP=',I6,
     &            ' : TIME= ',1PE12.5 )
 9570 FORMAT(/' ','##### MGRANK=',I5,' /',I5)
 9580 FORMAT(/' ','##### MGAREA=',I5,' /',I5)
 9590 FORMAT(/' ','##### MYRANK=',I5,' /',I5)
 9990 FORMAT(/' ','##### NORMAL END. ###########################'/)

C==== 終了 ===========================================================

 9999 CONTINUE
      STOP
      END
