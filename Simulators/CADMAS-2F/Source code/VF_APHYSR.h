      COMMON /VF_APHYSR/ UINI(2),VINI(2),WINI(2),
     &                   RHO0(2),ANU0(2),GRZ0,WVLVL,
     &                   AKMINK,AKMINE,AKINIK(2),AKINIE(2),
     &                   AKCMU,AKSGK,AKSGE,
     &                   AKC1,AKC2,AKC3,AKK0,AKA0,AKPR,AKSM(MAXNC),
     &                   TINI,TCP0,TCN0,TDT0,TDR0,
     &                   CINI(MAXNC),CDF0(MAXNC),CDC0(MAXNC),
     &                   CDR0(MAXNC),
     &                   BCTYP(11,4),DAMP(4,4),WBUB,
     &                   PVCP0,PVCGM,PVCDIV(MAXPVC),PVCPES(MAXPVC),
     &                   PVCPFS(MAXPVC),PVCVES(MAXPVC),PVCVFS(MAXPVC),
     $                   VVMAX

CD=== 概要 ===========================================================

CDT   VF_APHYSR.h:物理事象関連(物理量および物理モデル等):実数

C==== 内容 ===========================================================

CD    UINI(1) : CNS : R*8 : x方向流速の初期値（液相）
CD    VINI(1) : CNS : R*8 : y方向流速の初期値（液相）
CD    WINI(1) : CNS : R*8 : z方向流速の初期値（液相）
CD    UINI(2) : CNS : R*8 : x方向流速の初期値（気相）
CD    VINI(2) : CNS : R*8 : y方向流速の初期値（気相）
CD    WINI(2) : CNS : R*8 : z方向流速の初期値（気相）
CD    RHO0(1) : CNS : R*8 : 密度（液相）
CD    RHO0(2) : CNS : R*8 : 密度（気相）
CD    ANU0(1) : CNS : R*8 : 分子動粘性係数（液相）
CD    ANU0(2) : CNS : R*8 : 分子動粘性係数（気相）
CD    GRZ0   : CNS : R*8 : 重力加速度のz成分(負方向)
CD    WVLVL  : CNS : R*8 : 初期水面の高さ(z座標)
CD    AKMINK(1) : CNS : R*8 : 乱流エネルギの最小値（液相）
CD    AKMINE(1) : CNS : R*8 : 乱流エネルギ散逸の最小値（液相）
CD    AKINIK(1) : CNS : R*8 : 乱流エネルギの初期値（液相）
CD    AKINIE(1) : CNS : R*8 : 乱流エネルギ散逸の初期値（液相）
CD    AKMINK(2) : CNS : R*8 : 乱流エネルギの最小値（気相）
CD    AKMINE(2) : CNS : R*8 : 乱流エネルギ散逸の最小値（気相）
CD    AKINIK(2) : CNS : R*8 : 乱流エネルギの初期値（気相）
CD    AKINIE(2) : CNS : R*8 : 乱流エネルギ散逸の初期値（気相）
CD    AKCMU  : CNS : R*8 : Cμ
CD    AKSGK  : CNS : R*8 : σk
CD    AKSGE  : CNS : R*8 : σe
CD    AKC1   : CNS : R*8 : C1
CD    AKC2   : CNS : R*8 : C2
CD    AKC3   : CNS : R*8 : C3
CD    AKK0   : CNS : R*8 : 対数則のκ
CD    AKA0   : CNS : R*8 : 対数則のA
CD    AKPR   : CNS : R*8 : 乱流Prandtl数
CD    AKSM(MAXNC) : CNS : R*8 : 乱流Schmidt数
CD    TINI   : CNS : R*8 : 温度の初期値
CD    TCP0   : CNS : R*8 : 定圧比熱
CD    TCN0   : CNS : R*8 : 熱伝導率
CD    TDT0   : CNS : R*8 : 浮力計算のための基準温度
CD    TDR0   : CNS : R*8 : 密度の温度微分
CD    CINI(MAXNC) : CNS : R*8 : 濃度の初期値
CD    CDF0(MAXNC) : CNS : R*8 : 拡散係数
CD    CDC0(MAXNC) : CNS : R*8 : 浮力計算のための基準濃度
CD    CDR0(MAXNC) : CNS : R*8 : 密度の濃度微分
CD    BCTYP(11,4) : TRN : R*8 : 特殊境界に関する情報
CD                             (*,1):x座標最小位置の境界
CD                             (*,2):x座標最大位置の境界
CD                             (*,3):y座標最小位置の境界
CD                             (*,4):y座標最大位置の境界
CD                             造波境界の場合
CD                             (1,*):水深
CD                             (2,*):波高
CD                             (3,*):周期
CD                             (4,*):波長
CD                             (5,*):Ursell数
CD                             (6,*):水位変動ゼロ時の無次元位相
CD                             (7,*):造波したい現在の波高さ
CD                             (8,*):何周期かけて増幅するか
CD                             (9,*):入射角度(法線方向ゼロ)
CD                             (10,*):基準点のx座標値
CD                             (11,*):基準点のy座標値
CD                             開境界の場合
CD                             (1,*):水深
CD                             (2,*):ダミー
CD                             (3,*):周期
CD                             (4,*):波長
CD                             (5,*):ダミー
CD                             (6,*):波速
CD                             (7,*):ダミー
CD                             (8,*):ダミー
CD                             (9,*):ダミー
CD                             (10,*):ダミー
CD                             (11,*):ダミー
CD    DAMP(4,4)  : CNS : R*8 : 減衰領域に関する情報
CD                             (*,1):x座標最小位置近傍
CD                             (*,2):x座標最大位置近傍
CD                             (*,3):y座標最小位置近傍
CD                             (*,4):y座標最大位置近傍
CD                             (1,*):減衰関数の水平方向パラメータ
CD                             (2,*):減衰関数の鉛直方向パラメータ
CD                             (3,*):減衰領域の幅
CD                             (4,*):減衰領域の水深
CD    WBUB   : CNS : R*8 : 気泡の上昇速度(TimerDoor法)
CD                         < ZERO:処理を行わない
CD                         >=ZERO:処理を行う
CD    VVMAX         : CNS : R*8 : 流速の制限値(m/s)
