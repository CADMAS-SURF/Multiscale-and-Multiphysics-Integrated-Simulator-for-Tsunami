MODULE M_DRIFT
  IMPLICIT NONE
!----------------------------------------
!     漂流物データ
!
!     ND        漂流物の数
!
!     AM(ND)    漂流物の質量(kg)
!     AI(ND)    漂流物の慣性モーメント(kgm^2)
!     AL(ND)    漂流物の長さ(m)
!     AB(ND)    漂流物の幅(m)
!     AD(ND)    漂流物の喫水(m)
!     BL(ND)    接触判定用の漂流物の長さ(m)
!     BB(ND)    接触判定用の漂流物の幅(m)
!     BD(ND)    接触判定用の漂流物の喫水(m)
!     AZ1(ND)   表示用の漂流物の高さ1(m)
!     AZ2(ND)   表示用の漂流物の高さ2(m)
!
!     HMIN_INI(ND) 漂流物が動き出す際の流体計算基準面からの喫水面の下限値(m)
!     HMAX_INI(ND) 漂流物が動き出す際の流体計算基準面からの喫水面の上限値(m)
!     FMAX_INI(ND) 漂流物が動き出す際の加重(N)
!     TDST_INI(ND) 漂流物が動き出す時刻(s)
!
!     AM0(ND)   初期の漂流物の質量(kg)
!     AI0(ND)   初期の漂流物の慣性モーメント(kgm^2)
!     AD0(ND)   初期の漂流物の喫水(m)
!     BD0(ND)   初期の接触判定用の漂流物の喫水(m)
!     ALPD(ND)   漂流物内の浸水可能体積の割合(-)
!     CKD(ND)    沈降時の重力加速度の補正係数(-) ※H21沈降モデルでのみ使用
!     WSD(ND)    沈降時の沈降速度：下向き正(m/s)
!     IFLAG_S(ND)   漂流物の非破壊時の沈降開始フラグ =0:沈降前 =1:沈降中 =-1:沈降計算しない
!     IFLAG_D(ND)   漂流物の破壊時の沈降開始フラグ =0:沈降前 =1:沈降中 =-1:沈降計算しない
!
!     TIME_S(ND)    非破壊時の漂流物の沈降の時定数(s) ※H21沈降モデルでのみ使用
!     TIME_D(ND)    破壊時の漂流物の沈降の時定数(s)   ※H21沈降モデルでのみ使用
!     DST_LIMIT(ND) 破壊基準値(J)
!
!     QD(ND)        漂流物への浸水速度(m3/s)     ※H22沈降モデルでのみ使用
!     AHIN(ND)      漂流物の内部水位             ※H22沈降モデルでのみ使用
!     ABHOLE(ND)    漂流物の底面の隙間の面積(m2) ※H22沈降モデルでのみ使用
!     ATHOLE(ND)    漂流物の上面の隙間の面積(m2) ※H22沈降モデルでのみ使用
!     ABHOLED(ND)   破壊時の底面の隙間の面積(m2) ※H22沈降モデルでのみ使用
!     ATHOLED(ND)   破壊時の上面の隙間の面積(m2) ※H22沈降モデルでのみ使用
!     CQD(ND)       流量係数(-)                  ※H22沈降モデルでのみ使用
!     CDD(ND)       沈降時の抵抗係数(-)          ※H22沈降モデルでのみ使用
!
!     XD(ND)    漂流物の重心のX座標(m)
!     YD(ND)    漂流物の重心のY座標(m)
!     TD(ND)    漂流物の回転角(rad)
!     UD(ND)    漂流物の重心の移動速度のX成分(m/s)
!     VD(ND)    漂流物の重心の移動速度のY成分(m/s)
!     OD(ND)    漂流物の角速度(1/s)
!
!     XD1...OD1 時間積分により更新した後の漂流物の状態
!
!     XVERT(4,ND)     漂流物の頂点のX座標(m)
!     YVERT(4,ND)     漂流物の頂点のY座標(m)
!     XCOLLIS(2,ND)   衝突判定時の漂流物の線分の両端のX座標(m)
!     YCOLLIS(2,ND)   衝突判定時の漂流物の線分の両端のY座標(m)
!
!     HFD(ND)   (XD,YD)における水位(m)
!     HTD(ND)   (XD,YD)における標高値(m)
!     HZD(ND)   (XD,YD)における漂流物底面のZ座標値(m)：衝突判定用
!     HAD(ND)   (XD,YD)における漂流物底面のZ座標値(m)：抗力計算用
!     HAZ(ND)   (XD,YD)における漂流物上面もしくは水面のうちの低い方のZ座標値(m)
!     HZDO(ND)  前のステップにおける(XD,YD)における漂流物底面のZ座標値(m)：衝突判定用
!     HAZO(ND)  前のステップにおける(XD,YD)における漂流物上面もしくは水面のうちの低い方のZ座標値(m)
!
!     LD(ND)    漂流物の状態フラグ
!               = 0: 初期の係留状態
!               = 1: 通常状態
!               =-1: 底部が地面に乗り上げて停止している状態
!     NDDIV(2,ND)  漂流物の抗力計算時の分割数
!               (1,*): 長さ方向の計算点数(分割数+1)
!               (2,*): 幅方向の計算点数(分割数+1)
!
!     KFD(ND)   HFDの高さが含まれる流体側のセルインデックスK
!     KAD(ND)   HADの高さが含まれる流体側のセルインデックスK
!     KZD(ND)   HZDの高さが含まれる流体側のセルインデックスK
!     KAZ(ND)   HAZの高さが含まれる流体側のセルインデックスK
!
!
!
!     INAR(ND)  漂流物を含む領域番号
!
!     MXNSTP    (閉塞判定のステップ数)*2
!     LBLC(ND)  閉塞フラグ
!               = 0: 漂流（閉塞なし）
!               = 1: 閉塞（乗り上げ）
!               = 2: 閉塞（建物衝突）
!     LBL1(ND,MXNSTP)  衝突点の位置①
!                      =-1: 後部
!                      = 0: 中央部
!                      = 1: 前部
!     LBL2(ND,MXNSTP)  衝突点の位置②
!                      =-1: 左側
!                      = 0: 中央
!                      = 1: 右側
!
!----------------------------------------
!
      INTEGER,PARAMETER::MXSIDE=2000
      INTEGER::ND
!
      REAL(8),ALLOCATABLE::AM(:),AI(:)
      REAL(8),ALLOCATABLE::AL(:),AB(:),AD(:)
      REAL(8),ALLOCATABLE::BL(:),BB(:),BD(:)
      REAL(8),ALLOCATABLE::AZ1(:),AZ2(:)
!
      REAL(8),ALLOCATABLE::HMIN_INI(:),HMAX_INI(:),FMAX_INI(:),TDST_INI(:)
!
      REAL(8),ALLOCATABLE::AM0(:),AI0(:),AD0(:),BD0(:)
      REAL(8),ALLOCATABLE::ALPD(:),CKD(:),WSD(:)
      INTEGER,ALLOCATABLE::IFLAG_S(:),IFLAG_D(:)
      REAL(8),ALLOCATABLE::TIME_S(:),TIME_D(:),DST_LIMIT(:)
!
      REAL(8),ALLOCATABLE::QD(:),AHIN(:)
      REAL(8),ALLOCATABLE::ABHOLE(:),ATHOLE(:),ABHOLED(:),ATHOLED(:)
      REAL(8),ALLOCATABLE::CQD(:),CDD(:)
!
      REAL(8),ALLOCATABLE::XD(:),YD(:),TD(:)
      REAL(8),ALLOCATABLE::UD(:),VD(:),OD(:)
      REAL(8),ALLOCATABLE::XD1(:),YD1(:),TD1(:)
      REAL(8),ALLOCATABLE::UD1(:),VD1(:),OD1(:)
!
      REAL(8),ALLOCATABLE::XVERT(:,:),YVERT(:,:)
      REAL(8),ALLOCATABLE::XCOLLIS(:,:),YCOLLIS(:,:)
      REAL(8),ALLOCATABLE::HFD(:),HTD(:),HZD(:),HAD(:),HAZ(:)
      REAL(8),ALLOCATABLE::HZDO(:),HAZO(:)
!
      REAL(8),ALLOCATABLE::XD_INIT(:),YD_INIT(:),ZD_INIT(:)
!
      INTEGER,ALLOCATABLE::LD(:),NDDIV(:,:)
      INTEGER,ALLOCATABLE::KFD(:),KAD(:),KZD(:),KAZ(:)
      CHARACTER(4),ALLOCATABLE::TYPED(:)
!
      INTEGER,ALLOCATABLE::INAR(:)
!
!<<<<< (START) STOC-BLC VERSION  <<<<<<<
      INTEGER,PARAMETER::MXNSTP=20 ! (閉塞判定のステップ数)*2
      INTEGER,ALLOCATABLE::LBLC(:),LBL1(:,:),LBL2(:,:)
!
CONTAINS
!
SUBROUTINE ALLOCATE_DRIFT(ND,IERROR)
  INTEGER,INTENT(IN) :: ND
  INTEGER,INTENT(OUT) :: IERROR
!
  IERROR=0
!
  ALLOCATE(AM(ND),AI(ND) &
       &  ,AL(ND),AB(ND),AD(ND) &
       &  ,BL(ND),BB(ND),BD(ND) &
       &  ,AZ1(ND),AZ2(ND) &
       &  ,HMIN_INI(ND),HMAX_INI(ND),FMAX_INI(ND),TDST_INI(ND) &
       &  ,AM0(ND),AI0(ND),AD0(ND),BD0(ND) &
       &  ,ALPD(ND),CKD(ND),WSD(ND) &
       &  ,TIME_S(ND),TIME_D(ND),DST_LIMIT(ND) &
       &  ,QD(ND),AHIN(ND) &
       &  ,ABHOLE(ND),ATHOLE(ND),ABHOLED(ND),ATHOLED(ND) &
       &  ,CQD(ND),CDD(ND) &
       &  ,XD(ND),YD(ND),TD(ND) &
       &  ,UD(ND),VD(ND),OD(ND) &
       &  ,XD1(ND),YD1(ND),TD1(ND) &
       &  ,UD1(ND),VD1(ND),OD1(ND) &
       &  ,XVERT(4,ND),YVERT(4,ND) &
       &  ,XCOLLIS(2,ND),YCOLLIS(2,ND) &
       &  ,HFD(ND),HTD(ND),HZD(ND),HAD(ND),HAZ(ND) &
       &  ,HZDO(ND),HAZO(ND) &
       &  ,XD_INIT(ND),YD_INIT(ND),ZD_INIT(ND) &
       &  ,STAT=IERROR)
  IF(IERROR/=0) GOTO 900
!
  ALLOCATE(IFLAG_S(ND),IFLAG_D(ND) &
       &  ,LD(ND),NDDIV(2,ND) &
       &  ,KFD(ND),KAD(ND),KZD(ND),KAZ(ND) &
       &  ,INAR(ND) &
       &  ,LBLC(ND),LBL1(ND,MXNSTP),LBL2(ND,MXNSTP) &
       &  ,STAT=IERROR)
  IF(IERROR/=0) GOTO 901
!
  ALLOCATE(TYPED(ND),STAT=IERROR)
  IF(IERROR/=0) GOTO 902
!
  RETURN
900 CONTINUE
  WRITE(*,*) '### ERROR : ARRAY ALLOCATE ERROR : DRIFT : REAL'
  IERROR=1
  RETURN
901 CONTINUE
  WRITE(*,*) '### ERROR : ARRAY ALLOCATE ERROR : DRIFT : INTEGER'
  IERROR=1
  RETURN
902 CONTINUE
  WRITE(*,*) '### ERROR : ARRAY ALLOCATE ERROR : DRIFT : CHAR'
  IERROR=1
  RETURN
END SUBROUTINE ALLOCATE_DRIFT
!
END MODULE M_DRIFT
