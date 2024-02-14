MODULE SF_ARRAY

      USE SF_TYPE

!     IELM(24,NELM)     : I*4 : 構造物要素データ
!                               (1,I):要素番号
!                               (2,I):要素タイプ
!                                     =0:一般構造物
!                                     =1:地盤
!                                     =2:石
!                               (3,I):構成節点数
!                               (4:23,I):構成節点番号
!     POR(NELM)         : R*8 : 空隙率(石材料)
!     IENO(NELM)        : I*4 : 要素番号対応表(各プロセス -> 全体)
!     GRID(3,NNOD)      : R*8 : 構造物節点座標
!     GRDL(NNOD)        : R*8 : 構造物節点に接続する要素の最小サイズ
!     IGFC(NNOD)        : I*4 : 表面節点フラグ
!                               =0:内部節点
!                               =1:表面節点
!     INDG(NNOD)        : I*4 : 構造物節点番号
!     IGNO(NNOD)        : I*4 : 節点番号対応表(各プロセス -> 全体)
!     IPFACE(12,NPFC)   : I*4 : 構造物表面データ(プロセス毎)
!                               (1,I):表面の重心が属するセル番号
!                               (2,I):表面が属する要素タイプ
!                                     =0:一般構造物
!                                     =1:地盤
!                                     =2:石
!                               (3,I):表面が属する要素番号
!                               (4,I):構成節点数
!                               (5:12,I):構成節点番号
!     IPFACE0(12,NPFC0) : I*4 : 構造物表面データ(全プロセス)
!     IPFNO(NPFC)       : I*4 : 表面番号対応表(各プロセス -> 全体)
!     AFC(NPFC0)        : R*8 : 構造物表面の圧力がかかる面積率
!     IPGRID(2,NNOD)    : I*4 : 構造物表面節点データ(プロセス毎)
!                               (1,I):節点が属するセル番号
!                               (2,I):節点が属する要素タイプ
!                                     =0:地盤以外
!                                     =1:地盤
!     IPGRID0(2,NNOD0)  : I*4 : 構造物表面節点データ(全プロセス)
!     IPND0(NNOD0)      : I*4 : 構造物表面節点圧力フラッグ
!                               =0:圧力値無し
!                               =1:圧力値有り
!     PRES0(NNOD0)      : R*8 : 構造物表面節点圧力(全プロセス)
!     IRFACE(NPFC0)     : I*4 : 地盤 or 石に貼り付く面のタイプ
!                               =1:固定
!                               =2:可動
!                               =0 :それ以外
!     IRGRID(NNOD)      : I*4 : 地盤 or 石の構成節点のタイプ(プロセス毎)
!                               >0 :節点が属するセル番号
!                               =-1:内部点
!                               =0 :それ以外
!     IRGRID0(NNOD0)    : I*4 : 地盤 or 石の構成節点のタイプ(全プロセス)
!                               =1 :可動点
!                               =-1:内部点
!                               =0 :それ以外
!     DELZ0(NNOD0)      : R*8 : 地盤 or 石の構成節点のうち可動点の高さ変化
!     IFIX0(NNOD0)      : I*4 : 地盤 or 石の構成節点の高さ変化のフラグ
!                               >0 :高さ変化なしとする
!                               =0 :それ以外
!     ICRG(NICRG)       : I*4 : 各接触ボディの接触面エンドアドレス
!     ICTR(4,NCTR)      : I*4 : 接触面データ
!                               (1,I):接触面が属するIPFACE番号
!                               (2:4,I):構成節点番号
!     ICTB(NICRG,NICRG) : I*4 : 接触ボディの組み合わせ
!     POS(3,NNOD)       : R*8 : 構造物位置座標(プロセス毎)
!     POS1(3,NNOD)      : R*8 : 構造物位置座標(時刻:TSTR1)(プロセス毎)
!     POS2(3,NNOD)      : R*8 : 構造物位置座標(時刻:TSTR2)(プロセス毎)
!     POS0(3,NNOD0)     : R*8 : 構造物位置座標(全プロセス)
!     POS10(3,NNOD0)    : R*8 : 構造物位置座標(時刻:TSTR1)(全プロセス)
!     POS20(3,NNOD0)    : R*8 : 構造物位置座標(時刻:TSTR2)(全プロセス)
!     DVEL(3,NELM)      : R*8 : 地盤のダルシー流速
!     DVEL1(3,NELM)     : R*8 : 地盤のダルシー流速(時刻:TSTR1)(プロセス毎)
!     DVEL2(3,NELM)     : R*8 : 地盤のダルシー流速(時刻:TSTR2)(プロセス毎)
!     DVEL10(3,NELM0)   : R*8 : 地盤のダルシー流速(時刻:TSTR1)(全プロセス)
!     DVEL20(3,NELM0)   : R*8 : 地盤のダルシー流速(時刻:TSTR2)(全プロセス)
!     SPC(NELM)         : SPACE : 構造物要素がCADMASセルと干渉する体積,面積

      INTEGER         , DIMENSION(:,:  ), ALLOCATABLE :: IELM
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: POR
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: IENO
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: GRID
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: GRDL
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: IGFC
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: INDG
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: IGNO
      INTEGER         , DIMENSION(:,:  ), ALLOCATABLE :: IPFACE
      INTEGER         , DIMENSION(:,:  ), ALLOCATABLE :: IPFACE0
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: IPFNO
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: AFC
      INTEGER         , DIMENSION(:,:  ), ALLOCATABLE :: IPGRID,IPGRID0
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: IPND0
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: PRES0
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: IRFACE
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: IRGRID,IRGRID0
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: DELZ0
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: IFIX0
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: ICRG
      INTEGER         , DIMENSION(:,:  ), ALLOCATABLE :: ICTR,ICTB
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: POS,POS1,POS2,POS0,POS10,POS20
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: DVEL,DVEL1,DVEL2,DVEL10,DVEL20
      TYPE(SPACE),      DIMENSION(:    ), ALLOCATABLE :: SPC

END MODULE SF_ARRAY