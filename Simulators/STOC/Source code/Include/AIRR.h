C----------------------------------------------------------------------
C     風場計算用コモン(実数型)
C
C     ZGRIDA : 風場の鉛直方向の格子点座標値(入力用)
C     UBCAIRWES : 風速のx方向成分の西側境界値
C     UBCAIREAS : 風速のx方向成分の東側境界値
C     UBCAIRSOU : 風速のx方向成分の南側境界値
C     UBCAIRNOR : 風速のx方向成分の北側境界値
C     VBCAIRWES : 風速のy方向成分の西側境界値
C     VBCAIREAS : 風速のy方向成分の東側境界値
C     VBCAIRSOU : 風速のy方向成分の南側境界値
C     VBCAIRNOR : 風速のy方向成分の北側境界値
C     PBCAIR ： 上空の気圧
C     UINITAIR : 風速のx方向成分の初期値
C     VINITAIR : 風速のy方向成分の初期値
C     RHOAIR : 大気の密度
C     AMUAIR : 大気の粘性係数
C     PARAMAIR : 風場の差分スキームのパラメータ(=0.0:中心差分,=1.0:風上差分)
C     VVMAXAIR : 風速の上限値
C     VELMAXAIR: 時刻毎の最大風速
C     GZHAIR  ： 風場計算のリミッタ。水面(地面)に接する計算セルの水面より
C                上の部分がこの値より厚い場合に計算する
C     TIMEABC1 : 風速の側面境界値データファイルの読み込み時刻1
C     TIMEABC2 : 風速の側面境界値データファイルの読み込み時刻2
C----------------------------------------------------------------------
C
      INTEGER,PARAMETER:: MAXMZA=200
      REAL(8):: ZGRIDA(MAXMZA)
      REAL(8):: UBCAIRWES,VBCAIRWES,UBCAIREAS,VBCAIREAS,PBCAIR
      REAL(8):: UBCAIRSOU,VBCAIRSOU,UBCAIRNOR,VBCAIRNOR
      REAL(8):: AKBCAIRWES,EPBCAIRWES,AKBCAIREAS,EPBCAIREAS
      REAL(8):: AKBCAIRSOU,EPBCAIRSOU,AKBCAIRNOR,EPBCAIRNOR
      REAL(8):: UINITAIR,VINITAIR,AKINITAIR,EPINITAIR
      REAL(8):: RHOAIR,AMUAIR
      REAL(8):: PARAMAIR,PARAMAIR2,VVMAXAIR,GZHAIR,VELMAXAIR
      REAL(8):: TIMEABC1,TIMEABC2
      COMMON /AIRR/ ZGRIDA,UBCAIRWES,VBCAIRWES,UBCAIREAS,VBCAIREAS,
     $              UBCAIRSOU,VBCAIRSOU,UBCAIRNOR,VBCAIRNOR,PBCAIR,
     $              AKBCAIRWES,EPBCAIRWES,AKBCAIREAS,EPBCAIREAS,
     $              AKBCAIRSOU,EPBCAIRSOU,AKBCAIRNOR,EPBCAIRNOR,
     $              UINITAIR,VINITAIR,AKINITAIR,EPINITAIR,
     $              RHOAIR,AMUAIR,PARAMAIR,PARAMAIR2,VVMAXAIR,GZHAIR,
     $              VELMAXAIR,TIMEABC1,TIMEABC2
