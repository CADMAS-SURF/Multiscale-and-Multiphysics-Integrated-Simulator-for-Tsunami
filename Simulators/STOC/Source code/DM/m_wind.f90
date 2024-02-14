MODULE M_WIND
  USE M_COM_STOC,ONLY:MAXPE
  IMPLICIT NONE
!----------------------------------------
!     風応力用データ
!
!     WIN_CAL   風応力を考慮するか(t:考慮する、f:考慮しない、デフォルトはf)
!     WIN_FIX   風速を一定値とするか(t:一定値にする、f:ファイルから読み込む、デフォルトはf)
!     RHOA      空気の密度(kg/m3)
!     WIND_U    風速のx成分の一定値(ファイル読み込み時は無視される)
!     WIND_V    風速のy成分の一定値(ファイル読み込み時は無視される)
!     WIND_FILE 風速データファイル名(ファイル名を指定しないときは一定値を用いる)
!
!----------------------------------------
!
      LOGICAL::WIN_CAL=.false.
      LOGICAL::WIN_FIX(MAXPE)=.false.
      REAL(8)::RHOA=1.2D0
      REAL(8)::WIND_U=0.0D0,WIND_V=0.0D0
      CHARACTER(80):: WIND_FILE(MAXPE)=' '
END MODULE M_WIND
