      INTEGER,PARAMETER :: LOILF=56

      INTEGER :: ICAL_OIL   ! 0:油の移動計算をしない
                            ! 1:油の移動計算をSTOCとONLINE接続で行う
                            ! 1:油の移動計算をSTOCとOFFLINE接続で行う
      INTEGER :: NP_OIL     ! 油の移動計算を行うPE番号
      INTEGER :: OIL_WIN    ! 風データを設定しているか否か(=0:設定していない,=1:設定している)
      COMMON /COILI/ ICAL_OIL,NP_OIL,OIL_WIN

      REAL(8) :: ROILF(3)   ! OFFLINE用ファイルの出力間隔
      COMMON /COILR/ ROILF
