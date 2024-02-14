MODULE M_OUTPUT
  IMPLICIT NONE
!----------------------------------------
!     ファイル出力制御データ
!
!     IFL ! ログ出力ファイル(=16)
!     DRIFT_START      ! 漂流物ファイルの出力開始時刻(s) 
!     DRIFT_INTERVAL   ! 漂流物ファイルの出力間隔(s) 
!     RESTART_START    ! リスタートファイルの出力開始時刻(s) 
!     RESTART_INTERVAL ! リスタートファイルの出力間隔(s) 
!     BLOCK_START      ! 閉塞状況ファイルの出力開始時刻(s) 
!     BLOCK_INTERVAL   ! 閉塞状況ファイルの出力間隔(s) 
!     STL_START        ! STLファイルの出力開始時刻(s) 
!     STL_INTERVAL     ! STLファイルの出力間隔(s) 
!----------------------------------------
!
  INTEGER,PARAMETER::IFL=16
  REAL(8)::DRIFT_START=0.0D0,  DRIFT_INTERVAL  =1.0D0
  REAL(8)::RESTART_START=0.0D0,RESTART_INTERVAL=1000.0D0
  REAL(8)::BLOCK_START=0.0D0,  BLOCK_INTERVAL  =1.0D0
  REAL(8)::STL_START=1.0D+10,  STL_INTERVAL    =1.0D+10
END MODULE M_OUTPUT
