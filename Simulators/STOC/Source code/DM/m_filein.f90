MODULE M_FILEIN
  USE M_COM_STOC,ONLY:MAXPE
  IMPLICIT NONE
!----------------------------------------
!     流体データのファイル読込みに関するデータ
!
!     UUAR0,VVAR0,HHAR0   最後の一回前に読み込んだ流速・水位データ
!     UUAR1,VVAR1,HHAR1   最後に読み込んだ流速・水位データ
!                         (ただし，配列は(NA,NNN)の２次元)
!                         NNN = NI(NA)*NJ(NA) * (K-1) + (NI(NA) * (J-1) + I)
!                         NNN = NI(NA) * (J-1) + I
!                         AR : ARRAY
!
!     TIMFL0   最後の一回前に読み込んだ流動データの時刻(s)
!     TIMFL1   最後に読み込んだ流動データの時刻(s)
!     DTFL     流体計算結果の通信の時間間隔(s)
!     IEOF     ファイルのEOFを読み込んだ場合に1にする
!
!     TIMWIN0  最後の一回前に読み込んだ風速データの時刻(s)
!     TIMWIN1  最後に読み込んだ風速データの時刻(s)
!     IEOFWIN  風速ファイルのEOFを読み込んだ場合に1にする
!
!     IFLOFF(0:MAXPE) オフライン時にSTOCのデータを読み込むときの装置番号(100+N)
!     IFLWIN(MAXPE) 風速データを読み込むときの装置番号(200+N)
!
!     ONLINE   オンラインorオフラインの判定用変数(オン時.true.)
!     OFFLINE  オンラインorオフラインの判定用変数(オフ時.true.)
!     NOCAL    漂流物の計算を行うか否か(計算しない時.true.)
!     OFF_OUTPUT    オフライン計算用にSTOC-ML,IC,DS側でファイル出力を行うか(行うとき.true.)
!     OFF_START     オフライン計算用にSTOC-ML,IC,DS側で出力する出力開始時刻(s)
!     OFF_INTERVAL  オフライン計算用にSTOC-ML,IC,DS側で出力するデータの出力間隔(s)
!
!----------------------------------------
      REAL(8),ALLOCATABLE::UUAR0(:,:)
      REAL(8),ALLOCATABLE::VVAR0(:,:)
      REAL(8),ALLOCATABLE::HHAR0(:,:)
      REAL(8),ALLOCATABLE::WWXAR0(:,:)
      REAL(8),ALLOCATABLE::WWYAR0(:,:)
!
      REAL(8),ALLOCATABLE::UUAR1(:,:)
      REAL(8),ALLOCATABLE::VVAR1(:,:)
      REAL(8),ALLOCATABLE::HHAR1(:,:)
      REAL(8),ALLOCATABLE::WWXAR1(:,:)
      REAL(8),ALLOCATABLE::WWYAR1(:,:)
!
      REAL(8)::TIMFL0,TIMFL1
      REAL(8)::TIMWIN0,TIMWIN1
      REAL(8)::DTFL,OFF_INTERVAL=0.0D0,OFF_START=0.0D0
      INTEGER::IEOF=0,IEOFWIN,IFLOFF(0:MAXPE),IFLWIN(MAXPE)
      LOGICAL::ONLINE=.true., OFFLINE=.false., NOCAL=.false.,OFF_OUTPUT=.false.
!
CONTAINS
!
SUBROUTINE ALLOCATE_FILEIN(MXAREA,MXNIJ,MXNIJK,IERROR)
  INTEGER,INTENT(IN) :: MXAREA,MXNIJ,MXNIJK
  INTEGER,INTENT(OUT) :: IERROR
!
  IERROR=0
!
  ALLOCATE(UUAR0(MXAREA,MXNIJK) &
       &  ,VVAR0(MXAREA,MXNIJK) &
       &  ,HHAR0(MXAREA,MXNIJ ) &
       &  ,WWXAR0(MXAREA,MXNIJ ) &
       &  ,WWYAR0(MXAREA,MXNIJ ) &
       &  ,UUAR1(MXAREA,MXNIJK) &
       &  ,VVAR1(MXAREA,MXNIJK) &
       &  ,HHAR1(MXAREA,MXNIJ ) &
       &  ,WWXAR1(MXAREA,MXNIJ ) &
       &  ,WWYAR1(MXAREA,MXNIJ ) &
       &  ,STAT=IERROR)
  IF(IERROR/=0) GOTO 900
!
  RETURN
900 CONTINUE
  WRITE(*,*) '### ERROR : ARRAY ALLOCATE ERROR : FILEIN'
  IERROR=1
END SUBROUTINE ALLOCATE_FILEIN
!
END MODULE M_FILEIN
