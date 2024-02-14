MODULE M_TIME
  IMPLICIT NONE
!----------------------------------------
!     時間積分制御データ
!
!     NS        現在の計算ステップ数
!     MAXSTP    最大ステップ数
!     IDT       時間刻みの計算方法(=0：固定値,=1：自動計算)
!     IREST     リスタート計算を行うか否か(=0：行わない,=1：行う)
!     IFINISH   終了判定(=0：継続,=1：終了)
!
!     TIME      現在の時刻(s)
!     DT        タイムステップ(s)
!     TSTART    計算開始時刻(s)
!     TEND      計算終了時刻(s)
!     CSAFE     自動時間刻みの安全率(-)
!     DTMAX     自動時間刻みの上限(s)
!     DTMIN     自動時間刻みの下限(s)
!----------------------------------------
!
!<<<<< (START) STOC-DM VERSION  <<<<<<<
      INTEGER::NS,MAXSTP,IDT,IREST,IFINISH
!<<<<<  (END)  STOC-DM VERSION  <<<<<<<
      REAL(8)::TIME,DT,TSTART,TEND
      REAL(8)::CSAFE,DTMAX,DTMIN
END MODULE M_TIME
