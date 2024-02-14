module m_timectl
  implicit none
!----------------------------------------
! 時間積分制御データ
!
! nstep      現在の計算ステップ数
! maxstep    最大ステップ数
!
! time       現在の時刻(s)
! dt         時間刻み(s)
! time_start 計算開始時刻(s)
! time_end   計算終了時刻(s)
!----------------------------------------
!
  integer::nstep,maxstep
  real(8)::time,dt,time_start,time_end,time_begin_s,time_end_s
end module m_timectl
