subroutine initialize(ierr)
!----------------------------------------
! 初期化
!----------------------------------------
  use m_timectl,only: time,time_start
  use m_agent,only: rw_tnext &
                 & ,istat_escaped,istat_moving,istat_dead &
                 & ,n_agent,agent_status,i_agent,j_agent,agent_z
  use m_potential,only: agent_shelter
  use m_cadmas,only: height
  implicit none
!
!  [arguments]
  integer,intent(out):: ierr
!
!  [local variables]
  character(32):: rout='initialize'
  integer:: n,m
!
  ierr=0
!
!
!計算開始時刻の初期化
  time=time_start
!
!エージェントの状態を初期化
  agent_status(:)=1
  agent_shelter(:)=0
  do n=1,n_agent
    agent_z(n) = height(i_agent(n),j_agent(n))
  enddo
!
!エージェント統計値を初期化
  istat_escaped = 0
  istat_moving  = n_agent
  istat_dead    = 0
!
!RandomWalk更新時刻の初期化
  rw_tnext=0.0d0
!
  return
end subroutine initialize
