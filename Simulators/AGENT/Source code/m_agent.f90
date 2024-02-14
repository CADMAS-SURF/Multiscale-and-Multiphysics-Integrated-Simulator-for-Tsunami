module m_agent
  implicit none
!----------------------------------------
! エージェントデータ
!
! n_agent         エージェント数
! i_agent         エージェントのx方向セル位置
! j_agent         エージェントのy方向セル位置
! Fi_agent        エージェントのx方向初期セル位置
! Fj_agent        エージェントのy方向初期セル位置
! agent_x         エージェントのx座標
! agent_y         エージェントのy座標
! agent_z         エージェントのz座標
! agent_u         エージェントのx方向移動速度
! agent_v         エージェントのy方向移動速度
! vel             エージェントの移動速度
! agent_status    エージェントの状態（0:避難済み，1:移動中，2:移動中(水中)，3:死亡）
! istat_escaped   避難済みエージェント数
! istat_moving    移動中のエージェント数
! istat_dead      死亡したエージェント数
! deadline        エージェントが許容できる水深（この水深以上になると死亡）
! n_rw            進行方向不確実さを考慮するかのフラグ
! rw_dt           進行方向不確実さ（rw_theta）の更新間隔
! rw_tnext        次にrw_thetaを更新する時刻
! rw_sigma        目標方向とRandomWalkによる進行方向の角度差の標準偏差
! rw_theta        目標方向とRandomWalkによる進行方向の角度差（rw_dt毎に更新）
! weight_signpost エージェントの道標に従う確率
! iflag_signpost  各エージェントが道標に従うか否か
! weight_shelter  エージェントの避難経路ポテンシャルに関する重み
! weight_mob      エージェントの群衆心理ポテンシャルに関する重み
! weight_danger   エージェントの津波危険度ポテンシャルに関する重み
! n_slope         斜面勾配による移動速度の変化を考慮するか否かのフラグ
! agent_start     避難者毎の避難開始時間
!----仮定義（計算には未使用）--------------------
! agent_age
! agent_height
!
!----------------------------------------
!
  integer:: n_agent,n_rw,n_slope &
         & ,istat_escaped,istat_moving,istat_dead
  real(8):: rw_dt,rw_tnext
  real(8),PARAMETER:: vertical_evacuation_speed = 0.479   !垂直避難速度を仮定
  integer,allocatable:: i_agent(:),j_agent(:)   &
                     & ,agent_status(:)         &
                     & ,iflag_signpost(:,:)     &
                     & ,Fi_agent(:),Fj_agent(:)
  real(8),allocatable:: agent_x(:),agent_y(:)   &
                     & ,agent_z(:)              &
                     & ,agent_u(:),agent_v(:)   &
                     & ,vel(:),deadline(:)      &
                     & ,rw_sigma(:),rw_theta(:) &
                     & ,weight_signpost(:)      &
                     & ,weight_shelter(:)       &
                     & ,weight_mob(:)           &
                     & ,weight_danger(:)        &
                     & ,agent_start(:)
!----仮定義（計算には未使用）------------
  real(8),allocatable:: agent_age(:),agent_height(:)
!----------------------------------------
!

contains
!
!
  subroutine allocate_agent(ierr)
!----------------------------------------
! エージェント用配列へのメモリ割り当て
!----------------------------------------
    implicit none
!
!  [arguments]
    integer,intent(out):: ierr
!
!  [local variables]
    integer::ierror
    character(32):: rout='allocate_agent'
!    character(256):: line
!
    ierr=0
!
!
    allocate(i_agent(n_agent),j_agent(n_agent)    &
          & ,agent_x(n_agent),agent_y(n_agent)    &
          & ,agent_z(n_agent)                     &
          & ,agent_u(n_agent),agent_v(n_agent)    &
          & ,vel(n_agent),agent_status(n_agent)   &
          & ,Fi_agent(n_agent),Fj_agent(n_agent) &
          & ,deadline(n_agent),stat=ierror)
    if(ierror/=0) then
       ierr=-10
       call errmsg(rout,ierr)
       write(*,*) 'cannot allocate agent arrays'
    endif
!----仮定義（計算には未使用）------------
    allocate(agent_age(n_agent),agent_height(n_agent),stat=ierror)
    if(ierror/=0) then
       ierr=-10
       call errmsg(rout,ierr)
       write(*,*) 'cannot allocate agent arrays'
    endif
    agent_age(:)=20
    agent_height(:)=165.0d0
!----------------------------------------
!
    allocate(rw_sigma(n_agent),rw_theta(n_agent),stat=ierror)
    if(ierror/=0) then
       ierr=-20
       call errmsg(rout,ierr)
       write(*,*) 'cannot allocate rw_sigma,rw_theta arrays'
    endif
!
    allocate(weight_signpost(n_agent),stat=ierror)
    if(ierror/=0) then
       ierr=-21
       call errmsg(rout,ierr)
       write(*,*) 'cannot allocate weight_signpost arrays'
    endif
!
    allocate(weight_shelter(n_agent),stat=ierror)
    if(ierror/=0) then
       ierr=-22
       call errmsg(rout,ierr)
       write(*,*) 'cannot allocate weight_shelter arrays'
    endif
!
    allocate(weight_mob(n_agent),stat=ierror)
    if(ierror/=0) then
       ierr=-23
       call errmsg(rout,ierr)
       write(*,*) 'cannot allocate weight_mob arrays'
    endif
!
    allocate(weight_danger(n_agent),stat=ierror)
    if(ierror/=0) then
       ierr=-24
       call errmsg(rout,ierr)
       write(*,*) 'cannot allocate weight_danger arrays'
    endif
!
!
    allocate(agent_start(n_agent),stat=ierror)
    if(ierror/=0) then
       ierr=-25
       call errmsg(rout,ierr)
       write(*,*) 'cannot allocate agent_start arrays'
    endif
!

    return
!
  end subroutine allocate_agent
!
end module m_agent
