subroutine solver(ierr)
!----------------------------------------
!時間積分処理のメインルーチン
!----------------------------------------
  use m_timectl,only: nstep,maxstep,time,dt,time_end
  use m_potential,only: n_signpost,n_shelter,n_mob,agent_shelter
  use m_agent,only: n_rw,rw_tnext
  use m_cadmas,only: region
  implicit none
!
![arguments]
  integer,intent(out):: ierr
!
![local variables]
  character(32):: rout='solver'
  integer:: n,m
  real(8),parameter:: eps=1.0d-10
!
  ierr=0
!
!
!----------------------------------------
! 時間積分ループに入る前の処理
!----------------------------------------
  write(100,'(a)')"debug: call initialize"
  !write(*,*)"debug: call initialize"
  call initialize(ierr)                                   ! 初期化
   if(ierr<0) call errstop(rout,ierr)
! 
  write(100,'(a)')"debug: call read_cadmas"
  !write(*,*)"debug: call read_cadmas"
  call read_cadmas(1,ierr)                                ! CADMAS-SURF出力データの読み込み(津波データの更新)
   if(ierr<0) call errstop(rout,ierr)
!
  write(100,'(a)')"debug: call make_signpost"
  !write(*,*)"debug: call make_signpost"
  if(n_signpost>0)then
     call make_signpost(ierr)                             ! 道標の設定
      if(ierr<0) call errstop(rout,ierr)
    
  endif
!
  write(100,'(a)')"debug: call make_potential_shelter"
  !write(*,*)"debug: call make_potential_shelter"
  if(n_shelter>0)then
     call make_potential_shelter(ierr)                    ! 避難経路ポテンシャルの設定
      if(ierr<0) call errstop(rout,ierr)
  endif
!
  write(100,'(a)')"debug: call update_attribute"
  !write(*,*)"debug: call update_attribute"
  call update_attribute(ierr)                             ! エージェントの属性の更新
   if(ierr<0) call errstop(rout,ierr)
!
  write(100,'(a)')"debug: call output"
  !write(*,*)"debug: call output"
  call output(0,ierr)                                     ! ファイル出力の初期処理
   if(ierr<0) call errstop(rout,ierr)
!
!
  write(100,*) '# start time integration loop '
  write(*,*) '# start time integration loop '
!----------------------------------------
! 時間積分ループ(開始)
!----------------------------------------
  do nstep=1,maxstep
!
     time=time+dt                                         ! 時間の更新
!
     call read_cadmas(1,ierr)                             ! CADMAS-SURF出力データの読み込み(津波データの更新)
       if(ierr<0) call errstop(rout,ierr)
!
     if(n_rw==1 .and. time>=rw_tnext)then
       call update_rw(ierr)                               ! RandomWalk情報の更新
        if(ierr<0) call errstop(rout,ierr)
     endif
!
     if(n_mob==1)then
       call make_potential_mob(ierr)                      ! 群衆心理ポテンシャルの設定
        if(ierr<0) call errstop(rout,ierr)
     endif
!
     call move_agent(ierr)                                ! エージェントの移動計算
      if(ierr<0) call errstop(rout,ierr)
!
     call update_attribute(ierr)                          ! エージェントの属性の更新
      if(ierr<0) call errstop(rout,ierr)
!
     call output(1,ierr)                                  ! ファイル出力
      if(ierr<0) call errstop(rout,ierr)
!
     !if(mod(nstep,10)==0) then
     !   write(100,100) nstep,time,region(1)%cadmas_tnow
     !end if
!100  format('step=',i7,'  time=',f7.2,'  cadmas_tnow=',f7.2)
!
     if( time.ge.time_end .or. time_end-time.lt.eps) exit ! 終了判定
!
  enddo
!----------------------------------------
! 時間積分ループ(終了)
!----------------------------------------
  write(100,*) '# end time integration loop '
  write(*,*) '# end time integration loop '
!
  !open(unit=50,file='agent_shelter.txt')
  !do m=1,n_shelter
  !  write(50,*) m,',',agent_shelter(m)
  !enddo
!
  call output(2,ierr)                                     ! ファイルのクローズ
  if(ierr<0) call errstop(rout,ierr)
!
  return
end subroutine solver
