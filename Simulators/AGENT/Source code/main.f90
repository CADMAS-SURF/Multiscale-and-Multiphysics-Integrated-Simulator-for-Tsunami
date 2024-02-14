program main
!----------------------------------------
!メインルーチン
!----------------------------------------
  use mod_comm,only: init_mpmd
  use m_timectl,only: time_begin_s,time_end_s
  implicit none
!
![local variables]
  character(32):: rout='main'
  integer:: ierr
!
  write(*,*)"agent_ver3.5 start"
  call cpu_time(time_begin_s)
  !open(unit=99,file='cpu_time.txt')
  !write(99,*)'time_begin_s= ',time_begin_s
  call init_mpmd
!
  ierr=0
!
  call open_file(0,ierr)                  ! LOG出力(装置番号：90)開始
   if(ierr<0) call errstop(rout,ierr)
!
!
!----------------------------------------
! 入力データの読込み
!----------------------------------------
!
  write(100,'(a)')"debug: call read_condition"
  !write(*,*)"debug: call read_condition"
  call read_condition(ierr)               ! 計算条件の読み込み
   if(ierr<0) call errstop(rout,ierr)
!
   write(100,'(a)')"debug: call read_agent"
   !write(*,*)"debug: call read_agent"
   call read_agent(ierr)                   ! エージェントデータの読み込み
    if(ierr<0) call errstop(rout,ierr)
!
  write(100,'(a)')"debug: call allocate_potential"
  !write(*,*)"debug: call allocate_potential"
  call allocate_potential(ierr)           ! 使用するポテンシャル関連配列へのメモリ割り当て
   if(ierr<0) call errstop(rout,ierr)
!
  write(100,'(a)')"debug: call read_potential"
  !write(*,*)"debug: call read_potential"
  call read_potential(ierr)               ! ポテンシャル関連入力データの読み込み
   if(ierr<0) call errstop(rout,ierr)
!
  write(100,'(a)')"debug: call read_cadmas"
  !write(*,*)"debug: call read_cadmas"
  call read_cadmas(0,ierr)                ! CADMAS-SURF出力データの読み込み(初期設定＆地形読み込み)
   if(ierr<0) call errstop(rout,ierr)

   write(*,*)"finish reading input data"
!
!----------------------------------------
! 時間積分計算
!----------------------------------------
  !write(100,'(a)')"debug: call solver"
  !write(*,*)"debug: call solver"
  call solver(ierr)
  if(ierr<0) call errstop(rout,ierr)
!
  call mpi_finalize(ierr)
!
  call open_file(1,ierr)                  ! LOG出力(装置番号：90)終了
   if(ierr<0) call errstop(rout,ierr)
!
   !call cpu_time(time_end_s)              !CPU時間計測終了
   !write(99,*)'time_end_s= ',time_end_s
   !close(99)
!
   write(*,*)"agent_ver3.5 end"
end program main
