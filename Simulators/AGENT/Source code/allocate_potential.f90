subroutine allocate_potential(ierr)
!----------------------------------------
! 使用するポテンシャル配列へのメモリ割り当て
!----------------------------------------
  use m_potential,only: n_signpost,n_shelter,n_mob &
                     & ,allocate_move_boundary &
                     & ,allocate_signpost &
                     & ,allocate_pot_shelter &
                     & ,allocate_pot_mob &
                     & ,allocate_m_pot_shelter &
                     & ,allocate_n_potential
  use m_agent,only: n_agent
  use m_flag,only: flag_danger,flag_prob
  implicit none
!
![arguments]
  integer,intent(out):: ierr
!
![local variables]
  character(32):: rout='allocate_potential'
!
  ierr=0
!
!
!進入可否を指定する配列
  write(100,'(a)') "debug: call allocate_move_boundary"
  call allocate_move_boundary(ierr)
   if(ierr<0) call errstop(rout,ierr)
!
!道標関連配列
  write(100,'(a)')"debug: call allocate_signpost"
  if(n_signpost>0)then
     call allocate_signpost(ierr)
      if(ierr<0) call errstop(rout,ierr)
  endif
!
!避難経路ポテンシャル配列
  write(100,'(a)')"debug: call allocate_pot_shelter"
  if(n_shelter>0)then
     call allocate_pot_shelter(ierr)
      if(ierr<0) call errstop(rout,ierr)
  endif
!
!群衆ポテンシャル配列
  write(100,'(a)')"debug: call allocate_pot_mod"
  if(n_mob==1)then
     call allocate_pot_mob(ierr)
      if(ierr<0) call errstop(rout,ierr)
  endif
!
!危険回避ポテンシャル配列
!  write(100,'(a)')"debug: call allocate_pot_shelter"
!  if(flag_danger==1)then
!     call allocate_pot_danger(ierr)
!      if(ierr<0) call errstop(rout,ierr)
!  endif
!
!n_potential
  write(100,'(a)')"debug: call allocate_n_potential"
  call allocate_n_potential(n_agent,ierr)
   if(ierr<0) call errstop(rout,ierr)
!
!m_pot_shelter
  write(100,'(a)')"debug: call allocate_m_pot_shelter"
  call allocate_m_pot_shelter(n_shelter,ierr)
   if(ierr<0) call errstop(rout,ierr)

  return
end subroutine allocate_potential
