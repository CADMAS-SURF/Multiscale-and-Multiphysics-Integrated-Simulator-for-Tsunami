subroutine read_potential(ierr)
!----------------------------------------
! ポテンシャル関連入力ファイルの読み込み
!----------------------------------------
  use m_potential,only: n_signpost,n_shelter
!
  implicit none
!
![arguments]
  integer,intent(out):: ierr
!
![local variables]
  character(32):: rout='read_potential'
!
  ierr=0
!
!
!進入可否を指定する入力データ読み込み(Debug用?)
  write(100,'(a)')"debug: call read_move_boundary"
  call read_move_boundary(ierr)
   if(ierr<0) call errstop(rout,ierr)
!
!道標データの読み込み
   write(100,'(a)')"debug: call read_signpost"
  if(n_signpost>0)then
     call read_signpost(ierr)
      if(ierr<0) call errstop(rout,ierr)
  endif
!
!避難所データの読み込み
  write(100,'(a)')"debug: call read_shelter"
  if(n_shelter>0)then
     call read_shelter(ierr)
      if(ierr<0) call errstop(rout,ierr)
  endif
!
  return
!
end subroutine read_potential
