subroutine read_move_boundary(ierr)
!----------------------------------------
! 進入可否を指定する入力データ読み込み
!----------------------------------------
  use m_potential,only: ipmax,jpmax,move_boundary
!
  implicit none
!
![arguments]
  integer,intent(out):: ierr
!
![local variables]
  character(32):: rout='read_move_boundary'
  character(256):: line
  integer:: i,j
!
  ierr=0
!
!
  open(20,file='move_boundary.inp',status='old',form='formatted',err=99)
!
!
!上下方向の進入可否
!
  read(20,'(a256)') line
  if( line(1:1)/='#' )then
     ierr=-30
     call errmsg(rout,ierr)
     write(*,*) 'read error : move_boundary.inp'
  endif
!
  do j=jpmax,1,-1
     read(20,*,err=40) (move_boundary(i,j),i=1,ipmax)
  enddo
  goto 41
40 continue
     ierr=-40
     call errmsg(rout,ierr)
     write(*,*) 'read error : move_boundary.inp'
41 continue
!!
!
  close(20)
!
!
  return
!
99 continue
  ierr=-99
  call errmsg(rout,ierr)
  write(*,*) 'cannot open move_boundary.inp'
  return
end subroutine read_move_boundary

