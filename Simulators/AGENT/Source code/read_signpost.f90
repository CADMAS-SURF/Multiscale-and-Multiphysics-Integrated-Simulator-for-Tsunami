subroutine read_signpost(ierr)
!----------------------------------------
! 道標データの読み込み
!----------------------------------------
  use m_potential,only: n_signpost,i_signpost,j_signpost  &
                     & ,r_signpost,theta_signpost
!
  implicit none
!
![arguments]
  integer,intent(out):: ierr
!
![local variables]
  character(32):: rout='read_signpost'
  character(256):: line
  integer:: n,nn
  real(8):: theta_signpost_deg    !入力ファイル読み込み時は、RadianではなくDegreeで
  real(8),parameter:: pi=3.141592653589793d0
!
  ierr=0
!
!
  open(21,file='signpost.inp',status='old',form='formatted',err=99)
!
!
  n=0
  do
     read(21,'(a256)',end=10) line
     if( line(1:1)=='#' ) cycle
     n=n+1
     read(line,*,err=30) nn,i_signpost(n),j_signpost(n) &
                      & ,r_signpost(n),theta_signpost_deg
     theta_signpost(n)=theta_signpost_deg*pi/180d0    ! Degree => Radian
  enddo
10 continue
  if(n>n_signpost)then
     ierr=10
     call errmsg(rout,ierr)
     write(*,*) 'n > n_signpost : signpost.inp'
  endif
  goto 31
30 continue
     ierr=-30
     call errmsg(rout,ierr)
     write(*,*) 'read error : signpost.inp'
31 continue
!
!
  close(21)
!
  return
!
99 continue
  ierr=-99
  call errmsg(rout,ierr)
  write(*,*) 'cannot open signpost.inp'
  return
end subroutine read_signpost
