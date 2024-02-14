subroutine update_rw(ierr)
!----------------------------------------
! RandomWalk情報の更新
!----------------------------------------
  use m_agent,only: n_agent,rw_dt,rw_tnext,rw_sigma,rw_theta
  implicit none
!
![arguments]
  integer,intent(out):: ierr
!
![local variables]
  character(32):: rout='update_rw'
  integer:: n
!
  ierr=0
!
!
  do n=1,n_agent
     call random_normal(rw_sigma(n),rw_theta(n),ierr)
      if(ierr<0) call errstop(rout,ierr)
  enddo
!
  rw_tnext=rw_tnext+rw_dt
!
  return
end subroutine update_rw
