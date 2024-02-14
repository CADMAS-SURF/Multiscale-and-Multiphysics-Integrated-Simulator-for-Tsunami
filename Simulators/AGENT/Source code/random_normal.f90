subroutine random_normal(sigma,rand_norm,ierr)
!----------------------------------------
! 正規乱数を生成する（平均値0，標準偏差sigma）
!----------------------------------------
    implicit none
!
![arguments]
  real(8),intent(in)::  sigma
  real(8),intent(out):: rand_norm
  integer,intent(out):: ierr
!
![local variables]
  integer,save:: iflag=0
  real(8),save:: r1,r2
  real(8),parameter:: pi2=6.28318530718d0
!
  ierr=0
!
!
!Box-Muller transform
  if(iflag==0)then
    call random_number(r1)
    call random_number(r2)
    rand_norm=sqrt(-2.0d0*log(r1))*sin(pi2*r2)
    iflag=1
  else
    rand_norm=sqrt(-2.0d0*log(r1))*cos(pi2*r2)
    iflag=0
  endif
!
  rand_norm=rand_norm*sigma
!
!
  return
end subroutine random_normal
