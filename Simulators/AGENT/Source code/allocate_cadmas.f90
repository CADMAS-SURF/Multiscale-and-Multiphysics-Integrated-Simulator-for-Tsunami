subroutine allocate_cadmas(ierr)
!----------------------------------------
! remeshしたCADMASデータを格納する配列へのメモリ割り当て
!----------------------------------------
  use m_potential,only: ipmax,jpmax
  use m_cadmas,only: height,depth,uu,vv,max_depth
  implicit none
!
!  [arguments]
  integer,intent(out):: ierr
!
!  [local variables]
  integer::ierror
  character(32):: rout='allocate_cadmas'
!
  ierr=0
!
  allocate(height(ipmax,jpmax),depth(ipmax,jpmax) &
        & ,uu(ipmax,jpmax),vv(ipmax,jpmax)        &
        & ,max_depth(ipmax,jpmax),stat=ierror)
  max_depth(:,:)=-9999
  if(ierror/=0) then
     ierr=-10
     call errmsg(rout,ierr)
     write(*,*) 'cannot allocate cadmas data arrays'
  endif
!
  return
end subroutine allocate_cadmas
