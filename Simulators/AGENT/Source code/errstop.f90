subroutine errstop(rout,icode)
!----------------------------------------
!計算を停止する
!  (将来的にSTOP文をMPI_ABORTに置き換えること
!   を想定し、ここ以外ではSTOP文を使用しない
!----------------------------------------
  implicit none
  include 'mpif.h'
!
![arguments]
  character(*),intent(in):: rout
  integer,intent(in):: icode
  integer:: ierr
!
!
  write(*,*) 'Stop at subroutine: ',trim(rout)
  write(*,*) '        error code= ',icode
  call mpi_abort(mpi_comm_world,icode,ierr)
end subroutine errstop
