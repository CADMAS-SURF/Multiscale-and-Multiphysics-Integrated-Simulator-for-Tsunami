      subroutine errmsg(rout,ierr)
C----------------------------------------
C     エラーメッセージの定型部分を出力する
C     ierr>0: 計算を継続(Warning)
C     ierr<0: 計算を停止(Error)
C----------------------------------------
      use m_time,only: ns
      implicit none
      include 'mpif.h'
C
C     [arguments]
      character(*),intent(in):: rout
      integer,intent(in):: ierr
C
      integer:: icode
C
C
      if(ierr.gt.0) then
         write(*,*) 'Warning at subroutine ',trim(rout)
         write(*,*) '           error code=',ierr
         write(*,*) '                 step=',ns
      else
         write(*,*) 'Error   at subroutine ',trim(rout)
         write(*,*) '           error code=',ierr
         write(*,*) '                 step=',ns
         call mpi_abort(mpi_comm_world,icode,ierr)
      endif
!
      return
      end
