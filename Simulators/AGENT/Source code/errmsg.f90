subroutine errmsg(rout,ierr)
!----------------------------------------
!エラーメッセージの定型部分を出力する
!  ierr>0: 計算を継続(Warning)
!  ierr<0: 計算を停止(Error)
!----------------------------------------
  use m_timectl,only: nstep
  implicit none
!
![arguments]
  character(*),intent(in):: rout
  integer,intent(in):: ierr
!
!
  if(ierr>0) then
     write(*,*) 'Warning at subroutine ',trim(rout)
     write(*,*) '           error code=',ierr
     write(*,*) '                 step=',nstep
  else
     write(*,*) 'Error   at subroutine ',trim(rout)
     write(*,*) '           error code=',ierr
     write(*,*) '                 step=',nstep
  endif
!
  return
end subroutine errmsg
