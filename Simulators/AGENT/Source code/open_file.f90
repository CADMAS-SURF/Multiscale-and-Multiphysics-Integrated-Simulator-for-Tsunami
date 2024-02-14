subroutine open_file(iflag,ierr)
!----------------------------------------
! LOGファイル制御
!----------------------------------------
!
![arguments]
  integer,intent(in):: iflag
  integer,intent(out):: ierr
!
![local variables]
  character(32):: rout='open_file'
!
  ierr=0
!
!
!----------------------------------------
!  (0) オープン
!----------------------------------------
  if( iflag==0 )then
    !open(90,file='log_ma',status='replace',form='formatted',err=91)
    open(100,file="debug.txt",status='replace',form='formatted',err=91)
!!!!    write(*,*) 'open file: log_ma'
!
!----------------------------------------
! (2) クローズ
!----------------------------------------
  elseif( iflag==1 )then
    !close(90)
    close(100)
!!!!    write(*,*) 'close file: log_ma'
!
  endif
!
  return
!
!----------------------------------------
! エラー処理
!----------------------------------------
91 continue
  ierr=-10
  call errmsg(rout,ierr)
  write(*,*) 'cannot open log file'
  return

end subroutine open_file
