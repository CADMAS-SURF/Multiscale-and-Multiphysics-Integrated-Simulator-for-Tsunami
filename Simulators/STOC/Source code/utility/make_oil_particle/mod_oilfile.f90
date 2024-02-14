module mod_oilfile
!//////////////////////////////////////////////////
! OIL-PARIのファイルの入出力インターフェース
!//////////////////////////////////////////////////
  implicit none
  private
!
! public関数
  public:: init_oilfile
!
! public変数
  public:: vol
!
  real(8):: vol
!
! private変数
  integer,parameter:: handle=81  ! ファイル番号
!
!
contains
  subroutine init_oilfile
!//////////////////////////////////////////////////
!   OIL-PARIのdata.in_oilを読み込んで、以下の情報を抜き出す
!   ・油1粒子の体積  vol
!//////////////////////////////////////////////////
    character(64),parameter:: subname='init_oilfile'
    character(64),parameter:: input_file='data.in_oil'
!
    integer:: n,flag,ierr
    character(64):: line,left,right
!
!
    vol=-1.d9
!
    open(handle,file=input_file,form='formatted',action='read',iostat=ierr)
    if(ierr/=0) then
       write(*,*) 'Error: Cannot open input file.'
       write(*,*) '       file=',trim(input_file)
       call sub_err(subname,1)
    endif
!
! 簡易的な入力処理(%PARTICLEブロックのVOL変数を探すのみ)
    flag=0
    do
       read(handle,'(a64)',iostat=ierr) line
       if(ierr<0) exit
!
       n=index(line,'#')
       if(n>0) line(n:)=''
!
       if( line=='' ) cycle
!
!debug       if( flag==1 ) write(*,*) trim(line)
!
       if( line(1:9)=='%PARTICLE' ) then
          flag=1
       elseif( flag==1.and.line(1:4)=='%END' ) then
          flag=0
       elseif( flag==1 ) then
          n=index(line,'=')
          if(n>0) then
             left=adjustl(line(:n-1))
             right=adjustl(line(n+1:))
             if( left=='VOL' .or. left=='vol' .or. left=='Vol' ) then
                read(right,*) vol
                write(*,*) 'particle volume=',vol
             endif
          endif
       endif
!
    enddo
!
    close(handle)
!
    return
  end subroutine init_oilfile
!
!
  subroutine sub_err(subname,code)
!//////////////////////////////////////////////////
! エラー発生に終了処理を行う
!//////////////////////////////////////////////////
    character(*),intent(in):: subname
    integer,intent(in):: code
!
    write(*,*) ''
    write(*,*) 'Stop at subrouine ',trim(subname)
    write(*,*) ' source file name=mod_oilfile.f90'
    write(*,*) '       erorr code=',code
    stop
  end subroutine sub_err
end module mod_oilfile
