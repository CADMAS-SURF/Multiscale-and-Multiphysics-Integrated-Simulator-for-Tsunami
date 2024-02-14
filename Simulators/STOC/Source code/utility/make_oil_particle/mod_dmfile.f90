module mod_dmfile
!//////////////////////////////////////////////////
! STOC-DMのファイルの入出力インターフェース
!//////////////////////////////////////////////////
  implicit none
  private
!
! public関数
  public:: init_dmfile
  public:: read_dmtxtfile
!
! public変数
  public:: n_debris,xd,yd,xdo,ydo
  public:: coord,utm_mid_longitude,geodetic_system
!
  integer:: coord             ! 座標系(-1:経緯度, 0:UTM, 1-19:平面直角座標系の番号)
  real(8):: utm_mid_longitude ! UTM座標の場合の中央経線の経度(deg)
  integer:: geodetic_system   ! 測地系(1:日本測地系(Tokyo-Datum),2:世界測地系(JGD2000),3:世界測地系)
!
  integer:: n_debris ! 漂流物の数
  real(8),allocatable:: xd(:),yd(:),xdo(:),ydo(:)
!
! private変数
  integer,parameter:: handle=81  ! ファイル番号
  integer,parameter:: handle_txt=82  ! ファイル番号
!
!
contains
  subroutine init_dmfile
!//////////////////////////////////////////////////
!   STOC-DMのinput.datを読み込んで、以下の情報を抜き出す
!   ・座標系         coord(,utm_mid_longitude)
!   ・測地系         geodetic_system
!   また、STOC-DMのdrift.txtを読み込んで、以下の情報を抜き出す
!   ・漂流物数       n_debris
!//////////////////////////////////////////////////
    character(64),parameter:: subname='init_dmfile'
    character(64),parameter:: input_file='input.dat'
    character(64),parameter:: input_file2='drift.txt'
    integer:: ierr
!
    character(32):: coordinate, system
    character(64):: line
    integer:: rectangular_zone,nold,nnew
    real(8):: utm_center
    namelist /GEO_DATA/ COORDINATE,RECTANGULAR_ZONE,UTM_CENTER,SYSTEM
!
!
!   (1) input.datの読み込み
    coordinate=''
    rectangular_zone=0
    utm_center=0.d0
    system='JGD2000'
!
    open(handle,file=input_file,form='formatted',action='read',iostat=ierr)
    if(ierr/=0) then
       write(*,*) 'Error: Cannot open input file.'
       write(*,*) '       file=',trim(input_file)
       call sub_err(subname,1)
    endif
!
    read(handle,GEO_DATA,iostat=ierr)
    if(ierr>0) then
       write(*,*) 'Error: Unrecognized variable has been defined.'
       write(*,*) '       file=',trim(input_file)
       call sub_err(subname,2)
    elseif(ierr<0)then
       write(*,*) 'Error: GEO_DATA namelist is not found.'
       write(*,*) '       file=',trim(input_file)
       call sub_err(subname,3)
    endif
!
    if( coordinate=='LONGITUDE-LATITUDE' ) then
       coord=-1
    elseif( coordinate=='UTM' ) then
       coord=0
       utm_mid_longitude=utm_center
    elseif( coordinate=='JAPAN-PLANE-RECTANGULAR' ) then
       coord=rectangular_zone
    else
       write(*,*) 'Error: COORDINATE name is not recognized.'
       write(*,*) '       file=',trim(input_file)
       call sub_err(subname,9)       
    endif
!
    if(system=='TOKYO') then
       geodetic_system=1
    elseif(system=='JGD2000') then
       geodetic_system=2
    elseif(system=='WGS84') then
       geodetic_system=3
    endif
!
    close(handle)
!
!
!   (2) drift.txtの読み込み
    open(handle,file=input_file2,form='formatted',action='read',iostat=ierr)
!
    if(ierr/=0) then
       write(*,*) 'Error: Cannot open input file.'
       write(*,*) '       file=',trim(input_file2)
       call sub_err(subname,4)
    endif
!
    n_debris=0
    nnew=-1
    do
       read(handle,'(a64)',iostat=ierr) line
       if(ierr<0) exit
       if( line(1:1)=='#'.or.line=='' ) cycle
!
       nold=nnew
       read(line,*) nnew
       if( nnew<nold ) exit
!
       n_debris=n_debris+1
    enddo
    write(*,*) 'number of debris=',n_debris
    if(n_debris==0) call sub_err(subname,5)
!
    close(handle)
!
    allocate(xd(n_debris),yd(n_debris),xdo(n_debris),ydo(n_debris),stat=ierr)
    if(ierr/=0) call sub_err(subname,6)
!
    xd(:)=0.d0
    yd(:)=0.d0
    xdo(:)=0.d0
    ydo(:)=0.d0
!
    return
  end subroutine init_dmfile
!
!
  subroutine read_dmtxtfile(time,eof)
!//////////////////////////////////////////////////
! drift.txtファイルを読み込む
!//////////////////////////////////////////////////
    character(64),parameter:: subname='read_dmtxtfile'
    character(64),parameter:: input_file='drift.txt'
    real(8),intent(out):: time
    integer,intent(out):: eof
    logical,save:: lopn=.false.
    integer:: n,ierr
    real(8):: t,x,y
!
!
    if(.not.lopn) then
       open(handle_txt,file=input_file,form='formatted',action='read',iostat=ierr)
       if(ierr/=0) call sub_err(subname,7)
!      skip 1line
       read(handle_txt,*)
!
       lopn=.true.
    endif
!
!
    eof=0
    do n=1,n_debris
       read(handle_txt,'(7x,f10.2,2f12.3)',iostat=ierr) t,x,y
       if(ierr<0) then
          close(handle_txt)
          eof=-1
          exit
       elseif(ierr>0)then
          call sub_err(subname,8)
       endif
!
       xdo(n)=xd(n)
       ydo(n)=yd(n)
!
       time=t
       xd(n)=x
       yd(n)=y
    enddo
!
    return
  end subroutine read_dmtxtfile
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
    write(*,*) ' source file name=mod_dmfile.f90'
    write(*,*) '       erorr code=',code
    stop
  end subroutine sub_err
end module mod_dmfile
