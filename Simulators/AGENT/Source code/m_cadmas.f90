module m_cadmas
  implicit none
!----------------------------------------
! n_cadmas     連成するCADMAS及びSTOCの領域数
! icmax,jcmax  地形配列のサイズ
! xc,yc        地形メッシュの分割位置(不等間隔メッシュのため)
! cadmas_tnow  CADMAS-SURF出力データの時刻
! cadmas_tnext CADMAS-SURF出力データの次回更新時刻
! cadmas_rank  CADMAS-SURFのPE番号(連成しないときは-1)
! height_c     地形高さデータ配列(CADMAS-SURF出力データ)
! depth_c      水深データ配列(CADMAS-SURF出力データ)
! uu_c,vv_c    流速データ配列(CADMAS-SURF出力データ)
!----------------------------------------
! height       地形高さデータ配列(等間隔メッシュデータ)
! depth        水深データ配列(等間隔メッシュデータ)
! uu,vv        流速データ配列(等間隔メッシュデータ)
! max_depth    最大値格納用水深データ配列(等間隔メッシュデータ)
!----------------------------------------
!
  integer:: n_cadmas
  integer,parameter:: maxfile=10
  character(64):: cadfile(maxfile)
!
  type region_data
     integer:: icmax,jcmax
     real(4):: cadmas_tnow,cadmas_tnext
     integer:: cadmas_rank
!
     real(8),pointer:: xc(:),yc(:)
     real(4),pointer:: height_c(:,:),depth_c(:,:),uu_c(:,:),vv_c(:,:)
  end type region_data
  type(region_data),allocatable:: region(:)
!
  real(8),pointer:: height(:,:),depth(:,:),uu(:,:),vv(:,:),max_depth(:,:)
!
contains
!
!
  subroutine allocate_cadmas_0(ierr)
!----------------------------------------
! CADMAS-SURF連成用サイズデータ用配列へのメモリ割り当て
!----------------------------------------
    implicit none
!
!  [arguments]
    integer,intent(out):: ierr
!
!  [local variables]
    integer::ierror
    character(32):: rout='allocate_cadmas_0'
!
    ierr=0
!
    if (n_cadmas<=0) then
      !領域がない場合にはダミー領域を1つ作成する
      allocate(region(1),stat=ierror)
    else
      allocate(region(n_cadmas),stat=ierror)
    endif
    region(:)%cadmas_rank=-1
    region(:)%icmax=0
    region(:)%jcmax=0
    region(:)%cadmas_tnow=0.0
    region(:)%cadmas_tnext=0.0
    if(ierror/=0) then
       ierr=-10
       call errmsg(rout,ierr)
       write(*,*) 'cannot allocate cadmas data arrays'
    endif
!
    return
  end subroutine allocate_cadmas_0
!
!
  subroutine allocate_cadmas_c(m,ierr)
!----------------------------------------
! CADMAS-SURF出力データ用配列へのメモリ割り当て
!----------------------------------------
    implicit none
!
!  [arguments]
    integer,intent(in):: m
    integer,intent(out):: ierr
!
!  [local variables]
    integer::ierror
    character(32):: rout='allocate_cadmas_c'
!
    ierr=0
!
    allocate(region(m)%xc(0:region(m)%icmax),region(m)%yc(0:region(m)%jcmax) &
         &  ,region(m)%height_c(region(m)%icmax,region(m)%jcmax) &
         &  ,region(m)%depth_c(region(m)%icmax,region(m)%jcmax) &
         &  ,region(m)%uu_c(region(m)%icmax,region(m)%jcmax) &
         &   ,region(m)%vv_c(region(m)%icmax,region(m)%jcmax),stat=ierror)
    if(ierror/=0) then
       ierr=-10
       call errmsg(rout,ierr)
       write(*,*) 'cannot allocate cadmas data arrays'
    endif
!
    return
  end subroutine allocate_cadmas_c
!
end module m_cadmas
