module mod_dem
!
  implicit none
!
  integer :: ihidem     ! 構造物破壊解析プログラムHiDEMとの連成
                        !      ==0:連成しない
                        !      /=0:連成する

  integer :: nsize_dem  ! dem担当PE数

  integer :: nprwld     ! comm_2fc_demのプロセス数
  integer :: myrwld     ! comm_2fc_demでのランク

  integer :: ihidm      ! HiDEM の中での0番PEの comm_2fc_dem でのランク
                        ! HiDEM と代表通信を行うときの HiDEM側のランク
  integer :: icadm      ! HiDEMと代表通信を行うCADMASのランク(comm_2fc_dem)
  integer :: icadm2     ! HiDEMと代表通信を行うCADMASのランク(COMCAD内)
!
  real(8) :: dtcadm     ! cadmasの時間刻み幅
  real(8) :: glcadm     ! cadmasの体積多孔率の下限値
!
  real(8) :: xhidem(6)  ! DEM全体の解析範囲
  integer :: jcadm(6)   ! HiDEM解析領域のCADMASでの位置（インデックス）
  integer :: ncadm(3)   ! HiDEM解析領域のCADMASでの分割数
!
!
  integer,allocatable :: nrank_2fc_dem(:)  ! comm_2fc_dem のsizeで定義
                                       ! >0 dem担当PEでDEMの中での+(PE番号+1)
                                       ! <0 2fc担当PEで2FCの中での-(PE番号+1)
  integer,allocatable :: nrank_dem(:)      ! (0:nsize_dem-1) --> (0:nprwld-1)

  real(8),allocatable :: xhidem_sub(:,:)   ! (6,0:nsize_dem-1)
                          ! DEMの各PEが担当するsubgridを包括する範囲の座標値
  integer,allocatable :: jcadm_sub(:,:)    ! (6,0:nsize_dem-1)
                          ! xhidem_sub の範囲のindex(local)
  integer,allocatable :: ncadm_sub(:,:)    ! (3,0:nsize_dem-1)
                          ! xhidem_sub の範囲の分割数
!
end module mod_dem
