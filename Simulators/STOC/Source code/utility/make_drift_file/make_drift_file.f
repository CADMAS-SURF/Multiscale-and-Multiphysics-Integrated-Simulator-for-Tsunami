      program make_drift_file
!//////////////////////////////////////////////////
!     がれき算定プログラム ver.1.0
!//////////////////////////////////////////////////
      implicit none
      include 'CONTROL.h'
      include 'DOMAIN.h'
!
      real(8),allocatable:: rough(:,:)     ! マニングの粗度
!
      integer,parameter:: maxd=100
      integer:: n_inund                    ! 浸水深の基準値の数
      real(8):: d_inund(maxd)              ! 浸水深の基準値[m]
      real(8),allocatable:: t_inund(:,:,:) ! d_inundの到達時刻[s]
      real(8),allocatable:: t_maxd(:,:)    ! 最大浸水深の時刻[s]
      real(8),allocatable:: d_maxd(:,:)    ! 最大浸水深[m]
!
!     (STOC-DS-MODE時)
      integer,allocatable:: idst(:,:)      ! 破壊フラグ
      real(8),allocatable:: tmdst(:,:)     ! 破壊時刻
!
      integer:: ierr
!
!
      write(*,*) '### make_drift_file: ver.1.0'
!
!////////////////////////////////////////
! (1) control.datの読み込み
!////////////////////////////////////////
      call defalt
      call read_control_file
!
!
!////////////////////////////////////////
! (2) STOCの解析条件ファイルの読み込み
!////////////////////////////////////////
      call read_stoc_file
!
!
!////////////////////////////////////////
! (3) STOCのstrファイルの読み込み
!////////////////////////////////////////
      allocate(rough(mx,my),stat=ierr)
      if(ierr/=0) then
         call errmsg('make_drift_file',98)
         write(*,*) 'cannot allocate arrays: rough'
         call abort1('')
      endif
      call read_str_file(rough)
!
!
!////////////////////////////////////////
! (4) STOCのendファイルの読み込み
!////////////////////////////////////////
!     endファイルに出力されている浸水深到達時刻の数をカウントする
      call check_end_file(n_inund,d_inund,maxd)
!
      allocate(t_inund(n_inund,mx,my),t_maxd(mx,my),d_maxd(mx,my),
     $   idst(mx,my),tmdst(mx,my),stat=ierr)
      if(ierr/=0) then
         call errmsg('make_drift_file',99)
         write(*,*) 'cannot allocate arrays: t_inund ...'
         call abort1('')
      endif
!
!     endファイルから必要なデータを読み込む
      call read_end_file(n_inund,t_inund,t_maxd,d_maxd,idst,tmdst)
!
!
!////////////////////////////////////////
! (5) がれき発生とdrift.datの出力
!////////////////////////////////////////
      call generate_debris(rough,n_inund,d_inund,t_inund,
     $                     t_maxd,d_maxd,idst,tmdst)
!
      write(*,*) ''
      write(*,*) '### normal end'
      stop
      end
