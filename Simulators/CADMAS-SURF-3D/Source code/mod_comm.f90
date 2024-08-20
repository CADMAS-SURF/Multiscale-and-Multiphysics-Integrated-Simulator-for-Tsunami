!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! 使用方法
!!!
!!! 各モジュールへの組込み時は"my_group="と"my_model="の2行だけ書き換える
!!!
!!! メインルーチンの最初のところでinit_mpmdをCALLする
!!!
!!! init_mpmd以降では、STOC内(CADMAS内)ではmpi_comm_worldの代わりにcom_*を使用し、
!!!    use mod_comm,only: comm_*
!!! を追加する。
!!! mpi_comm_worldを使用するのは、mpi_abortのみ。
!!!
!!! その他の大域変数nsize_all,nrank_all,l_group,l_modelは使わなくてもよい。
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module mod_comm
  implicit none
!
! Common Definition
  integer,parameter:: &                            ! グループ番号の定義
       &              l_stoc=0, l_cadmas=1
  integer,parameter:: &                            ! モデル番号の定義
       &              l_stoc_ml   = 0, l_stoc_ic   = 1, l_stoc_ds = 2 &
       &             ,l_stoc_dm   = 3, l_stoc_oil  = 4                &
       &             ,l_cadmas_mg =10, l_cadmas_2fc=11, l_paridem =12 &
       &             ,l_mlt_agent =13, l_str       =14
  character(6),parameter::  c_group(0:1)=(/ &      ! グループ名
       &             'stoc  ', 'cadmas'   /)
  character(10),parameter:: c_model(0:19)=(/ &     ! モデル名
       &             'ml        ', 'ic        ', 'ds        ' &
       &            ,'dm        ', 'oil       ', '#5#       ' &
       &            ,'#6#       ', '#7#       ', '#8#       ' &
       &            ,'#9#       ', 'mg        ', '2fc       ' &
       &            ,'paridem   ', 'mlt_agent ', 'str       ' &
       &            ,'#15#      ', '#16#      ', '#17#      ' &
       &            ,'#18#      ', '#19#      '               &
       &                                   /)
!
! Definition for each model
  integer,parameter:: my_group=l_cadmas
  integer,parameter:: my_model=l_cadmas_mg
!
! Global variables
  integer:: comm_group             ! グループ内の通信に用いるコミュニケータ
  integer:: comm_model             ! モデル内の通信に用いるコミュニケータ
!                                  ! (例外1: STOC-ML,IC,DSは同じcomm_modelに属する)
!                                  ! (例外2: CADMAS-SURF/MGと2FCは同じcomm_modelに属する)
!
!                                  ! モデル間の通信に用いるコミュニケータ群(値の設定は各モデル内で別途行う。
!                                  ! 下記のテンポラリのコミュニケータを利用して生成する)
!                                  ! 1対1通信のみであれば、mpi_comm_worldやcomm_groupを用いてもよい。
  integer:: comm_mlicds_dm         ! STOC-ML,IC,DSとSTOC-DMの通信に用いるコミュニケータ
  integer:: comm_mlicds_oil        ! STOC-ML,IC,DSとSTOC-OILの通信に用いるコミュニケータ
  integer:: comm_ic_mg             ! STOC-ICとCADMAS-SURF/MGの通信に用いるコミュニケータ
  integer:: comm_mg_2fc            ! CADMAS-SURF/MGとCADMAS-SURF/2FCの通信に用いるコミュニケータ
  integer:: comm_2fc_dem           ! CADMAS-SURF/2FCとPARIDEMの通信に用いるコミュニケータ
  integer:: comm_2fc_str           ! CADMAS-SURF/2FCとSTRの通信に用いるコミュニケータ
  integer:: comm_mlicdsmg2fc_mlt   ! STOC-ML,IC,DSとCADMAS-SURF/MG,2FCとMLT_AGENTの通信に用いるコミュニケータ
  integer:: comm_mlicdsmg2fc       ! STOC-ML,IC,DSとCADMAS-SURF/MGと2FCの通信に用いるコミュニケータ
!
! テンポラリで設定するコミュニケータ(モデル間の通信に用いるコミュニケータを作成する前に、
!                                    COMM_SPLITに参加するモデルを絞ったコミュニケータを作成しておく)
  integer:: comm_work_mlicds_dm    ! STOC-ML,IC,DSとSTOC-DMを含むコミュニケータ
  integer:: comm_work_mlicds_oil   ! STOC-ML,IC,DSとSTOC-OILを含むコミュニケータ
  integer:: comm_work_ic_mg        ! STOC-ICとCADMAS-SURF/MGを含むコミュニケータ
  integer:: comm_work_mg_2fc       ! CADMAS-SURF/MGとCADMAS-SURF/2FCを含むコミュニケータ
  integer:: comm_work_2fc_dem      ! CADMAS-SURF/2FCとPARIDEMを含むコミュニケータ
  integer:: comm_work_2fc_str      ! CADMAS-SURF/2FCとSTRを含むコミュニケータ
  integer:: comm_work_mlicdsmg2fc_mlt ! STOC-ML,IC,DSとCADMAS-SURF/MG,2FCとMLT_AGENTを含むコミュニケータ
!
  integer:: nsize_all,nrank_all    ! mpi_comm_worldにおけるサイズとランク
  integer:: nsize_grp,nrank_grp    ! com_groupにおけるサイズとランク
!
  integer,allocatable:: l_group(:) ! PE番号とグループ番号の対応リスト
  integer,allocatable:: l_model(:) ! PE番号とモデル番号の対応リスト
!
!
contains
!
  subroutine init_mpmd
    include 'mpif.h'
!    use mpi
!    use ifport
    interface
       integer(4) function getcwd(dirname)
         character(len=*) dirname
       end function getcwd
!
       integer(4) function chdir(dirname)
         character(len=*) dirname
       end function chdir
    end interface
    integer:: l,m,n,imodel,icode=9876,ierr
    character(256):: dirname
!!!
!!! (1) 初期化とmpi_comm_worldのサイズ・ランクの取得
!!!
    call mpi_init(ierr)
         if(ierr/=0) goto 901
    call mpi_comm_size(mpi_comm_world,nsize_all,ierr)
         if(ierr/=0) goto 902
    call mpi_comm_rank(mpi_comm_world,nrank_all,ierr)
         if(ierr/=0) goto 903
!!!
!!! (2) グループリストとモデルリストの作成
!!!
    allocate(l_group(0:nsize_all-1),l_model(0:nsize_all-1),stat=ierr)
         if(ierr/=0) goto 904
    call mpi_allgather(my_group,1,mpi_integer,l_group,1,mpi_integer &    ! l_groupの作成
         &            ,mpi_comm_world,ierr)
         if(ierr/=0) goto 905
    call mpi_allgather(my_model,1,mpi_integer,l_model,1,mpi_integer &    ! l_modelの作成
         &            ,mpi_comm_world,ierr)
         if(ierr/=0) goto 905
!!!
!!! (3) コミュニケータの分割(mpi_comm_world -> comm_group -> comm_model)
!!!
    call mpi_comm_split(mpi_comm_world,my_group,nrank_all,comm_group &        ! comm_groupの作成
         &            ,ierr)
         if(ierr/=0) goto 906
    call mpi_comm_size(comm_group,nsize_grp,ierr)
         if(ierr/=0) goto 902
    call mpi_comm_rank(comm_group,nrank_grp,ierr)
         if(ierr/=0) goto 903
!
    imodel=my_model
    if( imodel==l_cadmas_2fc ) then
       imodel=l_cadmas_mg
!      cadmas_mgとcadmas_2fcは同じcomm_modelに属する
    elseif( imodel==l_stoc_ic.or.imodel==l_stoc_ds ) then
       imodel=l_stoc_ml
!      stoc_mlとstoc_icとstoc_dsは同じcomm_modelに属する
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_model &          ! comm_modelの作成
         &            ,ierr)
         if(ierr/=0) goto 907
!
    imodel=my_model
    if( imodel==l_stoc_ic.or.imodel==l_stoc_ds.or. &
      & imodel==l_cadmas_mg.or.imodel==l_cadmas_2fc ) then
       imodel=l_stoc_ml
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_mlicdsmg2fc &          ! comm_mlicdsmg2fcの作成
         &            ,ierr)
         if(ierr/=0) goto 912
!!!
!!! (4) テンポラリで設定するコミュニケータの分割
!!!
    imodel=my_model
    if( imodel==l_stoc_ml.or.imodel==l_stoc_ic.or. &
         &   imodel==l_stoc_ds.or.imodel==l_stoc_dm ) then
       imodel=0
    else
       imodel=1
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_work_mlicds_dm  &! comm_work_mlicds_dmの作成
         &            ,ierr)
         if(ierr/=0) goto 911
!
    imodel=my_model
    if( imodel==l_stoc_ml.or.imodel==l_stoc_ic.or. &
         &   imodel==l_stoc_ds.or.imodel==l_stoc_oil ) then
       imodel=0
    else
       imodel=1
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_work_mlicds_oil &! comm_work_mlicds_oilの作成
         &            ,ierr)
         if(ierr/=0) goto 911
!
    imodel=my_model
    if( imodel==l_stoc_ic.or.imodel==l_cadmas_mg ) then
       imodel=0
    else
       imodel=1
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_work_ic_mg &     ! comm_work_ic_mgの作成
         &            ,ierr)
         if(ierr/=0) goto 911
!
    imodel=my_model
    if( imodel==l_cadmas_mg.or.imodel==l_cadmas_2fc ) then
       imodel=0
    else
       imodel=1
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_work_mg_2fc &    ! comm_work_mg_2fcの作成
         &            ,ierr)
         if(ierr/=0) goto 911
!
    imodel=my_model
    if( imodel==l_cadmas_2fc.or.imodel==l_paridem ) then
       imodel=0
    else
       imodel=1
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_work_2fc_dem &   ! comm_work_2fc_demの作成
         &            ,ierr)
         if(ierr/=0) goto 911
!
    imodel=my_model
    if( imodel==l_cadmas_2fc.or.imodel==l_str ) then
       imodel=0
    else
       imodel=1
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_work_2fc_str &   ! comm_work_2fc_strの作成
         &            ,ierr)
         if(ierr/=0) goto 911
!
    imodel=my_model
    if( imodel==l_stoc_ml.or.imodel==l_stoc_ic.or.imodel==l_stoc_ds.or.imodel==l_cadmas_mg.or. &
         &              imodel==l_cadmas_2fc.or.imodel==l_mlt_agent ) then
       imodel=0
    else
       imodel=1
    endif
    call mpi_comm_split(mpi_comm_world,imodel,nrank_all,comm_work_mlicdsmg2fc_mlt &    ! comm_work_mlicdsmg2fc_mltの作成
         &            ,ierr)
         if(ierr/=0) goto 911
!!!
!!! (5) stocグループとcadmasグループの実行ディレクトリを分ける
!!!
    l=l_group(0)
    m=0
    do n=1,nsize_all-1
       if( l/=l_group(n) ) m=1                  ! 別グループを発見した場合にm=1とする
    enddo
!
 m=0 ! ディレクトリを分ける機能は一旦封印する
    if( m==1 ) then                             ! STOCとCADMASの連成ありのときディレクトリを分ける
!       if(nrank_all==0) ierr=getcwd(dirname)                            ! 起動時のディレクトリ名の取得
            if(ierr/=0) goto 908
!
       call mpi_bcast(dirname,256,mpi_character,0,mpi_comm_world,ierr)
            if(ierr/=0) goto 909
!
       if( m==1.and.my_group==l_stoc ) then
          dirname=trim(dirname)//'/stoc'
       else if( m==1.and.my_group==l_cadmas ) then
          dirname=trim(dirname)//'/cadmas'
       endif
!
!       ierr=chdir(dirname)                                              ! ディレクトリの移動
            if(ierr/=0) goto 910
!       ierr=getcwd(dirname)                                             ! 実行ディレクトリ名の取得
            if(ierr/=0) goto 908
    endif
!!!
!!!  (5) デバッグ出力
!!!
    if( nrank_all==0 ) then                                             ! mpmd参加リストの出力
       write(*,10) nsize_all
       do n=0,nsize_all-1
          write(*,20) n, c_group(l_group(n)), trim(c_model(l_model(n)))
       enddo
       write(*,*) ''
!
       write(*,30) c_group(my_group),nsize_grp
       m=0
       do n=0,nsize_all-1
          if( l_group(n)==my_group ) then
             write(*,40) m, trim(c_model(l_model(n)))
             m=m+1
          endif
       enddo
       write(*,*) ''
!
       if( nsize_all-nsize_grp>0 ) then
       write(*,30) c_group(1-my_group),nsize_all-nsize_grp
       m=0
       do n=0,nsize_all-1
          if( l_group(n)==1-my_group ) then
             write(*,40) m, trim(c_model(l_model(n)))
             m=m+1
          endif
       enddo
       write(*,*) ''
       endif
    endif
10 format('### MPMD Information ( SIZE=',I4,' ) ###',/, &
        & 'Rank Group  Module')
20 format(i4,1x,a6,1x,a)
30 format('### MPMD Subgroup Information:  ( GROUP=',A6,' SIZE=',I4,' ) ###',/, &
        & 'Rank Module')
40 format(i4,1x,a)
!
    return
!
901 continue
    write(*,*) 'Error at mpi_comm_size(init_mpmd)'
    goto 999
902 continue
    write(*,*) 'Error at mpi_comm_size(init_mpmd)'
    goto 999
903 continue
    write(*,*) 'Error at mpi_comm_rank(init_mpmd)'
    goto 999
904 continue
    write(*,*) 'Error at allocate(init_mpmd)'
    goto 999
905 continue
    write(*,*) 'Error at mpi_allgather(init_mpmd)'
    goto 999
906 continue
    write(*,*) 'Error at mpi_comm_split(init_mpmd):comm_group'
    goto 999
907 continue
    write(*,*) 'Error at mpi_comm_split(init_mpmd):comm_model'
    goto 999
908 continue
    write(*,*) 'Error at getcwd(init_mpmd)'
    goto 999
909 continue
    write(*,*) 'Error at mpi_bcast(init_mpmd)'
    goto 999
910 continue
    write(*,*) 'Error at chdir(init_mpmd)'
    goto 999
911 continue
    write(*,*) 'Error at mpi_comm_split(init_mpmd):comm_work_*'
    goto 999
912 continue
    write(*,*) 'Error at mpi_comm_split(init_mpmd):comm_mlicdsmg2fc'
    goto 999
999 continue
    call mpi_abort(mpi_comm_world,icode,ierr)
  end subroutine init_mpmd
!
end module mod_comm
