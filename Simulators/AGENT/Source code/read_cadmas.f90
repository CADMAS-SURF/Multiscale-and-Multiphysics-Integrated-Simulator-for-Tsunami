subroutine read_cadmas(iflag,ierr)
!----------------------------------------
! CADMAS-SURF出力データの読み込み
!----------------------------------------
  use mod_comm,only: nsize_all,l_model,l_group,l_cadmas_mg,l_cadmas &
                  & ,comm_mlicdsmg2fc_mlt,comm_work_mlicdsmg2fc_mlt &
                  & ,nrank_all
  use m_timectl,only: time,dt,time_end
  use m_cadmas,only: n_cadmas,region,allocate_cadmas_0,allocate_cadmas_c,cadfile
  use m_potential,only: xpin,ypin
!  use mpi,only: mpi_status_size,mpi_any_tag &
!       &       ,mpi_integer,mpi_real,mpi_double_precision
  implicit none
  include 'mpif.h'
!
![arguments]
  integer,intent(in):: iflag
  integer,intent(out):: ierr
!
![local variables]
  character(32):: rout='read_cadmas'
  integer:: i,j,m,n,immtyp,ireq,istat(mpi_status_size)
  integer:: icolor
  integer:: isize,irank,iwork,icode
  integer:: itag
!
  ierr=0
!
!
!----------------------------------------
!  (0) 初期設定＆地形読み込み
!----------------------------------------
  if(iflag==0) then
!
   icolor=1
   call mpi_comm_split(comm_work_mlicdsmg2fc_mlt,icolor,nrank_all, &
        &              comm_mlicdsmg2fc_mlt,ierr)
!
   call mpi_comm_size(comm_mlicdsmg2fc_mlt,isize,ierr)
   call mpi_comm_rank(comm_mlicdsmg2fc_mlt,irank,ierr)
   if( isize-1>0 ) n_cadmas=isize-1
   call allocate_cadmas_0(ierr)
!
!  MLT_AGENTのランクを送る
   call mpi_allreduce(irank,iwork,1,mpi_integer,mpi_max, &
        &             comm_mlicdsmg2fc_mlt,ierr)
!
   m=1
   do n=0,isize-1
      if( n==irank ) cycle
      itag=80+n
      call mpi_irecv(iwork,1,mpi_integer,n,itag, &
           &         comm_mlicdsmg2fc_mlt,ireq,ierr)
      call mpi_wait(ireq,istat,ierr)
      if( iwork/=n ) call mpi_abort(mpi_comm_world,icode,ierr)
      region(m)%cadmas_rank=iwork
      m=m+1
   enddo
!
!
   if( region(1)%cadmas_rank>=0 ) then
! MPIによる受信
      do m=1,n_cadmas
      call mpi_irecv(region(m)%icmax,1,mpi_integer,region(m)%cadmas_rank &
           &        ,mpi_any_tag,comm_mlicdsmg2fc_mlt,ireq,ierr)
      call mpi_wait(ireq,istat,ierr)
!
      call mpi_irecv(region(m)%jcmax,1,mpi_integer,region(m)%cadmas_rank &
           &        ,mpi_any_tag,comm_mlicdsmg2fc_mlt,ireq,ierr)
      call mpi_wait(ireq,istat,ierr)
!
      !write(100,*) 'region=',m
      !write(100,*) 'icmax=',region(m)%icmax
      !write(100,*) 'jcmax=',region(m)%jcmax
!
      call allocate_cadmas_c(m,ierr)
      if(ierr<0) call errstop(rout,ierr)
!
      call mpi_irecv(region(m)%xc,region(m)%icmax+1,mpi_double_precision,region(m)%cadmas_rank &
           &        ,mpi_any_tag,comm_mlicdsmg2fc_mlt,ireq,ierr)
      call mpi_wait(ireq,istat,ierr)
!
      call mpi_irecv(region(m)%yc,region(m)%jcmax+1,mpi_double_precision,region(m)%cadmas_rank &
           &        ,mpi_any_tag,comm_mlicdsmg2fc_mlt,ireq,ierr)
      call mpi_wait(ireq,istat,ierr)
!
      n=region(m)%icmax*region(m)%jcmax
      call mpi_irecv(region(m)%height_c,n,mpi_real,region(m)%cadmas_rank &
           &        ,mpi_any_tag,comm_mlicdsmg2fc_mlt,ireq,ierr)
      call mpi_wait(ireq,istat,ierr)
!
      call mpi_irecv(region(m)%cadmas_tnext,1,mpi_real,region(m)%cadmas_rank &
                & ,mpi_any_tag,comm_mlicdsmg2fc_mlt,ireq,ierr)
      call mpi_wait(ireq,istat,ierr)
      if(region(m)%cadmas_tnext<0.0d0) region(m)%cadmas_tnext=time_end+dt
      enddo
!
! ファイル入力
   else
!
   do m=1,n_cadmas
    open(300+m,file=trim(cadfile(m)),status='old',form='unformatted',err=99)
!
!  CADMASでのメッシュ分割数
    read(300+m,err=10) region(m)%icmax,region(m)%jcmax
    goto 11
10  continue
      ierr=-10
      call errmsg(rout,ierr)
      write(*,*) 'read error : icmax,jcmax in data.ma'
11  continue
    !write(100,*) 'icmax=',region(m)%icmax
    !write(100,*) 'jcmax=',region(m)%jcmax
!
!  CADMASデータの受け取り用配列へのメモリ割り当て
    call allocate_cadmas_c(m,ierr)
     if(ierr<0) call errstop(rout,ierr)
!
!  CADMASでの領域分割位置(基点を含む)
    read(300+m,err=20) (region(m)%xc(i),i=0,region(m)%icmax)
    read(300+m,err=20) (region(m)%yc(j),j=0,region(m)%jcmax)
    goto 21
20  continue
      ierr=-20
      call errmsg(rout,ierr)
      write(*,*) 'read error : xc,yc in data.ma'
21  continue
!
!  地表高さの読み込み
    read(300+m,err=30) ((region(m)%height_c(i,j),i=1,region(m)%icmax),j=1,region(m)%jcmax)
    goto 31
30  continue
      ierr=-30
      call errmsg(rout,ierr)
      write(*,*) 'read error : height in data.ma'
31  continue
!
!  水位データ等の最初時刻の読み込み
    read(300+m,end=40,err=41) region(m)%cadmas_tnext
    goto 42
40  region(m)%cadmas_tnext=time_end+dt  !データが無いので、計算終了まで水深等を更新しない
    goto 42
41  continue
    ierr=-40
    call errmsg(rout,ierr)
    write(*,*) 'read error : time in data.ma'
42  continue
!
   enddo
!
   endif
!
!
!  解析領域の基点を[0,0]にするため、CADMASメッシュの座標データをシフトする
   do m=1,n_cadmas
    do i=0,region(m)%icmax
      region(m)%xc(i)=region(m)%xc(i)-xpin
    enddo
    do j=0,region(m)%jcmax
      region(m)%yc(j)=region(m)%yc(j)-ypin
    enddo
!
!  リセット
    region(m)%cadmas_tnow=0.0d0
    region(m)%depth_c(:,:)=0.0d0
    region(m)%uu_c(:,:)=0.0d0
    region(m)%vv_c(:,:)=0.0d0
   enddo
!
!  remeshしたCADMASデータを格納する配列へのメモリ割り当て
    call allocate_cadmas(ierr)
     if(ierr<0) call errstop(rout,ierr)
!
   call remesh(-1,m,ierr)
   do m=1,n_cadmas
!  CADMAS出力データをマルチエージェントメッシュのデータに変換する準備
    call remesh(0,m,ierr)
!  CADMAS出力データをマルチエージェントメッシュのデータに変換
    call remesh(1,m,ierr)  !地形データ
    call remesh(2,m,ierr)  !水深データ
    call remesh(3,m,ierr)  !流量データ
   enddo
!
!
!----------------------------------------
!  (1) 津波データの更新
!----------------------------------------
  elseif(iflag==1) then
!更新時刻以降であれば水深データ等の読み込み
  do m=1,n_cadmas
  do while(time>=region(m)%cadmas_tnext)
!  MPIによる受信
    if(region(m)%cadmas_rank>=0) then
      n=region(m)%icmax*region(m)%jcmax
      call mpi_irecv(region(m)%depth_c,n,mpi_real,region(m)%cadmas_rank &
           &        ,mpi_any_tag,comm_mlicdsmg2fc_mlt,ireq,ierr)
      call mpi_wait(ireq,istat,ierr)

      call mpi_irecv(region(m)%uu_c,n,mpi_real,region(m)%cadmas_rank &
           &        ,mpi_any_tag,comm_mlicdsmg2fc_mlt,ireq,ierr)
      call mpi_wait(ireq,istat,ierr)

      call mpi_irecv(region(m)%vv_c,n,mpi_real,region(m)%cadmas_rank &
           &        ,mpi_any_tag,comm_mlicdsmg2fc_mlt,ireq,ierr)
      call mpi_wait(ireq,istat,ierr)
!
      region(m)%cadmas_tnow=region(m)%cadmas_tnext
      call mpi_irecv(region(m)%cadmas_tnext,1,mpi_real,region(m)%cadmas_rank &
                 & ,mpi_any_tag,comm_mlicdsmg2fc_mlt,ireq,ierr)
      call mpi_wait(ireq,istat,ierr)
      if(region(m)%cadmas_tnext<0.0d0) region(m)%cadmas_tnext=time_end+dt
!
!  ファイル入力
    else
!    水深の読み込み
      read(300+m,err=50) ((region(m)%depth_c(i,j),i=1,region(m)%icmax),j=1,region(m)%jcmax)
      goto 51
50    continue
        ierr=-50
        call errmsg(rout,ierr)
        write(*,*) 'read error : depth in data.ma'
51    continue
!    流量uの読み込み
      read(300+m,err=60) ((region(m)%uu_c(i,j),i=1,region(m)%icmax),j=1,region(m)%jcmax)
      goto 61
60    continue
        ierr=-60
        call errmsg(rout,ierr)
        write(*,*) 'read error : uu in data.ma'
61    continue
!    流量vの読み込み
      read(300+m,err=70) ((region(m)%vv_c(i,j),i=1,region(m)%icmax),j=1,region(m)%jcmax)
      goto 71
70    continue
        ierr=-70
        call errmsg(rout,ierr)
        write(*,*) 'read error : vv in data.ma'
71    continue
!    次回の更新時刻を読み込む
      region(m)%cadmas_tnow=region(m)%cadmas_tnext
      read(300+m,end=80,err=81) region(m)%cadmas_tnext
      goto 82
80    region(m)%cadmas_tnext=time_end+dt  !データが無いので、計算終了まで水深等を更新しない
      goto 82
81    continue
      ierr=-80
      call errmsg(rout,ierr)
      write(*,*) 'read error : time in data.ma'
82    continue
!
    endif
!
!  CADMAS出力データをマルチエージェントメッシュのデータに変換
    if(time<region(m)%cadmas_tnext)then
      call remesh(2,m,ierr)  !水深データ
      call remesh(3,m,ierr)  !流量データ
      exit
    endif

  enddo
  enddo
!
!
!----------------------------------------
!  (2) ファイルクローズ
!----------------------------------------
  elseif(iflag==2) then
    close(300+m)
  endif
!
  return
!
99 continue
  ierr=-99
  call errmsg(rout,ierr)
  write(*,*) 'cannot open data.ma'
  return
end subroutine read_cadmas
