program make_oil_particle
  use mod_dmfile,only: init_dmfile,read_dmtxtfile &
       & ,xd,yd,xdo,ydo &
       & ,coord,utm_mid_longitude,geodetic_system
  use mod_oilfile,only: init_oilfile,vol
  use mod_fault,only: set_param_fault,set_utm,en2lb,r2d
  implicit none
  character(64),parameter:: subname='make_oil_particle'
  character(16),parameter:: input_file='oil.dat'
  character(16),parameter:: output_file='part_init.txt'
  integer,parameter:: handle=11
!
  integer,allocatable:: ndbr(:)
  real(8),allocatable:: tout(:)
  real(8),allocatable:: param(:,:)
!
  character(64):: line
  integer:: m,n,nn,nspill,npart,from,to,eof,ierr
  real(8):: v,q,ts,dt
  real(8):: time,timeold,c1,c2,xout,yout,lout,bout
!
!
! (1) STOC-DMのinput.datとdrift.txtの最初の時刻分を読み込む
  call init_dmfile
!
! (2) OIL-PARIのdata.in_oilを読み込む
  call init_oilfile
!
! (3) oil.datを読み込む
  open(handle,file=input_file,form='formatted',action='read')
!
! (3-1) まず、油粒子の総数をカウントする
  nspill=0
  npart=0
  do
     read(handle,'(a64)',iostat=ierr) line
     if(ierr<0) exit
!
     n=index(line,'#')
     if(n>0) line(n:)=''
!
     if( line=='' ) cycle
!
     n=index(line,':')
     if(n>0) then
        read(line(:n-1),*) from
        read(line(n+1:),*,iostat=ierr) to,v,q,ts
        if(ierr/=0) call sub_err(subname,1)
     else
        read(line,*,iostat=ierr) from,v,q,ts
        if(ierr/=0) call sub_err(subname,2)
        to=from
     endif
!
     nspill=nspill+to-from+1
     npart=npart+nint(v/vol)*dble(to-from+1)
!debug     write(*,*) 'from,to=',from,to
  enddo
  write(*,*) 'number of spilt debris is',nspill
  write(*,*) 'number of particles is',npart
!
  rewind(handle)
!
! (3-2) 油粒子分の配列を確保して、配列に流出元の漂流物番号と流出時刻を格納する
  allocate(ndbr(npart),tout(npart),param(2,npart),stat=ierr)
  if(ierr/=0) call sub_err(subname,3)
!
  ndbr(:)=0
  tout(:)=0
!
  npart=0
  do
     read(handle,'(a64)',iostat=ierr) line
     if(ierr<0) exit
!
     n=index(line,'#')
     if(n>0) line(n:)=''
!
     if( line=='' ) cycle
!
     n=index(line,':')
     if(n>0) then
        read(line(:n-1),*) from
        read(line(n+1:),*,iostat=ierr) to,v,q,ts
        if(ierr/=0) call sub_err(subname,1)
     else
        read(line,*,iostat=ierr) from,v,q,ts
        if(ierr/=0) call sub_err(subname,2)
        to=from
     endif
!
     do n=from,to
        nn=nint(v/vol)
        dt=vol/q
        do m=1,nn
           npart=npart+1
           ndbr(npart)=n
           tout(npart)=ts+dt*(m-1)
        enddo
     enddo
  enddo
!
  close(handle)
!
! (3-3) 流出時刻の順にデータをソートする
  param(1,:)=tout(:)
  param(2,:)=dble(ndbr(:))
  call hsort(param,2,npart,1)
  tout(:)=param(1,:)
  ndbr(:)=nint(param(2,:))
!
! (3-4) drift.txtを読み込みながら、各油粒子の流出位置をファイルに出力する
  call set_param_fault
  if( coord==0 ) then
     call set_utm(utm_mid_longitude)
  endif
!
  open(handle,file=output_file,form='formatted',action='write')
!
  time=-1.d10
!
  n=0
  do
     if( n==npart ) exit
!
     timeold=time
     call read_dmtxtfile(time,eof)
     if(eof/=0) exit
!
     do
        if( timeold<tout(n+1).and.tout(n+1)<=time ) then
!
           n=n+1
           if( timeold==-1.d10 ) then
              c1=0.0d0
           else
              c1=(time-tout(n))/(time-timeold)
           endif
           c2=1.d0-c1
!
           m=ndbr(n)
           xout=c1*xdo(m)+c2*xd(m)
           yout=c1*ydo(m)+c2*yd(m)
!
!debug           write(99,'(i5,2f12.3,f9.2)') m,xout,yout,tout(n)
!
           if( coord/=-1 ) then
              call en2lb(xout,yout,lout,bout,coord,geodetic_system)
              xout=lout*r2d
              yout=bout*r2d
           endif
!
           write(handle,'(1p,3e15.8)') xout,yout,tout(n)
!
           if( n==npart ) exit
        else
           exit
        endif
     enddo
!
  enddo
!
  close(handle)
!
  write(*,*) ''
  write(*,*) trim(output_file),' file is generated'
  write(*,*) ''
  write(*,*) '### Change NUM value of %PARTICLE block in data.in_oil file'
  write(*,*) 'NUM = ',npart
  write(*,*) 'done'
!
  stop
!
!
contains
  subroutine sub_err(subname,code)
!//////////////////////////////////////////////////
! エラー発生に終了処理を行う
!//////////////////////////////////////////////////
    character(*),intent(in):: subname
    integer,intent(in):: code
!
    write(*,*) ''
    write(*,*) 'Stop at subrouine ',trim(subname)
    write(*,*) ' source file name=make_oil_particle.f90'
    write(*,*) '       erorr code=',code
    stop
  end subroutine sub_err
!
end program make_oil_particle
