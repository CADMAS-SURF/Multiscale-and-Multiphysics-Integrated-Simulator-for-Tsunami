program make_str_file
  implicit none
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!INPUT(from)!!!
  character(32),parameter:: infile ='depth.dat'
  character(32),parameter:: outfile='a.str'
  integer,parameter:: nx= 300         ! =mx-2 (x-dir. cell number without virtual cell)
  integer,parameter:: ny= 150         ! =my-2 (y-dir. cell number without virtual cell)
  integer,parameter:: nz= 1           ! =mz-2 (z-dir. cell number without virtual cell)
  real(8),parameter:: z(nz+1)=(/ &    ! axis of z-grid poit
                    & -10.0d0,30.0d0 &
                    &          /)
  real(8),parameter:: gvmin=1.0d-4    ! lower limit of porous value
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!INPUT(to)!!!!!
  integer,parameter:: in=11,out=12
  integer,parameter:: mode=0 !(=0: cell interface height is interpolated,
                             ! =1: cell interface height is equal to heigher value of nabor cells
  integer,parameter:: mx=nx+2, mxm=nx+1
  integer,parameter:: my=ny+2, mym=ny+1
  integer,parameter:: mz=nz+2, mzm=nz+1
!
  real(8):: dep(mx,my),depx(mx,my),depy(mx,my)
  integer:: indc(mx,my,mz)
  real(8):: gv(mx,my,mz)
  real(8):: gx(mx,my,mz)
  real(8):: gy(mx,my,mz)
  real(8):: gz(mx,my,mz)
  real(8):: hh(mx,my),amng(mx,my)
!
  integer:: i,j,k,n
!
!
!--------------------------------------------------
! (1) read depth file
!--------------------------------------------------
  open(in,file=infile,status='old')
!
  dep(:,:)=1.d+10
!
  read(in,*)                           ! skip 1 line
  do j=mym,2,-1                        ! read from north to south
     read(in,*) (dep(i,j),i=2,mxm)     ! read from west  to east
  enddo
  close(in)
!
! west  & east  boundary
  do j=1,my
     i=1
     dep(i,j)=dep(i+1,j)
     i=mx
     dep(i,j)=dep(i-1,j)
  enddo
!
! south & north boundary
  do i=1,mx
     j=1
     dep(i,j)=dep(i,j+1)
     j=my
     dep(i,j)=dep(i,j-1)
  enddo
!
!
! set depth at cell interface
  depx(:,:)=1.d+10
  depy(:,:)=1.d+10
  do j=2,mym
  do i=1,mxm
     depx(i,j)=0.5d0*(dep(i,j)+dep(i+1,j))
     depx(i,j)=min(max(depx(i,j),z(1)),z(mzm))
  enddo
  enddo
!
  do j=1,mym
  do i=2,mxm
     depy(i,j)=0.5d0*(dep(i,j)+dep(i,j+1))
     depy(i,j)=min(max(depy(i,j),z(1)),z(mzm))
  enddo
  enddo
!
! clip
  do j=1,my
  do i=1,mx
     dep(i,j)=min(max(dep(i,j),z(1)),z(mzm))
  enddo
  enddo
!
!
!--------------------------------------------------
! (2) initialize
!--------------------------------------------------
  indc=1
  gv=1.0d0
  gx=1.0d0
  gy=1.0d0
  gz=1.0d0
  hh=0.0d0      ! initial water level
  amng=0.025d0  ! manning
!
!
!--------------------------------------------------
! (3) make porous value(gv,gx,gy) and cell flag(indc)
!--------------------------------------------------
!
! set indc & gv
  do j=2,mym
  do i=2,mxm
     do k=2,mzm
        if( z(k)<=dep(i,j) ) then        ! under the bottom of sea -> not calculate
           indc(i,j,k)=0
           gv(i,j,k)=1.0d0
        else if( z(k-1)>dep(i,j) ) then  ! above the bottom of sea
           indc(i,j,k)=1
           gv(i,j,k)=1.0d0
        else                             ! at    the bottom of sea
           indc(i,j,k)=1
           gv(i,j,k)=(z(k)-dep(i,j))/(z(k)-z(k-1))
!
           if( nz>1.and.gv(i,j,k)<gvmin ) then
              indc(i,j,k) = 0
              gv(i,j,k) = 1.0d0
              dep(i,j)  = z(k)
           end if
        endif
     enddo
  enddo
enddo
!
! set gx
  do j=2,mym
  do i=1,mxm
     do k=2,mzm
!
        if( indc(i,j,k)==0 ) then        ! left  side  cell  is obstacle
           gx(i,j,k) = 1.0d0
        else if( indc(i+1,j,k)==0 ) then ! right side  cell  is obstacle
           gx(i,j,k) = 1.0d0
        else                             ! both  sides cells are fluid
           if( z(k-1)>depx(i,j) ) then   ! above the bottom of sea
              gx(i,j,k)=1.0d0
           else                          ! at    the bottom of sea
              if( mode==0 ) gx(i,j,k)=(z(k)-depx(i,j))/(z(k)-z(k-1)) ! smooth
              if( mode==1 ) gx(i,j,k)=min(gv(i,j,k),gv(i+1,j,k))     ! step
           endif
        end if
     enddo
  enddo
  enddo
!
! set gy
  do j=1,mym
  do i=2,mxm
     do k=2,mzm
!
        if( indc(i,j,k)==0 ) then        ! left  side  cell  is obstacle
           gy(i,j,k) = 1.0d0
        else if( indc(i,j+1,k)==0 ) then ! right side  cell  is obstacle
           gy(i,j,k) = 1.0d0
        else                             ! both  sides cells are fluid
           if( z(k-1)>depy(i,j) ) then   ! above the bottom of sea
              gy(i,j,k)=1.0d0
           else                          ! at    the bottom of sea
              if( mode==0 ) gy(i,j,k)=(z(k)-depy(i,j))/(z(k)-z(k-1)) ! smooth
              if( mode==1 ) gy(i,j,k)=min(gv(i,j,k),gv(i,j+1,k))     ! step
           endif
        end if
     enddo
  enddo
  enddo
!
! set hh
  do j=2,mym
  do i=2,mxm
     hh(i,j)=max(hh(i,j),dep(i,j))
  enddo
  enddo
!
!
!--------------------------------------------------
! (4) write str file
!--------------------------------------------------
  open(out,file=outfile,form='unformatted',status='unknown')
  write(out) nx,ny,nz
  write(out) (((indc(i,j,k),i=2,mxm),j=2,mym),k=2,mzm)
  write(out) (((gv(i,j,k),i=2,mxm),j=2,mym),k=2,mzm)
  write(out) (((gx(i,j,k),i=1,mxm),j=2,mym),k=2,mzm)
  write(out) (((gy(i,j,k),i=2,mxm),j=1,mym),k=2,mzm)
  write(out) (((gz(i,j,k),i=2,mxm),j=2,mym),k=1,mzm)
!
  write(out) ((dep (i,j),i=2,mxm),j=2,mym)
  write(out) ((hh  (i,j),i=2,mxm),j=2,mym)
  write(out) ((amng(i,j),i=2,mxm),j=2,mym)
  close(out)
!
  stop
end program make_str_file
