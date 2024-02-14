program read_str_file
  implicit none
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!INPUT(from)!!!
  character(20):: strfile='a.str'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!INPUT(to)!!!!!
  integer,parameter:: in=11
!
  integer,allocatable:: indc(:,:,:)
  real(8),allocatable:: gv(:,:,:)
  real(8),allocatable:: gx(:,:,:)
  real(8),allocatable:: gy(:,:,:)
  real(8),allocatable:: gz(:,:,:)
  real(8),allocatable:: hh(:,:),hdep(:,:),amng(:,:)
  integer,allocatable:: idst(:,:)
  real(8),allocatable:: htdst1(:,:),htdst2(:,:)
!
  integer:: nx,ny,nz,mxm,mym,mzm,mx,my,mz
  integer:: i,j,k
 real(8):: hmax,hmin,rmax,rmin,zmax,zmin
!
!
  open(in,file=strfile,status='old',form='unformatted')
!
  read(in) nx,ny,nz
  mxm=nx+1
  mym=ny+1
  mzm=nz+1
  mx =nx+2
  my =ny+2
  mz =nz+2
!
  allocate(indc(mx,my,mz),gv(mx,my,mz),gx(mx,my,mz),gy(mx,my,mz),gz(mx,my,mz) &
       &  ,hh(mx,my),hdep(mx,my),amng(mx,my),idst(mx,my),htdst1(mx,my),htdst2(mx,my))
  indc=0
  gv=0.0d0
  gx=0.0d0
  gy=0.0d0
  gz=0.0d0
  hh=0.0d0
  hdep=0.0d0
  amng=0.0d0
  idst=0
  htdst1=0.0d0
  htdst2=0.0d0
!
  read(in) (((indc(i,j,k),i=2,mxm),j=2,mym),k=2,mzm)
  read(in) (((gv(i,j,k),i=2,mxm),j=2,mym),k=2,mzm)
  read(in) (((gx(i,j,k),i=1,mxm),j=2,mym),k=2,mzm)
  read(in) (((gy(i,j,k),i=2,mxm),j=1,mym),k=2,mzm)
  read(in) (((gz(i,j,k),i=2,mxm),j=2,mym),k=1,mzm)
  read(in) ((hdep(i,j),i=2,mxm),j=2,mym)
  read(in) ((hh  (i,j),i=2,mxm),j=2,mym)
  read(in) ((amng(i,j),i=2,mxm),j=2,mym)
!
  read(in,end=10) ((idst(i,j),i=2,mxm),j=2,mym)
  read(in) ((htdst1(i,j),i=2,mxm),j=2,mym)
  read(in) ((htdst2(i,j),i=2,mxm),j=2,mym)
  write(*,*) 'available in stoc-ds-mode'
!
10 continue
  write(*,*) 'read end'
!
  hmax=-1.0d10
  hmin=1.0d010
  rmax=-1.0d10
  rmin=1.0d10
  zmax=-1.0d10
  zmin=1.0d010
  do j=2,mym
  do i=2,mxm
     hmax=max(hmax,hdep(i,j))
     hmin=min(hmin,hdep(i,j))
     rmax=max(rmax,amng(i,j))
     rmin=min(rmin,amng(i,j))
     zmax=max(zmax,hh(i,j))
     zmin=min(zmin,hh(i,j))
  enddo
  enddo
  write(*,*) 'hmax=',hmax
  write(*,*) 'hmin=',hmin
  write(*,*) 'rough max=',rmax
  write(*,*) 'rough min=',rmin
  write(*,*) 'zmax=',zmax
  write(*,*) 'zmin=',zmin
!
  deallocate(indc,gv,gx,gy,gz,hh,hdep,amng,idst,htdst1,htdst2)
  close(in)
  stop
!
end program read_str_file
