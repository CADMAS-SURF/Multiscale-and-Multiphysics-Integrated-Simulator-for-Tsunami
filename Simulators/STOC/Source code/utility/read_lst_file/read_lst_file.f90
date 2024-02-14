program read_lst_file
  implicit none
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!INPUT(from)!!!
  character(32),parameter:: lst_file='a.lst'
  character(32),parameter:: out_file='h.txt'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!INPUT(to)!!!!!
  integer,parameter:: in=11, out=12
!
  real(4),allocatable:: xx(:),yy(:),zz(:)
  real(4),allocatable:: hdep(:,:)
  real(4),allocatable:: phys(:,:,:)
!
  integer:: mx,my,mz
  integer:: mxm,mym,mzm
!
  integer:: isize,jsize,ksize,i,j,k
  integer:: ntime,nphys,istep
  real(4):: time
  character(8):: clist
  character(11):: cform
!
!
  open(in,file=lst_file,form='unformatted',status='old')
  open(out,file=out_file,form='formatted',status='unknown')
!
!--------------------------------------------------
! Header : 8 records (Axis of grid points & -water depth)
!--------------------------------------------------
  read(in) mxm
  mx=mxm+1
  allocate(xx(mxm))
  read(in) xx
!
  read(in) mym
  my=mym+1
  allocate(yy(mym))
  read(in) yy
!
  read(in) mzm
  mz=mzm+1
  allocate(zz(mzm))
  read(in) zz
!
  read(in) isize,jsize,ksize
  if( isize/=mxm .or. jsize/=mym .or. ksize/=1 ) stop 900
  allocate(hdep(mxm,mym))
  read(in) hdep
!
!
!--------------------------------------------------
! Results
!--------------------------------------------------
  ntime=0
  do
     ntime=ntime+1
!
     nphys=0
     do
        nphys=nphys+1
!
        read(in) clist,istep,time
        read(in) isize,jsize,ksize
        if( isize/=mxm .or. jsize/=mym ) stop 901
        write(*,'(a5,f9.2,1x,a5,i8,1x,a5,a8)') 'time=',time,'step=',istep,'phys=',clist
        allocate(phys(isize,jsize,ksize))
        read(in) phys
!
!
!
!------------------------------
!       Output water level (EXAMPLE)
!------------------------------
        if( clist=='H' ) then
           k=1
           do j=2,mym
           do i=2,mxm
              if( phys(i,j,k)-hdep(i,j)<1.e-4 ) phys(i,j,k)=-999.
           enddo
           enddo
!
           cform='(     f9.2)'
           write(cform(2:6),'(i5)') mxm-1
!
           write(out,'(a1,f9.2)') '#',time
           do j=mym,2,-1
              write(out,cform) (phys(i,j,k),i=2,mxm)
           enddo
!
           write(*,*) 'output',trim(clist)
        endif
!
!
!
        deallocate(phys)
     enddo
  enddo
!
  close(in)
  deallocate(xx,yy,zz,hdep)
!
end program read_lst_file
