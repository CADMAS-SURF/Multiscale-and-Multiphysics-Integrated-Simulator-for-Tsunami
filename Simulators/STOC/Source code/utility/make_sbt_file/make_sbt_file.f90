program make_sbt_file
  use mod_fault,only: set_param_fault,displace,en2lb,d2r,r2d
  implicit none
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!INPUT(from)!!!
  character(32),parameter:: infile ='fault.txt'
  character(32),parameter:: outfile='a.sbt'
  integer,parameter:: nx= 80          ! =mx-2 (x-dir. cell number without virtual cell)
  integer,parameter:: ny= 80          ! =my-2 (y-dir. cell number without virtual cell)
  real(8),parameter:: x0=-300000.d0   ! =x(1) at %grid block in area file
  real(8),parameter:: y0=-660000.d0   ! =y(1) at %grid block in area file
  real(8),parameter:: dx=12500.d0     ! mesh size [m]
  real(8),parameter:: xor=143.1d0     ! origin (Longitude)
  integer,parameter:: ig=10           !(=0:UTM,>1:Japan-Plane-Rectangular)
  integer,parameter:: id= 2           !(=1:Tokyo Datum,=2:JGD2000)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!INPUT(to)!!!!!
  integer,parameter:: in=11,out=12
!
  integer:: i,j
  real(8):: x,y
  real(8):: length,width,depth,strike,dip,slip,disp,b,l,time
  real(8):: larray(nx,ny)
  real(8):: barray(nx,ny)
  real(8):: delh(nx,ny),xor1
  real(8):: sum(nx,ny)
  real(8):: param(10),param2(6)
!
!     
! (1) set parameters
  call set_param_fault
!
!
! (2) calc coordinate
  larray(:,:)=0.d0
  barray(:,:)=0.d0
!
  do j=1,ny
  do i=1,nx
     x=x0+dx*(dble(i)-0.5d0)
     y=y0+dx*(dble(j)-0.5d0)
     call en2lb(x,y,l,b,ig,id)
     larray(i,j)=l
     barray(i,j)=b
  enddo
  enddo
!
!
! (3) calc deformation
  open(in,file=infile,form='formatted',status='old')
  open(out,file=outfile,form='formatted',status='unknown')
  sum(:,:)=0.d0
!
  do
     read(in,*,end=100) length,width,depth,strike,dip,slip,disp,b,l,time
!
     strike=strike*d2r
     dip   =dip   *d2r
     slip  =slip  *d2r
     b     =b     *d2r
     l     =l     *d2r
     xor1  =xor   *d2r
!
     param(1)=length
     param(2)=width
     param(3)=depth
     param(4)=strike
     param(5)=dip
     param(6)=slip
     param(7)=disp
     param(8)=b
     param(9)=l
     param(10)=time
!
     param2(1)=sin(dip)
     param2(2)=cos(dip)
     param2(3)=sin(slip)
     param2(4)=cos(slip)
     param2(5)=sin(strike)
     param2(6)=cos(strike)
!
     delh(:,:)=0.d0
!
     do j=1,ny
     do i=1,nx
        call displace(larray(i,j),barray(i,j),xor1,param,param2,delh(i,j))
     enddo
     enddo
!
     write(out,*) time,2,nx+1,2,ny+1
     do j=1,ny
        write(out,'(<nx>f8.3)') (delh(i,j),i=1,nx)
     enddo
!
     sum(:,:)=sum(:,:)+delh(:,:)
  enddo
100 continue
!
!
  write(*,*) 'max deform(uplift)    =',maxval(sum)
  write(*,*) 'min deform(subsidence)=',minval(sum)
!
  stop
end program make_sbt_file
