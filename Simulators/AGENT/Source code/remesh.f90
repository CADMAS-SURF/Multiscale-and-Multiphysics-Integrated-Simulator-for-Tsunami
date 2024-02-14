subroutine remesh(iflag,ireg,ierr)
!----------------------------------------
! CADMAS-SURF出力データをマルチエージェントメッシュのデータに変換
!----------------------------------------
  use m_potential,only: ipmax,jpmax,dxy
  use m_cadmas,only: n_cadmas,region &
                  & ,height,  depth,  uu,  vv&
                  & ,max_depth
  implicit none
!
!  [arguments]
  integer,intent(in):: iflag
  integer,intent(in):: ireg ! 補間元の領域番号
  integer,intent(out):: ierr
!
!  [local variables]
  integer::ierror
  character(32):: rout='remesh'
  integer:: i,j,ip,jp,m,ii,jj
  real(8):: x1,x2,y1,y2,coef1,coef2
  real(8):: x,y,xp,yp,xm,ym,f00,f10,f01,f11,cx,cy
  integer,save,allocatable:: im(:,:) ! 補間に用いる領域番号
  real(8),save,allocatable:: ia(:,:),ib(:,:),ic(:,:),id(:,:) &
                          & ,ja(:,:),jb(:,:),jc(:,:),jd(:,:) &
                          & ,coeff_x(:,:),coeff_y(:,:)
!
  ierr=0
  m=ireg
!
!
!----------------------------------------
!  (0) 変換条件を設定
!----------------------------------------
  if(iflag==-1) then
!
  allocate(im(ipmax,jpmax) &
       &  ,ia(ipmax,jpmax),ib(ipmax,jpmax),ic(ipmax,jpmax),id(ipmax,jpmax) &
        & ,ja(ipmax,jpmax),jb(ipmax,jpmax),jc(ipmax,jpmax),jd(ipmax,jpmax) &
        & ,coeff_x(ipmax,jpmax),coeff_y(ipmax,jpmax),stat=ierror)
  if(ierror/=0) then
     ierr=-10
     call errmsg(rout,ierr)
     write(*,*) 'cannot allocate remesh data arrays'
  endif
!
  im(:,:)=0
!
  elseif(iflag==0) then
!remesh用の変数を計算しておく
  do jp=1,jpmax
  do ip=1,ipmax
    yp=dxy*(dble(jp)-0.5d0)
    xp=dxy*(dble(ip)-0.5d0)
!
    ii=-1
    do i=0,region(m)%icmax 
      if( i==0 ) then
        x1=region(m)%xc(i)
      else
        x1=(region(m)%xc(i-1)+region(m)%xc(i  ))*0.5d0
      endif
      if( i==region(m)%icmax ) then
        x2=region(m)%xc(i  )
      else
        x2=(region(m)%xc(i  )+region(m)%xc(i+1))*0.5d0
      endif
!
      if(x1<=xp.and.xp<=x2) then
        ii=i
        coef1=(xp-x1)/(x2-x1)
        exit
      endif
    enddo
!
    jj=-1
    do j=0,region(m)%jcmax +1
      if( j==0 ) then
        y1=region(m)%yc(j)
      else
        y1=(region(m)%yc(j-1)+region(m)%yc(j  ))*0.5d0
      endif
      if( j==region(m)%jcmax ) then
        y2=region(m)%yc(j  )
      else
        y2=(region(m)%yc(j  )+region(m)%yc(j+1))*0.5d0
      endif
!
      if(y1<=yp.and.yp<=y2) then
        jj=j
        coef2=(yp-y1)/(y2-y1)
        exit
      endif
    enddo
!
    if( ii>=0.and.jj>=0 ) then
       im(ip,jp)=m
!
       !ia(ip,jp)=max(ii-1,1)
       !ib(ip,jp)=min(ii  ,region(m)%icmax)
       !ic(ip,jp)=max(ii-1,1)
       !id(ip,jp)=min(ii  ,region(m)%icmax)
       !coeff_x(ip,jp)=coef1
!
       !ja(ip,jp)=min(jj  ,region(m)%jcmax)
       !jb(ip,jp)=min(jj  ,region(m)%jcmax)
       !jc(ip,jp)=max(jj-1,1)
       !jd(ip,jp)=max(jj-1,1)
       !coeff_y(ip,jp)=coef2
!
       !!! modify ver3.4
       ia(ip,jp)=max(ii,1)
       ib(ip,jp)=min(ii+1  ,region(m)%icmax)
       ic(ip,jp)=max(ii,1)
       id(ip,jp)=min(ii+1  ,region(m)%icmax)
       coeff_x(ip,jp)=coef1
!
       ja(ip,jp)=min(jj+1  ,region(m)%jcmax)
       jb(ip,jp)=min(jj+1  ,region(m)%jcmax)
       jc(ip,jp)=max(jj,1)
       jd(ip,jp)=max(jj,1)
       coeff_y(ip,jp)=coef2
       !write(123,*)ip,jp
       !write(123,*)ia(ip,jp),ib(ip,jp),ic(ip,jp),id(ip,jp),coeff_x(ip,jp)
       !write(123,*)ja(ip,jp),jb(ip,jp),jc(ip,jp),jd(ip,jp),coeff_y(ip,jp)
    endif
  enddo
  enddo
!
  if(m==n_cadmas) then
    do jp=1,jpmax
    do ip=1,ipmax
      if(im(ip,jp)==0) then
        write(*,*) 'Error in remesh (iflag==0)'
        write(*,*) '   im is not defined at ',ip,jp
        ierr=-30
      endif
    enddo
    enddo
  endif
!
!
!----------------------------------------
!  (1) 地形データを変換
!----------------------------------------
  elseif(iflag==1) then
!

  do jp=1,jpmax
  do ip=1,ipmax
    if( m/=im(ip,jp) ) cycle
    f01=region(m)%height_c(ia(ip,jp),ja(ip,jp))
    f11=region(m)%height_c(ib(ip,jp),jb(ip,jp))
    f00=region(m)%height_c(ic(ip,jp),jc(ip,jp))
    f10=region(m)%height_c(id(ip,jp),jd(ip,jp))
    cx=coeff_x(ip,jp)
    cy=coeff_y(ip,jp)
    height(ip,jp)=(1.d0-cx)*(1.d0-cy)*f00 + cx*(1.d0-cy)*f10 &
         &       +(1.d0-cx)*      cy *f01 + cx*      cy *f11
  enddo
  enddo

  write(100,*)"height_c"
  do j=jpmax,1,-1
    do i=1,ipmax
      write(100,'(f16.4)',advance='no')region(1)%height_c(i,j)
    enddo
    write(100,*)
  enddo

  write(100,*)"height (remesh)"
  do j=jpmax,1,-1
    do i=1,ipmax
      write(100,'(f16.4)',advance='no')height(i,j)
    enddo
    write(100,*)
  enddo
!
!
!----------------------------------------
!  (2) 水深データを変換
!----------------------------------------
  elseif(iflag==2) then
!
  do jp=1,jpmax
  do ip=1,ipmax
    if( m/=im(ip,jp) ) cycle
    f01=region(m)%depth_c(ia(ip,jp),ja(ip,jp))
    f11=region(m)%depth_c(ib(ip,jp),jb(ip,jp))
    f00=region(m)%depth_c(ic(ip,jp),jc(ip,jp))
    f10=region(m)%depth_c(id(ip,jp),jd(ip,jp))
    cx=coeff_x(ip,jp)
    cy=coeff_y(ip,jp)
    depth(ip,jp)=(1.d0-cx)*(1.d0-cy)*f00 + cx*(1.d0-cy)*f10 &
         &      +(1.d0-cx)*      cy *f01 + cx*      cy *f11
    if(max_depth(ip,jp) < depth(ip,jp))then
       max_depth(ip,jp) = depth(ip,jp)
    endif
  enddo
  enddo
!
!
!----------------------------------------
!  (3) 流量データを変換
!----------------------------------------
  elseif(iflag==3) then
!
  do jp=1,jpmax
  do ip=1,ipmax
    if( m/=im(ip,jp) ) cycle
    f01=region(m)%uu_c(ia(ip,jp),ja(ip,jp))
    f11=region(m)%uu_c(ib(ip,jp),jb(ip,jp))
    f00=region(m)%uu_c(ic(ip,jp),jc(ip,jp))
    f10=region(m)%uu_c(id(ip,jp),jd(ip,jp))
    cx=coeff_x(ip,jp)
    cy=coeff_y(ip,jp)
    uu(ip,jp)=(1.d0-cx)*(1.d0-cy)*f00 + cx*(1.d0-cy)*f10 &
         &   +(1.d0-cx)*      cy *f01 + cx*      cy *f11
  enddo
  enddo
!
  do jp=1,jpmax
  do ip=1,ipmax
    if( m/=im(ip,jp) ) cycle
    f01=region(m)%vv_c(ia(ip,jp),ja(ip,jp))
    f11=region(m)%vv_c(ib(ip,jp),jb(ip,jp))
    f00=region(m)%vv_c(ic(ip,jp),jc(ip,jp))
    f10=region(m)%vv_c(id(ip,jp),jd(ip,jp))
    cx=coeff_x(ip,jp)
    cy=coeff_y(ip,jp)
    vv(ip,jp)=(1.d0-cx)*(1.d0-cy)*f00 + cx*(1.d0-cy)*f10 &
         &   +(1.d0-cx)*      cy *f01 + cx*      cy *f11
  enddo
  enddo
!
  endif
!
  return
end subroutine remesh
