subroutine make_signpost(ierr)
!----------------------------------------
! 道標の設定
!----------------------------------------
  use m_potential,only: ipmax,jpmax,dxy,n_signpost &
                     & ,i_signpost,j_signpost      &
                     & ,r_signpost,narea_signpost
  use m_agent,only: n_agent,weight_signpost,iflag_signpost
  implicit none
!
![arguments]
  integer,intent(out):: ierr
!
![local variables]
  integer:: n,m,i,j,is,js,ie,je,irsign
  real(8):: rand
  integer:: ierror
  character(32):: rout='make_signpost'
!
  ierr=0
!
!
!各エージェントが各標識に従うかどうかを設定
  allocate(iflag_signpost(0:n_signpost,n_agent),stat=ierror)
  if(ierror/=0) then
     ierr=-10
     call errmsg(rout,ierr)
     write(*,*) 'cannot allocate iflag_signpost arrays'
  endif
!
  do n=1,n_agent
    do m=1,n_signpost
      call random_number(rand)
      if(rand<=weight_signpost(n))then
        iflag_signpost(m,n)=1    !道標に従う
      else
        iflag_signpost(m,n)=0    !道標を無視
      endif
    enddo
        iflag_signpost(0,n)=0    !道標範囲外なら当然無視
  enddo
!
!
!領域内での各道標の有効域を設定
  narea_signpost(:,:)=0
  do n=1,n_signpost
    irsign =int(r_signpost(n)/dxy)
    is=i_signpost(n)-irsign
      if(is<1) is=1
    ie=i_signpost(n)+irsign
      if(ie>ipmax) ie=ipmax
    js=j_signpost(n)-irsign
      if(js<1) js=1
    je=j_signpost(n)+irsign
      if(je>jpmax) je=jpmax
    do j=js,je
    do i=is,ie
      if((i_signpost(n)-i)**2+(j_signpost(n)-j)**2<=irsign**2) &
       & narea_signpost(i,j)=n
    enddo
    enddo
  enddo
!
![DebugWrite]標識の有効範囲をmap_signpost.csvに出力
  open(99,file='map_signpost.csv')
  do j=jpmax,1,-1
     write(99,'(10000(i5,'',''))') (narea_signpost(i,j),i=1,ipmax)
  enddo
  close(99)
!
  return
end subroutine make_signpost
