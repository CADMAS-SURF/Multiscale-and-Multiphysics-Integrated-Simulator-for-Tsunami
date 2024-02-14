subroutine make_potential_mob(ierr)
!----------------------------------------
! 群衆ポテンシャルの設定
!----------------------------------------
  use m_potential,only: ipmax,jpmax,dxy &
                     & ,r_mob,nsum_agent,pot_mob
  use m_agent,only: n_agent,i_agent,j_agent,agent_status
  implicit none
!
![arguments]
  integer,intent(out):: ierr
!
![local variables]
  integer:: n,i,j,ip,jp,is,js,ie,je,ir2,irmob,irmob2
  real(8):: rmobdxy,rr
!  character(32):: rout='make_potential_mob'
!
  ierr=0
!
  rmobdxy=r_mob/dxy
  irmob =int(rmobdxy)
  irmob2=int(rmobdxy**2)
!
!
!各セル内の生存エージェント数を計算
  nsum_agent(:,:)=0
  do n=1,n_agent
    if(agent_status(n)/=3) &
     & nsum_agent(i_agent(n),j_agent(n))=nsum_agent(i_agent(n),j_agent(n))+1
  enddo
!
!
!群衆ポテンシャルの計算(範囲r_mob内に0人ならpot_mob=0，n人なら距離rによりpot_mob=-(n1/r1+n2/r2+…))
  pot_mob(:,:)=0.0d0
  do jp=1,jpmax
  do ip=1,ipmax
    is=ip-irmob
      if(is<1) is=1
    ie=ip+irmob
      if(ie>ipmax) ie=ipmax
    js=jp-irmob
      if(js<1) js=1
    je=jp+irmob
      if(je>jpmax) je=jpmax
    do j=js,je
    do i=is,ie
      ir2=(ip-i)**2+(jp-j)**2
      if(ir2<=irmob2)then
        if(ir2==0)then
          rr=dxy
        else
          rr=sqrt(dble(ir2))*dxy
        endif
        pot_mob(ip,jp)=pot_mob(ip,jp)-dble(nsum_agent(i,j))/rr
      endif
    enddo
    enddo
  enddo
  enddo
!
  return
end subroutine make_potential_mob
