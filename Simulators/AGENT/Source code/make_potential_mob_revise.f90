subroutine make_potential_mob_revise(n,ierr)
!----------------------------------------
! エージェント自身を含まない群衆ポテンシャルを計算
!----------------------------------------
  use m_potential,only: ipmax,jpmax,dxy &
                     & ,r_mob,pot_mob,pot_mob_revise
  use m_agent,only: i_agent,j_agent
  implicit none
!
![arguments]
  integer,intent(out):: ierr
!
![local variables]
  integer:: n,i,j,is,js,ie,je,ir2,irmob,irmob2
  real(8):: rmobdxy,rr
!  character(32):: rout='make_potential_mob_revise'
!
  ierr=0
!
  rmobdxy=r_mob/dxy
  irmob =int(rmobdxy)
  irmob2=int(rmobdxy**2)
!
!
!第nエージェントによる群衆ポテンシャル相当分をpot_mobから差し引いてpot_mob_reviseとする
  pot_mob_revise(:,:)=pot_mob(:,:)
  is=i_agent(n)-irmob
    if(is<1) is=1
  ie=i_agent(n)+irmob
    if(ie>ipmax) ie=ipmax
  js=j_agent(n)-irmob
    if(js<1) js=1
  je=j_agent(n)+irmob
    if(je>jpmax) je=jpmax
  do j=js,je
  do i=is,ie
    ir2=(i_agent(n)-i)**2+(j_agent(n)-j)**2
    if(ir2<=irmob2)then
      if(ir2==0)then
        rr=dxy
      else
        rr=sqrt(dble(ir2))*dxy
      endif
      pot_mob_revise(i,j)=pot_mob(i,j)+1.0d0/rr
    endif
  enddo
  enddo
!
  return
end subroutine make_potential_mob_revise
