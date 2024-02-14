real(8) function pot_total(n,i,j,ierr)
!----------------------------------------
! 考慮するポテンシャルを重みをつけて足し合わせる
!----------------------------------------
  use m_potential,only: n_shelter,n_potential &
                     & ,n_mob,pot_mob_revise
  use m_agent,only: weight_shelter,weight_mob,weight_danger
  implicit none
!
![arguments]
  integer,intent(in):: n,i,j
  integer,intent(out):: ierr
!
![local variables]
  character(32):: rout='pot_total'
!
  ierr=0
!
!
  pot_total=0.0d0
!
!避難経路ポテンシャル
  if(n_shelter>0)then
     pot_total=pot_total+n_potential(n,i,j)*weight_shelter(n)
  endif
!
!群衆ポテンシャル
  if(n_mob==1)then
     call make_potential_mob_revise(n,ierr)
      if(ierr<0) call errstop(rout,ierr)
     pot_total=pot_total+pot_mob_revise(i,j)*weight_mob(n)
  endif

  return
end function pot_total
