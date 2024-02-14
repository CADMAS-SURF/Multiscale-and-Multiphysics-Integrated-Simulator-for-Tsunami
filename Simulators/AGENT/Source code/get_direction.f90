subroutine get_direction(n,theta,ierr)
!----------------------------------------
! エージェントの移動方向を指定
!----------------------------------------
  use m_potential,only: ipmax,jpmax,dxy,move_boundary
  use m_agent,only: i_agent,j_agent,agent_x,agent_y
  implicit none
!
![arguments]
  integer,intent(in)::  n
  real(8),intent(out):: theta
  integer,intent(out):: ierr
!
![local variables]
  integer:: i,j,i_target,j_target
  real(8),parameter:: pi=3.141592653589793d0
  real(8),parameter:: root2=1.414213562373095d0
  real(8):: pot,potnow,potmin,target_dx,target_dy
  real(8),external:: pot_total
  character(32):: rout='get_direction'
!
  ierr=0
!
!
  potnow=pot_total(n,i_agent(n),j_agent(n),ierr)  !現在のセルのポテンシャル
!
!進行可能方向にあるポテンシャル最小位置を探す(周囲8方向探索Ver.)
!!Reset:周囲より現在のセルのポテンシャルが低い場合
  potmin=potnow
  i_target=i_agent(n)
  j_target=j_agent(n)
!!左方向探索
  i=i_agent(n)-1; j=j_agent(n)
  if(i<1)then
     !skip
  elseif(move_boundary(i,j)==9999d0)then
     !skip
  else
     pot=pot_total(n,i,j,ierr)
     if(pot<potmin)then
        potmin=pot
        i_target=i
        j_target=j
     endif
  endif
!!右方向探索
  i=i_agent(n)+1; j=j_agent(n)
  if(i>ipmax)then
     !skip
  elseif(move_boundary(i,j)==9999d0)then
     !skip
  else
     pot=pot_total(n,i,j,ierr)
     if(pot<potmin)then
        potmin=pot
        i_target=i
        j_target=j
     endif
  endif
!!下方向探索
  i=i_agent(n); j=j_agent(n)-1
  if(j<1)then
     !skip
  elseif(move_boundary(i,j)==9999d0)then
     !skip
  else
     pot=pot_total(n,i,j,ierr)
     if(pot<potmin)then
        potmin=pot
        i_target=i
        j_target=j
     endif
  endif
!!上方向探索
  i=i_agent(n); j=j_agent(n)+1
  if(j>jpmax)then
     !skip
  elseif(move_boundary(i,j)==9999d0)then
     !skip
  else
     pot=pot_total(n,i,j,ierr)
     if(pot<potmin)then
        potmin=pot
        i_target=i
        j_target=j
     endif
  endif
!!左下方向探索
  i=i_agent(n)-1; j=j_agent(n)-1
  if(i<1.or.j<1)then
     !skip
  elseif(move_boundary(i,j)==9999d0)then
     !skip
  else
     pot=(pot_total(n,i,j,ierr)-potnow)/root2+potnow
     if(pot<potmin)then
        potmin=pot
        i_target=i
        j_target=j
     endif
  endif
!!左上方向探索
  i=i_agent(n)-1; j=j_agent(n)+1
  if(i<1.or.j>jpmax)then
     !skip
  elseif(move_boundary(i,j)==9999d0)then
     !skip
  else
     pot=(pot_total(n,i,j,ierr)-potnow)/root2+potnow
     if(pot<potmin)then
        potmin=pot
        i_target=i
        j_target=j
     endif
  endif
!!右下方向探索
  i=i_agent(n)+1; j=j_agent(n)-1
  if(i>ipmax.or.j<1)then
     !skip
  elseif(move_boundary(i,j)==9999d0)then
     !skip
  else
     pot=(pot_total(n,i,j,ierr)-potnow)/root2+potnow
     if(pot<potmin)then
        potmin=pot
        i_target=i
        j_target=j
     endif
  endif
!!右上方向探索
  i=i_agent(n)+1; j=j_agent(n)+1
  if(i>ipmax.or.j>jpmax)then
     !skip
  elseif(move_boundary(i,j)==9999d0)then
     !skip
  else
     pot=(pot_total(n,i,j,ierr)-potnow)/root2+potnow
     if(pot<potmin)then
        potmin=pot
        i_target=i
        j_target=j
     endif
  endif
!
  if(ierr<0) call errstop(rout,ierr)
!
!移動方向を計算
  target_dx=dxy*(dble(i_target)-0.5d0)-agent_x(n)
  target_dy=dxy*(dble(j_target)-0.5d0)-agent_y(n)
  theta=atan2(target_dy,target_dx)
!
  return
end subroutine get_direction
