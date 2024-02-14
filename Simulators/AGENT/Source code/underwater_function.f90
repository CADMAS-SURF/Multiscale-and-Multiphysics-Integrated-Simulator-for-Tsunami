subroutine underwater_function(n,t,F)
!----------------------------------------
! 斜面勾配による移動速度の変化率を計算
!
! <入力>
! n,t
!
! <出力>
! F: 移動速度の補正係数[-]
!----------------------------------------
  use m_agent,only: i_agent,j_agent
  use m_cadmas,only: depth,uu,vv
  implicit none
!
![arguments]
  real(8),intent(in):: n,t
  real(8),intent(out):: F
!
![local variables]
  integer i_now,j_now
  real(8) depth_now,flow_now,depth_rank,flow_rank,depth_limit,flow_limit
!

  i_now = i_agent(n)
  j_now = j_agent(n)
!man
  depth_limit = 0.7
  flow_limit = 2.5

  depth_now = depth(i_now,j_now)
  if(uu(i_now,j_now)/=0 .and. vv(i_now,j_now)/=0)then
    flow_now = SQRT(uu(i_now,j_now)**2 + vv(i_now,j_now)**2)
  else
    flow_now = 0
  endif

  depth_rank = depth_now / depth_limit
  flow_rank = flow_now / flow_limit

  if(depth_rank <0.2)then
    if(flow_rank<0.2)then
      F=1.0
    elseif(flow_rank<0.4)then
      F=0.8
    elseif(flow_rank<0.6)then
      F=0.6
    elseif(flow_rank<0.8)then
      F=0.4
    elseif(flow_rank<1.0)then
      F=0.2
    else
      F=0.0
    endif
  elseif(depth_rank <0.4)then
    if(flow_rank<0.2)then
      F=0.8
    elseif(flow_rank<0.4)then
      F=0.6
    elseif(flow_rank<0.6)then
      F=0.4
    elseif(flow_rank<0.8)then
      F=0.2
    else
      F=0.0
    endif
  elseif(depth_rank <0.6)then
    if(flow_rank<0.2)then
      F=0.6
    elseif(flow_rank<0.4)then
      F=0.4
    elseif(flow_rank<0.6)then
      F=0.2
    else
      F=0.0
    endif
  elseif(depth_rank <0.8)then
    if(flow_rank<0.2)then
      F=0.4
    elseif(flow_rank<0.4)then
      F=0.2
    else
      F=0.0
    endif
  elseif(depth_rank <1.0)then
    if(flow_rank<0.2)then
      F=0.2
    else
      F=0.0
    endif
  else
    F=0.0
  endif
    
!
  return
end subroutine underwater_function
