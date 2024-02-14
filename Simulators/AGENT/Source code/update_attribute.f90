subroutine update_attribute(ierr)
!----------------------------------------
! エージェントの属性の更新
!----------------------------------------
  use m_agent,only: n_agent,i_agent,j_agent       &
         & ,istat_escaped,istat_moving,istat_dead &
         & ,agent_status,deadline,agent_u,agent_v &
         & ,vel,Fi_agent,Fj_agent,agent_z,vertical_evacuation_speed
  use m_potential,only: n_shelter,i_shelter,j_shelter &
                      & ,agent_shelter,shelter_height
  use m_cadmas,only: depth,uu,vv,height,max_depth
  use m_timectl,only: nstep,maxstep,time,dt,time_end
  implicit none
!
!  [arguments]
  integer,intent(out):: ierr
!
!  [local variables]
  character(32):: rout='update_attribute'
  integer:: n,m,k
  character filename*128
!
  ierr=0
!
! 
  do n=1,n_agent
!    write(25,*) n,i_agent(n),j_agent(n)
    if(agent_status(n)==1 .or. agent_status(n)==2)then  !移動中のエージェントの状態を更新
      do m=1,n_shelter
        !避難所セル
        if( i_agent(n)==i_shelter(m) .and. &
          & j_agent(n)==j_shelter(m) ) then
          !垂直避難
          if(agent_z(n)<shelter_height(m)) then
            if(max_depth(i_agent(n),j_agent(n))-agent_z(n)>deadline(n)) then
              agent_status(n)=3   !死亡
              agent_u(n)=0.0d0
              agent_v(n)=0.0d0
              istat_dead=istat_dead+1         !統計値の更新
              istat_moving=istat_moving-1     !統計値の更新
            endif
            goto 10
          endif
          agent_status(n)=0    !避難済み
          agent_u(n)=0.0d0
          agent_v(n)=0.0d0
          agent_z(n)=height(i_agent(n),j_agent(n)) + shelter_height(m)
          istat_escaped=istat_escaped+1 !統計値の更新
          istat_moving=istat_moving-1   !統計値の更新
          agent_shelter(m)=agent_shelter(m)+1 !避難所毎の避難者カウント
          goto 10
        endif
      enddo
      if(depth(i_agent(n),j_agent(n))<=0.0d0) then
        agent_status(n)=1      !移動中
           if(max_depth(i_agent(n),j_agent(n))>deadline(n)) then
              agent_status(n)=3   !死亡
              agent_u(n)=0.0d0
              agent_v(n)=0.0d0
              istat_dead=istat_dead+1         !統計値の更新
              istat_moving=istat_moving-1     !統計値の更新
           endif
      elseif(depth(i_agent(n),j_agent(n))<deadline(n)) then
        agent_status(n)=2      !移動中(水中)
           if(max_depth(i_agent(n),j_agent(n))>deadline(n)) then
              agent_status(n)=3   !死亡
              agent_u(n)=0.0d0
              agent_v(n)=0.0d0
              istat_dead=istat_dead+1         !統計値の更新
              istat_moving=istat_moving-1     !統計値の更新
              !write(1004,*)n,Fi_agent(n),Fj_agent(n),time
              !write(1005,*)n,i_agent(n),j_agent(n),time
           endif
      else
        agent_status(n)=3      !死亡
        agent_u(n)=0.0d0
        agent_v(n)=0.0d0
        istat_dead=istat_dead+1         !統計値の更新
        istat_moving=istat_moving-1     !統計値の更新
        !write(1004,*)n,Fi_agent(n),Fj_agent(n),time
        !write(1005,*)n,i_agent(n),j_agent(n),time
      endif
    elseif(agent_status(n)==0)then !避難済のエージェントの死亡判定
!      if(depth(i_agent(n),j_agent(n))>=deadline(n)) then
!        agent_status(n)=3      !死亡
!        istat_dead=istat_dead+1         !統計値の更新
!        istat_escaped=istat_escaped-1 !統計値の更新
!      endif
    elseif(agent_status(n)==5)then
         agent_status(n)=0    !避難済み
         agent_u(n)=0.0d0
         agent_v(n)=0.0d0
         istat_escaped=istat_escaped+1 !統計値の更新
         istat_moving=istat_moving-1   !統計値の更新
    endif
10  continue
  enddo

!  k=nstep
!!datファイルに書き出す
!write(filename,'("agent",i4.4,".dat")')k
!open(80,file=filename)
!do n=1,n_agent
!write(80,'(3i5)')i_agent(n),j_agent(n),agent_status(n)

!  enddo
!  close(80)
!
  return
end subroutine update_attribute
