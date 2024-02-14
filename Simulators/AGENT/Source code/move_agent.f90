subroutine move_agent(ierr)
  !----------------------------------------
  ! エージェントの移動計算
  !----------------------------------------
    use m_timectl,only: dt,time
    use m_potential,only: ipmax,jpmax,dxy,move_boundary,shelter_height &
                   & ,n_signpost,theta_signpost,narea_signpost,i_shelter,j_shelter,n_shelter
    use m_agent,only: n_agent,i_agent,j_agent &
                   & ,agent_x,agent_y,agent_z,agent_u,agent_v,vel &
                   & ,agent_status,deadline,agent_start   &
                   & ,n_rw,rw_theta,iflag_signpost,n_slope,vertical_evacuation_speed
    use m_cadmas,only: depth,height
    implicit none
  !
  ![arguments]
    integer,intent(out):: ierr
  !
  ![local variables]
    character(32):: rout='move_agent'
    integer:: n,m,inext,jnext,inow,jnow
    real(8),parameter:: pi=3.14159265d0
    real(8):: xnow,ynow,velocity,u,v,xn,yn,theta
    real(8):: S,F,xnext,ynext
  !
    ierr=0
  !
  !各エージェントの座標位置を更新
    do n=1,n_agent

      !
      !移動開始時刻の確認
      if(time<=agent_start(n)) return
      xnow=agent_x(n)
      ynow=agent_y(n)
  !
  !エージェントの現在位置の確認
      inow=int(xnow/dxy)+1
      jnow=int(ynow/dxy)+1
      if(inow<1 .or. ipmax<inow .or. jnow<1 .or. jpmax<jnow) cycle !領域外にいる場合はそのまま
      
  !
  !エージェントの状態による処理の分岐
      if(agent_status(n)==0 .or. agent_status(n)==3)then  !避難済みや死亡したエージェントは移動しない
        cycle
      elseif(agent_status(n)==1)then

        !垂直避難
        do m=1,n_shelter
          if(inow==i_shelter(m).and.jnow==j_shelter(m))then !避難所セルにいるとき
            if(agent_z(n)<shelter_height(m))then
              agent_z(n) = agent_z(n) + vertical_evacuation_speed*dt
              go to 10
            endif
          endif
        enddo

        velocity=vel(n)
      elseif(agent_status(n)==2)then                      !水中は移動速度が低下する(深さに比例)
        velocity=vel(n)*(1.0d0-depth(i_agent(n),j_agent(n))/deadline(n))
      endif
       call get_direction(n,theta,ierr)
      if(ierr<0) call errstop(rout,ierr)
  !     endif
  !!道標が有効な場合はthetaを上書き
      if(n_signpost>=1)then
        if(iflag_signpost(narea_signpost(i_agent(n),j_agent(n)),n)==1) &
           & theta=theta_signpost(narea_signpost(i_agent(n),j_agent(n)))
      endif
  !!進行方向の不確実性を考慮
      if(n_rw==1) theta=theta+rw_theta(n)
  !!xy方向の速度に換算
      xn=cos(theta)
      yn=sin(theta)
      if( n_slope==1 ) then ! 斜面勾配を考慮
          !ip=int(xnow/dxy-0.5d0)+2
          !jp=int(ynow/dxy-0.5d0)+2
          !ip=min(max(ip,2),ipmax)
          !jp=min(max(jp,2),jpmax)
          !im=ip-1
          !jm=jp-1
          !xm=(dble(im)-0.5d0)*dxy
          !xp=(dble(ip)-0.5d0)*dxy
          !ym=(dble(jm)-0.5d0)*dxy
          !yp=(dble(jp)-0.5d0)*dxy
          !S=xn/dxy/dxy*((yp-ynow)*(-height(im,jm)+height(ip,jm))  &
          !&            +(ynow-ym)*(-height(im,jp)+height(ip,jp))) &
          !&+yn/dxy/dxy*((xp-xnow)*(-height(im,jm)+height(im,jp))  &
          !&            +(xnow-xm)*(-height(ip,jm)+height(ip,jp)))

        xnext = xnow + xn*dxy
        ynext = ynow + yn*dxy
        inext = int(xnext/dxy) + 1
        jnext = int(ynext/dxy) + 1
        inext = min(max(1,inext),ipmax)
        jnext = min(max(1,jnext),jpmax)
        S = (height(inext,jnext)-height(inow,jnow))/dxy
        !write(190,*)time,n,inow,jnow,height(inow,jnow),inext,jnext,height(inext,jnext),S
        call slope_function(S,F)
        u=velocity*xn*F
        v=velocity*yn*F
      else
        u=velocity*xn
        v=velocity*yn
      endif

      xnext = xnow+u*dt
      ynext = ynow+v*dt
      inext = int(xnext/dxy) + 1
      jnext = int(ynext/dxy) + 1

      !if(move_boundary(inext,jnext)==-1)then
      !  write(150,*)"evacuee is going to move nonroad cell"
      !  write(150,*)time,n,inow,jnow,inext,jnext
      !  inext=inow
      !  jnext=jnow
      !  xnext=xnow
      !  ynext=ynow
      !elseif(inext<1 .or. jnext<1 .or. inext>ipmax .or. jnext>jpmax)then
      if(inext<1 .or. jnext<1 .or. inext>ipmax .or. jnext>jpmax)then
        write(150,*)"evacuee is going to move out of area"
        write(150,*)time,n,inow,jnow,inext,jnext
        inext=inow
        jnext=jnow
        xnext=xnow
        ynext=ynow
      endif

      agent_x(n)=xnext
      agent_y(n)=ynext
      i_agent(n)=inext
      j_agent(n)=jnext
      agent_z(n) = height(i_agent(n),j_agent(n))

      10 continue

    enddo
  !
    return
  end subroutine move_agent
  
