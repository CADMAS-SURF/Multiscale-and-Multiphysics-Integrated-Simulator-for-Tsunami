subroutine make_potential_shelter(ierr)
!----------------------------------------
! 避難経路ポテンシャルの設定
!----------------------------------------
  use m_potential,only: ipmax,jpmax,pot_shelter &
                     & ,n_shelter,i_shelter,j_shelter &
                     & ,dxy,n_potential,m_pot_shelter,shelter_height
  use m_agent,only: n_agent,vertical_evacuation_speed
  use m_flag,only: flag_WP,flag_RP,flag_danger,flag_prob
  use m_prob,only: n_pot_directory,flag_RPprob,flag_WPprob
  use m_cadmas,only: height
  implicit none
!
![arguments]
  integer,intent(out):: ierr
!
![local variables]
  integer:: n,m,i,j
  real:: vertical_evacuation_time
  character(32):: rout='make_potential_shelter'
  character(128) filename,filename2
!
  ierr=0
!
!
!!各避難所のポテンシャル読み込み
  if(flag_RP==1)then
    write(100,*)"debug: in make_potential_shelter flag_WP=0"
    do m=1,n_shelter
      write(100,'(a)',advance='no')"debug: reading potential of shelter No."
      write(100,'(i4)')m
      write(filename,'(i3.3,".txt")')m
      open(50,file=filename)
      do j=jpmax,1,-1
        read(50,*)(m_pot_shelter(m,i,j),i=1,ipmax)
      enddo
      close(50)
    enddo
!!各避難所のポテンシャル計算
  else
    do n=1,n_shelter
      pot_shelter(:,:)=9999d0
      write(100,'(a)',advance='no') 'debug: Start making potential of shelter No.'
      write(100,'(i4)')n
      pot_shelter(i_shelter(n),j_shelter(n))=1.0d-10/dxy
      call recursive_search_shelter(0.0d0,i_shelter(n),j_shelter(n),ierr)
      if(ierr<0)then
        call errstop(rout,ierr)
      endif
      write(100,*) 'debug: finish making potential of shelter No.',n,'/',n_shelter
      do j=jpmax,1,-1
        do i=1,ipmax
          m_pot_shelter(n,i,j)=pot_shelter(i,j)
        enddo
      enddo

      !!各避難所のポテンシャル書き出し
      write(filename,'(i3.3,".txt")')n
      if(flag_WP==1) then
        open(32,file=filename)
        do j=jpmax,1,-1
          write(32,'(1000F10.2)')(m_pot_shelter(n,i,j),i=1,ipmax)
        enddo
        close(32)
      endif

    enddo
  endif

!! 各避難所までの最短避難時間の算出及びスケール変換
  do m=1,n_shelter
    vertical_evacuation_time = 0
    if(shelter_height(m)>0)then
      vertical_evacuation_time = (shelter_height(m)-height(i_shelter(m),j_shelter(m)))/vertical_evacuation_speed
    endif
    do j=1,jpmax
      do i=1,ipmax
        if(m_pot_shelter(m,i,j)/=9999)then
          m_pot_shelter(m,i,j) = m_pot_shelter(m,i,j)*dxy
          m_pot_shelter(m,i,j) = m_pot_shelter(m,i,j) + vertical_evacuation_time
        endif
      enddo
    enddo
  enddo


!!津波危険度考慮計算(津波到達時間と比較)
  if(flag_danger==1)then

    write(100,'(a)')"debug: call make_n_potential"
    do n=1,n_agent
      call make_n_potential(n,1,ierr)
    enddo

!!津波到達確率考慮計算
  elseif(flag_prob==1)then
    write(100,'(a)')"debug: call make_n_potential"
    !書き出し
    if(flag_WPprob == 1)then
        do n=1,n_agent
          call make_n_potential(n,2,ierr)
          write(filename2,'(i5.5)')n
          filename2 = trim(filename2) // "_n_pot.txt"
          open(unit=20000,file=trim(filename2))
          do j=jpmax,1,-1
            do i=1,ipmax
              write(20000,'(f16.4)',advance='no')n_potential(n,i,j)
            enddo
            write(20000,*)
          enddo
          close(20000)
        enddo
    !読み込み
    elseif(flag_RPprob == 1) then
      do n=1,n_agent
          write(filename,'(i5.5)')n
          filename = trim(n_pot_directory) // trim(filename) // "_n_pot.txt"
          open(unit=20000,file=trim(filename),status='old')
          do j=jpmax,1,-1
            read(20000,*)(n_potential(n,i,j),i=1,ipmax)
          enddo
          close(20000)
      enddo
    else
        do n=1,n_agent
            call make_n_potential(n,2,ierr)
        enddo
    endif

!!最短
  else
    
    write(100,'(a)')"debug: call make_n_potential"
    do n=1,n_agent
      call make_n_potential(n,0,ierr)
    enddo

  endif

  do n=1,n_agent
    do j=1,jpmax
      do i=1,ipmax
        n_potential(n,i,j)=-1.0d0/n_potential(n,i,j)
      enddo
    enddo
  enddo

  return
end subroutine make_potential_shelter
