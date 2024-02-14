subroutine output(iflag,ierr)
!----------------------------------------
! ファイル出力制御
!----------------------------------------
  use m_timectl,only: time,dt,nstep
  use m_agent,only: n_agent,agent_x,agent_y,agent_z,agent_u,agent_v &
       & ,agent_status,agent_age,agent_height,i_agent,j_agent &
       & ,istat_escaped,istat_moving,istat_dead
  use m_potential,only: xpin,ypin
  use m_output
  implicit none
!
![arguments]
  integer,intent(in):: iflag
  integer,intent(out):: ierr
!
![local variables]
  real(8),parameter:: margin=0  !margin=0.3d0
  character(32):: rout='output'
  integer:: i,n
  integer,save:: jflag
!
  ierr=0
!
!
!----------------------------------------
!  (0) ファイルのオープン
!----------------------------------------
  if( iflag==0 ) then
     out_time=out_start
     jflag = 0
!
!----agent.out(BINARY)----------------
     open(14,file='agent.out',status='new',form='unformatted',err=91)
     write(100,*) 'open file: agent.out'
!record 1
     write(14) N_agent,N_I_statistics,N_R_statistics &
            & ,N_I_attribute_F,N_R_attribute_F &
            & ,N_I_attribute_V,N_R_attribute_V
!record 2
     write(14) (String_I_statistics(i),i=1,N_I_statistics)
!record 3
     write(14) (String_R_statistics(i),i=1,N_R_statistics)
!record 4
     write(14) (String_I_attribute_F(i),i=1,N_I_attribute_F)
!record 5
     write(14) (String_R_attribute_F(i),i=1,N_R_attribute_F)
!record 6
     write(14) (String_I_attribute_V(i),i=1,N_I_attribute_V)
!record 7
     write(14) (String_R_attribute_V(i),i=1,N_R_attribute_V)
     do n=1,n_agent
!record 8
        I_attribute_F(1)=agent_age(n)
        write(14) (I_attribute_F(i),i=1,N_I_attribute_F)
!record 9
        R_attribute_F(1)=agent_height(n)
        write(14) (R_attribute_F(i),i=1,N_R_attribute_F)
     enddo
!
!----statistics_*.csv(ASCII)----------------
     open(15,file='statistics_i.csv',status='new',form='formatted',err=92)
     write(100,*) 'open file: statistics_i.csv'
     write(15,1000) '#time',(',',trim(String_I_statistics(i)),i=1,N_I_statistics)
     open(16,file='statistics_r.csv',status='new',form='formatted',err=93)
     write(100,*) 'open file: statistics_r.csv'
     write(100,1000) '#time',(',',trim(String_R_statistics(i)),i=1,N_R_statistics)
1000 format(a,100a)
!
  endif
!
!
!----------------------------------------
! (1) リスト出力
!----------------------------------------
  if( (time>out_time-0.5d0*dt .and. time<=out_end) .or. (iflag==2 .and. jflag/=1) ) then
!
!----agent.out(BINARY)----------------
!record 1
     write(14) time, nstep
!record 2
     I_statistics(1)=istat_escaped
     I_statistics(2)=istat_moving
     I_statistics(3)=istat_dead
     write(14) (I_statistics(i),i=1,N_I_statistics)
!record 3
     write(14) (R_statistics(i),i=1,N_R_statistics)
     do n=1,n_agent
!record 4
        I_attribute_V(1)=agent_status(n)
        write(14) (I_attribute_V(i),i=1,N_I_attribute_V)
!record 5
        R_attribute_V(1)=agent_x(n)+xpin
        R_attribute_V(2)=agent_y(n)+ypin
        R_attribute_V(3)=agent_z(n)+margin
        R_attribute_V(4)=agent_u(n)
        R_attribute_V(5)=agent_v(n)
        write(14) (R_attribute_V(i),i=1,N_R_attribute_V)
!        write(95,*) R_attribute_V(3)
     enddo
!
!----statistics_*.csv(ASCII)----------------
     write(15,1001) time,(',',I_statistics(i),i=1,N_I_statistics)
1001 format(f10.2,100(a1,i9))
     write(16,1002) time,(',',R_statistics(i),i=1,N_R_statistics)
1002 format(f10.2,100(a1,e14.7))
!
     out_time = out_time + out_interval
     jflag = 1
!
  else
     jflag = 0
  endif
!
!
!----------------------------------------
! (2) ファイルのクローズ
!----------------------------------------
  if( iflag==2 ) then
!
!----agent.out(BINARY)----------------
     close(14)
     write(100,*) 'close file: agent.out'
!
!----statistics_*.csv(ASCII)----------------
     close(15)
     write(100,*) 'close file: statistics_i.csv'
     close(16)
     write(100,*) 'close file: statistics_r.csv'
!
  endif
!
  return
!
!----------------------------------------
! エラー処理
!----------------------------------------
91 continue
  ierr=-10
  call errmsg(rout,ierr)
  write(*,*) 'cannot open agent.out'
  return
!
92 continue
  ierr=-10
  call errmsg(rout,ierr)
  write(*,*) 'cannot open statistics_i.csv'
  return
!
93 continue
  ierr=-10
  call errmsg(rout,ierr)
  write(*,*) 'cannot open statistics_r.csv'
  return

end subroutine output
