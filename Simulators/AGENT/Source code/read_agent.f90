subroutine read_agent(ierr)
!----------------------------------------
! エージェントファイルの読み込み
!----------------------------------------
  use m_potential,only: ipmax,jpmax,xpin,ypin,dxy
  use m_agent,only: n_agent,i_agent,j_agent &
                 & ,Fi_agent,Fj_agent       &
                 & ,agent_x,agent_y         &
                 & ,agent_z                 &
                 & ,vel,deadline            &
                 & ,rw_sigma                &
                 & ,weight_signpost         &
                 & ,weight_shelter          &
                 & ,weight_mob              &
                 & ,weight_danger           &
                 & ,allocate_agent          &
                 & ,agent_start
  use m_cadmas,only: height
  implicit none
!
![arguments]
  integer,intent(out):: ierr
!
![local variables]
  character(32):: rout='read_agent'
  character(256):: line
  integer:: n,nn
  real(8):: agent_xabs,agent_yabs       !入力ファイル読み込み時は、絶対座標
  real(8):: rw_sigma_deg                !入力ファイル読み込み時は、RadianではなくDegreeで
  real(8),parameter:: pi=3.141592653589793d0
!
  ierr=0
!
!
  open(12,file='agent.inp',status='old',form='formatted',err=99)
!
  n_agent=0
!
  do
     read(12,'(a256)',end=20) line
     if( line(1:1)=='#' ) cycle
     n_agent=n_agent+1
  enddo
20 continue
!
  write(100,'(a)',advance='no')"debug:n_agent="
  write(100,*)n_agent
  rewind(12)
  call allocate_agent(ierr)
  if(ierr<0) call errstop(rout,ierr)
!
  n=0
  do
     read(12,'(a256)',end=10) line      !ファイルの読み込み (#で始まる行は読み飛ばす)
     if( line(1:1)=='#' ) cycle
!
     n=n+1
     read(line,*,err=30) nn,agent_xabs,agent_yabs,vel(n),deadline(n) &
                      & ,rw_sigma_deg,weight_signpost(n) &
                      & ,weight_shelter(n),weight_mob(n),agent_start(n)
!
     goto 40
30   continue
     ierr=-10
     call errmsg(rout,ierr)
     write(*,*) 'read error in agent.inp'
     write(*,*) 'line=',trim(line)
!
40   continue
     if(vel(n).lt.0.0d0) then
        ierr=-20
        call errmsg(rout,ierr)
        write(*,*) 'agent velocity must be >=0'
        write(*,*) 'line=',trim(line)
     endif
     if(rw_sigma_deg.lt.0.0d0) then
        ierr=-21
        call errmsg(rout,ierr)
        write(*,*) 'agent rw_sigma must be >=0'
        write(*,*) 'line=',trim(line)
     endif
     if(agent_start(n)<0) then
      ierr=-22
      call errmsg(rout,ierr)
      write(*,*) 'agent start must be >=0'
      write(*,*) 'line=',trim(line)
   endif
!
     rw_sigma(n)=rw_sigma_deg*pi/180d0  ! Degree => Radian
!
!解析領域の基点を[0,0]にするため、エージェントの位置をシフトする
     agent_x(n)=agent_xabs-xpin
     agent_y(n)=agent_yabs-ypin
!
!エージェントの初期セル位置を設定
     i_agent(n)=int(agent_x(n)/dxy)+1
        if(i_agent(n)<1)then
          i_agent(n)=1
        elseif(i_agent(n)>ipmax)then
          i_agent(n)=ipmax
        endif
     j_agent(n)=int(agent_y(n)/dxy)+1
        if(j_agent(n)<1)then
          j_agent(n)=1
        elseif(j_agent(n)>jpmax)then
          j_agent(n)=jpmax
        endif
!
!    初期位置を記憶
     Fi_agent(n)=i_agent(n)
     Fj_agent(n)=j_agent(n)
!

  enddo
!
10 continue
  close(12)
!
  return
!
99 continue
  ierr=-99
  call errmsg(rout,ierr)
  write(*,*) 'cannot open agent.inp'
  return
end subroutine read_agent
