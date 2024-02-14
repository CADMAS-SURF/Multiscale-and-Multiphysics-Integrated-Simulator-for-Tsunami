subroutine read_condition(ierr)
!----------------------------------------
!計算条件ファイルの読込み
!----------------------------------------
  use m_timectl,only: dt,time_start,time_end,maxstep
  use m_agent,only: n_rw,rw_dt,n_slope
  use m_potential,only: xpin,ypin,ipmax,jpmax,dxy,n_signpost,n_shelter,n_mob,r_mob
  use m_output,only: out_start,out_end,out_interval
  use m_cadmas,only: maxfile,cadfile,n_cadmas
  use m_prob,only: ini_prob,relaxation_rate,tsunami_prob_directory,flag_WPprob,flag_RPprob,n_pot_directory
  use m_flag,only: flag_WP,flag_RP,flag_danger,flag_prob
  use m_danger,only: danger_path
  implicit none
!
![arguments]
  integer,intent(out):: ierr
!
![local variables]
  character(32):: rout='read_condition'
  real(8):: start,end
  integer:: n,nregion=0
  character(64):: file(maxfile)
!
![namelist]
  namelist /time/ start,end,dt,maxstep
  namelist /agent/ n_rw,rw_dt,n_slope
  namelist /potential/ xpin,ypin,dxy,ipmax,jpmax,n_signpost,n_shelter,n_mob,r_mob
  namelist /flag/ flag_WP,flag_RP,flag_danger,flag_prob
  namelist /output/ out_start,out_end,out_interval
  namelist /offline/ nregion,file
  namelist /danger/ danger_path
  namelist /prob/ ini_prob,relaxation_rate,tsunami_prob_directory,flag_WPprob,flag_RPprob,n_pot_directory
!
  ierr=0
  file(:)=''
!
!
!----------------------------------------
! NAMELISTの読み込み
!----------------------------------------
  open(11,file='namelist.inp',status='old',form='formatted',err=91)
!
  rewind(11)
  read(11,time)
!
  rewind(11)
  read(11,agent)
!
  rewind(11)
  read(11,potential)
!
  rewind(11)
  read(11,flag)
!
  rewind(11)
  read(11,output)
!
  rewind(11)
  read(11,offline)
!
  if(flag_danger==1)then
    rewind(11)
    read(11,danger)
  endif
!
  if(flag_prob==1)then
    rewind(11)
    read(11,prob)
  endif
!
  close(11)
!
!
!----------------------------------------
! TIMEの処理
!----------------------------------------
  time_start = start
  time_end   = end
!
!----------------------------------------
! POTENTIALの処理
!----------------------------------------
! なし
!
!----------------------------------------
! OUTPUTの処理
!----------------------------------------
  if(out_start<time_start)then
    out_start=time_start
    write(100,*) 'Warning at subroutine',trim(rout)
    write(100,*) '  Fixed : out_start=time_start'
  endif
!
!----------------------------------------
! OFFLINEの処理
!----------------------------------------
  do n=1,nregion
     cadfile(n)=file(n)
     if( file(n)=='' ) then
        write(100,*) 'Error at subroutine read_condition'
        write(100,*) '  namelist : offline'
        write(100,*) '      file(',n,') is undefined.'
        call errstop('read_condition',900)
     endif
  enddo
  n_cadmas=nregion
!
!----------------------------------------
! NAMELISTの出力
!----------------------------------------
  !write(90,time)
  !write(90,agent)
  !write(90,potential)
  !write(90,output)
  !write(90,offline)
  write(100,flag)
  if(flag_danger==1)then
    write(100,danger)
  endif
  if(flag_prob==1)then
    write(100,prob)
  endif
!
  return
!
91 continue
  ierr=-30
  call errmsg(rout,ierr)
  write(100,*) 'cannot open namelist.inp'
  return
end subroutine read_condition
