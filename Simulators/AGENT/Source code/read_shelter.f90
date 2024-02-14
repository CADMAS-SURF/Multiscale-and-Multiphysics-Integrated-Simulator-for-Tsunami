subroutine read_shelter(ierr)
!----------------------------------------
! 避難所データの読み込み
!----------------------------------------
  use m_potential,only: n_shelter,i_shelter,j_shelter,shelter_height,move_boundary
!
  implicit none
!
![arguments]
  integer,intent(out):: ierr
!
![local variables]
  character(32):: rout='read_shelter'
  character(256):: line
  integer:: n,nn
!
  ierr=0
!
!
  open(22,file='shelter.inp',status='old',form='formatted',err=99)
!
!
  n=0
  do
     read(22,'(a256)',end=10) line
     if( line(1:1)=='#' ) cycle
     n=n+1
     read(line,*,err=30) nn,i_shelter(n),j_shelter(n),shelter_height(n)
     if(move_boundary(i_shelter(n),j_shelter(n)) /= 0)then
       write(100,*)"shelter",nn," is not on the road"
     endif
  enddo
10 continue
  if(n>n_shelter)then
     ierr=10
     call errmsg(rout,ierr)
     write(*,*) 'n > n_shelter : shelter.inp'
  endif
  goto 31
30 continue
     ierr=-30
     call errmsg(rout,ierr)
     write(*,*) 'read error : shelter.inp'
31 continue
!
!
  close(22)
!
  return
!
99 continue
  ierr=-99
  call errmsg(rout,ierr)
  write(*,*) 'cannot open shelter.inp'
  return
end subroutine read_shelter
