      subroutine flnam(str)
C
C     SET FILE NAME : CFLNM
C      '.dbg', '.bco', '.bci', ',rso', '.rsi', '.grp' ,'osd'
C     (HEADER PART IS ALREADY SET IN INCASE)
C
      implicit none
      include 'FILEI.h'
      include 'FILEC.h'
      include 'CONNEC.h'
      include 'GLOBAL.h'
c
      character(4),intent(in)::str
c
      if( IAUTOD.eq.1 ) then
         CFLNM(IFLNM-3:IFLNM-3)='_'
         WRITE(CFLNM(IFLNM-2:IFLNM-1),'(I2.2)') MYPROC
         CFLNM(IFLNM:IFLNM+3)=str
      else
         CFLNM(IFLNM-3:IFLNM)=str
      endif
c
      return
      end
