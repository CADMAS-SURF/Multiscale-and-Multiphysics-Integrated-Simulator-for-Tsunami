      SUBROUTINE READ_STOC_FILE
!
      IMPLICIT NONE
!
      INCLUDE 'CONTROL.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
!debug      include 'DOMAIN.h'
!debug      include 'GRID.h'
!
!     座標系と測地系
      integer:: icoord,isystem
      real(8):: lc_deg
!
      INTEGER :: IE,IEND,IS,N
!
!
      icoord =0
      isystem=0
      lc_deg =-999.9D0
!
      OPEN(INP,FILE=TRIM(AREAFILE),STATUS='OLD',FORM='FORMATTED',
     $     ERR=99)
!
      DO N=1,100000
        CALL GET1(IS,IE,IEND)
        IF( IEND.EQ.1 ) EXIT
!
        IF     ( CLINE(IS:IE) .EQ. '%GRID' ) THEN
           CALL INGRID
!
        ELSE IF( CLINE(IS:IE) .EQ. '%BOUNDARY' ) THEN
           CALL INBOUN(icoord,isystem,lc_deg)
!
        ELSE IF( CLINE(IS:IE) .EQ. '%CASE' ) THEN
           CALL INCASE
!
        ELSE
!       <<< SKIP >>>
        END IF
      END DO
!
      CLOSE(INP)
!
!debug      write(*,*) 'CFLNM=',trim(CFLNM)
!debug      write(*,*) 'NX,NY,NZ=',NX,NY,NZ
!debug      write(*,*) 'MX,MY,MZ=',MX,MY,MZ
!debug      write(*,*) 'XGRID(1)=',XGRID(1)
!debug      write(*,*) 'XGRID(MXM)=',XGRID(MXM)
!debug      write(*,*) 'YGRID(1)=',YGRID(1)
!debug      write(*,*) 'YGRID(MYM)=',YGRID(MYM)
!debug      write(*,*) 'ZGRID(1)=',ZGRID(1)
!debug      write(*,*) 'ZGRID(MZM)=',ZGRID(MZM)
      RETURN
!
   99 CONTINUE
      CALL ERRMSG('READ_STOC_FILE',10)
      WRITE(*,*) 'CANNOT OPEN ',TRIM(AREAFILE)
      CALL ABORT1('')
      END

