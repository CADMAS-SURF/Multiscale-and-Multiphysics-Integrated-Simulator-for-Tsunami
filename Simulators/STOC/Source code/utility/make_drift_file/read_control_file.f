      SUBROUTINE READ_CONTROL_FILE
!
      IMPLICIT NONE
!
      INCLUDE 'CONTROL.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
!
      INTEGER :: IE,IEND,IS,N
!
!
      OPEN(INP,FILE='control.dat',STATUS='OLD',FORM='FORMATTED',
     $     ERR=99)
!
      DO N=1,100000
        CALL GET1(IS,IE,IEND)
        IF( IEND.EQ.1 ) EXIT
!
        IF     ( CLINE(IS:IE) .EQ. '%STOC' ) THEN
           CALL INSTOC
!
        ELSE IF( CLINE(IS:IE) .EQ. '%METHOD' ) THEN
           CALL INMETHD
!
        ELSE IF( CLINE(IS:IE) .EQ. '%DRIFT' ) THEN
           CALL INDRIFT
!
        ELSE
!       <<< SKIP >>>
        END IF
      END DO
!
      CLOSE(INP)
!
!debug      write(*,*) 'AREAFILE=',trim(AREAFILE)
!debug      write(*,*) 'CMODEL=',trim(CMODEL)
!debug      write(*,*) 'SEED=',SEED
!debug      write(*,*) 'N_TBL=',N_TBL
!debug      DO N=1,N_TBL
!debug      write(*,*) N,TBL_INUND(N),TBL_PDST(N)
!debug      ENDDO
!debug      write(*,*) 'DIVX=',DIVX
!debug      write(*,*) 'DIVY=',DIVY
!debug      write(*,*) 'RHOD=',RHOD
!debug      write(*,*) 'QD  =',QD
!debug      write(*,*) 'HD  =',HD
      RETURN
!
   99 CONTINUE
      CALL ERRMSG('READ_CONTROL_FILE',11)
      WRITE(*,*) 'CANNOT OPEN control.dat'
      CALL ABORT1('')
      END
