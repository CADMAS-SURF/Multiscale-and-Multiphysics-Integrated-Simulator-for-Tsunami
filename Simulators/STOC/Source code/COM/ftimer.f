      SUBROUTINE FTIMER(NUM,IFLG)
C*--------------------------------------------------------------------*
C*--------------------------------------------------------------------*
C
C---------------------------------------------------------------------
C  * CPU TIME COUNTER
C---------------------------------------------------------------------
      IMPLICIT NONE
C
C--------COMMON
      INCLUDE 'CPUCHK.h'
C
      INTEGER,INTENT(INOUT)::NUM,IFLG
C
C--------LOCAL
      REAL(8)::CPUNOW
      INTEGER::NUMB,NWRK
C
C--------------------------------------------------------<< BEGIN >>--
      NWRK = NUM + IFLG
      IF( NWRK .EQ. 0     ) GOTO 10
C
      NUMB=NUM
c      IF( NUM.EQ.0 ) NUMB=NCPUTM
      IF( NUM.EQ.0 ) GOTO 9000
      IF( IFLG.EQ.0 ) GO TO 20
      IF( IFLG.EQ.1 ) GO TO 30
      GO TO 9000
C
C    << TIMER RESET >>
   10 CONTINUE
      CALL CLKON
      CPUSEC(NUM,1) = 0.0D0
      GO TO 9000
C
C    << WATCH CPU TIME >>
   20 CONTINUE
      CALL CLKNOW( CPUNOW )
      CPUSEC(NUMB,2) = CPUNOW
      GO TO 9000
C
C    << COUNT CPU TIME >>
   30 CONTINUE
      CALL CLKNOW( CPUNOW )
      CPUSEC(NUMB,1) = CPUSEC(NUMB,1) + ( CPUNOW - CPUSEC(NUMB,2) )
C
C-------------------------------------------------------<< FINISH >>--
 9000 CONTINUE
      RETURN
      END
C
      SUBROUTINE CLKON
      RETURN
      END
C
      SUBROUTINE CLKNOW( WTIME )
      use mod_comm,only: comm_model
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      REAL(8),INTENT(INOUT)::WTIME
      REAL(4)::RTIME,WORK2(2)
C
CCC LINUX
C      CALL CPU_TIME( RTIME )
C
CCC AS
C     RTIME = ETIME(WORK2)
C
      RTIME  = 0.0
      WTIME  = RTIME
      WTIME = MPI_WTIME()
      RETURN
      END
C
