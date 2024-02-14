      SUBROUTINE FTIMER(NAME,START_END)
C----------------------------------------
C     処理時間計測用ルーチン
C----------------------------------------
      USE M_OUTPUT,ONLY:IFL
      IMPLICIT NONE
C
      CHARACTER(*),INTENT(IN):: NAME,START_END
C
      INTEGER,PARAMETER:: NMAX=16
      CHARACTER(12):: SUBNAME(NMAX)=
     $ (/'total       ','timeloop    ','setdt       ','setflw      '
     $  ,'setwin      ','setarea     ','setlvl      ','setfence    '
     $  ,'drift       ','vertex      ','collision   ','sink        '
     $  ,'sink2       ','block       ','output      ','dshift      '/)
C
      LOGICAL,SAVE:: NOT_INITIALIZE=.TRUE.
      INTEGER,SAVE:: COUNT_MAX
      REAL(8),SAVE:: RRATE
      INTEGER,SAVE:: START_COUNT(NMAX+1)
      REAL(8),SAVE:: ELAPSE_TIME(NMAX+1)
C
      INTEGER:: COUNT,COUNT_RATE,COUNT_DIFF
      INTEGER:: N
      REAL(8):: ET,ER
C
C
      IF(NOT_INITIALIZE) THEN
         CALL SYSTEM_CLOCK(COUNT,COUNT_RATE,COUNT_MAX)
C
         RRATE=1.D0/DBLE(COUNT_RATE)
         START_COUNT(:)=0
         ELAPSE_TIME(:)=0.d0
C
         NOT_INITIALIZE=.FALSE.
      ENDIF
C
      DO N=1,NMAX
         IF( NAME==SUBNAME(N) ) EXIT
      ENDDO
C
      CALL SYSTEM_CLOCK(COUNT)
C
      IF( START_END=='start' ) THEN
         START_COUNT(N)=COUNT
C
      ELSEIF( START_END=='end' ) THEN
         COUNT_DIFF=COUNT-START_COUNT(N)
         IF( COUNT_DIFF<0 ) THEN
            IF( N.NE.1.AND.N.NE.2 ) THEN
               COUNT_DIFF=COUNT_DIFF+COUNT_MAX
               ELAPSE_TIME(1)=ELAPSE_TIME(1)+DBLE(COUNT_MAX)*RRATE
               ELAPSE_TIME(2)=ELAPSE_TIME(2)+DBLE(COUNT_MAX)*RRATE
            ENDIF
         ENDIF
         ELAPSE_TIME(N)=ELAPSE_TIME(N)+DBLE(COUNT_DIFF)*RRATE
C
      ELSEIF( START_END=='summerize' ) THEN
         WRITE(IFL,800)
C
         ER=100.D0/ELAPSE_TIME(1)
C
         N=1
         WRITE(IFL,810) SUBNAME(N)   ,ELAPSE_TIME(N),ELAPSE_TIME(N)*ER
         WRITE(IFL,'(a1)') '+'
C
         N=2
         ET=ELAPSE_TIME(1)-ELAPSE_TIME(N)
         WRITE(IFL,820)'pre-process ',ET,ET*ER
         WRITE(IFL,820) SUBNAME(N)   ,ELAPSE_TIME(N),ELAPSE_TIME(N)*ER
         WRITE(IFL,'(a1)') '+'
C
         ET=ELAPSE_TIME(2)
         DO N=3,NMAX
         WRITE(IFL,830) SUBNAME(N)   ,ELAPSE_TIME(N),ELAPSE_TIME(N)*ER
         ET=ET-ELAPSE_TIME(N)
         ENDDO
         N=NMAX+1
         WRITE(IFL,830)'other       ',ET,ET*ER
C
  800    FORMAT(/,'###  ELAPSE TIME INFORMATION  ###',
     $          /,' PROCEDURE       | ELAPSE_TIME(SEC) | OCCUPANCY(%)')
  810    FORMAT(1X,A12,4X,'|',F14.3,4X,'|',F9.2)
  820    FORMAT(3X,A12,2X,'|',F14.3,4X,'|',F9.2)
  830    FORMAT(5X,A12   ,'|',F14.3,4X,'|',F9.2)
C
      ELSE
         CALL ERRMSG('FTIMER',-1)
      ENDIF
C
      RETURN
      END
