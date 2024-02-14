      SUBROUTINE VF_FAULTT(DELH,TIM1,XX,YY)

CD=== 概要 ===========================================================

CDT   VF_FAULTT:断層パラメータから水位変動量を計算する

C==== 宣言 ===========================================================

      USE MOD_FAULT,ONLY: EN2LB,LB2LB,D2R,ICOORD,ISYSTEM,JSYSTEM,
     $                    NFLT,NFLTNOW,FPARAM,DISPLACE,XOR,YOR

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)
C      IMPLICIT NONE
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
C
      REAL(8):: DELH(NUMI0,NUMJ0)
      REAL(8):: XX(MAXG1,NUMI),YY(MAXG1,NUMJ)
      REAL(8):: TIM1
      REAL(8):: TIMESBX,X1,Y1,L1,B1,L2,B2,ZZ
      REAL(8):: PARAM2(6),STR,DIP,SLIP
      INTEGER:: I,J
      REAL(8),ALLOCATABLE:: L1ARRAY(:,:)
      REAL(8),ALLOCATABLE:: B1ARRAY(:,:)
C
C
c      ALLOCATE(L1ARRAY(2:NUMI0-1,2:NUMJ0-1),
c     $         B1ARRAY(2:NUMI0-1,2:NUMJ0-1))
      ALLOCATE(L1ARRAY(MYIS:MYIE,MYJS:MYJE),
     $         B1ARRAY(MYIS:MYIE,MYJS:MYJE))
C
c      DO J=2,NUMJ0-1
c      DO I=2,NUMI0-1
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
         X1=0.5D0*(XX(1,I)+XX(1,I+1))
         Y1=0.5D0*(YY(1,J)+YY(1,J+1))
C
         CALL EN2LB(X1,Y1,L1,B1,ICOORD,ISYSTEM)
C
         IF( ISYSTEM.NE.JSYSTEM ) THEN
            CALL LB2LB(L1,B1,L2,B2,ISYSTEM,JSYSTEM)
            L1=L2
            B1=B2
         ENDIF
C
         L1ARRAY(I,J)=L1
         B1ARRAY(I,J)=B1
      ENDDO
      ENDDO
C
  100 CONTINUE
C
      STR =FPARAM(4,NFLTNOW)
      DIP =FPARAM(5,NFLTNOW)
      SLIP=FPARAM(6,NFLTNOW)
      PARAM2(1)=SIN(DIP)
      PARAM2(2)=COS(DIP)
      PARAM2(3)=SIN(SLIP)
      PARAM2(4)=COS(SLIP)
      PARAM2(5)=SIN(STR)
      PARAM2(6)=COS(STR)
C
c      DO J=2,NUMJ0-1
c      DO I=2,NUMI0-1
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
         L1=L1ARRAY(I,J)
         B1=B1ARRAY(I,J)
         CALL DISPLACE(L1,B1,XOR,FPARAM(1,NFLTNOW),PARAM2,ZZ)
c
         IG = I + MYGIS - 1
         JG = J + MYGJS - 1 
         DELH(IG,JG)=DELH(IG,JG)+ZZ
      ENDDO
      ENDDO
C
      NFLTNOW=NFLTNOW+1
      IF(NFLTNOW.GT.NFLT) THEN
c         write(ILPFIL,*) '### FAULT DATA: END OF DATA'
         TIMESBX=1.D30
      ELSE
         TIMESBX=FPARAM(10,NFLTNOW)
      ENDIF
c      write(6,*) 'faultt: tim1,timesbx=',tim1,timesbx
      IF( TIM1.GE.TIMESBX ) GOTO 100
c
c      write(89,'(f8.3,4i8)') FPARAM(10,NFLTNOW-1),2,NUMI0-1,2,NUMJ0-1
c      do j=2,NUMJ0-1
c         write(89,'(<NUMI0-2>f8.3)') (delh(i,j),i=2,NUMI0-1)
c      enddo
C
      DEALLOCATE(L1ARRAY,B1ARRAY)
C
      RETURN
      END
