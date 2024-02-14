      SUBROUTINE FAULTT(DELH,TIM1)
C----------------------------------------------------------------------
C     断層パラメータから水位変動量を計算する
C----------------------------------------------------------------------
      USE MOD_FAULT,ONLY: EN2LB,LB2LB,D2R,ICOORD,ISYSTEM,JSYSTEM,
     $                    NFLT,NFLTNOW,FPARAM,DISPLACE,XOR,YOR
      IMPLICIT NONE
      INCLUDE 'FILE.h'
      INCLUDE 'GRID.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
C
      REAL(8):: DELH(MX,MY)
      REAL(8):: TIM1
      REAL(8):: TIMESBX,X1,Y1,L1,B1,L2,B2,ZZ
      REAL(8):: PARAM2(6),STR,DIP,SLIP
      INTEGER:: I,J
      REAL(8),ALLOCATABLE:: L1ARRAY(:,:)
      REAL(8),ALLOCATABLE:: B1ARRAY(:,:)
C
C
      ALLOCATE(L1ARRAY(2:MXM,2:MYM),B1ARRAY(2:MXM,2:MYM))
C
      DO J=2,MYM
      DO I=2,MXM
         X1=0.5D0*(XGRID(I-1)+XGRID(I))
         Y1=0.5D0*(YGRID(J-1)+YGRID(J))
C
C        直交
         IF( ICORDTYPE.EQ.1 ) THEN
            CALL EN2LB(X1,Y1,L1,B1,ICOORD,ISYSTEM)
C
C        球面
         ELSE
            L1=X1*D2R
            B1=Y1*D2R
         ENDIF
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
      DO J=2,MYM
      DO I=2,MXM
         L1=L1ARRAY(I,J)
         B1=B1ARRAY(I,J)
         CALL DISPLACE(L1,B1,XOR,FPARAM(1,NFLTNOW),PARAM2,ZZ)
         DELH(I,J)=DELH(I,J)+ZZ
      ENDDO
      ENDDO
C
      NFLTNOW=NFLTNOW+1
      IF(NFLTNOW.GT.NFLT) THEN
c         write(LP,*) '### FAULT DATA: END OF DATA'
         TIMESBX=1.D+99
      ELSE
         TIMESBX=FPARAM(10,NFLTNOW)
      ENDIF
      write(6,*) 'faultt: tim1,timesbx=',tim1,timesbx
      IF( TIM1.GE.TIMESBX ) GOTO 100
c
c      write(90+NRANK,'(f8.3,4i8)') FPARAM(10,NFLTNOW-1),2,mxm,2,mym
c      do j=2,mym
c         write(90+NRANK,'(<mxm-1>f8.3)') (delh(i,j),i=2,mxm)
c      enddo
C
      DEALLOCATE(L1ARRAY,B1ARRAY)
C
      RETURN
      END
