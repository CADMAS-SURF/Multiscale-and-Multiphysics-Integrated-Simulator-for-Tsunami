      SUBROUTINE LOADTR2(GRID,NOD,PRESS,DIR,ID,ICRD,TR,FC,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WGT(4),RL(3,4),XYZ(3,6),GRID(3,*),NOD(6),FC(3,6)
     &         ,R1(3),R2(3),P(2,6),H(6),DIR(3),E(3),XYZG(3),TRG(3,3)
     &         ,TR(12,*),E0(3),ICRD(*),PRESS(6)
      DATA WGT / -0.28125D0, 3*0.260416666666667D0 /
      DATA RL  / 
     &    0.333333333333333D0, 0.333333333333333D0, 0.333333333333333D0
     &  , 0.6D0, 0.2D0, 0.2D0
     &  , 0.2D0, 0.6D0, 0.2D0
     &  , 0.2D0, 0.2D0, 0.6D0 /
C-----------------------------------------------------------------------
      DO 100 I=1,6
        CALL SHIFT1(XYZ(1,I),GRID(1,NOD(I)),3)
  100 CONTINUE
C
      CALL VECML1(RNORM,DIR,DIR,3)
      IF(RNORM.NE.0.) CALL DIRCOS(E,DIR,3)
C
      CALL CLEAR1(FC,18)
C
      DO 200 IG=1,4
C
        CALL DERTR2(RL(1,IG),P,6)
        CALL CLEAR1(R1,3)
        CALL CLEAR1(R2,3)
        DO 300 I=1,6
          DO 400 J=1,3
            R1(J)=R1(J)+P(1,I)*XYZ(J,I)
            R2(J)=R2(J)+P(2,I)*XYZ(J,I)
  400     CONTINUE
  300   CONTINUE
C
        CALL VECML1(EE,R1,R1,3)
        CALL VECML1(FF,R1,R2,3)
        CALL VECML1(GG,R2,R2,3)
        FAC=DSQRT(EE*GG-FF*FF)*WGT(IG)
C
        CALL SFNTR2(RL(1,IG),H,6)
C
        CALL VECML1(PRESG,H,PRESS,6)
C
        IF(RNORM.EQ.0.) THEN
          CALL CROSS2(R1,R2,E0)
        ELSE
          IF( ID .GT. 0 ) THEN
            CALL CLEAR1(XYZG,3)
            DO 700 I=1,6
              CALL RMULT2(XYZG,XYZ(1,I),H(I),3)
  700       CONTINUE
            CALL TRNSMTX6(TRG,ICRD(ID),XYZG,TR(1,ID),ITO)
            CALL AXB(E0,TRG,E,3,3,1)
          ELSE
            CALL SHIFT1(E0,E,3)
          ENDIF
        ENDIF
C
        DO 500 I=1,6
          DO 600 J=1,3
            FC(J,I)=FC(J,I)+H(I)*PRESG*E0(J)*FAC
  600     CONTINUE
  500   CONTINUE
C
  200 CONTINUE
C
      RETURN
      END
