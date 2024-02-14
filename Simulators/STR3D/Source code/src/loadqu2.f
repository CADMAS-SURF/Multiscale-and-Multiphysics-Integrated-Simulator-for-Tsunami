      SUBROUTINE LOADQU2(GRID,NJ,NOD,PRESS,DIR,ID,ICRD,TR,FC,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WGT(3,3),XG(3,3),XYZ(3,9),GRID(3,*),NOD(NJ),FC(3,NJ)
     &         ,R1(3),R2(3),P(2,9),H(9),DIR(3),E(3),XYZG(3),TRG(3,3)
     &         ,TR(12,*),E0(3),ICRD(*),PRESS(NJ)
      DATA  XG   / 3*0.0D0
     &           , -.577350269189626D0, .577350269189626D0, 0.0D0
     &           , -.774596669241483D0, 0.0D0, .774596669241483D0 /
      DATA  WGT  / 2.0D0, 2*0.0D0
     &           , 2*1.0D0, 0.0D0
     &           , 0.55555555555555556D0, 0.88888888888888889D0
     &           , 0.55555555555555556D0 /
C-----------------------------------------------------------------------
      DO 100 I=1,NJ
        CALL SHIFT1(XYZ(1,I),GRID(1,NOD(I)),3)
  100 CONTINUE
C
      CALL VECML1(RNORM,DIR,DIR,3)
      IF(RNORM.NE.0.) CALL DIRCOS(E,DIR,3)
C
      CALL CLEAR1(FC,3*NJ)
C
      IF(NJ.EQ.4) THEN
        NG=2
      ELSE
        NG=3
      ENDIF
C
      DO 200 IG1=1,NG
      DO 200 IG2=1,NG
        XG1=XG(IG1,NG)
        XG2=XG(IG2,NG)
C
        CALL DERQU2(P,XG1,XG2,NOD,NJ)
        CALL CLEAR1(R1,3)
        CALL CLEAR1(R2,3)
        DO 300 I=1,NJ
          DO 400 J=1,3
            R1(J)=R1(J)+P(1,I)*XYZ(J,I)
            R2(J)=R2(J)+P(2,I)*XYZ(J,I)
  400     CONTINUE
  300   CONTINUE
C
        CALL VECML1(EE,R1,R1,3)
        CALL VECML1(FF,R1,R2,3)
        CALL VECML1(GG,R2,R2,3)
        FAC=DSQRT(EE*GG-FF*FF)*WGT(IG1,NG)*WGT(IG2,NG)
C
        CALL SFNQU2(H,XG1,XG2,NOD,NJ)
C
        CALL VECML1(PRESG,H,PRESS,NJ)
C
        IF(RNORM.EQ.0.) THEN
          CALL CROSS2(R1,R2,E0)
        ELSE
          IF( ID .GT. 0 ) THEN
            CALL CLEAR1(XYZG,3)
            DO 700 I=1,NJ
              CALL RMULT2(XYZG,XYZ(1,I),H(I),3)
  700       CONTINUE
            CALL TRNSMTX6(TRG,ICRD(ID),XYZG,TR(1,ID),ITO)
            CALL AXB(E0,TRG,E,3,3,1)
          ELSE
            CALL SHIFT1(E0,E,3)
          ENDIF
        ENDIF
C
        DO 500 I=1,NJ
          DO 600 J=1,3
            FC(J,I)=FC(J,I)+H(I)*PRESG*E0(J)*FAC
  600     CONTINUE
  500   CONTINUE
C
  200 CONTINUE
C
      RETURN
      END
