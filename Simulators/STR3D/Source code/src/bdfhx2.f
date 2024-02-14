      SUBROUTINE BDFHX2(FC,XYZ,ND,GRAV,R0,IGR,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WG(3,3),XG(3,3),XYZ(3,ND),P(3,20),XJI(3,3),H(20)
     &         ,FC(3,ND),VEC(3),GRAV(3),R0(3)
      DATA XG / 3*0.0D0
     &        , -.577350269189626D0, .577350269189626D0, 0.0D0
     &        , -.774596669241483D0, 0.0D0, .774596669241483D0 /
      DATA WG / 2.0D0, 2*0.0D0
     &        , 2*1.0D0, 0.0D0
     &        , 0.55555555555555556D0, 0.88888888888888889D0
     &        , 0.55555555555555556D0 /
C----&------------------------------------------------------------------
      IF(ND .EQ. 8) THEN
        NG=2
      ELSE
        NG=3
      ENDIF
C
      DO 100 IG=1,NG
      DO 100 JG=1,NG
      DO 100 KG=1,NG
C
        XG1=XG(IG,NG)
        XG2=XG(JG,NG)
        XG3=XG(KG,NG)
        WGT=WG(IG,NG)*WG(JG,NG)*WG(KG,NG)
C
        CALL DERHX2(XG1,XG2,XG3,ND,P)
        CALL JACOB3(ND,P,XYZ,XJI,DET,ITO)
C
        CALL SFNHX2(XG1,XG2,XG3,ND,H)
C
        DO 110 I=1,ND
C
          IF(IGR .EQ. 1) THEN
            CALL SHIFT1(VEC,GRAV,3)
          ELSE
            CALL SUBVEC(VEC,XYZ(1,I),R0,3)
          ENDIF
C
          DO 120 J=1,3
            FC(J,I)=FC(J,I)+H(I)*VEC(J)*DET*WGT
  120     CONTINUE
C
  110   CONTINUE
C
  100 CONTINUE
C
      RETURN
      END
