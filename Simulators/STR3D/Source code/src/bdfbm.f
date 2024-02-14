      SUBROUTINE BDFBM(FC,XYZ,D1,D2,GRAV,R0,IGR,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION  XYZ(3,2),VEC(3),GRAV(3),R0(3),FC(3,2)
C----&------------------------------------------------------------------
      ND=2
C
      CALL LENGTH(RL,XYZ(1,1),XYZ(1,2),3)
C
      DO 100 I=1,ND
C
        IF(IGR .EQ. 1) THEN
          CALL SHIFT1(VEC,GRAV,3)
        ELSE
          CALL SUBVEC(VEC,XYZ(1,I),R0,3)
        ENDIF
C
        CALL RMULT1(FC(1,I),VEC,D1*D2*RL*.5,3)
C
  100 CONTINUE
C
      RETURN
      END
