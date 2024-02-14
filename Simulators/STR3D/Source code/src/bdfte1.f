      SUBROUTINE BDFTE1(FC,XYZ,GRAV,R0,IGR,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XYZ(3,4),FC(3,4),VEC(3),GRAV(3),R0(3)
C----&------------------------------------------------------------------
      ND=4
C
      CALL TETVOL(XYZ,VOL)
C
      DO 100 I=1,ND
C
        IF(IGR .EQ. 1) THEN
          CALL SHIFT1(VEC,GRAV,3)
        ELSE
          CALL SUBVEC(VEC,XYZ(1,I),R0,3)
        ENDIF
C
        CALL RMULT1(FC(1,I),VEC,0.25*VOL,3)
C
  100 CONTINUE
C
      RETURN
      END
