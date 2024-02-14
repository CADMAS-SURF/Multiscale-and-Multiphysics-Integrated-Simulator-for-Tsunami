      LOGICAL FUNCTION VOLIN(XYZ,VEC,TOL)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XYZ(3,4),VEC(3),V12(3),V13(3),V14(3),E(3,3)
C----&------------------------------------------------------------------
      CALL SUBVEC(V12,XYZ(1,2),XYZ(1,1),3)
      CALL SUBVEC(V13,XYZ(1,3),XYZ(1,1),3)
      CALL SUBVEC(V14,XYZ(1,4),XYZ(1,1),3)
C
      CALL CROSS2(V12,V13,E(1,1))
      CALL CROSS2(V13,V14,E(1,2))
      CALL CROSS2(V14,V12,E(1,3))
C
      DO 100 I=1,3
        CALL VECML1(P,VEC,E(1,I),3)
        IF(P .LT. TOL) THEN
          VOLIN = .FALSE.
          RETURN
        ENDIF
  100 CONTINUE
C
      VOLIN = .TRUE.
C
      RETURN
      END
