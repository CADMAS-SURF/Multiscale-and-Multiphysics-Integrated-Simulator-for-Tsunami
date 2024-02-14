      SUBROUTINE LENCD(RT,V34,XYZ)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V12(3),V13(3),V14(3),V34(3),XYZ(3,3)
C----&------------------------------------------------------------------
      CALL SUBVEC(V12,XYZ(1,2),XYZ(1,1),3)
      CALL SUBVEC(V13,XYZ(1,3),XYZ(1,1),3)
C
      CALL VECML1(A,V13,V12,3)
      CALL VECML1(B,V12,V12,3)
      RT=A/B
C
      CALL RMULT1(V14,V12,RT,3)
      CALL SUBVEC(V34,V14,V13,3)
C
      RETURN
      END
