      SUBROUTINE AREACD(RL,RN,Z4,XYZ)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION GE(3,4),V12(3),V13(3),XYZ(3,4),E(3,3),GG(3,4),RL(3)
     &         ,RN(3)
C----&------------------------------------------------------------------
      CALL SUBVEC(V12,XYZ(1,2),XYZ(1,1),3)
      CALL SUBVEC(V13,XYZ(1,3),XYZ(1,1),3)
      CALL DIRCOS(E(1,1),V12,3)
      CALL CROSS2(E(1,1),V13,E(1,3))
      CALL CROSS2(E(1,3),E(1,1),E(1,2))
C
      DO 200 I=1,4
        CALL SUBVEC(GG(1,I),XYZ(1,I),XYZ(1,1),3)
  200 CONTINUE
C
      CALL ATXB(GE,E,GG,3,3,4)
C
      X1 = GE(1,1)
      Y1 = GE(2,1)
      X2 = GE(1,2)
      Y2 = GE(2,2)
      X3 = GE(1,3)
      Y3 = GE(2,3)
      X4 = GE(1,4)
      Y4 = GE(2,4)
C
      S2 = ( X2*Y3 + X1*Y2 + X3*Y1 ) - ( X2*Y1 + X1*Y3 + X3*Y2 )
C
      A1 = X2*Y3 - X3*Y2
      B1 = Y2 - Y3
      C1 = X3 - X2
C
      A2 = X3*Y1 - X1*Y3
      B2 = Y3 - Y1
      C2 = X1 - X3
C
      A3 = X1*Y2 - X2*Y1
      B3 = Y1 - Y2
      C3 = X2 - X1
C
      RL(1) = ( A1 + B1*X4 + C1*Y4 ) / S2
      RL(2) = ( A2 + B2*X4 + C2*Y4 ) / S2
      RL(3) = ( A3 + B3*X4 + C3*Y4 ) / S2
C
      CALL SHIFT1(RN,E(1,3),3)
C
      Z4 = GE(3,4)
C
      RETURN
      END
