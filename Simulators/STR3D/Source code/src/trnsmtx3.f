      SUBROUTINE TRNSMTX3(ND,KN,GRID,TE,GE)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION GG(3,20),GRID(3,*),E(3,3),TE(3,3),GE(3,ND),KN(ND)
     &         ,V12(3),V34(3),CENTER(3),CENTER1(3),CENTER2(3)
     &         ,CENTER3(3),CENTER4(3)
C---------------------------------------------------------------------==
C        1         2         3         4         5         6         7**
C23456789+123456789+123456789+123456789+123456789+123456789+123456789+12
C---------------------------------------------------------------------==
      DO 500 I=1,3
        CENTER1(I) = ( GRID(I,KN(1)) + GRID(I,KN(4)) + GRID(I,KN(5))
     &               + GRID(I,KN(8)) ) * 0.25
        CENTER2(I) = ( GRID(I,KN(2)) + GRID(I,KN(3)) + GRID(I,KN(6))
     &               + GRID(I,KN(7)) ) * 0.25
        CENTER3(I) = ( GRID(I,KN(1)) + GRID(I,KN(2)) + GRID(I,KN(5))
     &               + GRID(I,KN(6)) ) * 0.25
        CENTER4(I) = ( GRID(I,KN(4)) + GRID(I,KN(3)) + GRID(I,KN(8))
     &               + GRID(I,KN(7)) ) * 0.25
  500 CONTINUE
C
      CALL SUBVEC(V12,CENTER2,CENTER1,3)
      CALL SUBVEC(V34,CENTER4,CENTER3,3)
      CALL DIRCOS(E(1,1),V12,3)
      CALL CROSS2(E(1,1),V34,E(1,3))
      CALL CROSS2(E(1,3),E(1,1),E(1,2))
C
      DO 100 I=1,3
        DO 110 J=1,3
          TE(I,J)=E(J,I)
  110   CONTINUE
  100 CONTINUE
C
      DO 400 I=1,3
        CENTER(I) = ( CENTER1(I) + CENTER2(I) ) * 0.5
  400 CONTINUE
C
      DO 200 I=1,ND
        CALL SUBVEC(GG(1,I),GRID(1,KN(I)),CENTER,3)
  200 CONTINUE
C
      CALL AXB(GE,TE,GG,3,3,ND)
C
      RETURN
      END
