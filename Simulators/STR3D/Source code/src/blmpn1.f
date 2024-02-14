      SUBROUTINE BLMPN1(BL,DNDX,DUDX,IGNL)
C
C     BL   : OUT : [BL]
C     DNDX : IN  : DNDX(i,j) = ∂Nj/∂xi
C     DUDX : IN  : DUDX(i,j) = ∂uj/∂xi
C     IGNL : IN  : =0:微小変形, =1:大変形
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION BL(6,3*6),DNDX(3,6),DUDX(3,3)
C
      CALL CLEAR1(BL,6*3*6)
C
      DO 100 J=1,6
C
        J0=3*(J-1)
        BL(1,J0+1)=DNDX(1,J)
        BL(2,J0+2)=DNDX(2,J)
        BL(3,J0+3)=DNDX(3,J)
        BL(4,J0+1)=DNDX(2,J)
        BL(4,J0+2)=DNDX(1,J)
C
  100 CONTINUE
C
      IF(IGNL .EQ. 0) RETURN
C
      DO 200 J=1,6
        J0=3*(J-1)
        DO 210 K=1,3
          DO 220 I=1,3
            BL(I,J0+K)=BL(I,J0+K)+DUDX(I,K)*DNDX(I,J)
  220     CONTINUE
          BL(4,J0+K)=BL(4,J0+K)+DUDX(1,K)*DNDX(2,J)+DUDX(2,K)*DNDX(1,J)
  210   CONTINUE
  200 CONTINUE
C
      RETURN
      END
