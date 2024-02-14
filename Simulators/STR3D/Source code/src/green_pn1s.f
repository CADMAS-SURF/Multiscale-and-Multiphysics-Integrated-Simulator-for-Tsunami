      SUBROUTINE GREEN_PN1S(EPSS,DUDX,GQD,IGNL)
C
C     EPSS : OUT : EPSS(1,ig) = 2*ε23 (積分点ig)
C                  EPSS(2,ig) = 2*ε31 (積分点ig)
C     DUDX : IN  : DUDX(i,j,ig) = ∂uj/∂xi (積分点ig)
C     GQD  : IN  : [E] 
C     IGNL : IN  : =0:微小変形, =1:大変形
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EPS(6),DUDX(3,3,3),EPSB(6),EPSS(2,3),GQD(6,6)
C
      DO I = 1, 3

        EPS(I  )=DUDX(3,2,I)+DUDX(2,3,I)
        EPS(I+3)=DUDX(1,3,I)+DUDX(3,1,I)

        IF( IGNL == 0 ) CYCLE

        EPS(I  )=EPS(I  )+( DUDX(2,1,I)*DUDX(3,1,I)
     &                    + DUDX(2,2,I)*DUDX(3,2,I)
     &                    + DUDX(2,3,I)*DUDX(3,3,I) )
        EPS(I+3)=EPS(I+3)+( DUDX(3,1,I)*DUDX(1,1,I)
     &                    + DUDX(3,2,I)*DUDX(1,2,I)
     &                    + DUDX(3,3,I)*DUDX(1,3,I) )

      ENDDO
C
      CALL AXB(EPSB,GQD,EPS,6,6,1)
C
      DO I = 1, 3
        EPSS(1,I) = EPSB(I)
        EPSS(2,I) = EPSB(I+3)
      ENDDO
C
      RETURN
      END
