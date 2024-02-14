      SUBROUTINE XYSIDE(XSM,XSN,XSB,XPS,YSM,YSN,YSB,YPS,N)
C----------------------------------------
C     漂流物の辺上の点の座標を求める
C----------------------------------------
      USE M_DRIFT
C
      IMPLICIT NONE
C
      REAL(8)::XSM(MXSIDE),XSN(MXSIDE),XSB(MXSIDE),XPS(MXSIDE)
      REAL(8)::YSM(MXSIDE),YSN(MXSIDE),YSB(MXSIDE),YPS(MXSIDE)
      INTEGER,INTENT(IN)::N
C
      REAL(8)::C1,C2
      INTEGER::M
C
C
      DO M=1,NDDIV(1,N)                       ! 点の並びの順番はSN側からSM側へ向かう方向
         C2 = DBLE(M-1)/DBLE(NDDIV(1,N)-1)
         C1 = 1.0D0-C2
C
         XSB(M) = C1*XVERT(2,N)+C2*XVERT(1,N)
         YSB(M) = C1*YVERT(2,N)+C2*YVERT(1,N)
C
         XPS(M) = C1*XVERT(3,N)+C2*XVERT(4,N)
         YPS(M) = C1*YVERT(3,N)+C2*YVERT(4,N)
      ENDDO
C
      DO M=1,NDDIV(2,N)                       ! 点の並びの順番はSB側からPS側へ向かう方向
         C2 = DBLE(M-1)/DBLE(NDDIV(2,N)-1)
         C1 = 1.0D0-C2
C
         XSN(M) = C1*XVERT(2,N)+C2*XVERT(3,N)
         YSN(M) = C1*YVERT(2,N)+C2*YVERT(3,N)
C
         XSM(M) = C1*XVERT(1,N)+C2*XVERT(4,N)
         YSM(M) = C1*YVERT(1,N)+C2*YVERT(4,N)
      ENDDO
C
      RETURN
      END
