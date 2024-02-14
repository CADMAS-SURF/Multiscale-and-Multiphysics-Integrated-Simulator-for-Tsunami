      SUBROUTINE VERTEX
C----------------------------------------
C     漂流物の頂点と
C     衝突判定時の漂流物の線分の両端のX,Y座標を求める
C----------------------------------------
      USE M_DRIFT
C
      IMPLICIT NONE
C
      REAL(8)::COST,SINT
      INTEGER::N
C
C
      DO N=1,ND
         IF( LD(N).EQ.-2 ) CYCLE
         COST = COS(TD1(N))
         SINT = SIN(TD1(N))
C
         XVERT(1,N) = XD1(N) + 0.5D0*AL(N)*COST + 0.5D0*AB(N)*SINT     ! SM&SB
         XVERT(2,N) = XD1(N) - 0.5D0*AL(N)*COST + 0.5D0*AB(N)*SINT     ! SN&SB
         XVERT(3,N) = XD1(N) - 0.5D0*AL(N)*COST - 0.5D0*AB(N)*SINT     ! SN&PS
         XVERT(4,N) = XD1(N) + 0.5D0*AL(N)*COST - 0.5D0*AB(N)*SINT     ! SM&PS
C
         YVERT(1,N) = YD1(N) + 0.5D0*AL(N)*SINT - 0.5D0*AB(N)*COST     ! SM&SB
         YVERT(2,N) = YD1(N) - 0.5D0*AL(N)*SINT - 0.5D0*AB(N)*COST     ! SN&SB
         YVERT(3,N) = YD1(N) - 0.5D0*AL(N)*SINT + 0.5D0*AB(N)*COST     ! SN&PS
         YVERT(4,N) = YD1(N) + 0.5D0*AL(N)*SINT + 0.5D0*AB(N)*COST     ! SM&PS
C
         XCOLLIS(1,N) = XD1(N) + 0.5D0*(BL(N)-BB(N))*COST
         XCOLLIS(2,N) = XD1(N) - 0.5D0*(BL(N)-BB(N))*COST
C
         YCOLLIS(1,N) = YD1(N) + 0.5D0*(BL(N)-BB(N))*SINT
         YCOLLIS(2,N) = YD1(N) - 0.5D0*(BL(N)-BB(N))*SINT
      ENDDO
C
      RETURN
      END
