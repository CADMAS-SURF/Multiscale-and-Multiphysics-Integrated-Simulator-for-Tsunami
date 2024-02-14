      SUBROUTINE VERTEX2(XX,YY,ZZ,XD1,YD1,TD1,AL1,AB1,AZ1,ZH1)
C----------------------------------------
C     漂流物の頂点のX,Y座標を求める
C----------------------------------------
      IMPLICIT NONE
C
      REAL(8),INTENT(OUT)::XX(8),YY(8),ZZ(8)
      REAL(8),INTENT(IN)::XD1,YD1,TD1,AL1,AB1,AZ1,ZH1
C
      REAL(8)::COST,SINT
C
C
      COST = COS(TD1)
      SINT = SIN(TD1)
      XX(1) = XD1+0.5D0*AL1*COST-(-0.5D0*AB1)*SINT
      XX(2) = XD1+0.5D0*AL1*COST-0.5D0*AB1*SINT
      XX(5) = XD1+(-0.5D0*AL1)*COST-(-0.5D0*AB1)*SINT
      XX(6) = XD1+(-0.5D0*AL1)*COST-0.5D0*AB1*SINT
      XX(3) = XX(2)
      XX(4) = XX(1)
      XX(7) = XX(6)
      XX(8) = XX(5)
      YY(1) = YD1+0.5D0*AL1*SINT+(-0.5D0*AB1)*COST
      YY(2) = YD1+0.5D0*AL1*SINT+0.5D0*AB1*COST
      YY(5) = YD1+(-0.5D0*AL1)*SINT+(-0.5D0*AB1)*COST
      YY(6) = YD1+(-0.5D0*AL1)*SINT+0.5D0*AB1*COST
      YY(3) = YY(2)
      YY(4) = YY(1)
      YY(7) = YY(6)
      YY(8) = YY(5)
      ZZ(1) = ZH1
      ZZ(2) = ZZ(1)
      ZZ(3) = ZH1+AZ1
      ZZ(4) = ZZ(3)
      ZZ(5) = ZZ(1)
      ZZ(6) = ZZ(1)
      ZZ(7) = ZZ(3)
      ZZ(8) = ZZ(3)
C
      RETURN
      END
