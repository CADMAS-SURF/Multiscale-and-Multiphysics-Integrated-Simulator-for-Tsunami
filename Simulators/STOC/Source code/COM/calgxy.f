      SUBROUTINE CALGXY(GXM,GXI,GYM,GYJ,UT,VT,I,J,N)
C======================================================================
C     浮上型防波堤の流向依存型ポーラス値を計算する
C       (UT,VT):セル中心での水平方向流速
C       N      :浮上型防潮堤番号
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'OBSTI.h'
      INCLUDE 'OBSTR.h'
C
      REAL(8),INTENT(OUT)::GXM,GXI,GYM,GYJ
      REAL(8),INTENT(IN) ::UT,VT
      INTEGER,INTENT(IN) ::I,J,N
C
      REAL(8),PARAMETER ::PAI =3.14159265358993D0
      INTEGER::NN
      REAL(8)::PAI2=2.0D0*PAI
      REAL(8)::ANGV,ANG0,ANG1,ANG2,VELV,VEL1,VEL2
      REAL(8)::THETA,GXX,GYY
C
      IF(J.GE.IPORS(3,N).AND.I.EQ.IPORS(1,N)-1) THEN
        GXM = RPORS(2,N)
      ELSE
        GXM = RPORS(3,N)
      END IF
      IF(I.EQ.IPORS(2,N)) THEN
        GXI = RPORS(4,N)
      ELSE
        GXI = RPORS(3,N)
      END IF
      IF(I.GE.IPORS(1,N).AND.J.EQ.IPORS(3,N)) THEN
        GYM = RPORS(5,N)
      ELSE
        GYM = RPORS(6,N)
      END IF
      IF(J.EQ.IPORS(4,N)) THEN
        GYJ = RPORS(7,N)
      ELSE
        GYJ = RPORS(6,N)
      END IF
C
      NN = IPORS(7,N)
      ANGV = ATAN2(-VT,-UT)
      IF(ANGV.LT.0.0D0) ANGV=ANGV+PAI2
      ANG0 = FPORS(3,NN)
      ANG1 = ANG0+FPORS(4,NN)
      ANG2 = ANG0+FPORS(5,NN)
      VELV = - (UT*COS(ANG0)+VT*SIN(ANG0))
      IF( ANGV.GT.ANG1.OR.ANGV.LT.ANG2 ) THEN
        THETA = 0.0D0
      ELSE
        VELV = - (UT*COS(ANG0)+VT*SIN(ANG0))
        VEL1 = FPORS(6,NN)
        VEL2 = FPORS(7,NN)
        IF( VELV.LE.VEL2 ) THEN
          THETA = 0.0D0
        ELSE IF( VELV.GE.VEL1 ) THEN
          THETA = PAI      
        ELSE 
          THETA = PAI*(VELV-VEL2)/(VEL1-VEL2)
        END IF
      END IF
C
      GXM = GXM+(FPORS(1,NN)-GXM)*(1.0D0-COS(THETA))*0.5D0
      GXI = GXI+(FPORS(1,NN)-GXI)*(1.0D0-COS(THETA))*0.5D0
      GYM = GYM+(FPORS(2,NN)-GYM)*(1.0D0-COS(THETA))*0.5D0
      GYJ = GYJ+(FPORS(2,NN)-GYJ)*(1.0D0-COS(THETA))*0.5D0
C
      RETURN
      END
