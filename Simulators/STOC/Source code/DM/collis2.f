      SUBROUTINE COLLIS2(XA,YA,XB,YB,P,Q,RLEN,NA,NB,IRTN)
C----------------------------------------
C     漂流物同士の衝突判定を行う
C
C     (XA,YA) : 漂流物Aの線分上の衝突距離の計算位置
C     (XB,YB) : 漂流物Bの線分上の衝突距離の計算位置
C     (P,Q) : AからBに向かう衝突による作用方向のベクトル
C      RLEN : 漂流物同士の距離(RLEN**2=P**2+Q**2)
C      IRTN  : 衝突時1,非衝突時0を返す
C----------------------------------------
      USE M_DRIFT
C
      IMPLICIT NONE
C
      REAL(8),INTENT(OUT)::XA,YA,XB,YB,P,Q,RLEN
      INTEGER,INTENT(OUT)::IRTN
      INTEGER,INTENT(IN)::NA,NB
C
      REAL(8)::X1A,Y1A,X2A,Y2A,X1B,Y1B,X2B,Y2B,RA,RB
      REAL(8)::A,B,C,D,E1B,E2B,F,G,H,D2,E1A,E2A
      REAL(8)::RF,RL
C
C
      IRTN = 0
C
      X1A = XCOLLIS(1,NA)
      Y1A = YCOLLIS(1,NA)
      X2A = XCOLLIS(2,NA)
      Y2A = YCOLLIS(2,NA)
      X1B = XCOLLIS(1,NB)
      Y1B = YCOLLIS(1,NB)
      X2B = XCOLLIS(2,NB)
      Y2B = YCOLLIS(2,NB)
C
      RA = 0.5D0*BB(NA)
      RB = 0.5D0*BB(NB)
      RLEN = 1.0D10
C
C ... 線分αと点B1,B2との接触判定
      A = Y2A - Y1A                ! 直線の方程式:Ax+By+C=0
      B = X1A - X2A
      C = X2A*Y1A - X1A*Y2A
      D = SQRT(A**2+B**2)          ! 2点の距離
C
      E1B = -B*(X1B-X1A)+A*(Y1B-Y1A)
      RF = (A*X1B+B*Y1B+C)/D
      RL = ABS(RF)
      IF( RL.LT.RA+RB .AND. E1B.GE.0.0D0 .AND. E1B.LE.D**2 ) THEN
         IF( RL.LT.RLEN ) THEN
            IRTN = 1
            RLEN = RL
            P = -A*RF/D
            Q = -B*RF/D
            XA = X1B+P
            YA = Y1B+Q
            XB = X1B
            YB = Y1B
         ENDIF
      ENDIF
C
      E2B = -B*(X2B-X1A)+A*(Y2B-Y1A)
      RF = (A*X2B+B*Y2B+C)/D
      RL = ABS(RF)
      IF( RL.LT.RA+RB .AND. E2B.GE.0.0D0 .AND. E2B.LE.D**2 ) THEN
         IF( RL.LT.RLEN ) THEN
            IRTN = 1
            RLEN = RL
            P = -A*RF/D
            Q = -B*RF/D
            XA = X2B+P
            YA = Y2B+Q
            XB = X2B
            YB = Y2B
         ENDIF
      ENDIF
C
C ... 線分βと点B1,B2との接触判定
      F = Y2B - Y1B                ! 直線の方程式:Fx+Gy+H=0
      G = X1B - X2B
      H = X2B*Y1B - X1B*Y2B
      D2 = SQRT(F**2+G**2)         ! 2点の距離
C
      E1A = -G*(X1A-X1B)+F*(Y1A-Y1B)
      RF = (F*X1A+G*Y1A+H)/D2
      RL = ABS(RF)
      IF( RL.LT.RA+RB .AND. E1A.GE.0.0D0 .AND. E1A.LE.D2**2 ) THEN
         IF( RL.LT.RLEN ) THEN
            IRTN = 1
            RLEN = RL
            P = F*RF/D2
            Q = G*RF/D2
            XA = X1A
            YA = Y1A
            XB = X1A-P
            YB = Y1A-Q
         ENDIF
      ENDIF
C
      E2A = -G*(X2A-X1B)+F*(Y2A-Y1B)
      RF = (F*X2A+G*Y2A+H)/D2
      RL = ABS(RF)
      IF( RL.LT.RA+RB .AND. E2A.GE.0.0D0 .AND. E2A.LE.D2**2 ) THEN
         IF( RL.LT.RLEN ) THEN
            IRTN = 1
            RLEN = RL
            P = F*RF/D2
            Q = G*RF/D2
            XA = X2A
            YA = Y2A
            XB = X2A-P
            YB = Y2A-Q
         ENDIF
      ENDIF
C
      IF( IRTN.EQ.1 ) RETURN
C
C
C ... 点A1と点B1の距離
      IF( E1B.LT.0.0D0 ) THEN
         RL = SQRT((X1B-X1A)**2+(Y1B-Y1A)**2)
         IF( RL.LT.RA+RB ) THEN
            IRTN = 1
            RLEN = RL
            P = X1A-X1B
            Q = Y1A-Y1B
            XA = X1A
            YA = Y1A
            XB = X1B
            YB = Y1B
         ENDIF
      ENDIF
C
C ... 点A2と点B1の距離
      IF( E1B.GT.D**2 ) THEN
         RL = SQRT((X1B-X2A)**2+(Y1B-Y2A)**2)
         IF( RL.LT.RA+RB ) THEN
            IRTN = 1
            RLEN = RL
            P = X2A-X1B
            Q = Y2A-Y1B
            XA = X2A
            YA = Y2A
            XB = X1B
            YB = Y1B
         ENDIF
      ENDIF
C
C ... 点A1と点B2の距離
      IF( E2B.LT.0.0D0 ) THEN
         RL = SQRT((X2B-X1A)**2+(Y2B-Y1A)**2)
         IF( RL.LT.RA+RB ) THEN
            IRTN = 1
            RLEN = RL
            P = X1A-X2B
            Q = Y1A-Y2B
            XA = X1A
            YA = Y1A
            XB = X2B
            YB = Y2B
         ENDIF
      ENDIF
C
C ... 点A2と点B2の距離
      IF( E2B.GT.D**2 ) THEN
         RL = SQRT((X2B-X2A)**2+(Y2B-Y2A)**2)
         IF( RL.LT.RA+RB ) THEN
            IRTN = 1
            RLEN = RL
            P = X2A-X2B
            Q = Y2A-Y2B
            XA = X2A
            YA = Y2A
            XB = X2B
            YB = Y2B
         ENDIF
      ENDIF
C
      RETURN
      END
