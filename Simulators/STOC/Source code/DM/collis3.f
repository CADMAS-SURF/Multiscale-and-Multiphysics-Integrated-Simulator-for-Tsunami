      SUBROUTINE COLLIS3(X,Y,P,Q,RLEN,NA,NB,IRTN)
C----------------------------------------
C     漂流物とフェンス状構造物との衝突判定を行う
C
C     (X,Y) : フェンスとの衝突位置
C     (P,Q) : 地形との衝突位置から漂流物へのベクトル
C      RLEN : 漂流物とフェンスとの距離(RLEN**2=P**2+Q**2)
C      IRTN  : 衝突時1,非衝突時0を返す
C----------------------------------------
      USE M_DRIFT
      USE M_FENCE,ONLY:XFC1,YFC1,XFC2,YFC2
C
      IMPLICIT NONE
C
      REAL(8),INTENT(OUT)::X,Y,P,Q,RLEN
      INTEGER,INTENT(OUT)::IRTN
      INTEGER,INTENT(IN)::NA,NB
C
      REAL(8)::X1A,Y1A,X2A,Y2A,X1B,Y1B,X2B,Y2B,RA
      REAL(8)::A,B,C,D,E,E1B,E2B,F,G,H,D2,E1A,E2A
      REAL(8)::RF,RL
C
C
      IRTN = 0
C
      X1A = XCOLLIS(1,NA)
      Y1A = YCOLLIS(1,NA)
      X2A = XCOLLIS(2,NA)
      Y2A = YCOLLIS(2,NA)
      X1B = XFC1(NB)
      Y1B = YFC1(NB)
      X2B = XFC2(NB)
      Y2B = YFC2(NB)
C
      RA = 0.5D0*BB(NA)
      RLEN = 1.0D10
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
      IF( RL.LT.RA .AND. E1A.GE.0.0D0 .AND. E1A.LE.D2**2 ) THEN ! パターン3による接触
         IF( RL.LT.RLEN ) THEN
            IRTN = 1
            RLEN = RL
            P = F*RF/D2
            Q = G*RF/D2
            X = X1A-P
            Y = Y1A-Q
c            write(*,*) 'collision at 1'
         ENDIF
      ENDIF
C
      E2A = -G*(X2A-X1B)+F*(Y2A-Y1B)
      RF = (F*X2A+G*Y2A+H)/D2
      RL = ABS(RF)
      IF( RL.LT.RA .AND. E2A.GE.0.0D0 .AND. E2A.LE.D2**2 ) THEN ! パターン3による接触
         IF( RL.LT.RLEN ) THEN
            IRTN = 1
            RLEN = RL
            P = F*RF/D2
            Q = G*RF/D2
            X = X2A-P
            Y = Y2A-Q
c            write(*,*) 'collision at 2'
         ENDIF
      ENDIF
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
      IF( RL.LT.RA .AND. E1B.GE.0.0D0 .AND. E1B.LE.D**2 ) THEN ! パターン1による接触
         IF( RL.LT.RLEN ) THEN
            IRTN = 1
            RLEN = RL
            P = -A*RF/D
            Q = -B*RF/D
            X = X1B
            Y = Y1B
c            write(*,*) 'collision at 3'
         ENDIF
      ENDIF
C
      E2B = -B*(X2B-X1A)+A*(Y2B-Y1A)
      RF = (A*X2B+B*Y2B+C)/D
      RL = ABS(RF)
      IF( RL.LT.RA .AND. E2B.GE.0.0D0 .AND. E2B.LE.D**2 ) THEN ! パターン1による接触
         IF( RL.LT.RLEN ) THEN
            IRTN = 1
            RLEN = RL
            P = -A*RF/D
            Q = -B*RF/D
            X = X2B
            Y = Y2B
c            write(*,*) 'collision at 4'
         ENDIF
      ENDIF
C
CCC      IF( IRTN.EQ.1 ) RETURN
      IF( IRTN.EQ.1 ) GOTO 100
C
C
C ... 点A1と点B1の距離
      IF( E1B.LT.0.0D0 ) THEN
         RL = SQRT((X1B-X1A)**2+(Y1B-Y1A)**2)
         IF( RL.LT.RA ) THEN ! パターン2による接触
            IRTN = 1
            RLEN = RL
            P = X1A-X1B
            Q = Y1A-Y1B
            X = X1B
            Y = Y1B
c            write(*,*) 'collision at 5'
         ENDIF
      ENDIF
C
C ... 点A2と点B1の距離
      IF( E1B.GT.D**2 ) THEN
         RL = SQRT((X1B-X2A)**2+(Y1B-Y2A)**2)
         IF( RL.LT.RA ) THEN ! パターン2による接触
            IRTN = 1
            RLEN = RL
            P = X2A-X1B
            Q = Y2A-Y1B
            X = X1B
            Y = Y1B
c            write(*,*) 'collision at 6'
         ENDIF
      ENDIF
C
C ... 点A1と点B2の距離
      IF( E2B.LT.0.0D0 ) THEN
         RL = SQRT((X2B-X1A)**2+(Y2B-Y1A)**2)
         IF( RL.LT.RA ) THEN ! パターン2による接触
            IRTN = 1
            RLEN = RL
            P = X1A-X2B
            Q = Y1A-Y2B
            X = X2B
            Y = Y2B
c            write(*,*) 'collision at 7'
         ENDIF
      ENDIF
C
C ... 点A2と点B2の距離
      IF( E2B.GT.D**2 ) THEN
         RL = SQRT((X2B-X2A)**2+(Y2B-Y2A)**2)
         IF( RL.LT.RA ) THEN ! パターン2による接触
            IRTN = 1
            RLEN = RL
            P = X2A-X2B
            Q = Y2A-Y2B
            X = X2B
            Y = Y2B
c            write(*,*) 'collision at 8'
         ENDIF
      ENDIF
C
  100 CONTINUE
C
C<<<<< (START) STOC-BLC VERSION  <<<<<<<
      IF( IRTN.EQ.1 )THEN
         A = Y2A - Y1A                           ! 直線の方程式:Ax+By+C=0
         B = X1A - X2A
         C = X2A*Y1A - X1A*Y2A
         D = SQRT(A**2+B**2)                     ! 2点の距離
         E = -B*(X-X1A)+A*(Y-Y1A)
         IF( E-0.25D0*D*D.LT.0.0D0 ) LBL1(NA,1) = -1
         IF( E-0.25D0*D*D.EQ.0.0D0 ) LBL1(NA,1) =  0
         IF( E-0.25D0*D*D.GT.0.0D0 ) LBL1(NA,1) = +1
         IF( A*X+B*Y+C.LT.0.0D0    ) LBL2(NA,1) = -1
         IF( A*X+B*Y+C.EQ.0.0D0    ) LBL2(NA,1) =  0
         IF( A*X+B*Y+C.GT.0.0D0    ) LBL2(NA,1) = +1
c         write(*,*) 'LBL1,LBL2=',LBL1(NA,1),LBL2(NA,1)
      ENDIF
C<<<<<  (END)  STOC-BLC VERSION  <<<<<<<
C
      RETURN
      END
