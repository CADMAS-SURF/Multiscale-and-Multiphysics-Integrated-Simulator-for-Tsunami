C<<<<< (START) STOC-DS&DM VERSION  <<<<<<<
c      SUBROUTINE COLLIS1(X,Y,P,Q,RLEN,LLD,ZBT,N,IERR)
      SUBROUTINE COLLIS1(X,Y,P,Q,RLEN,LLD,ZBT,N,IWKDST,JWKDST,IERR)
C<<<<<  (END)  STOC-DS&DM VERSION  <<<<<<<
C----------------------------------------
C     漂流物と地形・建物側面との衝突判定を行う
C
C     (X,Y) : 地形との衝突位置
C     (P,Q) : 地形との衝突位置から線分へのベクトル
C      RLEN : 地形との線分との距離(RLEN**2=P**2+Q**2)
C      LLD  : 衝突時1,非衝突時0を返す
C----------------------------------------
      USE M_GRID
      USE M_GEOM
      USE M_DRIFT
      USE M_TIME
      USE M_OUTPUT,ONLY:IFL
C
      IMPLICIT NONE
C
      REAL(8),INTENT(OUT)::X,Y,P,Q,RLEN
      INTEGER,INTENT(OUT)::LLD,IERR
      REAL(8),INTENT(IN)::ZBT
      INTEGER,INTENT(IN)::N
C<<<<< (START) STOC-DS&DM VERSION  <<<<<<<
      INTEGER,INTENT(OUT)::IWKDST,JWKDST
C<<<<<  (END)  STOC-DS&DM VERSION  <<<<<<<
C
      REAL(8)::X1,Y1,X2,Y2,R0
      REAL(8)::XMIN,YMIN,XMAX,YMAX,XX,YY,A,B,C,D,E,RF,RL,R
      INTEGER::IMIN,JMIN,IMAX,JMAX,NA
      INTEGER::L,I,J,M,I0,J0
C
C
      IERR = 0
C
      X1 = XCOLLIS(1,N)
      Y1 = YCOLLIS(1,N)
      X2 = XCOLLIS(2,N)
      Y2 = YCOLLIS(2,N)
      R0 = 0.5D0*BB(N)
C
C ... 最大座標、最小座標のセット
C
      XMIN = MIN(X1,X2) - R0
      XMAX = MAX(X1,X2) + R0
      YMIN = MIN(Y1,Y2) - R0
      YMAX = MAX(Y1,Y2) + R0
C
C ... 領域外の判定
C
      NA = INAR(N)
      IF( XMIN.LT.XG(NA,0) .OR.YMIN.LT.YG(NA,0) .OR.
     $    XMAX.GT.XG(NA,NI(NA)).OR.YMAX.GT.YG(NA,NJ(NA)) ) THEN
c         WRITE(*,*) 'SHIP IS PLACED OUTSIDE OF REGION'
         IERR = -2
         RETURN
      ENDIF
C
C ... 漂流物の含まれるセルの範囲(IMIN,JMIN)-(IMAX,JMAX)を設定
C
      DO I=1,NI(NA)
         IF( XMIN.GE.XG(NA,I-1).AND.XMIN.LT.XG(NA,I) ) THEN
            IMIN = I
            EXIT
         ENDIF
      ENDDO
      DO I=IMIN,NI(NA)
         IF( XMAX.GT.XG(NA,I-1).AND.XMAX.LE.XG(NA,I) ) THEN
            IMAX = I
            EXIT
         ENDIF
      ENDDO
C
      DO J=1,NJ(NA)
         IF( YMIN.GE.YG(NA,J-1).AND.YMIN.LT.YG(NA,J) ) THEN
            JMIN = J
            EXIT
         ENDIF
      ENDDO
      DO J=JMIN,NJ(NA)
         IF( YMAX.GT.YG(NA,J-1).AND.YMAX.LE.YG(NA,J) ) THEN
            JMAX = J
            EXIT
         ENDIF
      ENDDO
C
C
C ... セルと漂流物の重なり判定
C
      RLEN = 1.0D10
C
      DO J=JMIN,JMAX
      DO I=IMIN,IMAX
         IF( ZBT.GT.HT(NA,I,J) ) CYCLE    ! 地形より上にあるときは重なり判定不要
C
         L = 0   ! 重なり判定フラグ(一時変数)の初期化
C
C ...... (1) セルの4頂点と漂流物の重なりを判定
         A = Y2 - Y1                               ! 直線の方程式:Ax+By+C=0
         B = X1 - X2
         C = X2*Y1 - X1*Y2
         D = SQRT(A**2+B**2)                       ! 2点の距離
C
         DO M=1,4
            I0 = I-1
            J0 = J-1
            IF( M.EQ.2.OR.M.EQ.4 ) I0 = I
            IF( M.EQ.3.OR.M.EQ.4 ) J0 = J
            XX = XG(NA,I0)
            YY = YG(NA,J0)
C
            RF = (A*XX+B*YY+C)/D
            RL = ABS(RF)
            IF( RL.LT.R0 ) THEN                    ! 直線と点との距離がR0より小さいか
               E = -B*(XX-X1)+A*(YY-Y1)
               IF( E.GT.0.AND.E.LT.D*D ) THEN        ! 線分の内部にあるか
                  IF( RL.LT.RLEN ) THEN
c                     write(*,*) 'pattern 1'
                     L = 1
                     RLEN = RL
                     X = XX
                     Y = YY
                     P = -A*RF/D
                     Q = -B*RF/D
                     IWKDST = I
                     JWKDST = J
                  ENDIF
               ELSE
                  IF( E.LE.0.0D0 ) THEN
                     R = SQRT((XX-X1)**2 + (YY-Y1)**2)
                  ELSE
                     R = SQRT((XX-X2)**2 + (YY-Y2)**2)
                  ENDIF
                  IF( R.LT.R0 ) THEN            ! 線分の両端を中心とする円の内部にあるか
                     IF( R.LT.RLEN ) THEN
c                        write(*,*) 'pattern 2'
                        L = 1
                        RLEN = R
                        IF( E.LE.0.0D0 ) THEN
                           X = X1
                           Y = Y1
                           P = X1 - XX
                           Q = Y1 - YY
                           IWKDST = I
                           JWKDST = J
                        ELSE
                           X = X2
                           Y = Y2
                           P = X2 - XX
                           Q = Y2 - YY
                           IWKDST = I
                           JWKDST = J
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
C
         IF( L.EQ.1 ) THEN                     ! CHECK
            IF( ABS(P**2+Q**2-RLEN**2).GT.1.0D-10 ) THEN
               WRITE(*,*) 'RLEN,|(P,Q)|=',RLEN,SQRT(P**2+Q**2)
               CALL ERRMSG('COLLIS1',-1)
            ENDIF
c            CYCLE
         ENDIF
C
C ...... (2) セルの辺でのみ漂流物の両端の円と重なる場合
         IF( X1.GE.XG(NA,I-1).AND.X1.LT.XG(NA,I) ) THEN
            IF( ( Y1.GT.YG(NA,J-1)-R0.AND.Y1.LT.YG(NA,J)+R0 ) ) THEN
C
               IF( Y1.GT.YG(NA,J) ) THEN
                  RL = Y1-YG(NA,J)
                  IF( RL.LT.RLEN ) THEN
                     L = 1
c                     write(*,*) 'pattern 3'
                     RLEN = RL
                     X = X1
                     Y = YG(NA,J)
                     P = 0.0D0
                     Q = Y1 - YG(NA,J)
                     IWKDST = I
                     JWKDST = J
                  ENDIF
               ELSEIF( Y1.LT.YG(NA,J-1) ) THEN
                  RL = YG(NA,J-1)-Y1
                  IF( RL.LT.RLEN ) THEN
                     L = 1
c                     write(*,*) 'pattern 3'
                     RLEN = RL
                     X = X1
                     Y = YG(NA,J-1)
                     P = 0.0D0
                     Q = Y1 - YG(NA,J-1)
                     IWKDST = I
                     JWKDST = J
                  ENDIF
               ELSE
                  WRITE(IFL,*) 'N=',N
                  WRITE(IFL,*) 'X1,Y2=',X1,Y1
                  WRITE(IFL,*) 'X2,Y2=',X2,Y2
                  CALL ERRMSG('COLLIS1',1)
               ENDIF
C
            ENDIF
         ENDIF
C
         IF( Y1.GE.YG(NA,J-1).AND.Y1.LT.YG(NA,J) ) THEN
            IF( ( X1.GT.XG(NA,I-1)-R0.AND.X1.LT.XG(NA,I)+R0 ) ) THEN
C
               IF( X1.GT.XG(NA,I) ) THEN
                  RL = X1 - XG(NA,I)
                  IF( RL.LT.RLEN ) THEN
                     L = 1
c                     write(*,*) 'pattern 3'
                     RLEN = RL
                     X = XG(NA,I)
                     Y = Y1
                     P = X1 - XG(NA,I)
                     Q = 0.0D0
                     IWKDST = I
                     JWKDST = J
                  ENDIF
               ELSEIF( X1.LT.XG(NA,I-1) ) THEN
                  RL = XG(NA,I-1) - X1
                  IF( RL.LT.RLEN ) THEN
                     L = 1
c                     write(*,*) 'pattern 3'
                     RLEN = RL
                     X = XG(NA,I-1)
                     Y = Y1
                     P = X1 - XG(NA,I-1)
                     Q = 0.0D0
                     IWKDST = I
                     JWKDST = J
                  ENDIF
               ELSE
                  WRITE(IFL,*) 'N=',N
                  WRITE(IFL,*) 'X1,Y2=',X1,Y1
                  WRITE(IFL,*) 'X2,Y2=',X2,Y2
                  CALL ERRMSG('COLLIS1',2)
               ENDIF
C
            ENDIF
         ENDIF
C
         IF( X2.GE.XG(NA,I-1).AND.X2.LT.XG(NA,I) ) THEN
            IF( ( Y2.GT.YG(NA,J-1)-R0.AND.Y2.LT.YG(NA,J)+R0 ) ) THEN
C
               IF( Y2.GT.YG(NA,J) ) THEN
                  RL = Y2-YG(NA,J)
                  IF( RL.LT.RLEN ) THEN
                     L = 1
c                     write(*,*) 'pattern 3'
                     RLEN = RL
                     X = X2
                     Y = YG(NA,J)
                     P = 0.0D0
                     Q = Y2 - YG(NA,J)
                     IWKDST = I
                     JWKDST = J
                  ENDIF
               ELSEIF( Y2.LT.YG(NA,J-1) ) THEN
                  RL = YG(NA,J-1)-Y2
                  IF( RL.LT.RLEN ) THEN
                     L = 1
c                     write(*,*) 'pattern 3'
                     RLEN = RL
                     X = X2
                     Y = YG(NA,J-1)
                     P = 0.0D0
                     Q = Y2 - YG(NA,J-1)
                     IWKDST = I
                     JWKDST = J
                  ENDIF
               ELSE
                  WRITE(IFL,*) 'N=',N
                  WRITE(IFL,*) 'X1,Y2=',X1,Y1
                  WRITE(IFL,*) 'X2,Y2=',X2,Y2
                  CALL ERRMSG('COLLIS1',3)
               ENDIF
C
            ENDIF
         ENDIF
C
         IF( Y2.GE.YG(NA,J-1).AND.Y2.LT.YG(NA,J) ) THEN
            IF( ( X2.GT.XG(NA,I-1)-R0.AND.X2.LT.XG(NA,I)+R0 ) ) THEN
C
               IF( X2.GT.XG(NA,I) ) THEN
                  RL = X2 - XG(NA,I)
                  IF( RL.LT.RLEN ) THEN
                     L = 1
c                     write(*,*) 'pattern 3'
                     RLEN = RL
                     X = XG(NA,I)
                     Y = Y2
                     P = X2 - XG(NA,I)
                     Q = 0.0D0
                     IWKDST = I
                     JWKDST = J
                  ENDIF
               ELSEIF( X2.LT.XG(NA,I-1) ) THEN
                  RL = XG(NA,I-1) - X2
                  IF( RL.LT.RLEN ) THEN
                     L = 1
c                     write(*,*) 'pattern 3'
                     RLEN = RL
                     X = XG(NA,I-1)
                     Y = Y2
                     P = X2 - XG(NA,I-1)
                     Q = 0.0D0
                     IWKDST = I
                     JWKDST = J
                  ENDIF
               ELSE
                  WRITE(IFL,*) 'N=',N
                  WRITE(IFL,*) 'X1,Y2=',X1,Y1
                  WRITE(IFL,*) 'X2,Y2=',X2,Y2
                  CALL ERRMSG('COLLIS1',4)
               ENDIF
C
            ENDIF
         ENDIF
C
         IF( L.EQ.1 ) THEN                     ! CHECK
            IF( ABS(P**2+Q**2-RLEN**2).GT.1.0D-10 ) THEN
               WRITE(*,*) 'RLEN,|(P,Q)|=',RLEN,SQRT(P**2+Q**2)
               CALL ERRMSG('COLLIS1',-2)
            ENDIF
         ENDIF
C
      ENDDO
      ENDDO
C
      IF( RLEN.NE.1.0D10 ) LLD = 1
C
C<<<<< (START) STOC-BLC VERSION  <<<<<<<
      IF( LLD.EQ.1 )THEN
         X1 = XCOLLIS(1,N)
         Y1 = YCOLLIS(1,N)
         X2 = XCOLLIS(2,N)
         Y2 = YCOLLIS(2,N)
         A = Y2 - Y1                               ! 直線の方程式:Ax+By+C=0
         B = X1 - X2
         C = X2*Y1 - X1*Y2
         D = SQRT(A**2+B**2)                       ! 2点の距離
         E = -B*(X-X1)+A*(Y-Y1)
         IF( E-0.25*D*D.LT.0.0 ) LBL1(N,1) = -1
         IF( E-0.25*D*D.EQ.0.0 ) LBL1(N,1) =  0
         IF( E-0.25*D*D.GT.0.0 ) LBL1(N,1) = +1
         IF( A*X+B*Y+C.LT.0.0 ) LBL2(N,1) = -1
         IF( A*X+B*Y+C.EQ.0.0 ) LBL2(N,1) =  0
         IF( A*X+B*Y+C.GT.0.0 ) LBL2(N,1) = +1
      ENDIF
C<<<<<  (END)  STOC-BLC VERSION  <<<<<<<
C
      RETURN
      END
