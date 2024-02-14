      SUBROUTINE MKDLIST(NLIST,LIST,X1,Y1,X2,Y2,R0,MXLIST,IERR,NA)
C----------------------------------------
C     漂流物を含むセルのリストを作成
C----------------------------------------
      USE M_GRID
      USE M_OUTPUT,ONLY:IFL
C
      IMPLICIT NONE
C
      REAL(8),INTENT(IN)::X1,Y1,X2,Y2,R0
      INTEGER,INTENT(IN)::MXLIST,NA
      INTEGER,INTENT(OUT)::NLIST,LIST(2,MXLIST)
      INTEGER,INTENT(OUT)::IERR
C
      REAL(8)::XMIN,YMIN,XMAX,YMAX,XX,YY,A,B,C,D,E,RL,R
      INTEGER::IMIN,JMIN,IMAX,JMAX
      INTEGER::L,N,I,J,I0,J0
C
C
      IERR = 0
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
C
      IF( XMIN.LT.XG(NA,0) .OR.YMIN.LT.YG(NA,0) .OR.
     $    XMAX.GT.XG(NA,NI(NA)).OR.YMAX.GT.YG(NA,NJ(NA)) ) THEN
         WRITE(IFL,*) 'SHIP IS PLACED OUTSIDE OF REGION'
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
      NLIST = 0
C
      DO J=JMIN,JMAX
      DO I=IMIN,IMAX
C
         L = 0   ! 重なり判定フラグ(一時変数)の初期化
C
C ...... (1) セルの4頂点と漂流物の重なりを判定
         A = Y2 - Y1                               ! 直線の方程式:Ax+By+C=0
         B = X1 - X2
         C = X2*Y1 - X1*Y2
         D = SQRT(A**2+B**2)                       ! 2点の距離
C
         DO N=1,4
            I0 = I-1
            J0 = J-1
            IF( N.EQ.2.OR.N.EQ.4 ) I0 = I
            IF( N.EQ.3.OR.N.EQ.4 ) J0 = J
            XX = XG(NA,I0)
            YY = YG(NA,J0)
C
            RL = ABS(A*XX+B*YY+C)/MAX(D,1.D-6)
            IF( RL.LT.R0 ) THEN                   ! 直線と点との距離がR0より小さいか
               E = -B*(XX-X1)+A*(YY-Y1)
               IF( E.GT.0.AND.E.LT.D*D ) THEN       ! 線分の内部にあるか
                  L = 1
                  EXIT
               ELSE
                  IF( E.LE.0.0D0 ) THEN
                     R = (XX-X1)**2 + (YY-Y1)**2
                  ELSE
                     R = (XX-X2)**2 + (YY-Y2)**2
                  ENDIF
                  IF( R.LT.R0**2 ) THEN           ! 線分の両端を中心とする円の内部にあるか
                     L = 1
                     EXIT
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
C

C ...... (2) セルの辺でのみ漂流物の両端の円と重なる場合
         IF( L.EQ.0.AND.X1.GE.XG(NA,I-1).AND.X1.LT.XG(NA,I) ) THEN
            IF( ( Y1.GT.YG(NA,J-1)-R0.AND.Y1.LT.YG(NA,J)+R0 ) ) L=1
         ENDIF
C
         IF( L.EQ.0.AND.Y1.GE.YG(NA,J-1).AND.Y1.LT.YG(NA,J) ) THEN
            IF( ( X1.GT.XG(NA,I-1)-R0.AND.X1.LT.XG(NA,I)+R0 ) ) L=1
         ENDIF
C
         IF( L.EQ.0.AND.X2.GE.XG(NA,I-1).AND.X2.LT.XG(NA,I) ) THEN
            IF( ( Y2.GT.YG(NA,J-1)-R0.AND.Y2.LT.YG(NA,J)+R0 ) ) L=1
         ENDIF
C
         IF( L.EQ.0.AND.Y2.GE.YG(NA,J-1).AND.Y2.LT.YG(NA,J) ) THEN
            IF( ( X2.GT.XG(NA,I-1)-R0.AND.X2.LT.XG(NA,I)+R0 ) ) L=1
         ENDIF
C
         IF( L.EQ.1 ) THEN
            NLIST = NLIST+1
            IF( NLIST.GT.MXLIST ) THEN
               WRITE(*,*) 'ERROR: NLIST > MXLIST'
               CALL ERRMSG('MKDLIST',-1)
            ENDIF
            LIST(1,NLIST) = I
            LIST(2,NLIST) = J
         ENDIF
C
      ENDDO
      ENDDO
C
      RETURN
      END
