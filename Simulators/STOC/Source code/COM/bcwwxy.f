      SUBROUTINE BCWWXY(WWX,WWY,UU,VV,WW,TMU,XC,YC,ZC,KG,LLWALL,INDP,
     $                  INDU,INDV,INDW,UUBCN,VVBCN,WWBCN)
C======================================================================
C     壁面、流速固定境界および自由流出境界の接線方向流速を設定する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'BOUNDR.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TABLER.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'CADMAS.h'
      INCLUDE 'RGWAVE.h'
      REAL(8),INTENT(INOUT)::ZC(8,MZ)
C
      REAL(8),INTENT(INOUT)::WWX(MX,MY,MZ),WWY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY)
      INTEGER,INTENT(INOUT)::KG(MX,MY)
C
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UUBCN(NXY,MZ,4),VVBCN(NXY,MZ,4)
      REAL(8),INTENT(INOUT)::WWBCN(NXY,MZ,4)
C
      INTEGER::NFL=1
C
      REAL(8)::AKK,DD1,DDD,ENU,HH1,PAI,SIGMA,UU1,VEL1,VTAU,VV1
      REAL(8)::WW1,WWD,WWW
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,IDIR,IE,INB1,IS,ITYP,J,JE,JNB1,JS,K,KE,KS,M,N
C
C
      CALL ZERCLR(WWX,MXYZ,0.0D0)
      CALL ZERCLR(WWY,MXYZ,0.0D0)
C
C----------------------------------------------------------------------
C     (1) 壁面境界
C----------------------------------------------------------------------
!CDIR NODEP
      DO 100 N=1,MLWALL1
         I = LLWALL(1,N)
         J = LLWALL(2,N)
         K = LLWALL(3,N)
         IDIR = LLWALL(4,N)
         M    = LLWALL(5,N)
         ITYP = LLWALL(6,N)
C
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.0 .OR. IDIR.EQ.1 ) THEN
            INB1 = I
            IF( IDIR.EQ.1 ) INB1 = I+1
C
C ......... ノースリップ
            IF( ITYP.EQ.1 ) THEN
               WWX(I,J,K) = 0.0D0
C
C ......... 接線方向速度固定
            ELSE IF( ITYP.EQ.3 ) THEN
               IF( IWALL(3,M).EQ. 0 ) THEN
                  WWX(I,J,K) = RWALL(3,M)
               ELSE
                  WWX(I,J,K) = TABLE(IWALL(3,M))
               END IF
C
C ......... スリップ
            ELSE IF( ITYP.EQ.0 ) THEN
               WWX(I,J,K) = VSLIP
            END IF
C
C
C ...... 法線方向がY方向の面
         ELSE IF( IDIR.EQ.2 .OR. IDIR.EQ.3 ) THEN
            JNB1 = J
            IF( IDIR.EQ.3 ) JNB1 = J+1
C
C ......... ノースリップ
            IF( ITYP.EQ.1 ) THEN
               WWY(I,J,K) = 0.0D0
C
C ......... 接線方向速度固定
            ELSE IF( ITYP.EQ.3 ) THEN
               IF( IWALL(3,M).EQ. 0 ) THEN
                  WWY(I,J,K) = RWALL(3,M)
               ELSE
                  WWY(I,J,K) = TABLE(IWALL(3,M))
               END IF
C
C ......... スリップ
            ELSE IF( ITYP.EQ.0 ) THEN
               WWY(I,J,K) = VSLIP
            END IF
         END IF
  100 CONTINUE
C
C
C----------------------------------------------------------------------
C     (1-2) 壁面境界(LOG-LAW)
C           CALL LOGLAWによりベクトル化できないため分離
C----------------------------------------------------------------------
      IF( ILGLWL.EQ.1 ) THEN
      DO 110 N=1,MLWALL1
         I = LLWALL(1,N)
         J = LLWALL(2,N)
         K = LLWALL(3,N)
         IDIR = LLWALL(4,N)
         M    = LLWALL(5,N)
         ITYP = LLWALL(6,N)
C
         IF( ITYP.EQ.2 ) THEN
C
C ......... 法線方向がX方向の面
            IF( IDIR.EQ.0 .OR. IDIR.EQ.1 ) THEN
               INB1 = I
               IF( IDIR.EQ.1 ) INB1 = I+1
C
               VV1  = 0.5D0*(VV(INB1,J-1,K)+VV(INB1,J,K))
               WW1  = 0.5D0*(WW(INB1,J,K-1)+WW(INB1,J,K))
               VEL1 = SQRT(VV1*VV1+WW1*WW1)
               DD1  = 0.5D0*XC(4,I,J)
               ENU  = TMU(INB1,J,K)+ANUH
               CALL LOGLAW(VTAU,VEL1,DD1,ANUH)
               WWX(I,J,K) = WW1 - WW1/MAX(VEL1,1.0D-20)
     $                    * VTAU*VTAU/ENU*DD1
C
C ...... 法線方向がY方向の面
            ELSE IF( IDIR.EQ.2 .OR. IDIR.EQ.3 ) THEN
               JNB1 = J
               IF( IDIR.EQ.3 ) JNB1 = J+1
C
               UU1  = 0.5D0*(UU(I-1,JNB1,K)+UU(I,JNB1,K))
               WW1  = 0.5D0*(WW(I,JNB1,K-1)+WW(I,JNB1,K))
               VEL1 = SQRT(UU1*UU1+WW1*WW1)
               DD1  = 0.5D0*YC(4,J)
               ENU  = TMU(I,JNB1,K)+ANUH
               CALL LOGLAW(VTAU,VEL1,DD1,ANUH)
               WWY(I,J,K) = WW1 - WW1/MAX(VEL1,1.0D-20)
     $                    * VTAU*VTAU/ENU*DD1
            END IF
         END IF
  110 CONTINUE
      END IF
C
C
C----------------------------------------------------------------------
C     (2) 流速固定境界
C----------------------------------------------------------------------
      DO 200 N=1,NINLT
         M  = MINLT(N)
         IS = IAREA(1,M)
         IE = IAREA(2,M)
         JS = IAREA(3,M)
         JE = IAREA(4,M)
         KS = IAREA(5,M)
         KE = IAREA(6,M)
         IDIR = IAREA(7,M)
C
         IF( IINLT(3,N).EQ. 0 ) THEN
            WW1 = RINLT(3,N)
         ELSE
            WW1 = TABLE(IINLT(3,N))
         END IF
C
C// 2004.01.16
C (模型実験対応)......................................................
C        流速を時間の関数にする場合、上のinclude文を追加しUU1を計算する
C           AMP:片振幅(m) , ALL:波長(m) , TTT :周期(s) , TIME:時刻(s)
C           AKK:角波数  , SIGMA:角周波数(1/s) , DDD:位相差(rad.)
C           HHH:水深(m)
C        境界条件としては、便宜的に一定値入力としておく
C
      IF(AMP.GT.0.0D0.or.AMP.LT.0.0D0) THEN
         PAI = 3.141592653897932D0
C         AMP = 0.005D0
C         ALL = 125.21D0
C         TTT = 40.D0
C         HHH = 1.0D0
         DDD = PAI*0.5D0
         SIGMA=2.0D0*PAI/TTT
         AKK = 2.0D0*PAI/ALL
         WWW = AMP*SIGMA
         HH1 = AMP*COS(-SIGMA*TIME+DDD)
      END IF
C END//.....................................................
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.1 ) THEN
            I = IS
            DO 210 K=KS,KE
            DO 210 J=JS,JE
C ......... ( 流出時は勾配ゼロ )
CC               IF( INDU(I,J,K).EQ.-1 ) WWX(I,J,K) = WW1
               IF( INDU(I,J,K).EQ.-1 ) THEN
                 WWD = WW1
                 IF(INDP(I  ,J,K).GT.0.AND.UU(I,J,K).GT.0.0D0)
     1             WWD = 0.5D0*(WW(I  ,J,K-1)+WW(I  ,J,K))
                 IF(INDP(I+1,J,K).GT.0.AND.UU(I,J,K).LT.0.0D0)
     1             WWD = 0.5D0*(WW(I+1,J,K-1)+WW(I+1,J,K))
      IF(AMP.GT.0.0D0.or.AMP.LT.0.0D0) THEN
                 WW1 = SINH(AKK*(ZC(2,K)+HHH))/SINH(AKK*HHH)
                 WWD = WWW*WW1*SIN(-SIGMA*TIME+DDD)
CC                 IF(HH1.LT.ZC(2,K)) WWD=0.0D0
                 IF(HH1.LT.ZC(1,K-1)) WWD=0.0D0
      END IF
                 IF( NB_SC.GT.0.AND.N.EQ.1 ) WWD=UWCAD(J-JS+1,K-KS+1,3)
                 IF( NB_SC.GT.0.AND.N.EQ.2 ) WWD=UECAD(J-JS+1,K-KS+1,3)
C
                 WWX(I,J,K) = WWD
               END IF
  210       CONTINUE
C
C ...... 法線方向がY方向の面
         ELSE IF( IDIR.EQ.2 ) THEN
            J = JS
            DO 220 K=KS,KE
            DO 220 I=IS,IE
C ......... ( 流出時は勾配ゼロ )
CC               IF( INDV(I,J,K).EQ.-1 ) WWY(I,J,K) = WW1
               IF( INDV(I,J,K).EQ.-1 ) THEN
                 WWD = WW1
                 IF(INDP(I,J  ,K).GT.0.AND.VV(I,J,K).GT.0.0D0)
     1             WWD = 0.5D0*(WW(I,J  ,K-1)+WW(I,J  ,K))
                 IF(INDP(I,J+1,K).GT.0.AND.VV(I,J,K).LT.0.0D0)
     1             WWD = 0.5D0*(WW(I,J+1,K-1)+WW(I,J+1,K))
      IF(AMP.GT.0.0D0.or.AMP.LT.0.0D0) THEN
                 WW1 = SINH(AKK*(ZC(2,K)+HHH))/SINH(AKK*HHH)
                 WWD = WWW*WW1*SIN(-SIGMA*TIME+DDD)
CC                 IF(HH1.LT.ZC(2,K)) WWD=0.0D0
                 IF(HH1.LT.ZC(1,K-1)) WWD=0.0D0
      END IF
                 IF( NB_SC.GT.0.AND.N.EQ.3 ) WWD=VSCAD(I-IS+1,K-KS+1,3)
                 IF( NB_SC.GT.0.AND.N.EQ.4 ) WWD=VNCAD(I-IS+1,K-KS+1,3)
C
                 WWY(I,J,K) = WWD
               END IF
  220       CONTINUE
         END IF
  200 CONTINUE
C
C ... 外側境界面上の値をセット
C
      IF(NESTFL.GT.0) THEN
C
        IF( IPECON(5,NRANK+1).LT.0 ) THEN
            I = 1
            DO 235 K=2,MZM
            DO 230 J=2,MYM
               IF( INDU(I,J,K).EQ.-1 ) THEN
                  IF(LNTANG.EQ.1.OR.
     $              (LNTANG.EQ.2.AND.UUBCN(J,K,2).LT.0.0D0)) THEN
                     WWX(I,J,K) = 0.5D0*(WW(I+1,J,K-1)+WW(I+1,J,K))
                  ELSE
                     WWX(I,J,K) = WWBCN(J,K,2)
                  ENDIF
               ENDIF
  230       CONTINUE
  235       CONTINUE
        END IF
C
        IF( IPECON(6,NRANK+1).LT.0 ) THEN
            I = MXM
            DO 245 K=2,MZM
            DO 240 J=2,MYM
               IF( INDU(I,J,K).EQ.-1 ) THEN
                  IF(LNTANG.EQ.1.OR.
     $              (LNTANG.EQ.2.AND.UUBCN(J,K,3).GT.0.0D0)) THEN
                     WWX(I,J,K) = 0.5D0*(WW(I  ,J,K-1)+WW(I  ,J,K))
                  ELSE
                     WWX(I,J,K) = WWBCN(J,K,3)
                  ENDIF
               ENDIF
  240       CONTINUE
  245       CONTINUE
        END IF
C
        IF( IPECON(4,NRANK+1).LT.0 ) THEN
            J = 1
            DO 255 K=2,MZM
            DO 250 I=2,MXM
               IF( INDV(I,J,K).EQ.-1 ) THEN
                  IF(LNTANG.EQ.1.OR.
     $              (LNTANG.EQ.2.AND.VVBCN(I,K,1).LT.0.0D0)) THEN
                     WWY(I,J,K) = 0.5D0*(WW(I,J+1,K-1)+WW(I,J+1,K))
                  ELSE
                     WWY(I,J,K) = WWBCN(I,K,1)
                  ENDIF
               ENDIF
  250       CONTINUE
  255       CONTINUE
        END IF
C
        IF( IPECON(7,NRANK+1).LT.0 ) THEN
            J = MYM
            DO 265 K=2,MZM
            DO 260 I=2,MXM
               IF( INDV(I,J,K).EQ.-1 ) THEN
                  IF(LNTANG.EQ.1.OR.
     $              (LNTANG.EQ.2.AND.VVBCN(I,K,4).GT.0.0D0)) THEN
                     WWY(I,J,K) = 0.5D0*(WW(I,J  ,K-1)+WW(I,J  ,K))
                  ELSE
                     WWY(I,J,K) = WWBCN(I,K,4)
                  ENDIF
               ENDIF
  260       CONTINUE
  265       CONTINUE
        END IF
C
      END IF
C
C----------------------------------------------------------------------
C     (3) 自由流入出境界
C----------------------------------------------------------------------
C     NFL = 1 ( WWX,WWYを使用せず )
C
      IF(NFL.EQ.0) THEN
C
      DO 300 N=1,NOUTLT
         M  = MOUTLT(N)
         IS = IAREA(1,M)
         IE = IAREA(2,M)
         JS = IAREA(3,M)
         JE = IAREA(4,M)
         KS = IAREA(5,M)
         KE = IAREA(6,M)
         IDIR = IAREA(7,M)
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.1 ) THEN
            I = IS
            DO 310 K=KS,KE
            DO 310 J=JS,JE
               IF( INDP(I,J,K).GT.0 ) THEN
                  WWX(I,J,K) = 0.5D0*(WW(I  ,J,K-1)+WW(I  ,J,K))
               ELSE
                  WWX(I,J,K) = 0.5D0*(WW(I+1,J,K-1)+WW(I+1,J,K))
               END IF
  310       CONTINUE
C
C ...... 法線方向がY方向の面
         ELSE IF( IDIR.EQ.2 ) THEN
            J = JS
            DO 320 K=KS,KE
            DO 320 I=IS,IE
               IF( INDP(I,J,K).GT.0 ) THEN
                  WWY(I,J,K) = 0.5D0*(WW(I,J  ,K-1)+WW(I,J  ,K))
               ELSE
                  WWY(I,J,K) = 0.5D0*(WW(I,J+1,K-1)+WW(I,J+1,K))
               END IF
  320       CONTINUE
         END IF
  300 CONTINUE
C
      END IF
C
      RETURN
      END
